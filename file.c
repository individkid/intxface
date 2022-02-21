#include "face.h"
#include "type.h"
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/errno.h>

int help[NUMFILE] = {0};
int anon[NUMFILE] = {0};
int fifo[NUMFILE] = {0};
int give[NUMFILE] = {0};
off_t tail[NUMFILE] = {0};
pthread_t thread[NUMFILE] = {0};
long long identifier = 0;
int face = 0;
int fieldsiz = sizeof(struct File);
int filesiz = FILESIZE - FILESIZE%sizeof(struct File);
int halfsiz = (FILESIZE/2) - (FILESIZE/2)%(FILESIZE - FILESIZE%sizeof(struct File));
double amount = BACKOFF;
extern int bufsize;

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

void huberr(const char *str, int num, int arg)
{
	exit(arg);
}

void fileStr(const char *str, int trm, void *arg)
{
	struct File *command = arg;
	if (!*str && !trm) command->loc = -1;
	else if (!trm) allocStr(&command->str,0);
	else allocStr(&command->str,str);
}

long long readGive(long long loc, long long pid, long long fieldsiz, int idx)
{
	struct File command = {0};
	command.act = ThdCmd;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	while (command.str == 0) {
		rdlkwFile(loc,fieldsiz,give[idx]);
		preadStr(fileStr,&command,idx,loc,fieldsiz);
		unlkFile(loc,fieldsiz,give[idx]);
		if (command.loc == -1) return 0;
		fieldsiz *= 2;}
	writeFile(&command,anon[idx]);
	fieldsiz = strlen(command.str);
	freeFile(&command);
	return fieldsiz;
}

void writeGive(long long loc, long long pid, const char *str, int idx)
{
	struct File command = {0};
	wrlkwFile(loc,fieldsiz,give[idx]);
	pwriteStr(str,1,idx,loc,strlen(str));
	unlkFile(loc,fieldsiz,give[idx]);
	command.act = ThdCmd;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	allocStr(&command.str,str);
	writeFile(&command,anon[idx]);
	freeFile(&command);
}

void writeHelp(long long loc, long long pid, long long siz, int tail, int idx)
{
	struct File command = {0};
	command.act = ThdThd;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	command.siz = siz;
	seekFile(tail,help[idx]);
	writeFile(&command,help[idx]);
}

void readHelp(struct File *command, int loc, int idx)
{
	seekFile(loc,help[idx]);
	readFile(command,help[idx]);
}

int checkHelp(int loc, int siz, int idx)
{
	struct File command = {0};
	if (checkFile(help[idx]) < loc+siz) return 0;
	seekFile(loc,help[idx]);
	readFile(&command,help[idx]);
	return (command.pid != 0);
}

void clearHelp(int loc, int idx)
{
	struct File command = {0};
	command.act = ThdThd;
	seekFile(loc,help[idx]);
	writeFile(&command,help[idx]);
}

#define ACT ptr->act
#define IDX ptr->idx
#define PID ptr->pid
#define STR ptr->str
#define NXT(LOC,INC) (LOC+INC*fieldsiz)%filesiz
#define TAIL tail[IDX]
#define NEXT NXT(TAIL,1)
#define HELP help[IDX]
#define FIFO fifo[IDX]
#define GIVE give[IDX]
#define ANON anon[IDX]
#define THRD thread[IDX]

void *func(void *arg)
{
	struct File *ptr = arg;
	struct File temp = {0};
	double backoff = 0.0;
	for (TAIL = filesiz; TAIL == filesiz || (backoff = 0.0); sleepSec(backoff += amount)) {
		for (int loc = 0; loc < filesiz; loc += fieldsiz) {
			if (rdlkFile(loc,fieldsiz,HELP)) {
				if (TAIL != filesiz) unlkFile(TAIL,fieldsiz,HELP);
				TAIL = loc;
				if (!checkHelp(TAIL,fieldsiz,IDX)) clearHelp(TAIL,IDX);
				else if (!checkHelp(NEXT,fieldsiz,IDX)) break;}
			else if (TAIL != filesiz) break;}}
	// previous is read locked
	for (int siz = 0, loc = 0;
		(siz = readGive(loc,0,bufsize,IDX));
		loc += siz);
	goto goRead;

	toRead:
	// previous and next are read locked
	unlkFile(NEXT,fieldsiz,HELP);
	// previous is read locked 
	if (wrlkFile(filesiz,1,HELP)) goto toWrite;

	goRead:
	// previous is read locked
	rdlkwFile(NEXT,fieldsiz,HELP);
	// previous and next are read locked
	if (!checkHelp(NEXT,fieldsiz,IDX)) {
	sleepSec(backoff); backoff += amount; goto toRead;}
	else backoff = 0.0;
	readHelp(&temp,NEXT,IDX);
	readGive(temp.loc,temp.pid,fieldsiz,IDX);
	unlkFile(TAIL,fieldsiz,HELP);
	// next is read locked
	TAIL = NEXT;
	// previous is read locked
	goto goRead;

	toWrite:
	// previous is read locked
	unlkFile(TAIL,fieldsiz,HELP);
	// nothing is read locked
	TAIL = NEXT;
	wrlkwFile(TAIL,fieldsiz,HELP);
	// previous is write locked
	clearHelp(TAIL,IDX);

	goWrite:
	// previous is write locked
	wrlkwFile(NEXT,fieldsiz,HELP);
	// previous and next are write locked
	clearHelp(NEXT,IDX);
	readFile(&temp,FIFO);
	writeGive(temp.loc,temp.pid,temp.str,IDX);
	writeHelp(temp.loc,temp.pid,strlen(temp.str),TAIL,IDX);
	unlkFile(TAIL,fieldsiz,HELP);
	// next is write locked
	TAIL = NEXT;
	// previous is write locked
	goto goWrite;
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	while (!identifier) identifier = ((long long)getpid()<<(sizeof(long long)/2))+(long long)time(0);
	if ((face = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	struct File *ptr = 0; allocFile(&ptr,1);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(ptr,sub);
	switch (ACT) {
		case (NewThd): {
		// TODO check that idx is unused
		// TODO duplicate from other idx instead of starting new thread if file already open
		char name[strlen(STR)+3];
		for (int i = 0; i < 2; i++) name[i] = '.';
		strcpy(name+2,STR);
		GIVE = openFile(name+2);
		FIFO = openAtom(name+1);
		HELP = openFile(name);
		ANON = openPipe();
		if (pthread_create(&THRD,0,func,ptr) < 0) ERROR(huberr,-1)
		allocFile(&ptr,1);
		break;}
		case (CmdThd): {
		if (sub != face) ERROR(huberr,-1)
		PID = identifier;
		writeFile(ptr,FIFO);
		flushBuf(FIFO);}
		case (ThdCmd): {
		if (sub != ANON) ERROR(huberr,-1)
		PID = (PID == identifier);
		writeFile(ptr,face);}
		default: {
		ERROR(huberr,-1)
		break;}}}
	return 0;
}
