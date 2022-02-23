#include "face.h"
#include "type.h"
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/errno.h>
#include <setjmp.h>
#include <libgen.h>

int help[NUMFILE] = {0};
int anon[NUMFILE] = {0};
int fifo[NUMFILE] = {0};
int give[NUMFILE] = {0};
off_t tail[NUMFILE] = {0};
pthread_t thread[NUMFILE] = {0};
jmp_buf jmpbuf[NUMFILE] = {0};
int number[NUMOPEN] = {0};
jmp_buf errbuf = {0};
long long identifier = 0;
int face = 0;
int fieldsiz = sizeof(struct File);
int filesiz = FILESIZE - FILESIZE%sizeof(struct File);
int halfsiz = (FILESIZE/2) - (FILESIZE/2)%(FILESIZE - FILESIZE%sizeof(struct File));
double amount = BACKOFF;
extern int bufsize;

void spokErr(const char *str, int num, int arg)
{
	longjmp(jmpbuf[number[arg]],1);
}

void hubErr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void fileStr(const char *str, int trm, void *arg)
{
	struct File *command = arg;
	command->trm = trm;
	allocStr(&command->str,str);
}

int readGive(long long loc, long long pid, int siz, int idx)
{
	struct File command = {0};
	command.act = StrThd;
	while (1) {
		rdlkwFile(loc,siz,give[idx]);
		preadStr(fileStr,&command,give[idx],loc,siz);
		unlkFile(loc,siz,give[idx]);
		if (command.trm == 1) break;
		if (strlen(command.str) != siz) {
			freeFile(&command); return -1;}
		siz += bufsize;}
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	writeFile(&command,anon[idx]);
	siz = strlen(command.str);
	freeFile(&command);
	return siz;
}

void writeGive(long long loc, long long pid, const char *str, int idx)
{
	struct File command = {0};
	int siz = strlen(str);
	wrlkwFile(loc,fieldsiz,give[idx]);
	pwriteStr(str,1,idx,loc,siz);
	unlkFile(loc,fieldsiz,give[idx]);
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	allocStr(&command.str,str);
	writeFile(&command,anon[idx]);
	freeFile(&command);
}

void appendGive(long long pid, const char *str, int idx)
{
	struct File command = {0};
	int siz = strlen(str);
	long long loc = -1;
	while (checkFile(idx) != loc) {
		loc = checkFile(idx);
		wrlkwFile(loc,siz,give[idx]);}
	pwriteStr(str,1,idx,loc,siz);
	unlkFile(loc,fieldsiz,give[idx]);
	command.act = ThdHub;
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
#define SLF ptr->slf
#define STR ptr->str
#define NXT(LOC,INC) (LOC+INC*fieldsiz)%filesiz
#define TAIL tail[IDX]
#define NEXT NXT(TAIL,1)
#define HELP help[IDX]
#define FIFO fifo[IDX]
#define GIVE give[IDX]
#define ANON anon[IDX]
#define THRD thread[IDX]
#define JBUF jmpbuf[IDX]

void *func(void *arg)
{
	struct File *ptr = arg;
	struct File temp = {0};
	double backoff = 0.0;
	if (setjmp(JBUF) != 0) return 0;
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
		(siz = readGive(loc,0,bufsize,IDX)) != -1;
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
	if (temp.act == HubThd) writeGive(temp.loc,temp.pid,temp.str,IDX);
	else appendGive(temp.pid,temp.str,IDX);
	writeHelp(temp.loc,temp.pid,strlen(temp.str),TAIL,IDX);
	unlkFile(TAIL,fieldsiz,HELP);
	// next is write locked
	TAIL = NEXT;
	// previous is write locked
	goto goWrite;
	return 0;
}

int main(int argc, char **argv)
{
	if (argc != 4) ERROR(exitErr,-1);
	while (!identifier) identifier = ((long long)getpid()<<(sizeof(long long)/2))+(long long)time(0);
	if ((face = pipeInit(argv[1],argv[2])) < 0) ERROR(exitErr,-1);
	readJump(hubErr,face); writeJump(hubErr,face);
	struct File *ptr = 0; allocFile(&ptr,1);
	if (setjmp(errbuf) != 0) ERROR(exitErr,-1)
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(ptr,sub);
	switch (ACT) {
		case (NewHub): {
		int len = strlen(STR);
		char basestr[len+1];
		char dirstr[len+1];
		char name[len+4];
		if (IDX < 0 || IDX >= NUMFILE) ERROR(hubErr,-1)
		strcpy(basestr,basename(STR));
		strcpy(dirstr,dirname(STR));
		if (checkRead(GIVE)) ERROR(hubErr,-1)
		strcat(strcat(strcpy(name,dirstr),"/"),basestr);
		if (findIdent(name) != -1) ERROR(hubErr,-1)
		if ((GIVE = openFile(name)) == -1) ERROR(hubErr,-1)
		else {number[GIVE] = IDX; readJump(spokErr,GIVE); writeJump(spokErr,GIVE);}
		strcat(strcat(strcpy(name,dirstr),"/."),basestr);
		if ((FIFO = openAtom(name)) == -1) ERROR(hubErr,-1)
		else {number[FIFO] = IDX; readJump(spokErr,FIFO); writeJump(hubErr,FIFO);}
		strcat(strcat(strcpy(name,dirstr),"/.."),basestr);
		if ((HELP = openFile(name)) == -1) ERROR(hubErr,-1)
		else {number[HELP] = IDX; readJump(spokErr,HELP); writeJump(spokErr,HELP);}
		if ((ANON = openPipe()) == -1) ERROR(hubErr,-1)
		else {number[ANON] = IDX; readJump(hubErr,ANON); writeJump(spokErr,ANON);}
		if (pthread_create(&THRD,0,func,ptr) != 0) ERROR(hubErr,-1)
		allocFile(&ptr,1);
		break;}
		case (CfgHub): case (AppHub): {
		if (sub != face) ERROR(hubErr,-1)
		ACT = (ACT == CfgHub ? HubThd : AppThd );
		PID = identifier;
		writeFile(ptr,FIFO);
		flushBuf(FIFO);}
		case (ThdHub): {
		if (sub != ANON) ERROR(hubErr,-1)
		ACT = HubCfg;
		SLF = (PID == identifier);
		writeFile(ptr,face);}
		default: {
		ERROR(hubErr,-1)
		break;}}}
	return 0;
}
