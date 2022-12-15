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
int fieldsiz = 0;
int filesiz = 0;
double amount = BACKOFF;
extern int bufsize;
char *errstr = 0;

void errNote(const char *str, int num, int arg)
{
	if (arg == face) exit(-1);
	asprintf(&errstr,"spokeErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	longjmp(jmpbuf[number[arg]],1);
}

void hubErr(const char *str, int num)
{
	asprintf(&errstr,"hubErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	longjmp(errbuf,1);
}

void writeThd(int idx)
{
	struct File command = {0};
	command.act = ThdErr;
	command.idx = idx;
	assignStr(&command.str,errstr);
	writeFile(&command,anon[idx]);
}

void writeHub()
{
	struct File command = {0};
	command.act = HubErr;
	assignStr(&command.str,errstr);
	writeFile(&command,face);
}

int readGive(long long loc, long long pid, int idx)
{
	struct File command = {0};
	struct Text text = {0};
	int siz = bufsize;
	int val = 0;
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	text.trm = 1;
	text.str = &command.str;
	while (1) {
		rdlkwFile(loc,siz+1,give[idx]);
		preadStr(textStr,&text,loc,give[idx]);
		unlkFile(loc,siz+1,give[idx]);
		val = strlen(command.str);
		if (val <= siz && text.trm == 1) break;
		if (val <= siz) {freeFile(&command); return -1;}
		siz = val;}
	// fprintf(stderr,"readGive %lld %s\n",command.loc,command.str); fflush(stderr);
	writeFile(&command,anon[idx]);
	freeFile(&command);
	return val;
}

void writeGive(long long loc, long long pid, const char *str, int idx)
{
	struct File command = {0};
	int siz = strlen(str);
	wrlkwFile(loc,siz+1,give[idx]);
	pwriteStr(str,1,loc,give[idx]);
	unlkFile(loc,siz+1,give[idx]);
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	assignStr(&command.str,str);
	// fprintf(stderr,"writeGive loc %lld\n",loc); fflush(stderr);
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
		wrlkwFile(loc,siz+1,give[idx]);}
	pwriteStr(str,1,loc,give[idx]);
	unlkFile(loc,siz+1,give[idx]);
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	assignStr(&command.str,str);
	// fprintf(stderr,"appendGive loc %lld\n",loc); fflush(stderr);
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
	// fprintf(stderr,"writeHelp tail %d loc %lld\n",tail,loc); fflush(stderr);
	writeFile(&command,help[idx]);
}

void readHelp(struct File *command, int loc, int idx)
{
	seekFile(loc,help[idx]);
	readFile(command,help[idx]);
	// fprintf(stderr,"readHelp tail %d loc %lld\n",loc,command->loc); fflush(stderr);
}

int checkHelp(int loc, int idx)
{
	struct File command = {0};
	int siz = fieldsiz;
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
	if (setjmp(JBUF) != 0) {writeThd(IDX); return 0;}
	// fprintf(stderr,"%d func\n",getpid()); fflush(stderr);
	for (TAIL = filesiz; TAIL == filesiz || (backoff = 0.0); sleepSec(backoff += amount)) {
		for (int loc = 0; loc < filesiz; loc += fieldsiz) {
			if (rdlkFile(loc,fieldsiz,HELP)) {
				if (TAIL != filesiz) unlkFile(TAIL,fieldsiz,HELP);
				TAIL = loc;
				if (!checkHelp(TAIL,IDX)) clearHelp(TAIL,IDX);
				else if (!checkHelp(NEXT,IDX)) break;}
			else if (TAIL != filesiz) break;}}
	// previous is read locked
	for (int siz = 0, loc = 0;
		(siz = readGive(loc,0,IDX)) != -1;
		loc += siz+1);
	goto goRead;

	toRead:
	// fprintf(stderr,"%d toRead\n",getpid()); fflush(stderr);
	// previous and next are read locked
	unlkFile(NEXT,fieldsiz,HELP);
	// previous is read locked 
	if (wrlkFile(filesiz,1,HELP)) goto toWrite;

	goRead:
	// fprintf(stderr,"%d goRead\n",getpid()); fflush(stderr);
	// previous is read locked
	rdlkwFile(NEXT,fieldsiz,HELP);
	// previous and next are read locked
	if (!checkHelp(NEXT,IDX)) {
	sleepSec(backoff); backoff += amount; goto toRead;}
	else backoff = 0.0;
	readHelp(&temp,NEXT,IDX);
	readGive(temp.loc,temp.pid,IDX);
	unlkFile(TAIL,fieldsiz,HELP);
	// next is read locked
	TAIL = NEXT;
	// previous is read locked
	goto goRead;

	toWrite:
	// fprintf(stderr,"%d toWrite\n",getpid()); fflush(stderr);
	// previous is read locked
	unlkFile(TAIL,fieldsiz,HELP);
	// nothing is read locked
	TAIL = NEXT;
	wrlkwFile(TAIL,fieldsiz,HELP);
	// previous is write locked
	clearHelp(TAIL,IDX);

	goWrite:
	// fprintf(stderr,"%d goWrite\n",getpid()); fflush(stderr);
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
	if (argc != 4) exitErr(__FILE__,__LINE__);
	while (!identifier) identifier = ((long long)getpid()<<(sizeof(long long)/2))+(long long)time(0);
	if ((face = pipeInit(argv[1],argv[2])) < 0) exitErr(__FILE__,__LINE__);
	noteFunc(errNote); errFunc(errNote);
	struct File *ptr = 0; allocFile(&ptr,1);
	ptr->act = ThdThd; fieldsiz = sizeFile(ptr);
	for (IDX = 0; IDX < NUMFILE; IDX++) GIVE = -1;
	filesiz = FILESIZE - FILESIZE%fieldsiz;
	if (setjmp(errbuf) != 0) {writeHub(); return -1;}
	for (int sub = waitRead(0.0,-1); sub >= 0; sub = waitRead(0.0,-1)) {
	readFile(ptr,sub);
	switch (ACT) {
		case (NewHub): {
		int len = strlen(STR);
		char basestr[len+1];
		char dirstr[len+1];
		char name[len+4];
		if (IDX < 0 || IDX >= NUMFILE) hubErr(__FILE__,__LINE__);
		strcpy(basestr,basename(STR));
		strcpy(dirstr,dirname(STR));
		if (checkRead(GIVE)) hubErr(__FILE__,__LINE__);
		strcat(strcat(strcpy(name,dirstr),"/"),basestr);
		if (findIdent(name) != -1) hubErr(__FILE__,__LINE__);
		if ((GIVE = openFile(name)) == -1) hubErr(__FILE__,__LINE__);
		else number[GIVE] = IDX;
		strcat(strcat(strcpy(name,dirstr),"/."),basestr);
		if ((FIFO = openFifo(name)) == -1) hubErr(__FILE__,__LINE__);
		else number[FIFO] = IDX;
		strcat(strcat(strcpy(name,dirstr),"/.."),basestr);
		if ((HELP = openFile(name)) == -1) hubErr(__FILE__,__LINE__);
		else number[HELP] = IDX;
		if ((ANON = openPipe()) == -1) hubErr(__FILE__,__LINE__);
		else number[ANON] = IDX;
		if (pthread_create(&THRD,0,func,ptr) != 0) hubErr(__FILE__,__LINE__);
		allocFile(&ptr,1);
		break;}
		case (CfgHub): case (AppHub): {
		if (sub != face) hubErr(__FILE__,__LINE__);
		ACT = (ACT == CfgHub ? HubThd : AppThd );
		PID = identifier;
		writeFile(ptr,FIFO);
		flushBuf(FIFO);
		break;}
		case (ThdHub): {
		if (sub != ANON) hubErr(__FILE__,__LINE__);
		ACT = HubCfg;
		SLF = (PID == identifier);
		writeFile(ptr,face);
		break;}
		case (ThdErr): {
		writeFile(ptr,face);
		break;}
		default: {
		hubErr(__FILE__,__LINE__);
		break;}}}
	return 0;
}
