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
#include <signal.h>

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
int maxsiz = BUFSIZE;
char *errstr = 0;

void errErr(const char *str, int num, int arg)
{
	if (arg == face) exitErr(str,num);
	exitErr(str,num);
	asprintf(&errstr,"spokeErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	longjmp(jmpbuf[number[arg]],1);
}

void errNote(int arg)
{
	if (arg == face) exit(0);
	asprintf(&errstr,"spokeErr %d %lld\n",errno,(long long)getpid());
	longjmp(jmpbuf[number[arg]],1);
}

void hubErr(const char *str, int num)
{
	asprintf(&errstr,"hubErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	longjmp(errbuf,1);
}

void hubSig(int sig)
{
	for (int i = 0; i < NUMFILE; i++)
	if (thread[i]) {
	if (pthread_cancel(thread[i]) < 0) ERROR();
	if (pthread_join(thread[i],0) < 0) ERROR();}
	fprintf(stderr,"hubSig %d\n",sig); fflush(stderr);
	stackErr();
	exit(-1);
}

void spokeSig(void *arg)
{
	for (int i = 0; i < NUMFILE; i++)
	if (thread[i] && pthread_equal(pthread_self(),thread[i])) {
	fprintf(stderr,"spokeSig %d\n",i); fflush(stderr);
	stackErr();}
}

int fileTerm(const char *str, int len, int idx)
{
	int num = 0;
	if (*userIdent(idx) == 0) {
	if (strnlen(str,len) == len) return 0;
	return strlen(str) + 1;}
	for (int i = 0; i < len; i++) {
		if (str[i] == '(') num++;
		if (str[i] == ')' && num == 0) return -1;
		if (str[i] == ')' && num == 1) return i+1;
		if (str[i] == ')' && num > 1) num--;}
	return 0;
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
	int siz = maxsiz;
	int val = 0;
	char *str = 0;
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	while (1) {
		rdlkwFile(loc,siz,give[idx]);
		preadStr(&str,loc,give[idx]);
		unlkFile(loc,siz,give[idx]);
		if (*str == 0) {freeFile(&command); return -1;}
		val = strlen(str);
		if (val <= siz) break;
		siz = val;}
	assignStr(&command.str,str);
	free(str);
	writeFile(&command,anon[idx]);
	freeFile(&command);
	return val;
}

void writeGive(long long loc, long long pid, const char *str, int idx)
{
	struct File command = {0};
	int siz = strlen(str);
	wrlkwFile(loc,siz+1,give[idx]);
	pwriteStr(str,loc,give[idx]);
	unlkFile(loc,siz+1,give[idx]);
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	assignStr(&command.str,str);
	writeFile(&command,anon[idx]);
	freeFile(&command);
}

void appendGive(long long pid, const char *str, int idx)
{
	struct File command = {0};
	int siz = strlen(str);
	long long loc = -1;
	while (checkFile(give[idx]) != loc) {
		loc = checkFile(give[idx]);
		wrlkwFile(loc,siz+1,give[idx]);}
	pwriteStr(str,loc,give[idx]);
	unlkFile(loc,siz+1,give[idx]);
	command.act = ThdHub;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	assignStr(&command.str,str);
	writeFile(&command,anon[idx]);
	freeFile(&command);
}

void writeHelp(long long loc, long long pid, int tail, int idx)
{
	struct File command = {0};
	command.act = ThdThd;
	command.loc = loc;
	command.pid = pid;
	seekFile(tail,help[idx]);
	writeFile(&command,help[idx]);
}

void readHelp(struct File *command, int loc, int idx)
{
	seekFile(loc,help[idx]);
	readFile(command,help[idx]);
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
#define TAIL tail[IDX]
#define NEXT (tail[IDX]+fieldsiz)%filesiz
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
	sigset_t mask = {0};
	sigemptyset(&mask); sigaddset(&mask,SIGINT);
	if (pthread_sigmask(SIG_BLOCK,&mask,0) < 0) {writeThd(IDX); return 0;}
	pthread_cleanup_push(spokeSig,0);
	if (setjmp(JBUF) != 0) {writeThd(IDX); goto goExit;}
	for (TAIL = filesiz; TAIL == filesiz; sleepSec(backoff += amount)) {
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
		loc += siz);
	backoff = 0.0;
	goto goRead;

	toRead:
	// previous and next are read locked
	unlkFile(NEXT,fieldsiz,HELP);
	// previous is read locked 
	if (wrlkFile(filesiz,1,HELP)) {backoff = 0.0; goto toWrite;}

	goRead:
	// previous is read locked
	rdlkwFile(NEXT,fieldsiz,HELP);
	// previous and next are read locked
	if (!checkHelp(NEXT,IDX)) {
	sleepSec(backoff += amount); goto toRead;}
	else backoff = 0.0;
	readHelp(&temp,NEXT,IDX);
	readGive(temp.loc,temp.pid,IDX);
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
	writeHelp(temp.loc,temp.pid,TAIL,IDX);
	unlkFile(TAIL,fieldsiz,HELP);
	// next is write locked
	TAIL = NEXT;
	// previous is write locked
	goto goWrite;

	goExit:
	pthread_cleanup_pop(0);
	return 0;
}

int main(int argc, char **argv)
{
	struct sigaction act;
	act.__sigaction_u.__sa_handler = hubSig;
	if (sigaction(SIGINT,&act,0) < 0) ERROR();
	while (!identifier) identifier = ((long long)getpid()<<(sizeof(long long)/2))+(long long)time(0);
	if ((face = wrapIdent(Filez,argv[1])) < 0) exitErr(__FILE__,__LINE__);
	termFunc(fileTerm); noteFunc(errNote); errFunc(errErr);
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
		else {number[GIVE] = IDX; *userIdent(GIVE) = (void*)1;}
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
		ACT = (ACT == CfgHub ? HubThd : AppThd);
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
