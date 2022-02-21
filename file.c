#include "face.h"
#include "type.h"
#include <unistd.h>

int help[NUMFILE] = {0};
int anon[NUMFILE] = {0};
int fifo[NUMFILE] = {0};
int give[NUMFILE] = {0};
off_t tail[NUMFILE] = {0};
pthread_t thread[NUMFILE] = {0};
long long identifier = 0;
int face = 0;
int fieldsiz = sizeof(struct File);
int filesiz = FILESIZE - FILESIZE%fieldsiz;
int halfsiz = (FILESIZE/2) - (FILESIZE/2)%fieldsiz;
int safesiz = 3;
double amount = BACKOFF;
int bufsize = BUFSIZE;

void fileStr(const char *str, int trm, void *arg)
{
	struct File command *ptr = arg;
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

void readHelp(struct File *command, int tail, int idx)
{
	seekFile(tail,help[idx]);
	readFile(command,help[idx]);
}

int checkHelp(int loc, int idx)
{
	// TODO return 1 if loc is valid ThdThd
	return 0;
}

void clearHelp(int loc, int idx)
{
	// TODO write lock loc and mark as invalid ThdThd
}

#define IDX ptr->idx
#define NXT(LOC,INC) (LOC+INC*fieldsiz)%filesiz
#define TAIL tail[IDX]
#define NEXT NXT(TAIL,1)
#define HELP help[IDX]
#define FIFO fifo[IDX]
#define GIVE give[IDX]

void *func(void *arg)
{
	struct File *ptr = arg;
	struct File temp = {0};
	double backoff = 0.0;
	char name[strlen(ptr->str)+3];
	// open files
	for (int i = 0; i < 2; i++) name[i] = '.';
	strcpy(name+2,ptr->str);
	HELP = openFile(name);
	FIFO = openAtom(name+1);
	GIVE = openFile(name+2);
	for (int loc = 0; loc < filesiz; loc += fieldsiz) {
		TAIL = loc;
		if (rdlckFile(loc,fieldsiz,HELP)) {
			if (checkHelp(loc,IDX) && !checkHelp(NXT(loc,1),IDX)) break;
			else unlckFile(loc,fieldsiz,HELP);}}
	// previous is read locked
	for (int siz = 0, loc = 0;
		(siz = readGive(loc,0,bufsize,IDX));
		loc += siz);

	goRole:
	// previous and next are read locked
	unlckFile(NEXT,fieldsiz,HELP);
	// previous is read locked 
	sleepSec(backoff); backoff += amount;
	if (wrlckFile(filesiz,2,HELP)) {
		backoff = 0.0; goto toWrite;}

	goRead:
	// previous is read locked
	rdlckwFile(NEXT,fieldsiz,HELP);
	// previous and next are read locked
	if (!checkHelp(NEXT,IDX)) goto goRole;
	else backoff = 0.0;
	readHelp(&temp,NEXT,IDX);
	readGive(temp.loc,temp.pid,fieldsiz,IDX);
	unlckFile(TAIL,fieldsiz,HELP);
	// next is locked
	TAIL = NEXT;
	// previous is read locked
	goto goRead;

	toWrite:
	// previous is read locked and filesiz+1 is write locked
	unlckFile(TAIL,fieldsiz,HELP);
	// filesize+1 is write locked
	TAIL = NEXT;
	wrlckwFile(TAIL,fieldsiz,HELP);
	unlckFile(fieldsiz+1,1,HELP);
	// invalid previous is write locked

	goWrite:
	// invalid previous is write locked
	for (int loc = NEXT, num = 0;
		num < safesiz;
		loc = NXT(loc,1))
		clearHelp(loc,IDX);
	readFile(&temp,FIFO);
	writeGive(temp.loc,temp.pid,temp.str,IDX);	
	writeHelp(temp.loc,temp.pid,strlen(temp.str,IDX),TAIL);
	// valid previous is write locked
	wrlckwFile(NEXT,fieldsiz,HELP);
	// valid previous and invalid next are write locked
	unlckFile(TAIL,HELP);
	// invalid next is write locked
	TAIL = NEXT;
	// invalid previous is write locked
	goto goWrite;
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	while (!identifier) identifier = ((long long)getpid()<<(sizeof(long long)/2))+(long long)time(0);
	if ((face = pipeInit(argv[1],argv[2])) < 0) ERROR(0,-1);
	struct File *ptr = 0; allocFile(&ptr,1);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(ptr,sub);
	switch (ptr->act) {
		case(NewThd):
		// TODO duplicate from other idx instead of starting new thread if file already open
		anon[ptr->idx] = openPipe();
		if (pthread_create(&thread[ptr->idx],0,func,ptr->str) < 0) ERROR(huberr,-1)
		allocFile(&ptr,1);
		break;
	}
	return 0;
}
