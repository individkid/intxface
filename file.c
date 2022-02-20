#include "face.h"
#include "type.h"
#include <unistd.h>

int help[NUMFILE] = {0};
int anon[NUMFILE] = {0};
int fifo[NUMFILE] = {0};
int give[NUMFILE] = {0};
off_t head[NUMFILE] = {0};
off_t tail[NUMFILE] = {0};
pthread_t thread[NUMFILE] = {0};
long long identifier = 0;
int face = 0;

void fileStr(const char *str, int trm, void *arg)
{
	struct File command *ptr = arg;
	if (!*str && !trm) command->loc = -1;
	else if (!trm) allocStr(&command->str,0);
	else allocStr(&command->str,str);
}

long long lockRead(int idx, long long loc, long long pid, long long siz)
{
	struct File command = {0};
	command.act = ThdCmd;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	while (command.str == 0) {
		rdlkwFile(loc,siz,give[idx]);
		preadStr(fileStr,&command,idx,loc,siz);
		unlkFile(loc,siz,give[idx]);
		if (command.loc == -1) return 0;
		siz *= 2;}
	writeFile(&command,anon[idx]);
	siz = strlen(command.str);
	freeFile(&command);
	return siz;
}

void lockWrite(int idx, long long loc, long long pid, const char *str)
{
	struct File command = {0};
	wrlkwFile(loc,siz,give[idx]);
	pwriteStr(str,1,idx,loc,strlen(str));
	unlkFile(loc,siz,give[idx]);
	command.act = ThdCmd;
	command.idx = idx;
	command.loc = loc;
	command.pid = pid;
	allocStr(&command.str,str);
	writeFile(&command,anon[idx]);
	freeFile(&command);
}

void *func(void *arg)
{
	struct File *ptr = arg;
	int siz = sizeof(struct File);
	int filesiz = FILESIZE - FILESIZE%siz;
	int next = 0;
	struct File temp = {0};
	int save = 0;
	// open files
	char name[strlen(ptr->str)+3];
	for (int i = 0; i < 2; i++) name[i] = '.';
	strcpy(name+2,ptr->str);
	help[ptr->idx] = openFile(name);
	fifo[ptr->idx] = openAtom(name+1);
	give[ptr->idx] = openFile(name+2);
	// find head with read lock on previous head and head
	head[ptr->idx] = filesiz;
	for (int loc = 0, hlf = halfsiz; head[ptr->idx] == filesiz; loc=(loc+siz)%filesiz, hlf=(hlf+siz)%filesiz) {
		if (rdlckFile(loc,siz,help[ptr->idx]) && !validHelp(loc,ptr->idx)) head[ptr->idx] = loc;
		else unlkFile(loc,siz,help[ptr->idx]);
		if (rdlckFile(hlf,siz,help[ptr->idx]) && !validHelp(hlf,ptr->idx)) head[ptr->idx] = hlf;
		else unlkFile(hlf,siz,help[ptr->idx]);}
	for (int ofs = 0; ofs < filesiz; ofs += siz) {
		next = (head[ptr->idx]+siz)%filesiz;
		if (validHelp(next,ptr->idx)) break;
		if (!rdlckFile(next,siz,help[ptr->idx])) break;
		unlkFile(head[ptr->idx],help[ptr->idx]);
		head[ptr->idx] = next;}
	// lock read given and write anonymous
	for (long long siz = 0, loc = 0; (siz = lockRead(ptr->idx,loc,0,BUFSIZE)); loc += siz);
	// use head locks as tail locks
	tail[ptr->idx] = head[ptr->idx];

	// goon to read from here
	goRead:
	// wait for read lock on tail
	next = (tail[ptr->idx]+siz)%filesiz;
	rdlckwFile(next,siz,help[ptr->idx]);
	// if tail is invalid goto retry
	if (!validHelp(next,ptr->idx)) goto goRole;
	// read from helper at tail
	seekFile(next,help[ptr->idx]);
	readFile(&temp,help[ptr->idx]);
	// lock read given and write anonymous
	lockRead(ptr->idx,temp.loc,temp.pid,temp.siz);
	// release lock on previous tail
	unlckFile(tail[ptr->idx],siz,help[ptr->idx]);
	tail[ptr->idx] = next;
	// goon to read
	goto goRead;

	// retry from here
	goRole:
	// release lock on tail but keep previous to prevent wrap
	unlckFile(next,siz,help[ptr->idx]);
	// try for write lock on role and ready bytes in given
	// goto switch to read if try failed
	// goto switch to write if try succeeded
	if (wrlckFile(INFINITE,2,give[ptr->idx])) goto toRead; else goto toWrite;

	// switch to read from here
	toRead:
	// wait for and release read lock on ready byte
	rdlckwFile(INFINITE+1,1,give[ptr->idx]);
	unlckFile(INFINITE+1,1,give[ptr->idx])
	// goon to read
	goto goRead;

	// switch to write from here
	toWrite:
	// release lock on previous tail
	unlckFile(tail[ptr->idx],siz,help[ptr->idx]);
	tail[ptr->idx] = next;
	// wait for write lock on next tail
	wrlckwFile(tail[ptr->idx],siz,help[ptr->idx]);
	// release write lock from ready byte
	unlckFile(INFINITE+1,1,give[ptr->idx]);

	// goon to write from here
	goWrite:
	// wait read from named pipe
	readFile(&temp,fifo[ptr->idx]);
	// lock write given and write anonymous
	lockWrite(ptr->idx,temp.loc,temp.pid,temp.str);	
	// write indication to helper at tail
	save = strlen(temp.str);
	freeFile(&temp);
	temp.act = ThdThd;
	temp.siz = save;
	seekFile(tail[ptr->idx],help[ptr->idx]);
	writeFile(&temp,help[ptr->idx]);
	// wait for write lock on next tail
	next = (tail[ptr->idx]+siz)%filesiz;
	wrlckwFile(next,siz,help[ptr->idx]);
	// releasae lock on previous tail
	unlckFile(tail[ptr->idx],help[ptr->idx]);
	tail[ptr->idx] = next;
	// goon to write
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
