#include "face.h"
#include "type.h"
#include <unistd.h>

int help[NUMFILE] = {0};
int anon[NUMFILE] = {0};
int fifo[NUMFILE] = {0};
int give[NUMFILE] = {0};
off_t readee[NUMFILE] = {0};
off_t writee[NUMFILE] = {0};
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
		preadFile(fileStr,&command,idx,loc,siz);
		unlkFile(loc,siz,give[idx]);
		if (command.loc == -1) return 0;
		siz *= 2;}
	writeFile(&command,anon[idx]);
	siz = strlen(command.str);
	freeFile(&command);
	return siz;
}

void *func(void *arg)
{
	struct File *ptr = arg;
// open files
	char name[strlen(ptr->str)+3];
	for (int i = 0; i < 2; i++) name[i] = '.';
	strcpy(name+2,ptr->str);
	help[ptr->idx] = openFile(name);
	fifo[ptr->idx] = openAtom(name+1);
	give[ptr->idx] = openFile(name+2);
// find head with read lock on previous head and head
	// read given and write to anonymous pipe
	for (long long siz = 0, loc = 0; (siz = lockRead(ptr->idx,loc,0,BUFSIZE)); loc += siz);
// use head locks as tail locks

// goon to read from here
// if tail is invalid goto retry
// read from helper at tail
// lock read from given at indicated location
// write to anonymous pipe
// release lock on previous tail
// wait for read lock on tail
// goon to read

// retry from here
// release lock on tail but keep previous to prevent wrap
// try for write lock on role and ready bytes in given
// goto switch to read if try failed
// goto switch to write if try succeeded

// switch to read from here
// wait for and release read lock on ready byte
// wait for read lock on tail
// goon to read

// switch to write from here
// release lock on previous
// wait for write lock on tail
// release write lock from ready byte

// goon to write from here
// wait for named pipe readable
// read from named pipe
// lock write to given at indicated location
// write to anonymous
// write indication to helper at tail
// wait for write lock on tail
// releasae lock on previous tail
// goon to write

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
		anon[ptr->idx] = openPipe();
		if (pthread_create(&thread[ptr->idx],0,func,ptr->str) < 0) ERROR(huberr,-1)
		allocFile(&ptr,1);
		break;
	}
	return 0;
}
