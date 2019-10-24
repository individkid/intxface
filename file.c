/*
*    file.c
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "type.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/errno.h>
#include "face.h"
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

int face = 0;
int anonym[BUFSIZE] = {0};
int number[BUFSIZE] = {0};
char *name[BUFSIZE] = {0};
int named[BUFSIZE] = {0};
pthread_t thread[BUFSIZE] = {0};

#define INFINITE 1000000000ull
#define FILESIZE 1000
#define VOIDARG(x) ((void*)(((char*)(0))+(x)))
#define ARGVOID(x) (((char*)(x))-((char*)(0)))
#define NAMES 4
#define GIVEN 3
#define NAMED 2
#define HELPER 1
#define CONTROL 0

struct flock *ctlck(struct flock *lock, off_t pos)
{
	lock->l_start = pos;
	lock->l_len = 1;
	lock->l_type = F_WRLCK;
	lock->l_whence = SEEK_SET;
	return lock;
}

struct flock *wrlck(struct flock *lock, off_t pos)
{
	lock->l_start = pos;
	lock->l_len = INFINITE;
	lock->l_type = F_WRLCK;
	lock->l_whence = SEEK_SET;
	return lock;
}

struct flock *rdlck(struct flock *lock, off_t pos)
{
	lock->l_start = pos;
	lock->l_len = 1;
	lock->l_type = F_RDLCK;
	lock->l_whence = SEEK_SET;
	return lock;
}

struct flock *delck(struct flock *lock, off_t pos)
{
	lock->l_start = pos;
	lock->l_len = INFINITE;
	lock->l_type = F_UNLCK;
	lock->l_whence = SEEK_SET;
	return lock;	
}

struct flock *unlck(struct flock *lock, off_t pos)
{
	lock->l_start = pos;
	lock->l_len = 1;
	lock->l_type = F_UNLCK;
	lock->l_whence = SEEK_SET;
	return lock;	
}

void *file(void *arg)
{
	int idx = ARGVOID(arg);
	int given = 0;
	int control = 0;
	int helper = 0;
	int seqnum = 0;
	struct flock lock = {0};
	helper = addFile(-1,-1);
	if ((given = open(name[idx]+GIVEN,O_RDWR|O_CREAT,0666)) < 0) ERROR
	if ((control = open(name[idx]+CONTROL,O_RDWR|O_CREAT,0666)) < 0) ERROR
	if (fcntl(control,F_SETLKW,ctlck(&lock,0)) < 0) ERROR
	off_t config = 0;
	int stage = 0;
	int fdesc = 0;
	while (1) {
		int fd = 0;
		if ((fd = open(name[idx]+HELPER,O_RDWR|O_CREAT,0666)) < 0) ERROR
		if (fdesc > 0) close(fdesc);
		fdesc = fd;
		setFile(fd,fd,helper);
		if (todoFile(helper) < sizeof(int)) {
			seqnum = seqnum + 1;
			writeInt(seqnum,helper);
		} else {
			int saved = seqnum;
			seqnum = readInt(helper);
			if (seqnum != saved + 1) {config = 0; stage = 0;}
		}
		if (fcntl(control,F_SETLK,unlck(&lock,0)) < 0) ERROR
		off_t append = 0;
		struct File command = {0};
		while (append < FILESIZE) {
			if (stage == 0) {
				// todoFile keeping buffer ahead of command
				// setting stage = 1 upon buffer eof
			} else {
				// writelock or readlock for commands
			}
			writeFile(&command,named[idx]);
		}
		if (fcntl(control,F_SETLKW,ctlck(&lock,0)) < 0) ERROR
		if (unlink(name[idx]+HELPER) < 0 && errno != ENOENT) ERROR
	}
	return 0;
}

int main(int argc, char **argv)
{
	if (argc != 4) ERROR
	face = pipeInit(argv[1],argv[2]);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
		struct File command;
		readFile(&command,sub);
		if (sub == face && named[command.idx] <= 0) {
			if (command.idx < 0 || command.idx >= BUFSIZE) ERROR
			if (command.num != 1) ERROR
			int fd[2];
			if (pipe(fd) < 0) ERROR
			anonym[command.idx] = addPipe(fd[0],fd[1]);
			number[anonym[command.idx]] = command.idx;
			name[command.idx] = malloc(strlen(command.str[0])+NAMES);
			for (int i = 0; i < NAMES-1; i++) name[command.idx][i] = '.';
			strcpy(name[command.idx]+GIVEN,command.str[0]);
			int fi,fo;
			if (mkfifo(name[command.idx]+NAMED,0666) < 0) ERROR
			if ((fi = open(name[command.idx]+NAMED,O_RDONLY)) < 0) ERROR
			if ((fo = open(name[command.idx]+NAMED,O_WRONLY)) < 0) ERROR
			named[command.idx] = addPipe(fi,fo);
			if (pthread_create(&thread[command.idx],0,file,VOIDARG(command.idx)) < 0) ERROR
		} else if (sub == face) {
			writeFile(&command,named[command.idx]);
		} else {
			// check for close file command, clean up, and zero out array entries
			command.idx = number[sub];
			writeFile(&command,face);
		}
	}
	return 0;
}
