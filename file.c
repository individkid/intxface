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
#include <setjmp.h>

#define NUMFILE 128
int face = 0;
int anonym[NUMFILE] = {0};
int number[NUMOPEN] = {0};
char *name[NUMFILE] = {0};
int named[NUMFILE] = {0};
int valid[NUMFILE] = {0};
pthread_t thread[NUMFILE] = {0};
long long identifier = 0;
jmp_buf jmpbuf[NUMFILE] = {0};
jmp_buf errbuf = {0};

#define INFINITE 1000000000ull
#define FILESIZE 1000
#define CMDSIZE 100
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

void spokerr(int arg)
{
	longjmp(jmpbuf[number[arg]],1);
}

void huberr(int arg)
{
	longjmp(errbuf,1);
}

void filerr(int arg)
{
	longjmp(jmpbuf[arg],1);
}

void exiterr(int arg)
{
	exit(-1);
}

void *file(void *arg)
{
	int idx = ARGVOID(arg);
	int given = 0;
	int control = 0;
	int helper = 0;
	int seqnum = 0;
	struct flock lock = {0};
	struct File command = {0};
	char buffer[CMDSIZE] = {0};
	int fdesc = 0;
if (setjmp(jmpbuf[idx]) == 0) {
	if ((helper = addFile(-1)) < 0) ERROR(filerr,idx)
	number[helper] = idx; bothJump(spokerr,helper);
	writeJump(spokerr,anonym[idx]);
	number[named[idx]] = idx; readJump(spokerr,named[idx]);
	if ((given = open(name[idx]+GIVEN,O_RDWR|O_CREAT,0666)) < 0) ERROR(filerr,idx)
	if ((control = open(name[idx]+CONTROL,O_RDWR|O_CREAT,0666)) < 0) ERROR(filerr,idx)
	if (fcntl(control,F_SETLKW,ctlck(&lock,0)) < 0) ERROR(filerr,idx)
	off_t config = 0;
	int stage = 0;
	while (1) {
		int fd = 0;
		if ((fd = open(name[idx]+HELPER,O_RDWR|O_CREAT,0666)) < 0) ERROR(filerr,idx)
		if (fdesc > 0 && close(fdesc) < 0) ERROR(filerr,idx)
		fdesc = fd;
		setFile(fd,helper);
		if (todoFile(helper)) {
			int saved = seqnum;
			seqnum = readInt(helper);
			if (seqnum != saved + 1) {config = 0; stage = 0;}
		} else {
			seqnum = seqnum + 1;
			writeInt(seqnum,helper);
		}
		if (fcntl(control,F_SETLK,unlck(&lock,0)) < 0) ERROR(filerr,idx)
		off_t append = 0;
		int valid = 0;
		int size = 0;
		while (append < FILESIZE) {
			if (stage == 0) {
				if (!valid && todoFile(helper)) {
					readFile(&command,helper);
					valid = 1;
					size = command.num - 1;
					for (int i = 0; i < command.num; i++) size += strlen(command.str[i]);
				}
				if (valid && config >= command.loc + size) {
					writeFile(&command,anonym[idx]);
					valid = 0;
				} else {
					int len = 0;
					if ((len = read(given,buffer,CMDSIZE-1)) < 0) ERROR(filerr,idx)
					if (len == 0) {
						if (valid) writeFile(&command,anonym[idx]);
						valid = 0;
						stage = 1;
						break;
					}
					buffer[len] = 0;
					char *ptr = buffer;
					struct File temp = {0};
					temp.num = 0;
					for (int i = 0; i < len;) {
						int size = 0;
						size = strlen(ptr)+1;
						temp.str[temp.num] = ptr;
						ptr += size; i += size; temp.num++;
					}
					temp.idx = idx;
					temp.loc = config;
					writeFile(&temp,anonym[idx]);
					config += len;
				}
			} else {
				int val = 0;
				val = fcntl(helper,F_SETLK,wrlck(&lock,append));
				if (val < 0 && errno != EAGAIN) ERROR(filerr,idx)
				if (val < 0) {
					if (fcntl(helper,F_SETLKW,rdlck(&lock,append)) < 0) ERROR(filerr,idx)
					if (todoFile(helper)) {
						readFile(&command,helper);
						writeFile(&command,anonym[idx]);
						if (fcntl(helper,F_SETLK,unlck(&lock,append)) < 0) ERROR(filerr,idx)
						append += sizeFile(&command);
					} else {
						if (fcntl(helper,F_SETLK,unlck(&lock,append)) < 0) ERROR(filerr,idx)
					}
				} else {
					if (todoFile(helper)) {
						if (fcntl(helper,F_SETLK,delck(&lock,append)) < 0) ERROR(filerr,idx)
					} else {
						readFile(&command,named[idx]);
						if (command.num == 0 && command.loc == identifier) {
							if (fdesc) close(fdesc);
							if (given) close(given);
							if (control) close(control);
							return 0;
						}
						for (int i = 0; i < command.num; i++) {
							int len = strlen(command.str[i]);
							if (i < command.num-1) len++;
							if (write(given,command.str[i],len) < len) ERROR(filerr,idx)
						}
						writeFile(&command,helper);
						writeFile(&command,anonym[idx]);
						if (fcntl(helper,F_SETLK,delck(&lock,append)) < 0) ERROR(filerr,idx)
						append += sizeFile(&command);
					}
				}
			}
		}
		if (fcntl(control,F_SETLKW,ctlck(&lock,0)) < 0) ERROR(filerr,idx)
		if (unlink(name[idx]+HELPER) < 0) ERROR(filerr,idx)
	}
} else {
	writeJump(0,anonym[idx]);
	if (fdesc && close(fdesc) < 0) ERROR(exiterr,0)
	if (given && close(given) < 0) ERROR(exiterr,0)
	if (control && close(control) < 0) ERROR(exiterr,0)
	command.num = 0;
	writeFile(&command,anonym[idx]);
}
	return 0;
}

void cleanup(int sub)
{
	if (pthread_join(thread[sub],0) < 0) ERROR(huberr,-1)
	closeIdent(anonym[sub]);
	closeIdent(named[sub]);
	valid[sub] = 0;
}

void finish(int sub)
{
	struct File command = {0};
	command.num = 0;
	command.loc = identifier;
	writeFile(&command,named[sub]);
	cleanup(sub);
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	struct File command = {0};
	int fd[2] = {0};
	int fi = 0;
	int fo = 0;
	// TODO identifier = pid + seconds-since-publish
	if ((face = pipeInit(argv[1],argv[2])) < 0) return -1;
if (setjmp(errbuf) == 0) {
	bothJump(huberr,face);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
		readFile(&command,sub);
		if (sub == face && command.idx >= 0 && command.idx < NUMFILE && !valid[command.idx]) {
			valid[command.idx] = 1;
			if (command.num != 1) ERROR(huberr,-1)
			if (pipe(fd) < 0) ERROR(huberr,-1)
			if ((anonym[command.idx] = addPipe(fd[0],fd[1])) < 0) ERROR(huberr,-1)
			readJump(huberr,anonym[command.idx]);
			if ((name[command.idx] = malloc(strlen(command.str[0])+NAMES)) == 0) ERROR(huberr,-1)
			for (int i = 0; i < NAMES-1; i++) name[command.idx][i] = '.';
			strcpy(name[command.idx]+GIVEN,command.str[0]);
			if ((mkfifo(name[command.idx]+NAMED,0666) < 0) && errno != EEXIST) ERROR(huberr,-1)
			if ((fi = open(name[command.idx]+NAMED,O_RDONLY)) < 0) ERROR(huberr,-1)
			if ((fo = open(name[command.idx]+NAMED,O_WRONLY)) < 0) ERROR(huberr,-1)
			if ((named[command.idx] = addPipe(fi,fo)) < 0) ERROR(huberr,-1)
			writeJump(huberr,named[command.idx]);
			number[anonym[command.idx]] = command.idx;
			if (pthread_create(&thread[command.idx],0,file,VOIDARG(command.idx)) < 0) ERROR(huberr,-1)
		} else if (sub == face && command.idx >= 0 && command.idx < NUMFILE && command.num == 0) {
			finish(command.idx);
		} else if (sub == face && command.num == 0) {
			for (int i = 0; i < NUMFILE; i++) if (valid[i]) finish(i);
			return 0;
		} else if (sub == face) {
			if (command.idx < 0) ERROR(huberr,-1)
			if (command.idx >= NUMFILE) ERROR(huberr,-1)
			writeFile(&command,named[command.idx]);
		} else {
			command.idx = number[sub];
			if (command.num == 0) cleanup(command.idx);
			writeFile(&command,face);
		}
	}
} else {
	writeJump(0,face);
	command.idx = -1;
	command.num = 0;
	writeFile(&command,face);
}
	return -1;
}
