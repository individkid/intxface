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

#define VOIDARG(x) ((void*)(((char*)(0))+(x)))
#define ARGVOID(x) (((char*)(x))-((char*)(0)))

void *file(void *arg)
{
	int idx = ARGVOID(arg);
	int given = 0;
	int helper = 0;
	int seqnum = 0;
	if ((given = open(name[idx]+2,O_RDWR|O_CREAT,0666)) < 0) ERROR
	helper = addFile(-1,-1);
	while (1) {
		off_t append = 0;
		off_t config = 0;
		struct File command = {0};
		int fd = 0;
		if ((fd = open(name[idx]+0,O_RDWR|O_CREAT,0666)) < 0) ERROR
		setFile(fd,fd,helper);
		while (todoFile(helper) < sizeof(int)) {
			// writelock and writeInt seqnum
		}
		seqnum = readInt(helper);
		while (1) {
			// todoFile keeping buffer ahead of command
		}
		while (1) {
			while (1) {
				// writelock or readlock for commands
			}
			// reopen helper and check seqnum
		}
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
			name[command.idx] = malloc(strlen(command.str[0])+3);
			name[command.idx][0] = name[command.idx][1] = '.';
			strcat(name[command.idx],command.str[0]);
			int fi,fo;
			if (mkfifo(name[command.idx]+1,0666) < 0) ERROR
			if ((fi = open(name[command.idx]+1,O_RDONLY)) < 0) ERROR
			if ((fo = open(name[command.idx]+1,O_WRONLY)) < 0) ERROR
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
