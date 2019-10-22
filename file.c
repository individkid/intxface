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

int face = 0;
int anonym[BUFSIZE] = {0};
int number[BUFSIZE] = {0};
int given[BUFSIZE] = {0};
int named[BUFSIZE] = {0};
int helper[BUFSIZE] = {0};
int seqnum[BUFSIZE] = {0};
int append[BUFSIZE] = {0};
pthread_t thread[BUFSIZE] = {0};

void *file(void *arg)
{
	int idx = (int)arg;
	return 0;
}

#define BIGPTR
#ifdef BIGPTR
#define VOIDARG(x) ((void*)(long long)(x))
#else
#define VOIDARG(x) ((void*)(x))
#endif

int main(int argc, char **argv)
{
	if (argc != 4) ERROR
	face = pipeInit(argv[1],argv[2]);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
		struct File command;
		readFile(&command,sub);
		if (sub == face && named[command.idx] <= 0) {
			if (command.idx < 0 || command.idx >= BUFSIZE) ERROR
			int fd[2];
			if (pipe(fd) < 0) ERROR
			anonym[command.idx] = addPipe(fd[0],fd[1]);
			number[anonym[command.idx]] = command.idx;
			// regular open command.str[0] into given[command.idx]
			int fi,fo;
			// fifo open concat(".",command.str[0]) into fi and fo
			named[command.idx] = addPipe(fi,fo);
			// regular open or create concat("..",command.str[0]) into helper[command.idx]
			// read from start of helper[command.idx] for seqnum, or use default
			// lseek end of helper[command.idx] into append
			// pthread create with command.idx as argument
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
