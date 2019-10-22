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

#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include "face.h"
#include "type.h"

int face = 0;
int anonym[BUFSIZE] = {0};
int number[BUFSIZE] = {0};
int given[BUFSIZE] = {0};
int named[BUFSIZE] = {0};
int helper[BUFSIZE] = {0};
int seqnum[BUFSIZE] = {0};
int append[BUFSIZE] = {0};

int main(int argc, char **argv)
{
	if (argc != 4) ERROR
	face = pipeInit(argv[1],argv[2]);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
		struct File command;
		readFile(&command,sub);
		if (sub == face && named[command.idx] <= 0) {
			int fd[2];
			if (pipe(fd) < 0) ERROR
			anonym[command.idx] = addPipe(fd[0],fd[1]);
			number[anonym[command.idx]] = command.idx;
			// regular open command.str[0] into given[command.idx]
			// fifo open concat(".",command.str[0]) into named[command.idx]
			// regular open or create concat("..",command.str[0]) into helper[command.idx]
			// read from start of helper[command.idx] for seqnum, or use default
			// lseek end of helper[command.idx] into append
			// pthread create with command.idx as argument
		} else if (sub == face) {
			// write command to named[command.idx]
		} else {
			command.idx = number[sub];
			writeFile(&command,face);
		}
	}
	return 0;
}
