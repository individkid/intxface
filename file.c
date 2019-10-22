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

#include <unistd.h>
#include "face.h"

int sub = 0;
int size = 0;
int anonym[BUFSIZE] = {0};
int given[BUFSIZE] = {0};
int named[BUFSIZE] = {0};
int helper[BUFSIZE] = {0};
int seqnum[BUFSIZE] = {0};
int append[BUFSIZE] = {0};

int main(int argc, char **argv)
{
	if (argc != 4) ERROR
	sub = pipeInit(argv[1],argv[2]);
	if (0) { // command.idx >= size
		int fd[2];
		if (pipe(fd) < 0) ERROR
		anonym[size] = addPipe(fd);
		// regular open command.str[0] into given[size]
		// fifo open concat(".",command.str[0]) into named[size]
		// regular open or create concat("..",command.str[0]) into helper[size]
		// read from start of helper[size] for seqnum, or use default
		// lseek end of helper[size] into append
		// pthread create with size as argument
		size++;
	}
	return 0;
}
