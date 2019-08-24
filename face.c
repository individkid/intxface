/*
*    face.c
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

#define ERROR {fprintf(stderr,"%s(%d): %s\n",,__FILE__,__LINE__,strerror(errno));exit(-1);}

int inp = -1;
int oup = -1;

void forkExec(const char *exe)
{
	int ifd[2], ofd[2], val;
	pid_t pid;
	val = pipe(ifd); if (val < 0) ERROR
	val = pipe(ofd); if (val < 0) ERROR
	pid = fork(); if (pid < 0) ERROR
	if (pid == 0) {
		char ist[33], ost[33];
		val = close(ifd[0]); if (val < 0) ERROR
		val = close(ofd[1]); if (val < 0) ERROR
		val = snprintf(ist,32,"%d",ifd[1]); if (val < 0 || val > 32) ERROR
		val = snprintf(ost,32,"%d",ofd[0]); if (val < 0 || val > 32) ERROR
		val = execl(exe,exe,ist,ost,0); if (val < 0) ERROR
		ERROR
	}
	val = close(ifd[1]); if (val < 0) ERROR
	val = close(ofd[0]); if (val < 0) ERROR
	inp = ifd[0];
	oup = ofd[1];
}