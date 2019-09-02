/*
*    facer.c
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

#include "face.h"
#include <stdio.h>

int main(int argc, char **argv)
{
	printf("start spoke%s\n",argv[3]);
	pipeInit(argv[1],argv[2]);
	int val = readInt(0);
	printf("spoke%s %d\n",argv[3],val);
	writeInt(val,0);
	double num = readNum(0);
	printf("spoke%s %f\n",argv[3],num);
	writeNum(num,0);
	const char *str = readString(0);
	printf("spoke%s %s\n",argv[3],str);
	writeString(str,0);
	printf("spoke done%s\n",argv[3]);
	return 0;
}