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
#include <string.h>

int main(int argc, char **argv)
{
	if (argc == 4) {
	// printf("start spoke%s\n",argv[3]);
	pipeInit(argv[1],argv[2]);
	int val = readInt(0);
	// printf("spoke%s %d\n",argv[3],val);
	writeInt(val,0);
	double num = readNum(0);
	// printf("spoke%s %f\n",argv[3],num);
	writeNum(num,0);
	const char *str = readStr(0);
	// printf("spoke%s %s\n",argv[3],str);
	writeStr(str,0);
	// printf("spoke done%s\n",argv[3]);
	return 0;}
	// printf("start hub\n");
	forkExec("a.out");
	forkExec("b.out");
	forkExec("facer.lua");
	sleepSec(1);
	int expectInt[] = {0,1,2};
	double expectNum[] = {0.1,1.1,2.1};
	const char *expectStr[] = {"zero","one","two"};
	for (int index = 0; index < 3; index++) {
		writeInt(expectInt[index],index);
		writeNum(expectNum[index],index);
		writeStr(expectStr[index],index);}
	int done[3] = {0};
	for (int index = waitAny(); index < 3; index = waitAny()) switch (done[index]) {
	case (0): {
		int value = readInt(index);
		// printf("hub %d %d\n",index,value);
		if (value != expectInt[index]) {printf("mismatch %d %d\n",value,index); return -1;}
		done[index]++; break;}
	case (1): {
		double value = readNum(index);
		// printf("hub %d %f\n",index,value);
		if (value != expectNum[index]) {printf("mismatch %f %d\n",value,index); return -1;}
		done[index]++; break;}
	case (2): {
		const char *value = readStr(index);
		// printf("hub %d %s\n",index,value);
		if (strcmp(value,expectStr[index]) != 0) {printf("mismatch %s %d\n",value,index); return -1;}
		done[index]++; break;}
	default: {
		readInt(index); writeInt(-1,index);
		// printf("hub %d done\n",index);
		break;}}
	printf("hub done %d %d %d %d %d %d\n",
	checkRead(0),checkRead(1),checkRead(2),
	checkWrite(0),checkWrite(1),checkWrite(2));
	return 0;
}
