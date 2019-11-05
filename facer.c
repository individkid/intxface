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
#include <fcntl.h>

int errcheck = 0;
int exccheck = 0;
char buffer[BUFSIZE] = {0};
void errfunc(const char *str, int num, int arg)
{
	errcheck = arg;
}
void excfunc(const char *str, int num, int arg)
{
	closeIdent(arg);
	exccheck++;
}
void buffunc(char *buf, int trm, void *arg)
{
	strncpy(buffer,buf,strlen(buf)+trm);
}

int main(int argc, char **argv)
{
	if (argc == 4) {
	pipeInit(argv[1],argv[2]);
	int val = readInt(0);
	writeInt(val,0);
	double num = readNum(0);
	writeNum(num,0);
	readStr(buffunc,0,0);
	const char *str = buffer;
	writeStr(str,1,0);
	strcpy(buffer,"oops");
	long long nnm = readNew(0);
	writeNew(nnm,0);
	float old = readOld(0);
	writeOld(old,0);
	return 0;}
	if (forkExec("facerC") != 0) {printf("a.out\n"); return -1;}
	if (forkExec("facerHs") != 1) {printf("b.out\n"); return -1;}
	if (forkExec("facerLua") != 2) {printf("facer.ex\n"); return -1;}
	for (int i = 0; i < 3; i++) readNote(excfunc,i);
	int handle = openFile("oops.txt"); bothJump(errfunc,handle);
	sleepSec(1);
	int expectInt[] = {0,1,2};
	double expectNum[] = {0.1,1.1,2.1};
	const char *expectStr[] = {"zero","one","two"};
	long long expectNew[] = {10,11,12};
	float expectOld[] = {0.2,1.2,2.2};
	for (int index = 0; index < 3; index++) {
		writeInt(expectInt[index],index);
		writeNum(expectNum[index],index);
		writeStr(expectStr[index],1,index);
		writeNew(expectNew[index],index);
		writeOld(expectOld[index],index);}
	int done[3] = {0};
	for (int index = waitAny(); index >= 0; index = waitAny()) {
	printf("index %d done %d\n",index,done[index]);
	switch (done[index]) {
	case (0): {
		int value = readInt(index);
		if (value != expectInt[index]) {printf("mismatch %d %d %d\n",value,index,done[index]); return -1;}
		done[index]++; break;}
	case (1): {
		double value = readNum(index);
		if (value != expectNum[index]) {printf("mismatch %f %d %d\n",value,index,done[index]); return -1;}
		done[index]++; break;}
	case (2): {
		readStr(buffunc,0,index);
		const char *value = buffer;
		if (strcmp(value,expectStr[index]) != 0) {printf("mismatch %s %d %d\n",value,index,done[index]); return -1;}
		strcpy(buffer,"oops");
		done[index]++; break;}
	case (3): {
		long long value = readNew(index);
		if (value != expectNew[index]) {printf("mismatch %lld %d %d\n",value,index,done[index]); return -1;}
		done[index]++; break;}
	case (4): {
		float value = readOld(index);
		if (value != expectOld[index]) {printf("mismatch %f %d %d\n",value,index,done[index]); return -1;}
		done[index]++; break;}
	default: {
		readInt(index);
		break;}}}
	char empty[1] = {0}; writeStr(empty,1,handle);
	seekFile(0,handle); readInt(handle);
	return (checkRead(0)||checkRead(1)||checkRead(2)||
		checkWrite(0)||checkWrite(1)||checkWrite(2)||
		errcheck!=handle || exccheck != 3) ? -1 : 0;
}
