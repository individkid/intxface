#include "face.h"
#include <string.h>
#include <stdio.h>

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
void buffunc(const char *buf, int trm, void *arg)
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
	int size = 0;
	if (forkExec("facerC") != size++) {printf("a.out\n"); return -1;}
	if (forkExec("facerLua") != size++) {printf("facer.ex\n"); return -1;}
	for (int i = 0; i < size; i++) readNote(excfunc,i);
	int handle = openFile("oops.tmp"); bothJump(errfunc,handle);
	sleepSec(1);
	int expectInt[] = {0,1,2};
	double expectNum[] = {0.1,1.1,2.1};
	const char *expectStr[] = {"zero","one","two"};
	long long expectNew[] = {10,11,12};
	float expectOld[] = {0.2,1.2,2.2};
	for (int index = 0; index < size; index++) {
		writeInt(expectInt[index],index);
		writeNum(expectNum[index],index);
		writeStr(expectStr[index],1,index);
		writeNew(expectNew[index],index);
		writeOld(expectOld[index],index);}
	int done[3] = {0};
	for (int index = waitAny(); index >= 0; index = waitAny()) {
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
	for (int i = 0; i < size; i++)
		if (checkRead(i)||checkWrite(i)) return -1;
	return (errcheck!=handle || exccheck != 2) ? -1 : 0;
}
