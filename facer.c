#include "face.h"
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int errcheck = -1;
int exccheck = 0;
void errfunc(const char *str, int num, int arg)
{
	errcheck = arg;
}
void excfunc(int arg)
{
	closeIdent(arg);
	exccheck++;
}

int main(int argc, char **argv)
{
	if (argc == 4) {
	pipeInit(argv[1],argv[2]);
	int val = readInt(0);
	writeInt(val,0);
	double num = readNum(0);
	writeNum(num,0);
	char *str = 0;
	readStr(&str,0);
	writeStr(str,0);
	free(str);
	long long nnm = readNew(0);
	writeNew(nnm,0);
	float old = readOld(0);
	writeOld(old,0);
	return 0;}
	int size = 0;
	if (forkExec("facerC") != size++) {fprintf(stderr,"facerC: cannot execute file: facerC\n"); return -1;}
	if (forkExec("facerLua") != size++) {fprintf(stderr,"facerC: cannot execute file: facerLua\n"); return -1;}
	if (forkExec("facerHs") != size++) {fprintf(stderr,"facerC: cannot execute file: facerHs\n"); return -1;}
	for (int i = 0; i < size; i++) noteFunc(excfunc);
	int handle = openFile("oops.tmp"); errFunc(errfunc); errFunc(errfunc);
	int hello = openFile("hello.tmp");
	int ok = openFifo("ok.tmp");
	int again = openFifo("again.tmp");
	sleepSec(1);
	int expectInt[] = {0,1,2};
	double expectNum[] = {0.1,1.1,2.1};
	const char *expectStr[] = {"zero","one","two"};
	long long expectNew[] = {10,11,12};
	float expectOld[] = {0.2,1.2,2.2};
	for (int index = 0; index < size; index++) {
		writeInt(expectInt[index],index);
		writeNum(expectNum[index],index);
		writeStr(expectStr[index],index);
		writeNew(expectNew[index],index);
		writeOld(expectOld[index],index);
	}
	int done[3] = {0};
	for (int index = waitRead(0.0,-1); index >= 0; index = waitRead(0.0,-1)) {
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
		char *str = 0;
		readStr(&str,index);
		if (strcmp(str,expectStr[index]) != 0) {printf("mismatch %s %d %d\n",str,index,done[index]); return -1;}
		free(str);
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
		readInt(index); // increment exccheck
		break;}}}
	char empty[1] = {0}; writeStr(empty,handle);
	seekFile(0,handle); readInt(handle);
	for (int i = 0; i < size; i++)
		if (checkRead(i)||checkWrite(i)) return -1;
	writeStr("hello ok again and again",hello);
	pwriteStr("again",strlen("hello ok "),hello);
	char *str = 0;
	preadStr(&str,strlen("hello ok again")+1,hello);
	if (strcmp(str,"and again") != 0) {printf("mismatch %s\n",str); return -1;}
	preadStr(&str,0,hello);
	if (strcmp(str,"hello ok again") != 0) {printf("mismatch %s\n",str); return -1;}
	free(str); str = 0;
	writeInt(123,ok); flushBuf(ok);
	int fifo = readInt(ok);
	writeInt(456,again); flushBuf(again);
	int atom = readInt(again);
	if (fifo != 123 || atom != 456 || errcheck!=handle ||
	exccheck != size+1/*+1 for notice on partial read that caused exception*/) {
	printf("mismatch %d %d %d %d %d %d\n",fifo,atom,errcheck,handle,exccheck,size); return -1;}
	return 0;
}
