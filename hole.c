#include "face.h"
#include "type.h"
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>

jmp_buf errbuf = {0};
int mapping[NUMINET][NUMOPEN] = {0}; // to NUMOPEN
int address[NUMINET][NUMOPEN] = {0}; // to NUMINET
int number[NUMINET] = {0}; // to NUMOPEN
enum {Unused,Cluster,System,Server,Client} layer[NUMINET] = {0};
// only fub on System layer
// only hub on Cluster layer
// sub might be any of Server to main
// sub might be any of Client of main

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

int splitStr(char **str, char **adr, char **num)
{
	char *tmp[3] = {0};
	int val = -1;
	for (int i = 0; i < 3; i++) assignStr(tmp+i,*str);
	if (*str == 0 || sscanf(*str," %s %s %s",tmp[0],tmp[1],tmp[2]) != 3) {
		for (int i = 0; i < 3; i++) assignStr(tmp+i,0);
		return 0;}
	assignStr(str,tmp[0]);
	assignStr(adr,tmp[1]);
	assignStr(num,tmp[2]);
	for (int i = 0; i < 3; i++) assignStr(tmp+i,0);
	return 1;
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int rub = 0; // unused listen
	int fub = 0; // file system
	int hub = 0; // process cluster
	int sub = 0; // hub, fub, connect, or accept
	char *adr = 0;
	char *num = 0;
	struct File file = {0};
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((fub = forkExec("fileC")) < 0) {fprintf(stderr,"error: cannot execute file: facerHs\n"); return -1;}
	if ((rub = openInet(0,argv[3])) < 0) ERROR(exiterr,-1);
	readJump(huberr,hub); writeJump(huberr,hub);
	readJump(huberr,fub); writeJump(huberr,fub);
	readJump(huberr,rub); writeJump(huberr,rub);
	layer[hub] = Cluster; layer[fub] = System;
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	// read from readable
	readFile(&file,sub);
	// find new mappings
	int snd = rub;
	if (layer[sub] == Cluster && file.act == NewHub && splitStr(&file.str,&adr,&num)) {
	if ((snd = inetIdent(adr,num)) == -1) {snd = openInet(adr,num); layer[snd] = Server;}}
	else if (layer[sub] == Unused) {snd = fub; layer[sub] = Client;}
	else if (file.act == NewHub) snd = fub;
	// set up new mapping
	if (snd != rub) {
	address[sub][file.idx] = snd;
	int num = mapping[sub][file.idx] = number[snd]++;
	address[snd][num] = sub;
	mapping[snd][num] = file.idx;}
	// translate and forward
	int idx = address[sub][file.idx];
	file.idx = mapping[sub][file.idx];
	writeFile(&file,idx);}}}
	return -1;
}
