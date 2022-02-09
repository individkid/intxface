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

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int rub = 0; // unused listen
	int fub = 0; // file system
	int hub = 0; // process cluster
	int sub = 0; // hub, fub, connect, or accept
	struct File file = {0};
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((fub = forkExec("file")) < 0) ERROR(exiterr,-1);
	if ((rub = openInet(0,argv[3])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub); bothJump(huberr,fub); bothJump(huberr,rub);
	layer[hub] = Cluster; layer[fub] = System;
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	// read from readable
	readFile(&file,sub);
	// find new mappings
	int snd = rub;
	if (layer[sub] == Cluster && file.act == NewThd && file.num == 3) {
	if (pollInet(file.ptr[1],file.ptr[2])) snd = checkInet(file.ptr[1],file.ptr[2]);
	else {snd = openInet(file.ptr[1],file.ptr[2]); layer[snd] = Server;}}
	else if (layer[sub] == Unused) {snd = fub; layer[sub] = Client;}
	else if (file.act == NewThd) snd = fub;
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
