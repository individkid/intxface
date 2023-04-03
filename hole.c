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
	struct Argument arg = {0};
	int rub = 0; // unused listen
	int fub = 0; // file system
	int hub = 0; // process cluster
	int sub = 0; // hub, fub, connect, or accept
	char *adr = 0;
	char *num = 0;
	struct File file = {0};
	if (wrapInit(&arg,argv[1]) < 0 || arg.typ != Holez) ERROR();
	if ((hub = arg.idx) < 0) ERROR();
	if (wrapType(Filez,argv[0],"fileC") < 0) {fprintf(stderr,"holeC: cannot fork process\n"); return -1;}
	if ((fub = arg.idx) < 0) ERROR();
	if ((rub = openInet(0,arg.str)) < 0) ERROR();
	layer[hub] = Cluster; layer[fub] = System;
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitRead(0.0,-1); sub >= 0; sub = waitRead(0.0,-1)) {
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
