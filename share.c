#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

struct Between {
	int msb;
	int siz;
	int *typ;
	void **dat;
};

int idx = 0;
void *dat = 0;
int note = 0;

int shareRead(int fildes, void *buf, int nbyte)
{
	memcpy(buf,datxData(dat),*(int*)dat);
	return 0;
}
int shareWrite(int fildes, const void *buf, int nbyte)
{
	dat = realloc(dat,nbyte+sizeof(int));
	*(int*)dat = nbyte;
	memcpy(datxData(dat),buf,nbyte);
	return 0;
}
void shareNote(int idx)
{
	note = 1;
}
int shareType(const char *str, int *len)
{
	for (int typ = 0; identSubtype(typ,0)!=-1; typ++) {
		int tmp = 0;
		note = 0;
		writeType(str,typ,idx);
		// TODO add &tmp to writeType to indicate how much of str consumed
		if (tmp > *len) *len = tmp;
		if (note == 0) return typ;}
	return -1;
}
void addBetween(struct Between *bet, void *dat)
{
	if (bet->siz == 1<<bet->msb) {
	void **tmp = bet->dat;
	allocDat(&bet->dat,1<<++bet->msb);
	for (int j = 0; j < bet->siz; j++) bet->dat[j] = tmp[j];
	allocDat(&tmp,0);}
	assignDat(bet->dat[(bet->siz)++],dat);
}

int runStage(struct Between *dst, const struct Between *src, const struct Stage *ptr)
{
	return 0; // TODO return -1 for more in src; return 1 for stage done
}
void runPipe(struct Between *dst, const struct Between *src, const struct Stage *ptr)
{
	// TODO count links; allocate array of links; start with last; go back on -1;
	// use src for first; return dst on 1 from last; use dst as src of nxt on 1;
}

int main(int argc, char **argv)
{
	struct Stage *start = 0;
	struct Stage **next = &start;
	struct Between dst = {0};
	struct Between src = {0};
	int done = 0;
	noteFunc(shareNote);
	idx = buntInit(0,0,shareRead,shareWrite);
	for (int i = 1; i < argc; i++) {
		int len = 0;
		int typ = 0;
		if (strcmp(argv[i],"--") == 0) {done = 1; continue;}
		typ = shareType(argv[i],&len);
		if (typ < 0) {
			fprintf(stderr,"ERROR: invalid type: %s\n",argv[i]);
			fprintf(stderr,"---------------------");
			for (int j = 0; j < len; j++) fprintf(stderr,"-");
			fprintf(stderr,"^\n");
			exit(-1);}
		else if (done || typ != identType("Stage")) {
			addBetween(&src,dat);}
		else {
			allocStage(next,1);
			hideStage(*next,argv[i],&len);
			next = &(*next)->nxt;}}
	runPipe(&dst,&src,start);
	for (int i = 0; i < dst.siz; i++) {
		char *str = 0;
		assignDat(dat,dst.dat[i]);
		readType(&str,dst.typ[i],idx);
		printf("%s\n",str);}
	return 0;
}