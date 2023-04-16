#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

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
void **planeEntry(struct Queue *que, int idx)
{
	if (idx >= que->lim) return 0;
	return &que->dat[(que->fst+idx) % que->siz];
}
int *planeType(struct Queue *que, int idx)
{
	if (idx >= que->lim) return 0;
	return &que->typ[(que->fst+idx) % que->siz];
}
void planeAlloc(struct Queue *que, int idx)
{
	while (idx >= que->lim)
	if (que->lim == que->siz) {
	void **dat = 0;
	int *typ = 0;
	int siz = que->siz ? que->siz*2 : 1;
	allocDat(&dat,siz);
	allocInt(&typ,siz);
	for (int j = 0; j < que->lim; j++) {
	dat[j] = *planeEntry(que,j);
	typ[j] = *planeType(que,j);}
	for (int j = que->lim; j < siz; j++) {
	dat[j] = 0; typ[j] = -1;}
	allocDat(&que->dat,0);
	allocInt(&que->typ,0);
	que->dat = dat;
	que->typ = typ;
	que->fst = 0;
	que->siz = siz;
	que->lim += 1;}
}
void planeEnque(struct Queue *que, void *dat, int typ)
{
	int sub = que->lim++;
	planeAlloc(que,sub);
	assignDat(planeEntry(que,sub),dat);
	*planeType(que,sub) = typ;
}
void planeDeque(struct Queue *que, void **dat, int *typ)
{
	if (que->lim == 0) ERROR();
	assignDat(dat,*planeEntry(que,0));
	*typ = *planeType(que,0);
	que->lim -= 1;
	que->fst = (que->fst+1) % que->siz;
}
int runStage(struct Queue *que, struct Stage *ptr)
{
	// TODO deque as many from ptr->que as needed to enque the required to que
	return 0; // TODO return -1 for more in ptr->que; return 1 for stage done
}
void runPipe(struct Queue *que, struct Stage *ptr)
{
	struct Stage **lst = 0; int siz = 0;
	for (struct Stage *tmp = ptr; tmp; tmp = tmp->nxt) siz++;
	lst = malloc(siz*sizeof(struct Stage*)); siz = 0;
	for (struct Stage *tmp = ptr; tmp; tmp = tmp->nxt) lst[siz++] = tmp;
	for (int sub = 0, val = 1; sub < siz && sub >= 0; sub += val) {
	if (val == 1) while (que->lim > 0) {
	void *dat = 0; int typ = 0;
	planeDeque(que,&dat,&typ);
	planeEnque(&lst[sub]->que,dat,typ);}
	val = runStage(que,lst[sub]);}
}

int main(int argc, char **argv)
{
	struct Stage *start = 0;
	struct Stage **next = &start;
	struct Queue que = {0};
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
			planeEnque(&que,dat,typ);}
		else {
			allocStage(next,1);
			len = 0; hideStage(*next,argv[i],&len);
			next = &(*next)->nxt;}}
	runPipe(&que,start);
	for (int i = 0; i < que.siz; i++) {
		char *str = 0;
		assignDat(dat,que.dat[i]);
		readType(&str,que.typ[i],idx);
		printf("%s\n",str);}
	return 0;
}
