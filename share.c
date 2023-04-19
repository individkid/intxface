#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int idx0 = 0;
int idx1 = 0;
void *dat0 = 0;
void *dat1 = 0;
int note = 0;
int unique = 0;

int shareRead(int fildes, void *buf, int nbyte)
{
	void **dat = (fildes == idx0 ? &dat0 : &dat1);
	void *pre = 0;
	void *suf = 0;
	datxSplit(&pre,&suf,*dat,nbyte);
	if (*(int*)pre != nbyte) return 0;
	memcpy(buf,datxData(pre),nbyte);
	assignDat(dat,suf);
	free(pre);
	free(suf);
	return nbyte;
}
int shareWrite(int fildes, const void *buf, int nbyte)
{
	void **dat = (fildes == idx0 ? &dat0 : &dat1);
	void *suf = malloc(sizeof(int)+nbyte);
	void *pre = 0;
	assignDat(&pre,*dat);
	datxJoin(dat,pre,suf);
	free(pre);
	free(suf);
	return nbyte;
}
void shareNote(int idx)
{
	note = 1;
}
int sharePeek(const char *str, int *len)
{
	for (int typ = 0; identSubtype(typ,0)!=-1; typ++) {
		int tmp = 0;
		note = 0;
		writeType(str,typ,idx0);
		flushBuf(idx0);
		// TODO add &tmp to writeType to indicate how much of str consumed
		if (tmp > *len) *len = tmp;
		if (note == 0) return typ;}
	return -1;
}
void **shareEntry(struct Queue *que, int sub)
{
	if (sub >= que->lim) return 0;
	return &que->dat[(que->fst+sub) % que->siz];
}
int *shareType(struct Queue *que, int sub)
{
	if (sub >= que->lim) return 0;
	return &que->typ[(que->fst+sub) % que->siz];
}
void shareAlloc(struct Queue *que, int sub)
{
	while (sub >= que->lim) {
	if (que->lim == que->siz) {
	void **dat = 0;
	int *typ = 0;
	int siz = que->siz ? que->siz*2 : 1;
	allocDat(&dat,siz);
	allocInt(&typ,siz);
	for (int j = 0; j < que->lim; j++) {
	dat[j] = *shareEntry(que,j);
	typ[j] = *shareType(que,j);}
	for (int j = que->lim; j < siz; j++) {
	dat[j] = 0; typ[j] = -1;}
	allocDat(&que->dat,0);
	allocInt(&que->typ,0);
	que->dat = dat;
	que->typ = typ;
	que->fst = 0;
	que->siz = siz;}
	que->lim += 1;}
}
void shareEnque(struct Queue *que, void *dat, int typ)
{
	int sub = que->lim + 1;
	shareAlloc(que,sub);
	assignDat(shareEntry(que,sub),dat);
	*shareType(que,sub) = typ;
}
void shareDeque(struct Queue *que, void **dat, int *typ)
{
	if (que->lim == 0) ERROR();
	assignDat(dat,*shareEntry(que,0));
	*typ = *shareType(que,0);
	que->lim -= 1;
	que->fst = (que->fst+1) % que->siz;
}
int shareStage(struct Queue *dst, struct Queue *src, struct Stage *ptr);
void sharePipe(struct Queue *que, struct Pipe *ptr)
{
	for (int sub = 0, val = 1; sub < ptr->siz && sub >= 0; sub += val) {
	if (val == 1) while (que->lim > 0) {
	void *dat = 0; int typ = 0;
	shareDeque(que,&dat,&typ);
	shareEnque(&ptr->que[sub],dat,typ);}
	val = shareStage(que,&ptr->que[sub],&ptr->stg[sub]);}
}
int shareStage(struct Queue *dst, struct Queue *src, struct Stage *ptr)
{
	struct Queue que = {0};
	struct Pipe ppe = {0};
	void *dat = 0;
	void *pre = 0;
	void *suf = 0;
	char *str = 0;
	int typ = 0;
	int fld = 0;
	int req = 0;
	int idx = 0;
	switch (ptr->tag) {
	case (Encode): req = 1; break;
	case (Decode): req = 1; break;
	case (Construct): req = 1; break;
	case (Destruct): req = 1; break;
	case (Insert): req = 2; break;
	case (Extract): req = 1; break;
	case (Divide): req = 1; break;
	case (Combine): req = 2; break;
	case (Permute): req = ptr->nrd; break;
	case (Constant): break;
	case (Unique): req = ptr->nun; break;
	case (Execute): req = ptr->mrd; break;
	case (Pipex): break;
	case (Fifox): break;
	case (Execx): break;
	case (Filex): break;
	case (Follow): break;
	case (Select): break;
	default: ERROR();}
	if (src->lim < ptr->num+req) return -1;
	for (int i = 0; i < ptr->num; i++) {
		shareDeque(src,&dat0,&typ);
		shareEnque(dst,dat0,typ);}
	switch (ptr->tag) {
	case (Encode):
		shareDeque(src,&dat0,&typ);
		if (typ != identType("Str")) ERROR();
		readStr(&str,idx0);
		datxStr(&dat0,""); note = 0; writeType(str,ptr->typ,idx0); if (note) ERROR();
		shareEnque(dst,dat0,ptr->typ);
		break;
	case (Decode):
		shareDeque(src,&dat0,&typ);
		readType(&str,typ,idx0);
		datxStr(&dat0,""); writeStr(str,idx0);
		shareEnque(dst,dat0,identType("Str"));
		break;
	case (Construct):
		shareDeque(src,&dat,&typ);
		if (typ != identType("Dat")) ERROR();
		shareEnque(dst,dat,ptr->typ);
		break;
	case (Destruct):
		shareDeque(src,&dat,&typ);
		shareEnque(dst,dat,identType("Dat"));
		break;
	case (Insert):
		shareDeque(src,&dat0,&typ);
		shareDeque(src,&dat1,&fld);
		if (identSubtype(typ,ptr->fld) != fld) ERROR();
		datxStr(&dat0,""); note = 0; readField(typ,ptr->fld,ptr->idx,idx0,idx1,idx0); if (note) ERROR();
		shareEnque(dst,dat0,typ);
		break;
	case (Extract):
		shareDeque(src,&dat0,&typ);
		datxStr(&dat1,""); note = 0; writeField(typ,ptr->fld,ptr->idx,idx0,idx1); if (note) ERROR();
		shareEnque(dst,dat1,identSubtype(typ,ptr->fld));
		break;
	case (Divide):
		shareDeque(src,&dat,&typ);
		if (typ != identType("Dat")) ERROR();
		datxSplit(&pre,&suf,&dat,ptr->len);
		shareEnque(dst,pre,typ);
		shareEnque(dst,suf,typ);
		break;
	case (Combine):
		shareDeque(src,&pre,&typ);
		if (typ != identType("Dat")) ERROR();
		shareDeque(src,&suf,&typ);
		if (typ != identType("Dat")) ERROR();
		datxJoin(dat,pre,suf);
		shareEnque(dst,dat,typ);
		break;
	case (Permute):
		for (int i = 0; i < ptr->nrd; i++) {
			shareDeque(src,&dat,&typ);
			shareEnque(&que,dat,typ);}
		for (int i = 0; i < ptr->nwr;) {
			if (ptr->ord[i] < 0 || ptr->ord[i] >= ptr->nrd) ERROR();
			shareEnque(dst,que.dat[ptr->ord[i]],que.typ[ptr->ord[i]]);}
		break;
	case (Constant):
		shareEnque(dst,ptr->dat,identType("Dat"));
		break;
	case (Unique):
		shareDeque(src,&dat,&typ);
		if (typ != identType("Dat")) ERROR();
		idx = datxFind(1,dat);
		if (idx == -1) {
			idx = unique++;
			datxInsert(1,dat,unique++);}
		datxStr(&dat0,""); writeInt(idx,idx0);
		shareEnque(dst,dat0,identType("Int"));
		break;
	case (Execute):
		shareDeque(src,&dat0,&typ);
		if (typ != identType("Pipe")) ERROR();
		readPipe(&ppe,idx0);
		for (int i = 0; i < ptr->nrd; i++) {
			shareDeque(src,&dat,&typ);
			shareEnque(&que,dat,typ);}
		sharePipe(&que,&ppe);
		for (int i = 0; i < que.lim; i++) {
			shareDeque(&que,&dat,&typ);
			shareEnque(dst,&que,typ);}
		break;
	case (Pipex):
		idx = datxFind(0,ptr->str);
		if (idx == -1) {
			idx = rdwrInit(ptr->inp,ptr->out);
			datxStr(&dat0,""); writeStr(ptr->str,idx0);
			datxInsert(0,dat0,idx);}
		break;
	case (Fifox):
		idx = datxFind(0,ptr->str);
		if (idx == -1) {
			idx = openFifo(ptr->url);
			datxStr(&dat0,""); writeStr(ptr->str,idx0);
			datxInsert(0,dat0,idx);}
		break;
	case (Execx):
		idx = datxFind(0,ptr->str);
		if (idx == -1) {
			idx = forkExec(ptr->url);
			datxStr(&dat0,""); writeStr(ptr->str,idx0);
			datxInsert(0,dat0,idx);}
		break;
	case (Filex):
		idx = datxFind(0,ptr->str);
		if (idx == -1) {
			idx = openFile(ptr->url);
			datxStr(&dat0,""); writeStr(ptr->str,idx0);
			datxInsert(0,dat0,idx);}
		break;
	case (Follow):
		idx = datxFind(0,ptr->str);
		if (idx == -1) ERROR();
		datxStr(&dat0,""); loopType(ptr->typ,idx,idx0);
		shareEnque(dst,dat0,ptr->typ);
		break;
	case (Select):
		break;
	default: ERROR();}
	freeQueue(&que);
	freePipe(&ppe);
	free(dat);
	free(pre);
	free(suf);
	free(str);
	return 1;
}

int main(int argc, char **argv)
{
	struct Pipe pipe = {0};
	struct Queue queue = {0};
	int done = 0; int sub = 0;
	int len = 0; int typ = 0;
	char *str = 0;
	noteFunc(shareNote);
	idx0 = puntInit(0,0,shareRead,shareWrite);
	idx1 = puntInit(0,0,shareRead,shareWrite);
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i],"--") == 0) done = 1;
		else if ((len = 0, typ = sharePeek(argv[i],&len)) < 0);
		else if (done || typ != identType("Stage"));
		else pipe.siz++;}
	allocStage(&pipe.stg,pipe.siz);
	allocQueue(&pipe.que,pipe.siz);
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i],"--") == 0) done = 1;
		else if ((len = 0, typ = sharePeek(argv[i],&len)) < 0) {
			fprintf(stderr,"ERROR: invalid type: %s\n",argv[i]);
			fprintf(stderr,"---------------------");
			for (int j = 0; j < len; j++) fprintf(stderr,"-");
			fprintf(stderr,"^\n");
			exit(-1);}
		else if (done || typ != identType("Stage")) {
			shareEnque(&queue,dat0,typ);}
		else {
			len = 0; hideStage(&pipe.stg[sub],argv[i],&len); sub += 1;}}
	sharePipe(&queue,&pipe);
	for (int i = 0; i < queue.siz; i++) {
		assignDat(dat0,queue.dat[i]);
		readType(&str,queue.typ[i],idx0);
		printf("%s\n",str);}
	return 0;
}
