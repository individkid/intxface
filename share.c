#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/stat.h>

int idx0 = 0;
int idx1 = 0;
void *dat0 = 0;
void *dat1 = 0;
int note = 0;
int unique = 0;
pthread_t thd[NUMOPEN] = {0};
struct Pipe *gpp[NUMOPEN] = {0};
int idt[NUMOPEN] = {0};
int odt[NUMOPEN] = {0};
int ldt = 0;

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
int shareWInput(int fildes, const void *buf, int nbyte)
{
	return writeBuf(buf,nbyte,idt[fildes]);
}
int shareRInput(int fildes, void *buf, int nbyte)
{
	return readBuf(buf,nbyte,idt[fildes]);
}
int shareWOutput(int fildes, const void *buf, int nbyte)
{
	return writeBuf(buf,nbyte,odt[fildes]);
}
int shareROutput(int fildes, void *buf, int nbyte)
{
	return readBuf(buf,nbyte,odt[fildes]);
}
int shareFind(const char *dim, void *suf)
{
	void *pre = 0;
	void *dat = 0;
	int val = 0;
	datxStr(&pre,dim);
	datxJoin(&dat,pre,suf);
	val = *datxIntz(0,datxFind(dat));
	free(pre);
	free(dat);
	return val;
}
void shareInsert(const char *dim, void *suf, int val)
{
	void *pre = 0;
	void *dat = 0;
	void *tmp = malloc(sizeof(int)+sizeof(int));
	*(int*)tmp = sizeof(int);
	*datxIntz(0,tmp) = val;
	datxStr(&pre,dim);
	datxJoin(&dat,pre,suf);
	datxInsert(dat,tmp);
	free(pre);
	free(dat);
}
int shareFindD(void *dat)
{
	return shareFind("D",dat);
}
void shareInsertD(void *dat, int val)
{
	shareInsert("D",dat,val);
}
int shareFindU(void *dat)
{
	return shareFind("U",dat);
}
void shareInsertU(void *dat, int val)
{
	shareInsert("U",dat,val);
}
int shareExec(const char *exe)
{
	int idx = openFork();
	struct Argument arg = {0};
	char *str = 0;
	int len = 0;
	struct stat new;
	if (openCheck(idx) == -1) return idx;
	arg.typ = Processs;
	if (stat(exe,&new) == 0) for (enum Process i = 0; i < Processs; i++) {
		struct stat old;
		if (stat(Execname__Process__Str(i),&old) != 0) continue;
		if (new.st_dev == old.st_dev && new.st_ino == old.st_ino) {arg.typ = i; break;}}
	arg.inp = openRdfd(idx);
	arg.out = openWrfd(idx);
	datxStr(&dat1,""); writeArgument(&arg,idx1);
	readType(&str,&len,identType("Argument"),idx1);
	return openExec(exe,str);
}
int sharePeek(const char *str, int *len)
{
	for (int typ = 0; identSubtype(typ,0)!=-1; typ++) {
		int tmp = 0;
		note = 0;
		writeType(str,&tmp,typ,idx0);
		flushBuf(idx0);
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
void *shareThread(void *arg);
int shareStage(struct Queue *dst, struct Queue *src, struct Stage *ptr)
{
	struct Queue que = {0};
	void *dat = 0;
	char *str = 0;
	int len = 0;
	int typ = 0;
	int fld = 0;
	int req = 0;
	int idx = 0;
	int msk = 0;
	int val = 0;
	switch (ptr->tag) {
	case (Encode): req = 1; break;
	case (Decode): req = 1; break;
	case (Insert): req = 2; break;
	case (Extract): req = 1; break;
	case (Permute): req = ptr->nrd; break;
	case (Pipex): break;
	case (Fifox): break;
	case (Execx): break;
	case (Filex): break;
	case (Threadx): break;
	case (Loopx): break;
	case (Follow): break;
	case (Precede): req = ptr->nds; break;
	case (Select): break;
	case (Unique): req = ptr->nky; break;
	default: break;}
	if (src->lim < ptr->num+req) return -1;
	for (int i = 0; i < ptr->num; i++) {
		shareDeque(src,&dat0,&typ);
		shareEnque(dst,dat0,typ);}
	switch (ptr->tag) {
	case (Encode):
		shareDeque(src,&dat0,&typ);
		if (typ != identType("Str")) ERROR();
		readStr(&str,idx0);
		datxStr(&dat0,""); note = 0; len = 0; writeType(str,&len,ptr->typ,idx0); if (note) ERROR();
		shareEnque(dst,dat0,ptr->typ);
		break;
	case (Decode):
		shareDeque(src,&dat0,&typ);
		len = 0; readType(&str,&len,typ,idx0);
		datxStr(&dat0,""); writeStr(str,idx0);
		shareEnque(dst,dat0,identType("Str"));
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
	case (Permute):
		for (int i = 0; i < ptr->nrd; i++) {
			shareDeque(src,&dat,&typ);
			shareEnque(&que,dat,typ);}
		for (int i = 0; i < ptr->nwr;) {
			if (ptr->ord[i] < 0 || ptr->ord[i] >= ptr->nrd) ERROR();
			shareEnque(dst,que.dat[ptr->ord[i]],que.typ[ptr->ord[i]]);}
		break;
	case (Pipex):
		if (shareFindD(ptr->str) == -1) {datxStr(&dat0,""); writeStr(ptr->str,idx0); shareInsertD(dat0,rdwrInit(ptr->inp,ptr->out));}
		break;
	case (Fifox):
		if (shareFindD(ptr->str) == -1) {datxStr(&dat0,""); writeStr(ptr->str,idx0); shareInsertD(dat0,openFifo(ptr->url));}
		break;
	case (Execx):
		if (shareFindD(ptr->str) == -1) {datxStr(&dat0,""); writeStr(ptr->str,idx0); shareInsertD(dat0,shareExec(ptr->url));}
		break;
	case (Filex):
		if (shareFindD(ptr->str) == -1) {datxStr(&dat0,""); writeStr(ptr->str,idx0); shareInsertD(dat0,openFile(ptr->url));}
		break;
	case (Threadx):
		if (shareFindD(ptr->str) == -1) {datxStr(&dat0,""); writeStr(ptr->str,idx0); shareInsertD(dat0,puntInit(ldt,ldt,shareROutput,shareWInput));
			datxStr(&dat0,""); len = 0; writeType(ptr->url,&len,identType("Pipe"),idx0);
			allocPipe(&gpp[ldt],1); readPipe(gpp[ldt],idx0);
			idt[ldt] = openPipe(); odt[ldt] = openPipe();
			if (pthread_create(&thd[ldt],0,shareThread,(void*)(size_t)ldt) != 0) ERROR();
			ldt++;}
		break;
	case (Loopx):
		if (shareFindD(ptr->str) == -1) {datxStr(&dat0,""); writeStr(ptr->str,idx0); shareInsertD(dat0,openPipe());}
		break;
	case (Follow):
		for (int i = 0; i < ptr->nsr; i++) {datxStr(&dat0,""); loopType(ptr->fdt[i],shareFindD(ptr->src[i]),idx0);shareEnque(dst,dat0,ptr->fdt[i]);}
		break;
	case (Precede):
		for (int i = 0; i < ptr->nds; i++) {shareDeque(src,&dat0,&typ); loopType(typ,idx0,shareFindD(ptr->dst[i]));}
		break;
	case (Select):
		msk = 0;
		for (int i = 0; i < ptr->siz-1; i++) {msk |= 1<<shareFindD(ptr->msk[i]);}
		idx = waitRead(ptr->dly,msk);
		if (idx < 0 && ptr->siz > 0) {idx = shareFindD(ptr->msk[ptr->siz-1]);}
		for (int i = 0; i < ptr->siz; i++) if (idx == shareFindD(ptr->msk[i])) {loopType(ptr->how[i],shareFindD(ptr->msk[i]),shareFindD(ptr->map[i]));}
		break;
	case (Unique):
		datxStr(dat0,""); for (int i = 0; i < ptr->nky; i++) {shareDeque(src,&dat1,&typ); loopType(typ,idx1,idx0);}
		if ((val = shareFindU(dat0)) == -1) {val = unique++; shareInsertU(dat0,val);}
		datxStr(dat0,""); writeInt(val,idx0); shareEnque(dst,dat0,identType("Int"));
		break;
	default: break;}
	freeQueue(&que);
	free(dat);
	free(str);
	return 1;
}
void sharePipe(struct Queue *dst, struct Queue *src, struct Pipe *ptr)
{
	void *dat = 0;
	int typ = 0;
	for (int sub = ptr->siz-1, val = 1; sub < ptr->siz && sub >= 0; sub += val) {
	val = shareStage(dst,&ptr->que[sub],&ptr->stg[sub]);
	if (val < 0 && sub == 0 && src->lim) {
	shareDeque(src,&dat,&typ);
	shareEnque(&ptr->que[sub],dat,typ);
	val = 0;}
	if (val > 0 && sub < ptr->siz-1) {
	while (dst->lim) {
	shareDeque(dst,&dat,&typ);
	shareEnque(&ptr->que[sub+1],dat,typ);}}}
	free(dat);
}
void *shareThread(void *arg)
{
	int ldt = (int)(size_t)arg;
	int idx = puntInit(ldt,ldt,shareRInput,shareWOutput);
	struct Pipe *ptr = gpp[ldt];
	struct Queue dst = {0};
	void *dat = 0;
	int typ = 0;
	for (int sub = ptr->siz-1, val = 1; sub < ptr->siz && sub >= 0; sub += val) {
	val = shareStage(&dst,&ptr->que[sub],&ptr->stg[sub]);
	if (val < 0 && sub == 0) {
	readDat(&dat,idx);
	shareEnque(&ptr->que[0],dat,identType("Dat"));
	val = 0;}
	if (val > 0 && sub == ptr->siz-1) {
	while (dst.lim) {
	shareDeque(&dst,&dat0,&typ);
	loopType(typ,idx0,idx);}
	val = 0;}
	if (val > 0 && sub < ptr->siz-1) {
	while (dst.lim) {
	shareDeque(&dst,&dat,&typ);
	shareEnque(&ptr->que[sub+1],dat,typ);}}}
	freeQueue(&dst);
	free(dat);
	return 0;
}

int main(int argc, char **argv)
{
	struct Pipe ppe = {0};
	struct Queue src = {0};
	struct Queue dst = {0};
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
		else ppe.siz++;}
	allocStage(&ppe.stg,ppe.siz);
	allocQueue(&ppe.que,ppe.siz);
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i],"--") == 0) done = 1;
		else if ((len = 0, typ = sharePeek(argv[i],&len)) < 0) {
			fprintf(stderr,"ERROR: invalid type: %s\n",argv[i]);
			fprintf(stderr,"---------------------");
			for (int j = 0; j < len; j++) fprintf(stderr,"-");
			fprintf(stderr,"^\n");
			exit(-1);}
		else if (done || typ != identType("Stage")) {
			shareEnque(&src,dat0,typ);}
		else {
			len = 0; hideStage(&ppe.stg[sub],argv[i],&len); sub += 1;}}
	sharePipe(&dst,&src,&ppe);
	for (int i = 0; i < src.siz; i++) {
		assignDat(dat0,src.dat[i]);
		len = 0; readType(&str,&len,src.typ[i],idx0);
		printf("src: %s\n",str);}
	for (int i = 0; i < dst.siz; i++) {
		assignDat(dat0,dst.dat[i]);
		len = 0; readType(&str,&len,dst.typ[i],idx0);
		printf("dst: %s\n",str);}
	return 0;
}
