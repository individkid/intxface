#include "type.h"
#include "face.h"
#include "datx.h"
#include <stdlib.h>
#include <string.h>

struct Data *base[NUMOPEN] = {0}; // last read
int *next[NUMOPEN] = {0}; // bytes per sub
int totl[NUMOPEN] = {0}; // bytes since open
int last[NUMOPEN] = {0}; // bytes at last read
int *todo[NUMOPEN] = {0}; // opcodes to run
int done[NUMOPEN] = {0}; // opcodes total
int *limt[NUMOPEN] = {0}; // bytes per sub
int *jump[NUMOPEN] = {0}; // opcode per sub
int lmts[NUMOPEN] = {0}; // subs total
int mark[NUMOPEN] = {0}; // opcode to jump
int strt[NUMOPEN] = {0}; // opcode to restart

struct Node {
	void *key;
	void *box;
	int siz;
	struct Node *ptr[3];
	struct Node *ref;
	int dep;
} tree = {0};

void datxOpen(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (base[idx] == 0) {
		allocData(&base[idx],1);
		readData(base[idx],idx);
		allocInt(&next[idx],base[idx]->siz);}
}
void datxClose(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (base[idx] != 0) allocData(&base[idx],0);
	if (next[idx] != 0) allocInt(&next[idx],0);
	if (todo[idx] != 0) allocInt(&todo[idx],0);
	if (limt[idx] != 0) allocInt(&limt[idx],0);
	if (jump[idx] != 0) allocInt(&jump[idx],0);
	totl[idx] = 0; last[idx] = 0; done[idx] = 0;lmts[idx] = 0;
}
void datxProg(int sub, int idx)
{
	struct Data *dat = 0;
	int *nxt = 0;
	enum Logic *opc = 0;
	int *lim = 0;
	int *jmp = 0;
	int *loc = 0;
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	dat = base[idx];
	nxt = next[idx];
	opc = dat->opc;
	lim = dat->lim;
	jmp = &dat->jmp[sub];
	loc = &dat->loc[sub];
	while (*jmp >= 0 && *jmp < dat->len) {
	int opd = opc[*jmp]>>4;
	switch ((enum Logic)(opc[*jmp]&0xf)) {
	case (WrpJmp): if (nxt[opd] == lim[opd]) {*jmp = *loc; continue;} break;
	case (WrpYld): if (nxt[opd] == lim[opd]) {*jmp = *loc; return;} break;
	case (RunJmp): if (nxt[opd] < lim[opd]) {*jmp = *loc; continue;} break;
	case (RunYld): if (nxt[opd] < lim[opd]) {*jmp = *loc; return;} break;
	case (ClrVal): nxt[opd] = 0; break;
	case (ClrJmp): nxt[opd] = 0; *jmp = *loc; continue;
	case (ClrYld): nxt[opd] = 0; *jmp = *loc; return;
	case (SkpVal): nxt[opd] = lim[opd]; break;
	case (SkpJmp): nxt[opd] = lim[opd]; *jmp = *loc; continue;
	case (SkpYld): nxt[opd] = lim[opd]; *jmp = *loc; return;
	case (ImmJmp): *jmp = opd; continue;
	case (ImmYld): *jmp = opd; return;
	case (SetJmp): *loc = opd; break;
	default: ERROR();}
	*jmp += 1;}
}
void datxCopy(void **ptr, void *dat, int loc, int siz)
{
	void *pre = 0;
	void *suf = 0;
	void *tmp = 0;
	datxSplit(&pre,&tmp,dat,loc);
	datxSplit(ptr,&suf,tmp,siz);
	free(pre);
	free(suf);
	free(tmp);
}
void datxRead(void **ptr, int sub, int num, int idx)
{
	void *ret = 0;
	struct Data *dat = 0;
	int *nxt = 0;
	int tot = 0;
	int lst = 0;
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	dat = base[idx];
	nxt = next[idx];
	tot = totl[idx]; // total number copied
	lst = last[idx]; // start of current dat
	while (num > 0) {
		int inc = (*(int*)(dat->dat))-(tot-lst);
		if (inc <= 0) break;
		for (int i = 0; i < dat->siz; i++) {
			int dif = dat->lim[i]-nxt[i];
			if (dif > 0 && dif < inc) inc = dif;}
		for (int i = 0; i < dat->siz; i++) {
			if (nxt[i] < dat->lim[i]) nxt[i] += inc;}
		for (int i = 0; i < dat->siz; i++) {
			datxProg(idx,i);}
		if (nxt[sub] == 0) num--;
		tot += inc;}
	if (num > 0) ERROR();
	datxCopy(ptr,dat->dat,totl[idx]-lst,tot-totl[idx]);
	totl[idx] = tot;
	if (tot-lst >= *(int*)(dat->dat)) {
		struct Data *tmp = 0;
		allocData(&tmp,1);
		readData(tmp,idx);
		if (tmp->siz == 0) {
			assignDat(dat->dat,tmp->dat);
			allocData(&tmp,0);}
		else {
			allocData(&base[idx],0);
			base[idx] = tmp;}
		last[idx] = tot;}
}
void datxInit(int sub, int lim, int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (done[idx] != 0) ERROR();
	if (lmts[idx] <= sub) {
	resizeInt(&limt[idx],sub,sub+1);
	resizeInt(&jump[idx],sub,sub+1);
	lmts[idx] = sub+1;}
	limt[idx][sub] = lim; jump[idx][sub] = done[idx]; strt[idx] = done[idx]; mark[idx] = done[idx];
	appendInt(&todo[idx],Logics,&done[idx]);
}
void datxStep(int sub, int opc, int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (sub < 0 || sub >= lmts[idx]) ERROR();
	if (opc < 0 || opc >= ImmJmp) ERROR();
	appendInt(&todo[idx],((sub<<4)|opc),&done[idx]);
}
void datxMark(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (todo[idx][mark[idx]] != Logics) ERROR();
	todo[idx][mark[idx]] = ((done[idx]<<4)|SetJmp);
	mark[idx] = done[idx];
	appendInt(&todo[idx],Logics,&done[idx]);
}
void datxWrite(void *dat, int idx)
{
	struct Data tmp = {0};
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (done[idx] != 0) {
	if (todo[idx][mark[idx]] != Logics) ERROR();
	todo[idx][mark[idx]] = ((done[idx]<<4)|SetJmp);
	appendInt(&todo[idx],((strt[idx]<<4)|ImmYld),&done[idx]);
	allocInt(&(tmp.lim),lmts[idx]);
	allocInt(&(tmp.jmp),lmts[idx]);
	allocLogic(&(tmp.opc),done[idx]);
	for (int i = 0; i < lmts[idx]; i++) tmp.lim[i] = limt[idx][i];
	for (int i = 0; i < lmts[idx]; i++) tmp.jmp[i] = jump[idx][i];
	for (int i = 0; i < done[idx]; i++) tmp.opc[i] = todo[idx][i];
	allocInt(&limt[idx],0);
	allocInt(&jump[idx],0);
	allocInt(&todo[idx],0);
	tmp.siz = lmts[idx]; tmp.len = done[idx];
	done[idx] = 0; lmts[idx] = 0;}
	assignDat(&tmp.dat,dat);
	writeData(&tmp,idx);
	freeData(&tmp);
}
int datxMeta(int sub, int idx)
{
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	return next[idx][sub];
}
void datxStr(void **dat, const char *str)
{
	*dat = realloc(*dat,strlen(str)+sizeof(int));
	*(int*)*dat = strlen(str);
	memcpy((void*)((int*)*dat+1),str,strlen(str));
}
void datxSplit(void **pre, void **suf, const void *dat, int len)
{
	*pre = realloc(*pre,len+sizeof(int));
	*suf = realloc(*suf,*(int*)dat-len+sizeof(int));
	*(int*)*pre = len;
	*(int*)*suf = *(int*)dat-len;
	memcpy((void*)((int*)*pre+1),(void*)((int*)dat+1),len);
	memcpy((void*)((int*)*suf+1),(char*)((int*)dat+1)+len,*(int*)dat-len);
}
void datxJoin(void **dat, const void *pre, const void *suf)
{
	*dat = realloc(*dat,*(int*)pre+*(int*)suf+sizeof(int));
	*(int*)dat = *(int*)pre+*(int*)suf;
	memcpy((void*)((int*)*dat+1),(void*)((int*)pre+1),*(int*)pre);
	memcpy((char*)((int*)*dat+1)+*(int*)pre,(void*)((int*)suf+1),*(int*)suf);
}
int datxCompare(void *one, void *oth)
{
	int lne = *(int*)one;
	int lth = *(int*)oth;
	int len = lne+sizeof(int);
	if (lne < lth) return -1;
	if (lne > lth) return 1;
	for (int i = sizeof(int); i < len; i++) {
		if (*((char*)one+i) < *((char*)oth+i)) return -1;
		if (*((char*)one+i) > *((char*)oth+i)) return 1;}
	return 0;
}
struct Node *datxNode(void *key)
{
	struct Node *tmp = &tree;
	while (tmp->dep) for (int i = tmp->siz; i > 0; i--) if (datxCompare(tmp->ptr[i-1]->key,key) <= 0) {
		tmp = tmp->ptr[i-1]; break;}
	return tmp;
}
void *datxFind(void *key)
{
	struct Node *leaf = datxNode(key);
	if (datxCompare(leaf->key,key) == 0) {
		return leaf->box;}
	return 0;
}
void datxInsert(void *key, void *box)
{
	struct Node *leaf = datxNode(key);
	if (datxCompare(leaf->key,key) == 0) {
		leaf->box = box; return;}
	while (leaf->siz == 3 && leaf != &tree) {
		leaf = leaf->ref;}
	if (leaf->siz == 3) {
		struct Node *tmp = malloc(sizeof(struct Node));
		*tmp = *leaf; tmp->ref = leaf;
		leaf->siz = 1; leaf->ptr[0] = tmp; leaf->dep++;}
	while (leaf->dep) for (int i = leaf->siz; i > 0; i--) if (datxCompare(leaf->ptr[i-1]->key,key) > 0) {
		leaf->ptr[i] = leaf->ptr[i-1];} else {
		struct Node *tmp = malloc(sizeof(struct Node));
		tmp->key = key; tmp->box = 0; tmp->siz = 1;
		tmp->ptr[0] = leaf->ptr[i=1]; tmp->ref = leaf; tmp->dep = leaf->dep-1;
		leaf->ptr[i-1] = tmp; leaf->siz++; leaf = tmp; break;}
	leaf->box = box;
}
int datxPtrs(void *dat)
{
	return *(int*)dat;
}
int datxChrs(void *dat)
{
	return *(int*)dat/sizeof(char);
}
int datxInts(void *dat)
{
	return *(int*)dat/sizeof(int);
}
void *datxData(void *dat)
{
	return (void*)(((int*)dat)+1);
}
void *datxPtr(int num, void *dat)
{
	if (num >= datxPtrs(dat)) ERROR();
	return (void*)(((char*)datxData(dat))+num);
}
char *datxChrz(int num, void *dat)
{
	if (num >= datxChrs(dat)) ERROR();
	return (char*)datxPtr(num*sizeof(char),dat);
}
int *datxIntz(int num, void *dat)
{
	if (num >= datxInts(dat)) ERROR();
	return (int*)datxPtr(num*sizeof(int),dat);
}
