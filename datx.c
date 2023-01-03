#include "type.h"
#include "face.h"
#include "datx.h"

struct Data *base[NUMOPEN] = {0}; // last read
int *next[NUMOPEN] = {0}; // bytes per sub
int totl[NUMOPEN] = {0}; // bytes since open
int last[NUMOPEN] = {0}; // bytes at last read
int *todo[NUMOPEN] = {0}; // opcode to wrap
int done[NUMOPEN] = {0}; // opcode count
int *limt[NUMOPEN] = {0};
int *jump[NUMOPEN] = {0};
int lmts[NUMOPEN] = {0};
int mark[NUMOPEN] = {0};
int strt[NUMOPEN] = {0};
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
	int jmp = 0;
	int loc = 0;
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	dat = base[idx];
	nxt = next[idx];
	opc = dat->opc;
	lim = dat->lim;
	loc = jmp = dat->jmp[sub];
	while (jmp >= 0 && jmp < dat->len) {
	int opd = opc[jmp]>>4;
	switch ((enum Logic)(opc[jmp]&0xf)) {
	case (WrpVal): if (nxt[opd] == lim[opd]) jmp = loc; break;
	case (WrpYld): if (nxt[opd] == lim[opd]) {jmp = loc; return;} break;
	case (RunVal): if (nxt[opd] < lim[opd]) jmp = loc; break;
	case (RunYld): if (nxt[opd] < lim[opd]) {jmp = loc; return;} break;
	case (ClrVal): nxt[opd] = 0; break;
	case (ClrJmp): nxt[opd] = 0; jmp = loc; break;
	case (ClrYld): nxt[opd] = 0; jmp = loc; return;
	case (SkpVal): nxt[opd] = lim[opd]; break;
	case (SkpJmp): nxt[opd] = lim[opd]; jmp = loc; break;
	case (SkpYld): nxt[opd] = lim[opd]; jmp = loc; return;
	case (ImmJmp): jmp = opd; break;
	case (ImmYld): jmp = opd; return;
	case (SetJmp): loc = opd; break;
	default: ERROR();}}
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
	tot = totl[idx];
	lst = last[idx];
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
	allocDat(ptr,tot-totl[idx]);
	assignDat(ptr,dat->dat,0,tot-lst,tot-totl[idx]);
	totl[idx] = tot;
	if (tot-lst >= *(int*)(dat->dat)) {
		struct Data *tmp = 0;
		allocData(&tmp,1);
		readData(tmp,idx);
		if (tmp->siz == 0) {
			allocDat(&dat->dat,0);
			dat->dat = tmp->dat;
			allocDat(&tmp->dat,0);}
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
void datxCond(int sub, enum Logic opc, int idx)
{
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
	assignDat(&tmp.dat,dat,0,0,*(int*)dat);
	writeData(&tmp,idx);
	freeData(&tmp);
}
int datxMeta(int sub, int idx)
{
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	return next[idx][sub];
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
char datxChr(int num, void *dat)
{
	return *datxChrz(num,dat);
}
int datxInt(int num, void *dat)
{
	return *datxIntz(num,dat);
}
