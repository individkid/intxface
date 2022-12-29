#include "type.h"
#include "face.h"

struct Data *base[NUMOPEN] = {0};
void *data[NUMOPEN] = {0};
int *next[NUMOPEN] = {0};
int totl[NUMOPEN] = {0};
int last[NUMOPEN] = {0};
void datxOpen(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (base[idx] == 0) allocData(&base[idx],1);
	if (base[idx]->siz == 0) {
		readData(base[idx],idx);
		allocInt(&next[idx],base[idx]->siz+1);}
}
void datxClose(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (base[idx] != 0) allocData(&base[idx],0);
	if (data[idx] != 0) assignDat(data[idx],0,0,0);
	if (next[idx] != 0) allocInt(&next[idx],0);
	totl[idx] = 0; last[idx] = 0;
}
int datxProg(int sub, int idx)
{
	struct Data *dat = 0;
	int *nxt = 0;
	int *opc = 0;
	int jmp = 0;
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	dat = base[idx];
	nxt = next[idx];
	opc = dat->opc;
	jmp = dat->jmp[sub];
	while (jmp >= 0 && jmp < dat->len && sub >= 0 && sub < dat->siz) {
	switch ((enum Logic)(opc[jmp]&0xf)) {
	case (DoKeep): return 0;
	case (OrKeep): if (nxt[sub]==0 || nxt[sub]==dat->met[sub]) return 0; break;
	case (ExKeep): if (nxt[sub]==0) return 0; break;
	case (AdKeep): if (nxt[sub]==dat->met[sub]) return 0; break;
	case (DoWrap): return 1;
	case (OrWrap): if (nxt[sub]==0 || nxt[sub]==dat->met[sub]) return 1; break;
	case (ExWrap): if (nxt[sub]==0) return 1; break;
	case (AdWrap): if (nxt[sub]==dat->met[sub]) return 1; break;
	case (DoJump): jmp = (opc[jmp]>>4); break;
	case (OrJump): jmp = ((nxt[sub]==0 || nxt[sub]==dat->met[sub]) ? ((opc[jmp]>>4)&0xff) : (opc[jmp]>>12)); break;
	case (ExJump): jmp = ((nxt[sub]==0) ? ((opc[jmp]>>4)&0xff) : (opc[jmp]>>12)); break;
	case (AdJump): jmp = ((nxt[sub]==dat->met[sub]) ? ((opc[jmp]>>4)&0xff) : (opc[jmp]>>12)); break;
	case (DoSelf): sub = (opc[jmp]>>4); break;
	case (OrSelf): sub = ((nxt[sub]==0 || nxt[sub]==dat->met[sub]) ? ((opc[jmp]>>4)&0xff) : (opc[jmp]>>12)); break;
	case (ExSelf): sub = ((nxt[sub]==0) ? ((opc[jmp]>>4)&0xff) : (opc[jmp]>>12)); break;
	case (AdSelf): sub = ((nxt[sub]==dat->met[sub]) ? ((opc[jmp]>>4)&0xff) : (opc[jmp]>>12)); break;
	default: return -1;}}
	return -1;
}
void *datxNext(int sub, int num, int idx)
{
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
			int dif = dat->met[i]-nxt[i];
			if (dat->opt[i] && dif < inc) inc = dif;}
		tot += inc;
		for (int i = 0; i < dat->siz; i++) {
			if (nxt[i] < dat->met[i]) nxt[i] += inc;}
		for (int j = 1; (j ? ((j=0),1) : 0);) {
			for (int i = 0; i < dat->siz; i++, j=0) {
				if (datxProg(i,idx)) {nxt[i] = 0; j = 1;}}}
		if (nxt[sub] == 0) num--;}
	if (num > 0) ERROR();
	assignDat(&data[idx],dat->dat,tot-lst,tot-totl[idx]);
	totl[idx] = tot;
	if (tot-lst >= *(int*)(dat->dat)) {
		struct Data *tmp = 0;
		allocData(&tmp,1);
		readData(tmp,idx);
		if (tmp->siz == 0) {
			assignDat(&dat->dat,tmp->dat,0,*(int*)(tmp->dat));
			allocData(&tmp,0);}
		else {
			allocData(&base[idx],0);
			base[idx] = tmp;}
		last[idx] = tot;}
	return data[idx];
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
	return (void*)(((char*)datxData(dat))+num);
}
char *datxChrz(int num, void *dat)
{
	return (char*)datxPtr(num*sizeof(char),dat);
}
int *datxIntz(int num, void *dat)
{
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
