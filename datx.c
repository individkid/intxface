#include "type.h"
#include "face.h"
#include "datx.h"

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
	while (num > 0 && tot-lst < datxPtrs(dat->dat)) {
		tot++;
		for (int i = 0; i < dat->siz; i++) {
			nxt[i]++;}
		for (int i = 0; i < dat->siz; i++) {
			int rst = dat->rst[i];
			if (nxt[rst] == dat->met[rst]) nxt[i] = 0;}
		for (int i = 0; i < dat->siz; i++) {
			if (nxt[i] == dat->met[i]) nxt[i] = 0;}
		if (nxt[sub] == 0) num--;}
	if (num > 0) ERROR();
	assignDat(&data[idx],dat->dat,tot-lst,tot-totl[idx]);
	totl[idx] = tot;
	if (tot-lst >= datxPtrs(dat->dat)) {
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
