#include "call.h"
#include <stdio.h>
#include <stdlib.h>
tftype clk[NUMCALL] = {0}; // time wheel function
fftype flw[NUMCALL] = {0}; // data flow function
nftype nst[NUMCALL] = {0}; // flow control function
void *bnd[NUMCALL] = {0}; // per-idx argument
struct {
	int rgt; // towards leaf
	int nxt; // towards larger key
	int lst; // towards smaller key
	long long key;
	int box;
} lnk[NUMNODE] = {0}; // linked list tree
int idt[NUMCALL] = {0}; // stack of caller idx
int dep = 0; // dedep of caller stack
int lft[NUMCALL] = {0}; // flow control opens
int rgt[NUMCALL] = {0}; // flow control closes
int lim = 0;
int initCall(void *arg, tftype tfn, fftype ffn, nftype nfn, int lft, int rgt)
{
	if (lim >= NUMCALL) {fprintf(stderr,"full list\n"); exit(-1);}
	clk[lim] = tfn;
	flw[lim] = ffn;
	nst[lim] = nfn;
	bnd[lim] = arg;
	return lim++;
}
int takeTime()
{
	int idx = lnk[0].lst;
	if (idx == 0) {fprintf(stderr,"empty pool\n"); exit(-1);}
	lnk[0].lst = lnk[idx].lst;
	return idx;
}
void makeTime(int idx, int rgt, long long val, int box)
{
	int tak = takeTime();
	lnk[tak].key = val;
	lnk[tak].box = box;
	lnk[tak].lst = lnk[idx].lst;
	lnk[tak].nxt = idx;
	lnk[tak].rgt = rgt;
	lnk[idx].lst = tak;
}
void postTime(int idx, int rgt, long long val, int box)
{
	int tak = takeTime();
	lnk[tak].key = val;
	lnk[tak].box = box;
	lnk[tak].lst = idx;
	lnk[tak].nxt = lnk[idx].nxt;
	lnk[tak].rgt = rgt;
	lnk[idx].nxt = tak;
}
void freeTime(int idx)
{
	lnk[idx].lst = lnk[0].lst;
	lnk[0].lst = idx;
}
void countTime(int *num, int *mid, int lvl)
{
	num[lvl]++;
	if ((num[lvl] % 2) == 0) mid[lvl] = lnk[mid[lvl]].nxt;
}
void pendTime(void *ovr, int box, long long val)
{
	int idx = 0;
	int lvl = 0;
	int num[NUMLEVEL];
	int mid[NUMLEVEL];
	int pth[NUMLEVEL];
	if (val <= 0) {fprintf(stderr,"negative value\n"); exit(-1);}
	while (1) {
		pth[lvl] = idx;
		mid[lvl] = idx;
		while (lnk[idx].nxt != 0 && lnk[idx].key < val) {
			countTime(num,mid,lvl);
			idx = lnk[idx].nxt;}
		lvl++;
		if (lvl >= NUMLEVEL) {fprintf(stderr,"infinite path\n"); exit(-1);}
		idx = lnk[idx].rgt;
		if (idx == 0) break;}
	mid[lvl] = 0;
	while (lvl > 0) {
		lvl--;
		if (lnk[pth[lvl]].key < val) {
			postTime(pth[lvl],mid[lvl+1],val,box);
			countTime(num,mid,lvl);}
		else {
			makeTime(pth[lvl],mid[lvl+1],val,box);}
		if (num[lvl] < NUMEACH) break;
		val = lnk[pth[lvl]].key;}
	if (lnk[0].nxt != 0) {
		idx = takeTime();
		lnk[idx] = lnk[0];
		lnk[0].rgt = idx;
		lnk[0].nxt = 0;}
}
void callTime(void *ovr, long long val)
{
	int idx = lnk[0].rgt;
	while (idx != 0) {
		int rgt = lnk[idx].rgt;
		while (lnk[idx].key < val) {
			int nxt = lnk[idx].nxt;
			if (lnk[idx].rgt == 0) {
				int box = lnk[idx].box;
				clk[box](bnd[box],ovr,lnk[idx].key);}
			freeTime(idx);
			idx = nxt;}
		idx = rgt;}
}
void callFlow(void *ovr, int idx)
{
	int tmp = 0;
	if (dep > 0) tmp = idt[dep-1];
	idt[dep++] = idx; flw[idx](bnd[idx],ovr,tmp); dep--;
}
void callNest(void *ovr)
{
	int idx = 0;
	while (idx < lim) {
		int ret;
		idt[dep++] = idx; ret = nst[idx](bnd[idx],ovr); dep--;
		if (ret == 0) idx++;
		while (ret > 0 && idx < lim) {
			if (rgt[idx] < ret) ret -= rgt[idx];
			else ret = 0;
			idx++;}
		while (ret < 0 && idx > 0) {
			if (lft[idx] < -ret) ret += lft[idx];
			else ret = 0;
			if (ret < 0 && idx > 0) idx--;}}
}
