#include "call.h"
#include <stdio.h>
#include <stdlib.h>
tftype clk[NUMCALL] = {0}; // time wheel function
fftype flw[NUMCALL] = {0}; // data flow function
nftype nst[NUMCALL] = {0}; // flow control function
void *bnd[NUMCALL] = {0}; // per-idx argument
struct {
	int rgt; // towards leaf
	int nxt; // towards larger val
	long long val;
	int box;
} lnk[NUMNODE] = {0}; // linked list tree
int idt[NUMCALL] = {0}; // stack of caller idx
int dep = 0; // dedep of caller stack
int lft[NUMCALL] = {0}; // flow control opens
int rgt[NUMCALL] = {0}; // flow control closes
int lim = 0;
int poo = 0; // unused from lnk
int initCall(void *arg, tftype tfn, fftype ffn, nftype nfn, int brg, int crg)
{
	if (lim >= NUMCALL) {fprintf(stderr,"full list\n"); exit(-1);}
	clk[lim] = tfn;
	flw[lim] = ffn;
	nst[lim] = nfn;
	bnd[lim] = arg;
	lft[lim] = brg;
	rgt[lim] = crg;
	return lim++;
}
int takeTime()
{
	int idx = poo;
	if (poo == 0 && lnk[0].rgt == 0) for (int i = 0; i < NUMNODE; i++) lnk[i].rgt = i+1;
	if (poo == NUMNODE) {fprintf(stderr,"empty pool\n"); exit(-1);}
	poo = lnk[idx].rgt;
	return idx;
}
void freeTime(int lst, int lft)
{
	int idx = -1;
	if (lst != -1 && lft != -1) {fprintf(stderr, "conflicting last left\n"); exit(-1);}
	if (lst != -1) {idx = lnk[lst].nxt; lnk[lst].nxt = lnk[idx].nxt;}
	if (lft != -1) {idx = lnk[lft].rgt; lnk[lft].rgt = lnk[idx].rgt;}
	if (idx == -1) {fprintf(stderr, "missing last left\n"); exit(-1);}
	lnk[idx].rgt = poo;
	poo = idx;
}
int makeTime(int lft, int rgt, int lst, int nxt, int box, long long val)
{
	int tak = takeTime();
	if (lft != -1 && rgt != 0 && lnk[lft].rgt != rgt) {fprintf(stderr,"conflicting left right\n"); exit(-1);}
	if (lst != -1 && nxt != 0 && lnk[lst].nxt != nxt) {fprintf(stderr,"conflicting last next\n"); exit(-1);}
	if (lft != -1) {rgt = lnk[lft].rgt; lnk[lft].rgt = tak;}
	if (lst != -1) {nxt = lnk[lst].nxt; lnk[lst].nxt = tak;}
	lnk[tak].val = val;
	lnk[tak].box = box;
	lnk[tak].nxt = nxt;
	lnk[tak].rgt = rgt;
	return tak;
}
int pathTime(int *pth, long long val)
{
	int rgt = lnk[0].rgt;
	int len = 0;
	pth[len++] = 0;
	while (rgt != 0) {
		int lst = rgt;
		int nxt = lnk[rgt].nxt;
		while (nxt != 0 && lnk[nxt].val < val) {
			lst = nxt;
			nxt = lnk[nxt].nxt;}
		pth[len++] = lst;
		rgt = lnk[rgt].rgt;}
	return len;
}
int findTime(int lst, int rgt)
{
	int nxt =rgt;
	int cnt = 0;
	int lim = lnk[lst].nxt;
	if (lim != 0) lim = lnk[lim].rgt;
	while (nxt != lim) {
		if ((++cnt)%2 == 0) rgt = lnk[rgt].nxt;
		nxt = lnk[nxt].nxt;}
	if (cnt < NUMEACH) return 0;
	return rgt;
}
void saveTime(void *ovr, int box, long long val)
{
	int pth[NUMLEVEL];
	int len = pathTime(pth,val);
	if (len-- != 0) {
		int lst = pth[len];
		int nxt = lnk[lst].nxt;
		if (lst == 0) lst = makeTime(0,0,-1,0,0,0);
		makeTime(-1,0,lst,nxt,box,val);}
	while (len-- != 0) {
		int lst = pth[len];
		int rgt = lnk[lst].rgt;
		rgt = findTime(lst,rgt);
		if (rgt != 0) {
			int nxt = lnk[lst].nxt;
			box = lnk[rgt].box;
			val = lnk[rgt].val;
			if (lst == 0) lst = makeTime(0,lnk[0].rgt,-1,0,0,0);
			makeTime(-1,rgt,lst,nxt,box,val);}}
}
void callTime(void *ovr, long long val)
{
	int rgt = lnk[0].rgt;
	while (rgt != 0) {
		int tmp = lnk[rgt].rgt;
		int nxt = lnk[rgt].nxt;
		if (tmp == 0) while (nxt != 0 && lnk[nxt].val < val) {
			int tmp = lnk[nxt].nxt;
			int box = lnk[nxt].box;
			idt[dep++] = box; clk[box](bnd[box],ovr,lnk[nxt].val); dep--;
			freeTime(rgt,-1);
			nxt = tmp;}
		else while (nxt != 0 && lnk[nxt].val < val) {
			int tmp = lnk[nxt].nxt;
			freeTime(rgt,-1);
			nxt = tmp;}
		rgt = tmp;}
	while ((rgt = lnk[0].rgt) != 0 && lnk[rgt].nxt == 0) {
		freeTime(-1,0);}
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
