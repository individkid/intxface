#include "call.h"
#include <stdio.h>
#include <stdlib.h>
struct {
	int rgt; // towards leaf
	int nxt; // towards larger val
	long long val;
	tftype fnc;
	void *bnd;
} lnk[NUMCALL] = {0}; // linked list tree
int tlm = 0;
struct {
	fftype fnc;
	void *bnd;
} flw[NUMCALL] = {0}; // data flow function
int flm = 0;
int idt[NUMCALL] = {0}; // stack of caller idx
int dep = 0; // dedep of caller stack
struct {
	int lft;
	int rgt;
	nftype fnc;
	void *bnd;
} nst[NUMCALL] = {0}; // flow control function
int nlm = 0;
struct {
	int srt[2];
	long long val;
	eftype fnc;
	void *bnd;
} edg[NUMCALL] = {0}; // state machine function
int elm = 0;
long long now = 0;
int rdy = 0;
int takeTime()
{
	int idx = tlm;
	if (tlm == 0 && lnk[0].rgt == 0) for (int i = 0; i < NUMCALL; i++) lnk[i].rgt = i+1;
	if (tlm == NUMCALL) {fprintf(stderr,"empty time\n"); exit(-1);}
	tlm = lnk[idx].rgt;
	return idx;
}
void freeTime(int lst, int lft)
{
	int idx = -1;
	if (lst != -1 && lft != -1) {fprintf(stderr, "conflicting last left\n"); exit(-1);}
	if (lst != -1) {idx = lnk[lst].nxt; lnk[lst].nxt = lnk[idx].nxt;}
	if (lft != -1) {idx = lnk[lft].rgt; lnk[lft].rgt = lnk[idx].rgt;}
	if (idx == -1) {fprintf(stderr, "missing last left\n"); exit(-1);}
	lnk[idx].rgt = tlm;
	tlm = idx;
}
int saveTime(int lft, int rgt, int lst, int nxt, long long val, tftype fnc, void *bnd)
{
	int tak = takeTime();
	if (lft != -1 && rgt != 0 && lnk[lft].rgt != rgt) {fprintf(stderr,"conflicting left right\n"); exit(-1);}
	if (lst != -1 && nxt != 0 && lnk[lst].nxt != nxt) {fprintf(stderr,"conflicting last next\n"); exit(-1);}
	if (lft != -1) {rgt = lnk[lft].rgt; lnk[lft].rgt = tak;}
	if (lst != -1) {nxt = lnk[lst].nxt; lnk[lst].nxt = tak;}
	lnk[tak].val = val;
	lnk[tak].fnc = fnc;
	lnk[tak].bnd = bnd;
	lnk[tak].nxt = nxt;
	lnk[tak].rgt = rgt;
	return tak;
}
int markTime(int *pth, long long val)
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
void makeTime(void *ovr, tftype fnc, void *bnd, long long val)
{
	int pth[NUMLEVEL];
	int len = markTime(pth,val);
	if (len-- != 0) {
		int lst = pth[len];
		int nxt = lnk[lst].nxt;
		if (lst == 0) lst = saveTime(0,0,-1,0,0,0,0);
		saveTime(-1,0,lst,nxt,val,fnc,bnd);}
	while (len-- != 0) {
		int lst = pth[len];
		int rgt = lnk[lst].rgt;
		rgt = findTime(lst,rgt);
		if (rgt != 0) {
			int nxt = lnk[lst].nxt;
			fnc = lnk[rgt].fnc;
			bnd = lnk[rgt].bnd;
			val = lnk[rgt].val;
			if (lst == 0) lst = saveTime(0,lnk[0].rgt,-1,0,0,0,0);
			saveTime(-1,rgt,lst,nxt,val,fnc,bnd);}}
}
void callTime(void *ovr, long long val)
{
	int rgt = lnk[0].rgt;
	while (rgt != 0) {
		int tmp = lnk[rgt].rgt;
		int nxt = lnk[rgt].nxt;
		if (tmp == 0) while (nxt != 0 && lnk[nxt].val < val) {
			int tmp = lnk[nxt].nxt;
			lnk[nxt].fnc(lnk[nxt].bnd,ovr,lnk[nxt].val);
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
int makeFlow(fftype fnc, void *bnd)
{
	if (flm == NUMCALL) {fprintf(stderr, "empty flow\n"); exit(-1);}
	flw[flm].fnc = fnc;
	flw[flm].bnd = bnd;
	return flm++;
}
void callFlow(void *ovr, int idx)
{
	int tmp = 0;
	if (dep > 0) tmp = idt[dep-1];
	idt[dep++] = idx;
	flw[idx].fnc(flw[idx].bnd,ovr,tmp);
	dep--;
}
int makeNest(nftype fnc, void *bnd, int lft, int rgt)
{
	if (nlm == NUMCALL) {fprintf(stderr, "empty nest\n"); exit(-1);}
	nst[nlm].lft = lft;
	nst[nlm].rgt = rgt;
	nst[nlm].fnc = fnc;
	nst[nlm].bnd = bnd;
	return nlm++;
}
void callNest(void *ovr)
{
	int idx = 0;
	while (idx < nlm) {
		int ret;
		ret = nst[idx].fnc(nst[idx].bnd,ovr);
		if (ret == 0) idx++;
		while (ret > 0 && idx < nlm) {
			if (nst[idx].rgt < ret) ret -= nst[idx].rgt;
			else ret = 0;
			idx++;}
		while (ret < 0 && idx > 0) {
			if (nst[idx].lft < -ret) ret += nst[idx].lft;
			else ret = 0;
			if (ret < 0 && idx > 0) idx--;}}
}
void sortEdge(int sub, int min, int lim)
{
	int mid = (min+lim)/2;
	if (mid == min) return;
	for (int i = min; i < lim; i++) {
		edg[i].srt[!sub] = edg[i].srt[sub];}
	sortEdge(!sub,min,mid);
	sortEdge(!sub,mid,lim);
	for (int i = min, j = min, k = mid; i < lim; i++) {
		if (j < mid && k < lim && edg[edg[j].srt[!sub]].val < edg[edg[k].srt[!sub]].val) {
			edg[i].srt[sub] = edg[j].srt[!sub];}
		else if (j < mid && k < lim) {
			edg[i].srt[sub] = k++;}
		else if (j < mid) {
			edg[i].srt[sub] = j++;}
		else {
			edg[i].srt[sub] = k++;}}
}
int makeEdge(eftype fnc, void *bnd, long long src, long long dst)
{
	int bit = sizeof(long long)*4;
	long long msk = (1 << bit) - 1;
	if (elm == NUMCALL) {fprintf(stderr, "empty edge\n"); exit(-1);}
	edg[elm].val = (src & msk) | ((dst & msk) << bit);
	edg[elm].fnc = fnc;
	edg[elm].bnd = bnd;
	return elm++;
}
void callEdge(void *ovr, long long how, long long sel)
{
	int bit = sizeof(long long)*4;
	long long msk = (1 << bit) - 1;
	long long set = how & sel;
	long long clr = ~how & sel;
	long long src = now;
	long long dst = ((src | set) & clr);
	long long val = (src & msk) | ((dst & msk) << bit);
	int sub = elm/2;
	int mid = sub/2;
	if (!rdy) {
		for (int i = 0; i < elm; i++) {
			edg[i].srt[0] = i;}
		sortEdge(0,0,elm);
		rdy = 1;}
	while (val != edg[sub].val && mid != 0) {
		if (val < edg[sub].val) {
			sub -= mid;}
		else {
			sub += mid;}
		mid /= 2;}
	if (val != edg[sub].val) {fprintf(stderr,"missing edge\n"); exit(-1);}
	now = dst;
	edg[sub].fnc(edg[sub].bnd,ovr,src,dst);
}
