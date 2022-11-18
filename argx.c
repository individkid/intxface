#include "proto.h"
#include "argx.h"
#include "memx.h"
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdlib.h>

struct ArgxNest nst[NUMARGX] = {0}; // data flow control steps
int lst = 0; // number of steps
const char *opt[NUMARGX] = {0}; // dash option types
struct ArgxNest fnc[NUMARGX] = {0}; // function to use
int lpt = 0; // number of types
int use = 0; // which fnc to use
int dsh = 0; // which dash matched

int nestJumpF(int idx, int dir, int cnt, int cmp, int lvl)
{
	int dpt = 0;
	if (dir > 1 || dir < -1 || dir == 0) ERROR(exitErr,0);
	if (cmp > 1 || cmp < -1 || cmp == 0) ERROR(exitErr,0);
	while (idx >= 0 && idx < lst && cnt > 0) {
		int cnd = 0; // maybe count if flow or nest exit
		if (nst[idx].opt == NestTag) {
			int dif = nst[idx].hnc(idx,nst);
			dpt += dir*dif;
			cnd = ((dir > 0) != (dif > 0));}
		if (nst[idx].opt == FlowTag) cnd = 1;
		// count if given level is given relation to nest level
		if (cnd && ((cmp > 0) == (dpt > lvl) || dpt == lvl)) cnt -= 1;
		idx += dir;}
	return idx;
}
int nestJump(int idx, struct ArgxNest *nst, void *jmp)
{
	int stp = 0;
	while (idx >= 0 && idx < lst) {
		enum ArgxStep dir = memxInt(memxElem(memxElem(jmp,stp),0));
		int cnt = memxInt(memxElem(memxElem(jmp,stp),1));
		switch (dir) {
		case (FwdSkpStep): idx = nestJumpF(idx,1,cnt,-1,0); break;
		case (RevSkpStep): idx = nestJumpF(idx,-1,cnt,-1,0); break;
		case (FwdEntStep): idx = nestJumpF(idx,1,1,1,cnt); break;
		case (RevEntStep): idx = nestJumpF(idx,-1,1,1,cnt); break;
		case (FwdExtStep): idx = nestJumpF(idx,1,1,-1,-cnt); break;
		case (RevExtStep): idx = nestJumpF(idx,-1,1,-1,-cnt); break;
		default: break;}
	}
	return lst;
}
int addFlow(const char *str, nftype nft, nftype nit)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	opt[lpt] = str;
	fnc[lpt].opt = FlowTag;
	fnc[lpt].fnc = nft;
	fnc[lpt].nit = nit;
	return lpt++;
}
int addJump(const char *str, mftype mft, nftype nit)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	opt[lpt] = str;
	fnc[lpt].opt = JumpTag;
	fnc[lpt].gnc = mft;
	fnc[lpt].nit = nit;
	return lpt++;
}
int addNest(const char *str, oftype oft, nftype nit)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	opt[lpt] = str;
	fnc[lpt].opt = NestTag;
	fnc[lpt].hnc = oft;
	fnc[lpt].nit = nit;
	return lpt++;
}
int useLocation(const char *opt)
{
	return lst++; // TODO remember opt for flows to cache into lst
}
int useArgument(const char *str)
{
	int result = lst;
	if (str[0] == '-' && str[1] != '-') {
		for (int i = 0; i < lpt; i++) {
			for (int j = 0; opt[i][j]; j++) {
				if (str[1] == opt[i][j]) {
					use = i; dsh = j;}}}}
	else {
		nst[lst] = fnc[use];
		nst[lst].idx = dsh;
		nst[lst].str = str;
		nst[lst].nit(lst,nst);
		lst++;}
	return result;
}
void runProgram()
{
	int idx = 0;
	while (idx >= 0 && idx < lst) switch (nst[idx].opt) {
	case (FlowTag): nst[idx].fnc(idx,nst); idx++; break;
	case (JumpTag): idx = nestJump(idx,nst,nst[idx].gnc(idx,nst)); break;
	case (NestTag): idx++; break; // hnc called by nestJump
	default: ERROR(exitErr,0); break;}
}