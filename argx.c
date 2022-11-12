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

int nestJump(int idx, struct ArgxNest *nst, void *jmp)
{
	return 0;
}
int addFlow(const char *str, nftype nft)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	opt[lpt] = str;
	fnc[lpt].opt = FlowTag;
	fnc[lpt].fnc = nft;
	return lpt++;
}
int addJump(const char *str, mftype mft)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	opt[lpt] = str;
	fnc[lpt].opt = JumpTag;
	fnc[lpt].gnc = mft;
	return lpt++;
}
int addNest(const char *str, oftype oft)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	opt[lpt] = str;
	fnc[lpt].opt = NestTag;
	fnc[lpt].hnc = oft;
	return lpt++;
}
int useAcum(int idx)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	fnc[lpt].opt = NoopTag;
	fnc[lpt].arg = memxAcum(fnc[idx].arg);
	return lpt++;
}
int useHist(int idx)
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	fnc[lpt].opt = NoopTag;
	fnc[lpt].arg = memxHist(fnc[idx].arg);
	return lpt++;
}
int useNoop()
{
	if (lpt == NUMARGX) ERROR(exitErr,0);
	fnc[lpt].opt = NoopTag;
	fnc[lpt].arg = memxNoop();
	return lpt++;
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