#include "argx.h"
#include "memx.h"
#include "type.h"
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdlib.h>

const char *str[NUMARGX] = {0}; // dash option types
struct ArgxNest fnc[NUMARGX] = {0}; // function to use
int sim = 0; // number of types
const char *ctr[NUMARGX] = {0}; // dash option callback
struct Function cnc[NUMARGX] = {0}; // function to use
int cef[NUMARGX] = {0}; // reference for function
int cim = 0; // number of types
const char *dtr[NUMARGX] = {0}; // dash option default
struct Function dnc[NUMARGX] = {0}; // function to use
int def[NUMARGX] = {0}; // reference for function
int aim = 0; // number of types
const char *etr[NUMARGX] = {0}; // dash option context
struct Function enc[NUMARGX] = {0}; // function to use
int eef[NUMARGX] = {0}; // reference for function
int eim = 0; // number of types

struct ArgxNest nst[NUMARGX] = {0}; // data flow control steps
int nim = 0; // number of steps

int use = 0; // which fnc to use
int opt = 0; // which dash matched

int idx = 0; // program counter

int nestJumpF(int idx, int dir, int cnt, int cmp, int lvl)
{
	int dpt = 0;
	if (dir > 1 || dir < -1 || dir == 0) ERROR(exitErr,0);
	if (cmp > 1 || cmp < -1 || cmp == 0) ERROR(exitErr,0);
	while (idx >= 0 && idx < nim && cnt > 0) {
		int cnd = 0; // maybe count if flow or nest exit
		if (nst[idx].opt == NestTag) {
			int dif = memxInt(nst[idx].run);
			dpt += dir*dif;
			cnd = ((dir > 0) != (dif > 0));}
		if (nst[idx].opt == FlowTag) cnd = 1;
		// count if given level is given relation to nest level
		if (cnd && ((cmp > 0) == (dpt > lvl) || dpt == lvl)) cnt -= 1;
		idx += dir;}
	return idx;
}
int nestJump(int idx, void *jmp)
{
	int stp = 0;
	while (idx >= 0 && idx < nim && stp < memxSize(jmp)) {
		void *mem = memxSkip(jmp,stp);
		int cnt = memxInt(memxSkip(mem,1));
		switch ((enum Step)memxInt(memxSkip(mem,0))) {
		case (FwdSkp): idx = nestJumpF(idx,1,cnt,-1,0); break;
		case (RevSkp): idx = nestJumpF(idx,-1,cnt,-1,0); break;
		case (FwdEnt): idx = nestJumpF(idx,1,1,1,cnt); break;
		case (RevEnt): idx = nestJumpF(idx,-1,1,1,cnt); break;
		case (FwdExt): idx = nestJumpF(idx,1,1,-1,-cnt); break;
		case (RevExt): idx = nestJumpF(idx,-1,1,-1,-cnt); break;
		default: break;}
	}
	return idx;
}
int argxJump(void *use)
{
	void *tmp = 0;
	const char *str = 0;
	int val = 0;
	memxCopy(&tmp,use);
	val = nestJump(idx,tmp);
	memxDone(&tmp);
	return val;
}
void argxCopy(void **run, void *use)
{
	memxCopy(run,nst[nestJump(idx,use)].run);
}
void argxKeep(void **run, void *use)
{
	memxKeep(run,nst[nestJump(idx,use)].run);
}
struct ArgxNest *argxGet(int idx)
{
	return nst+idx;
}
void *argxUse(int idx)
{
	return nst[idx].use;
}
void *argxRun(int idx)
{
	return nst[idx].run;
}
int argxHere()
{
	return idx;
}
int getLocation()
{
	nst[nim].opt = NoopTag;
	return nim++;
}
int addOption(const char *opt, struct Function use, struct Function run)
{
	str[sim] = opt;
	fnc[sim].opc = FlowTag;
	fnc[sim].fnc = use;
	fnc[sim].gnc = run;
	return sim++;
}
int addFlag(const char *opt, struct Function use, struct Function run)
{
	str[sim] = opt;
	fnc[sim].opc = FlagTag;
	fnc[sim].fnc = use;
	fnc[sim].gnc = run;
	return sim++;
}
int addJump(const char *opt, struct Function use, struct Function run)
{
	str[sim] = opt;
	fnc[sim].opc = JumpTag;
	fnc[sim].fnc = use;
	fnc[sim].gnc = run;
	return sim++;
}
int addNest(const char *opt, struct Function use, struct Function run)
{
	str[sim] = opt;
	fnc[sim].opc = NestTag;
	fnc[sim].fnc = use;
	fnc[sim].gnc = run;
	return sim++;
}
int mapCallback(const char *opt, int ref, struct Function fnc)
{
	ctr[cim] = opt;
	cnc[cim] = fnc;
	cef[cim] = ref;
	return cim++;
}
int mapDefault(const char *opt, int ref, struct Function fnc)
{
	dtr[aim] = opt;
	dnc[aim] = fnc;
	def[aim] = ref;
	return aim++;
}
int mapContext(const char *opt, int ref, struct Function fnc)
{
	etr[eim] = opt;
	enc[eim] = fnc;
	eef[eim] = ref;
	return eim++;
}
int useArgument(const char *arg)
{
	if (arg[0] == '-' && arg[1] != '-') {
		for (int i = 0; i < sim; i++) {
			for (int j = 0; str[i][j]; j++) {
				if (arg[1] == str[i][j]) {
					use = i; opt = j;}}}
		if (fnc[use].opc != FlagTag) return nim;}
	nst[nim] = fnc[use];
	nst[nim].opt = opt;
	memxInit(&nst[nim].str,arg);
	for (int i = 0; i < eim; i++) {
		for (int j = 0; etr[i][j]; j++) {
			if (opt == etr[i][j]) {
				memxBack(&nst[nim].use,&nst[eef[i]].use,enc[i]);}}}
	memxCall(&nst[nim].use,nst[nim].str,nst[nim].fnc);
	for (int i = 0; i < cim; i++) {
		for (int j = 0; ctr[i][j]; j++) {
			if (opt == ctr[i][j]) {
				memxBack(&nst[nim].run,&nst[cef[i]].run,cnc[i]);}}}
	for (int i = 0; i < aim; i++) {
		for (int j = 0; dtr[i][j]; j++) {
			if (opt == dtr[i][j]) {
				memxDflt(&nst[nim].run,&nst[def[i]].run,dnc[i]);}}}
	return nim++;
}
void runProgram()
{
	while (idx >= 0 && idx < nim) {
		if (nst[idx].opt != NoopTag) memxCall(&nst[idx].run,nst[idx].use,nst[idx].gnc);
		if (nst[idx].opt == JumpTag) idx = memxInt(nst[idx].run);
		else idx++;}
}