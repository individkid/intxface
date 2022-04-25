#include "argx.h"
#include <string.h>
#include <lua.h>

extern lua_State *luaptr; // global context for expressions
struct ArgxNest nst[NUMNEST] = {0}; // data flow control steps
int nlm = 0; // number of steps
void *dat = 0; // data to flow between streams
int siz = 0; // size of data to flow
int typ = 0; // type of data to flow
int stp = 0; // index into program steps
int mod[NUMMODE] = {0}; // arguments passed to factory
int mlm = 0; // changing way to handle undashed arguments
enum ArgxCode und; // for consuming next undashed argument
fftype fac = 0; // factory for undashed arguments
const char *flg[NUMMODE] = {0}; // char position is mod value
int adx[NUMMODE] = {0}; // which mod to use
int alm = 0; // mututally exclusive way to change handling
const char *nxt[NUMMODE] = {0}; // char position is opcode
int blm = 0; // next undashed is value for opcode
const char *dbl[NUMMODE] = {0}; // name of custom stream
sftype cst[NUMMODE] = {0}; // function for custom stream
int clm = 0; // double dash like custom undashed

struct ArgxCnst getConst(const char *str)
{
	struct ArgxCnst cnst = {0};
	// TODO return idx if str is constant, otherwise return str
	return cnst;
}

fftype setFactory(fftype fnc)
{
	fftype tmp = fac;
	fac = fnc;
	return tmp;
}
int addFlags(int idx, const char *chr)
{
	adx[alm] = idx;
	flg[alm] = chr;
	return alm++;
}
int addMode(const char *chr)
{
	nxt[blm] = chr;
	return blm++;
}
int addCustom(const char *pat, sftype fnc)
{
	dbl[clm] = pat;
	cst[clm] = fnc;
	return clm++;
}
int useArgument(const char *arg)
{
	for (int i = 0; i < clm; i++) {
		if (arg[0] == '-' && arg[1] == '-' && strcmp(arg+2,dbl[i]) == 0) {
			nst[nlm].opc = FlowArgx;
			nst[nlm].fnc = cst[i];
			return nlm++;}}
	for (int i = 1; i < blm && i < ArgxArgx; i++) {
		if (arg[0] == '-' && strcmp(arg+1,nxt[i]) == 0) {
			und = i;
			return nlm;}}
	for (int i = 0; i < alm; i++) {
		for (int j = 0; j < strlen(flg[i]); j++) {
			if (arg[0] == '-' && arg[1] == flg[i][j] && arg[2] == 0) {
				mod[adx[i]] = j;
				return nlm;}}}
	if (und) {
		struct ArgxCnst cnst = getConst(arg);
		nst[nlm].opc = und;
		nst[nlm].str = cnst.str;
		nst[nlm].idx = cnst.idx;
		return nlm++;}
	else if (fac) {
		struct ArgxFlow flow = fac(arg,mod,mlm);
		nst[nlm].opc = FlowArgx;
		nst[nlm].fnc = flow.fnc;
		nst[nlm].idx = flow.idx;
		return nlm++;}
	return -1;
}
void runProgram()
{
	// TODO
}
