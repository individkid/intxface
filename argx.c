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
int idx = 0; // for constant or immediate expression next
enum ArgxCode opc; // for consuming next undashed argument
fftype fac = 0; // factory for undashed arguments
const char *ach[NUMMODE] = {0}; // char position is mod value
int adx[NUMMODE] = {0}; // which mod to use
int alm = 0; // mututally exclusive way to change handling
const char *bch[NUMMODE] = {0}; // char position is mod offset
int bdx[NUMMODE] = {0}; // base mod to use
int blm = 0; // constant or lua result for factory argument
const char *cch[NUMMODE] = {0}; // char position is opcode
int clm = 0; // next undashed is value for opcode
const char *dst[NUMMODE] = {0}; // name of custom stream
sftype dfn[NUMMODE] = {0}; // function for custom stream
int dlm = 0; // double dash like custom undashed

struct ArgxCnst getConst(const char *str)
{
	struct ArgxCnst cnst = {0};
	// TODO return idx if str is constant, otherwise return str
	return cnst;
}
int getValue(const char *str)
{
	// TODO return atoi identifier or lua evaluation
	return 0;
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
	ach[alm] = chr;
	return alm++;
}
int addConst(int idx, const char *chr)
{
	bdx[blm] = idx;
	bch[blm] = chr;
	return blm++;
}
int addMode(const char *chr)
{
	cch[clm] = chr;
	return clm++;
}
int addCustom(const char *pat, sftype fnc)
{
	dst[dlm] = pat;
	dfn[dlm] = fnc;
	return dlm++;
}
int useArgument(const char *arg)
{
	for (int i = 0; i < dlm; i++) {
		if (arg[0] == '-' && arg[1] == '-' && strcmp(arg+2,dst[i]) == 0) {
			nst[nlm].opc = FlowArgx;
			nst[nlm].fnc = dfn[i];
			return nlm++;}}
	for (int i = 1; i < clm && i < ArgxArgx; i++) {
		if (arg[0] == '-' && strcmp(arg+1,cch[i]) == 0) {
			opc = i;
			return nlm;}}
	for (int i = 0; i < blm; i++) {
		for (int j = 0; j < strlen(bch[i]); j++) {
			if (arg[0] == '-' && arg[1] == bch[i][j] && arg[2] == 0) {
				idx = bdx[i]+j+1;
				return nlm;}}}
	for (int i = 0; i < alm; i++) {
		for (int j = 0; j < strlen(ach[i]); j++) {
			if (arg[0] == '-' && arg[1] == ach[i][j] && arg[2] == 0) {
				mod[adx[i]] = j;
				return nlm;}}}
	if (idx) {
		mod[idx-1] = getValue(arg);
		idx = 0;
		return nlm;}
	else if (opc) {
		struct ArgxCnst cnst = getConst(arg);
		nst[nlm].opc = opc;
		nst[nlm].str = cnst.str;
		nst[nlm].idx = cnst.idx;
		opc = 0;
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
