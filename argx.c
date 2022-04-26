#include "argx.h"
#include <string.h>
#include <lua.h>

extern lua_State *luaptr; // global context for expressions
char *str = 0;
struct ArgxNest nst[NUMNEST] = {0}; // data flow control steps
int nlm = 0; // number of steps
void *dat = 0; // data to flow between streams
int siz = 0; // size of data to flow
int typ = 0; // type of data to flow
int stp = 0; // index into program steps
int arg[NUMMODE] = {0}; // arguments passed to factory
int mlm = 0; // number of factory arguments
int idx = 0; // for constant or immediate expression next
enum ArgxCode opc; // for consuming next undashed argument
fftype fac = 0; // factory for undashed arguments
const char *atr[NUMMODE] = {0}; // char pos is arg value
int adx[NUMMODE] = {0}; // which arg to change
int aim = 0; // mututally exclusive arg values
const char *btr[NUMMODE] = {0}; // add char pos to index
int bdx[NUMMODE] = {0}; // base index of arg to change
int bim = 0; // consume constant or lua result for arg value
const char *ctr[NUMMODE] = {0}; // char pos is opcode
int cim = 0; // consume lua expression for opcode
const char *dtr[NUMMODE] = {0}; // name of arg changes
int ddx[NUMMODE][NUMMODE] = {0}; // arg index
int drg[NUMMODE][NUMMODE] = {0}; // arg value
int djm[NUMMODE] = {0}; // number of args to change
int dim = 0; // double dash multiple arg changes

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
int addFlags(const char *str, int idx)
{
	adx[aim] = idx;
	atr[aim] = str;
	return aim++;
}
int addConst(const char *str, int idx)
{
	bdx[bim] = idx;
	btr[bim] = str;
	return bim++;
}
int addMode(const char *str)
{
	ctr[cim] = str;
	return cim++;
}
int addCustom(const char *str, int idx, int arg)
{
	drg[dim][djm[dim]] = arg;
	ddx[dim][djm[dim]] = idx;
	djm[dim]++;
	dtr[dim] = str;
	return dim++;
}
int useArgument(const char *str)
{
	for (int i = 0; i < dim; i++) {
		if (str[0] == '-' && str[1] == '-' && strcmp(str+2,dtr[i]) == 0) {
			for (int j = 0; j < djm[i]; j++) {
				arg[ddx[i][j]] = drg[i][j];
				return nlm;}}}
	for (int i = 1; i < cim && i < ArgxArgx; i++) {
		if (str[0] == '-' && strcmp(str+1,ctr[i-1]) == 0) {
			opc = i;
			return nlm;}}
	for (int i = 0; i < bim; i++) {
		for (int j = 0; j < strlen(btr[i]); j++) {
			if (str[0] == '-' && str[1] == btr[i][j] && str[2] == 0) {
				idx = bdx[i]+j+1;
				return nlm;}}}
	for (int i = 0; i < aim; i++) {
		for (int j = 0; j < strlen(atr[i]); j++) {
			if (str[0] == '-' && str[1] == atr[i][j] && str[2] == 0) {
				arg[adx[i]] = j;
				return nlm;}}}
	if (idx) {
		arg[idx-1] = getValue(str);
		idx = 0;
		return nlm;}
	else if (opc) {
		struct ArgxCnst cnst = getConst(str);
		nst[nlm].opc = opc;
		nst[nlm].str = cnst.str;
		nst[nlm].idx = cnst.idx;
		opc = 0;
		return nlm++;}
	else if (fac) {
		struct ArgxFlow flow = fac(str,arg,mlm);
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
