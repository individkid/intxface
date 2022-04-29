#include "argx.h"
#include <string.h>
#include <lua.h>

extern lua_State *luaptr; // global context for expressions
lftype env = 0; // function to initialize lua environment
const char *scr = 0; // script to execute before consuming undashed
struct ArgxNest nst[NUMNEST] = {0}; // data flow control steps
int nlm = 0; // number of steps
int arg[NUMMODE] = {0}; // arguments passed to factory
int mlm = 0; // number of factory arguments
int lab[NUMJUMP] = {0}; // label stack and heap
int llm = 0; // label stack depth
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

void initLua();
const char *getString(const char *scr, const char *str)
{
	if (!luaptr) initLua();
	// TODO execute given script in luaptr environment
	return 0;
}
int getValue(const char *scr)
{
	if (!luaptr) initLua();
	// TODO return atoi identifier or lua evaluation
	return 0;
}
int getCount(int idx)
{
	if (nst[idx].str) return getValue(nst[idx].str);
	return nst[idx].idx;
}
struct ArgxCnst getConst(const char *scr)
{
	struct ArgxCnst cnst = {0};
	if (!luaptr) initLua();
	// TODO return idx if scr is constant, otherwise return scr
	return cnst;
}
lftype setLua(lftype fnc)
{
	lftype tmp = env;
	env = fnc;
	return tmp;
}
const char *setScript(const char *str)
{
	const char *tmp = scr;
	scr = str;
	return tmp;
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
int addMulti(const char *str)
{
	djm[dim] = 0;
	dtr[dim] = str;
	return dim++;
}
int addElem(int dim, int idx, int arg)
{
	drg[dim][djm[dim]] = arg;
	ddx[dim][djm[dim]] = idx;
	return djm[dim]++;
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
	if (scr) str = getString(scr,str);
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
	void *dat = 0; // data to flow between streams
	int siz = 0; // size of data to flow
	int typ = 0; // type of data to flow
	int idx = 0;
	while (idx < nlm) {
		switch (nst[idx].opc) {
		case(FlowArgx): {
			nst[idx].fnc(nst[idx].idx,&typ,&siz,&dat);
			idx++;
			break;}
		case(NestArgx): {
			idx++;
			break;}
		case(JumpArgx): {
			int val = getCount(idx);
			if (val > 0) lab[val-1] = ++idx;
			else if (val < 0) idx = lab[val+1];
			else idx++;
			break;}
		case(PushArgx): {
			int val = getCount(idx);
			for (idx++;val > 0;val--) lab[llm++] = idx;
			for (;val < 0;val++) idx = lab[--llm];
			break;}
		case(LoopArgx): {
			int val = getCount(idx);
			while (val > 0 && ++idx < nlm) {
				if (nst[idx].opc == NestArgx) {
					val += getCount(idx);
					if (val < 0) val = 0;}}
			while (val < 0 && --idx > 0) {
				if (nst[idx].opc == NestArgx) {
					val += getCount(idx);
					if (val > 0) val = 0;}}
			break;}
		case(BackArgx): {
			int val = getCount(idx);
			if (val == 0) idx++;
			for (;val > 0 && idx < nlm;idx++) if (nst[idx].opc == FlowArgx) val--;
			for (;val < 0 && idx > 0;idx--) if (nst[idx].opc == FlowArgx) val++;
			break;}
		default: {
			idx++;
			break;}}}
}
void initLua()
{
	// TODO has access to lots of the above
	// TODO call env if nonzero to give even more access
}