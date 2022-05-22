#include "argx.h"
#include "type.h"
#include "face.h"
#include <string.h>
#include <lua.h>

lua_State *luaptr = 0; // global context for expressions
gftype scr = 0; // script to execute before consuming undashed
int slm = 0; // count of number of arguments
struct ArgxNest nst[NUMNEST] = {0}; // data flow control steps
int nlm = 0; // number of steps
int arg[NUMMODE] = {0}; // arguments passed to factory
int mlm = 0; // number of factory arguments
fftype fac = 0; // factory for undashed arguments
int idx = 0; // for constant or immediate expression next
enum ArgxCode opc; // for consuming next undashed argument
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
int dod[NUMMODE] = {0}; // whether to factory
int dim = 0; // double dash multiple arg changes

void initLua();
void luaSwitch(const char *str, int idx, int *typ, int *siz, void **dat)
{
	if (!luaptr) initLua();
	// TODO modify data according to given script
}
void luaStream(const char *str, int idx, int *siz, void **dat)
{
	if (!luaptr) initLua();
	// TODO modify data according to given script
}
int getValue(const char *scr)
{
	if (!luaptr) initLua();
	return 0; // TODO return lua evaluation of str
}
void getNone(int idx)
{
	if (!luaptr) initLua();
	if (nst[idx].str == 0) return;
	// TODO just execute nst[idx].str
}
double getNumber(int idx)
{
	if (!luaptr) initLua();
	if (nst[idx].str == 0) return nst[idx].idx;
	return 0.0; // TODO return lua evaluation of nst[idx].str
}
int getCount(int idx)
{
	if (nst[idx].str == 0) return nst[idx].idx;
	return getValue(nst[idx].str);
}
struct ArgxCnst getConst(const char *scr)
{
	struct ArgxCnst cnst = {0};
	if (!luaptr) initLua();
	// TODO return idx if scr is constant, otherwise return scr
	return cnst;
}
gftype setScript(gftype fnc)
{
	gftype tmp = scr;
	scr = fnc;
	return tmp;
}
fftype setFactory(fftype fnc)
{
	fftype tmp = fac;
	fac = fnc;
	return tmp;
}
int setArg(int val, int idx)
{
	int tmp = arg[idx];
	arg[idx] = val;
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
int addMulti(const char *str, int mod)
{
	djm[dim] = 0;
	dod[dim] = mod;
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
	for (int rep = 0; (scr == 0 && rep == 0) || ((str = scr(str,slm,rep)) != 0); rep++) {
	for (int i = 0; i < dim; i++) {
		if (str[0] == '-' && str[1] == '-' && strcmp(str+2,dtr[i]) == 0) {
			for (int j = 0; j < djm[i]; j++) {
				arg[ddx[i][j]] = drg[i][j];}
			if (dod[i] != 0) {
				opc = dod[i];
			} else {
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
		nst[nlm].cnt = cnst.cnt;
		opc = 0;
		return nlm++;}
	else if (fac) {
		nst[nlm] = fac(str,arg,mlm);
		return nlm++;}}
	slm++;
	return -1;
}
void runProgram()
{
	void *dat = 0; // data to flow between streams
	int siz = 0; // size of data to flow
	int typ = 0; // type of data to flow
	int idx = 0; // program counter
	int lab[NUMJUMP] = {0}; // label stack and heap
	int llm = 0; // label stack depth
	while (idx < nlm) {
		switch (nst[idx].opc) {
		case (FlowArgx): {
			if (nst[idx].typ == -1) {
				if (nst[idx].str) luaSwitch(nst[idx].str,nst[idx].idx,&typ,&siz,&dat);
				else nst[idx].fnc(nst[idx].idx,&typ,&siz,&dat);}
			else {
				if (nst[idx].str) luaStream(nst[idx].str,nst[idx].idx,&siz,&dat);
				else nst[idx].gnc(nst[idx].idx,&siz,&dat);
				typ = nst[idx].typ;}
			idx++;
			break;}
		case (NoneArgx): {
			getNone(idx);
			break;}
		case (WaitArgx): {
			double val = getNumber(idx);
			pauseAny(val);
			break;}
		case (BackArgx): {
			int val = getCount(idx);
			if (val == 0) idx++;
			for (;val > 0 && idx < nlm;idx++) if (nst[idx].opc == FlowArgx) val--;
			for (;val < 0 && idx > 0;idx--) if (nst[idx].opc == FlowArgx) val++;
			break;}
		case (JumpArgx): {
			int val = getCount(idx);
			if (val > 0 && val-1 < NUMJUMP) lab[val-1] = ++idx;
			else if (val < 0 && 1-val < NUMJUMP) idx = lab[1-val];
			else idx++;
			break;}
		case (LoopArgx): {
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
		case (NestArgx): {
			idx++;
			break;}
		default: {
			idx++;
			break;}}}
}
void initLua()
{
	// TODO lua has access to lots of the above
}
