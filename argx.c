#include "argx.h"
#include "face.h"
#include "type.h"
#include <string.h>
#include <lua.h>

lua_State *luaptr = 0; // global context for expressions
struct ArgxNest nst[NUMNEST] = {0}; // data flow control steps
int nlm = 0; // number of steps
int arg[NUMMODE] = {0}; // arguments passed to factory
fftype fac = 0; // factory for undashed arguments
int idx = 0; // for constant or immediate expression next
enum Argx opc; // for consuming next undashed argument
const char *atr[NUMMODE] = {0}; // char pos is arg value
int adx[NUMMODE] = {0}; // which arg to change
int aim = 0; // mututally exclusive arg values
const char *btr[NUMMODE] = {0}; // add char pos to index
int bdx[NUMMODE] = {0}; // base index of arg to change
int bim = 0; // consume constant or lua result for arg value
const char *ctr = 0; // char pos is opcode
const char *dtr[NUMMODE] = {0}; // name of arg changes
int ddx[NUMMODE][NUMMODE] = {0}; // arg index
int drg[NUMMODE][NUMMODE] = {0}; // arg value
int djm[NUMMODE] = {0}; // number of args to change
int dod[NUMMODE] = {0}; // whether to factory
int dim = 0; // double dash multiple arg changes
const char *etr[NUMDFLT] = {0}; // regex default condition
enum Dflt epc[NUMDFLT] = {0}; // default action
int edx[NUMDFLT] = {0}; // arg index for action
int erg[NUMDFLT] = {0}; // arg value for action
int eim = 0; // default argument machine
int eoc = 0; // default machine location

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
const char *setMode(const char *str)
{
	const char *tmp = ctr;
	ctr = str;
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
int addDflt(const char *str, enum Dflt opc, int idx, int arg)
{
	etr[eim] = str;
	epc[eim] = opc;
	edx[eim] = idx;
	erg[eim] = arg;
	return eim++;
}
int useArgument(const char *str)
{
	// TODO while str matches regex etr[eoc] change arg or eoc
	for (int i = 0; i < dim; i++) {
		if (str[0] == '-' && str[1] == '-' && strcmp(str+2,dtr[i]) == 0) {
			for (int j = 0; j < djm[i]; j++) {
				arg[ddx[i][j]] = drg[i][j];}
			if (dod[i] != 0) {
				opc = dod[i];
			} else {
				return nlm;}}}
	for (int i = 1; ctr[i-1] && i < ArgxArgx; i++) {
		if (str[0] == '-' && strncmp(str+1,ctr+i-1,1) == 0) {
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
		nst[nlm] = fac(str,arg);
		return nlm++;}
	return -1;
}
void runProgram()
{
	void *dat = 0; // data to flow between streams
	int siz = 0; // size of data to flow
	int typ = 0; // type of data to flow
	int idx = 0; // program counter
	while (idx < nlm) {
		switch (nst[idx].opc) {
		case (FlowArgx): {
			if (nst[idx].typ == -1) {
				if (nst[idx].str) luaSwitch(nst[idx].str,nst[idx].idx,&typ,&siz,&dat);
				else nst[idx].fnc(nst[idx].idx,&typ,&siz,&dat);}
			else { // TODO call hnc for field read or write
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
		case (JumpArgx): {
			// TODO allow comparison of arg[FieldShare] to getField or simply getValue for condition
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
const char *headName(const char *str)
{
	return 0; // TODO get ip address
}
const char *tailName(const char *str)
{
	return 0; // TODO get port number
}
const char *skipName(int num)
{
	return "0"; // TODO string from num
}
struct ArgxNest argxFactory(const char *str, int *arg)
{
	struct ArgxNest nest = {0};
	nest.opc = FlowArgx;
	nest.typ = arg[TypeShare];
	switch (arg[FlowShare]) {
		case (0/*e*/): nest.idx = forkExec(str); break;
		case (1/*f*/): nest.idx = openFile(str); break;
		case (2/*g*/): nest.idx = openInet(headName(str),tailName(str)); break;
		case (3/*h*/): nest.str = str; return nest;
		case (4/*p*/): nest.idx = pipeInit(arg[GateShare]?skipName(arg[SkipShare]):str,!arg[GateShare]?skipName(arg[SkipShare]):str); break;
		case (5/*q*/): nest.idx = openFifo(str); break;
		default: break;}
	if (arg[TypeShare] == -1) {
		switch (arg[GateShare]) {
			case (0/*i*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.fnc = 0; break; // TODO switch read text
				case (1/*b*/): nest.fnc = 0; break; // TODO switch read binary
				default: break;} break;
			case (1/*o*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.fnc = 0; break; // TODO switch write text
				case (1/*b*/): nest.fnc = 0; break; // TODO switch write binary
				default: break;} break;
			default: break;}}
	else if (arg[FieldShare] == -1) {
		switch (arg[GateShare]) {
			case (0/*i*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.gnc = 0; break; // TODO type read text
				case (1/*b*/): nest.gnc = 0; break; // TODO type read binary
				default: break;} break;
			case (1/*o*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.gnc = 0; break; // TODO type write text
				case (1/*b*/): nest.gnc = 0; break; // TODO type write binary
				default: break;} break;
			default: break;}}
	else {
		switch (arg[GateShare]) {
			case (0/*i*/): switch (arg[WrapShare]) {
				case (0/*a*/): /*nest.hnc = 0;*/ break; // TODO field read text
				case (1/*b*/): /*nest.hnc = 0;*/ break; // TODO field read binary
				default: break;} break;
			case (1/*o*/): switch (arg[WrapShare]) {
				case (0/*a*/): /*nest.hnc = 0;*/ break; // TODO field write text
				case (1/*b*/): /*nest.hnc = 0;*/ break; // TODO field write binary
				default: break;} break;
			default: break;}}
	return nest;
}
