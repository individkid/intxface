#include "type.h"
#include "face.h"
#include "datx.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <regex.h>

// these are thread safe is set once and left
void *prefix = 0;
rktype datxNoteFp = 0;
retfp retptr = 0;
setfp setptr = 0;
getfp getptr = 0;
putfp putptr = 0;
fldfp fldptr = 0;
extfp extptr = 0;
immfp immptr = 0;

// these are not thread safe
void ***datx = 0;
int ndatx = 0;
int datxSubs = 0;
int datxSub0 = 0;
int datxSub1 = 0;
int datxSub2 = 0;
int datxSub3 = 0;
int datxIdx0 = 0;
int datxIdx1 = 0;
int datxIdx2 = 0;
int datxIdx3 = 0;
void **datxDat0 = 0;
void **datxDat1 = 0;
void **datxDat2 = 0;
void **datxDat3 = 0;

// TODO protect with sem_t for thread safety
int sizs = 0;
int *typs = 0;
void **boxs = 0;
void **keys = 0;

// TODO protect with sem_t for thread safety
regex_t *regexp = 0;
int regsiz = 0;
struct Irrex *irrexp = 0;
int irrsiz = 0;

int datxSub()
{
	int sub = ndatx;
	void *dat = 0; // null dat is allocated when it is passed as a result parameter
	void **ptr = malloc(sizeof(void*)); *ptr = dat; // ptr is a result parameter, passed as is
	datx = realloc(datx,(++ndatx)*sizeof(void**)); // datx is an array of result parameters
	datx[sub] = ptr;
	return sub;
}
void **datxDat(int sub)
{
	return datx[sub];
}
void datxNon()
{
	// TODO free memory created by datxSub
}
void datxSingle()
{
	if (datxSubs) return;
	datxSubs = 4;
	datxSub0 = datxSub();
	datxSub1 = datxSub();
	datxSub2 = datxSub();
	datxSub3 = datxSub();
	datxIdx0 = puntInit(datxSub0,datxSub0,datxReadFp,datxWriteFp);
	datxIdx1 = puntInit(datxSub1,datxSub1,datxReadFp,datxWriteFp);
	datxIdx2 = puntInit(datxSub2,datxSub2,datxReadFp,datxWriteFp);
	datxIdx3 = puntInit(datxSub3,datxSub3,datxReadFp,datxWriteFp);
	datxDat0 = datxDat(datxSub0);
	datxDat1 = datxDat(datxSub1);
	datxDat2 = datxDat(datxSub2);
	datxDat3 = datxDat(datxSub3);
}

void datxVoid(void **dat, int siz)
{
	*dat = realloc(*dat,siz+sizeof(int));
	*(int*)*dat = siz;
}
int datxVoids(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat;
}
void *datxVoidz(int num, void *dat)
{
	if (!dat) ERROR();
	return (void*)((char*)(((int*)dat)+1)+num);
}
int datxReadFp(int fildes, void *buf, int nbyte)
{
	void **dat = datxDat(fildes);
	void *pre = 0;
	void *suf = 0;
	datxSplit(&pre,&suf,*dat,nbyte);
	if (*(int*)pre != nbyte) return 0;
	memcpy(buf,datxVoidz(0,pre),nbyte);
	assignDat(dat,suf);
	free(pre);
	free(suf);
	return nbyte;
}
int datxWriteFp(int fildes, const void *buf, int nbyte)
{
	void **dat = datxDat(fildes);
	void *suf = malloc(sizeof(int)+nbyte);
	void *pre = 0;
	*(int*)suf = nbyte;
	memcpy(datxVoidz(0,suf),buf,nbyte);
	assignDat(&pre,*dat);
	datxJoin(dat,pre,suf);
	free(pre);
	free(suf);
	return nbyte;
}
void datxSplit(void **pre, void **suf, const void *dat, int len)
{
	*pre = realloc(*pre,len+sizeof(int));
	*suf = realloc(*suf,*(int*)dat-len+sizeof(int));
	*(int*)*pre = len;
	*(int*)*suf = *(int*)dat-len;
	memcpy((void*)((int*)*pre+1),(void*)((int*)dat+1),len);
	memcpy((void*)((int*)*suf+1),(char*)((int*)dat+1)+len,*(int*)dat-len);
}
void datxJoin(void **dat, const void *pre, const void *suf)
{
	int zro = 0;
	if (pre == 0) pre = &zro;
	if (suf == 0) suf = &zro;
	*dat = realloc(*dat,*(int*)pre+*(int*)suf+sizeof(int));
	*(int*)*dat = *(int*)pre+*(int*)suf;
	memcpy((void*)((int*)*dat+1),(void*)((int*)pre+1),*(int*)pre);
	memcpy((char*)((int*)*dat+1)+*(int*)pre,(void*)((int*)suf+1),*(int*)suf);
}
int datxCompare(void *one, void *oth)
{
	int lne = (one ? *(int*)one : 0);
	int lth = (oth ? *(int*)oth : 0);
	int len = lne+sizeof(int);
	if (lne == 0 && lth == 0) return 0;
	if (lne < lth) return -1;
	if (lne > lth) return 1;
	for (int i = sizeof(int); i < len; i++) {
		if (*((char*)one+i) < *((char*)oth+i)) return -1;
		if (*((char*)one+i) > *((char*)oth+i)) return 1;}
	return 0;
}
int datxFind(void **val, void *key)
{
	int idx = 0; int siz = sizs; void *dat = 0;
	if (prefix) datxJoin(&dat,prefix,key); else assignDat(&dat,key);
	while (siz > 0) {int cmp = 0;
	if (idx < 0 || idx >= sizs) ERROR();
	cmp = datxCompare(dat,keys[idx+siz/2]);
	if (cmp == 0) {assignDat(val,boxs[idx+siz/2]); free(dat); return typs[idx+siz/2];}
	if (cmp < 0) {siz = siz/2;}
	if (cmp > 0) {idx = idx + siz/2 + 1; siz = siz - siz/2 - 1;}}
	if (datxCompare(dat,keys[idx]) != 0) {free(dat); return -1;}
	assignDat(val,boxs[idx]); free(dat); return typs[idx];
}
void datxInsert(void *key, void *val, int typ)
{
	int idx = 0; int siz = sizs; void *dat = 0;
	if (prefix) datxJoin(&dat,prefix,key);
	else assignDat(&dat,key);
	while (siz > 0) {int cmp = 0;
	if (idx < 0 || idx >= sizs) ERROR();
	cmp = datxCompare(dat,keys[idx+siz/2]);
	if (cmp == 0) {
	assignDat(&boxs[idx+siz/2],val);
	typs[idx+siz/2] = typ; free(dat); return;}
	if (cmp < 0) {siz = siz/2;}
	if (cmp > 0) {idx = idx + siz/2 + 1; siz = siz - siz/2 - 1;}}
	sizs++; keys = realloc(keys,sizs*sizeof(void*)); boxs = realloc(boxs,sizs*sizeof(void*)); typs = realloc(typs,sizs*sizeof(int));
	for (int i = sizs-1; i > idx; i--) {
	keys[i] = 0; assignDat(&keys[i],keys[i-1]);
	boxs[i] = 0; assignDat(&boxs[i],boxs[i-1]);
	typs[i] = typs[i-1];}
	if (datxNoteFp) datxNoteFp(dat);
	keys[idx] = 0; assignDat(&keys[idx],dat);
	boxs[idx] = 0; assignDat(&boxs[idx],val);
	typs[idx] = typ; free(dat);
}
int datxFinds(void **val, const char *pre, const char *str)
{
	void *key = 0; void *sav = 0; int typ = 0; assignDat(&sav,prefix);
	datxStr(&prefix,pre); datxStr(&key,str); typ = datxFind(val,key);
	assignDat(&prefix,sav); free(sav);
	return typ;
}
void datxInserts(const char *pre, const char *str, void *val, int typ)
{
	void *key = 0; void *sav = 0; assignDat(&sav,prefix);
	datxStr(&prefix,pre); datxStr(&key,str); datxInsert(key,val,typ);
	assignDat(&prefix,sav); free(sav);
}
int datxChrs(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat/sizeof(char);
}
int datxInts(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat/sizeof(int);
}
int datxInt32s(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat/sizeof(int32_t);
}
int datxNews(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat/sizeof(long long);
}
int datxNums(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat/sizeof(double);
}
int datxOlds(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat/sizeof(float);
}
char *datxChrz(int num, void *dat)
{
	if (num >= datxChrs(dat)) ERROR();
	return (char*)datxVoidz(num*sizeof(char),dat);
}
int *datxIntz(int num, void *dat)
{
	if (num >= datxInts(dat)) ERROR();
	return (int*)datxVoidz(num*sizeof(int),dat);
}
int32_t *datxInt32z(int num, void *dat)
{
	if (num >= datxInt32s(dat)) ERROR();
	return (int32_t*)datxVoidz(num*sizeof(int32_t),dat);
}
long long *datxNewz(int num, void *dat)
{
	if (num >= datxInt32s(dat)) ERROR();
	return (long long*)datxVoidz(num*sizeof(long long),dat);
}
double *datxNumz(int num, void *dat)
{
	if (num >= datxNums(dat)) ERROR();
	return (double*)datxVoidz(num*sizeof(double),dat);
}
float *datxOldz(int num, void *dat)
{
	if (num >= datxNums(dat)) ERROR();
	return (float*)datxVoidz(num*sizeof(double),dat);
}
void datxNone(void **dat)
{
	*dat = realloc(*dat,sizeof(int));
	*(int*)*dat = 0;
}
void datxChr(void **dat, char val)
{
	*dat = realloc(*dat,sizeof(val)+sizeof(int));
	*(int*)*dat = sizeof(val);
	*datxChrz(0,*dat) =val;
}
void datxStr(void **dat, const char *val)
{
	*dat = realloc(*dat,strlen(val)+1+sizeof(int));
	*(int*)*dat = strlen(val)+1;
	memcpy((void*)((int*)*dat+1),val,strlen(val)+1);
}
void datxInt(void **dat, int val)
{
	*dat = realloc(*dat,sizeof(val)+sizeof(int));
	*(int*)*dat = sizeof(val);
	*datxIntz(0,*dat) = val;
}
void datxInt32(void **dat, int32_t val)
{
	*dat = realloc(*dat,sizeof(val)+sizeof(int));
	*(int*)*dat = sizeof(val);
	*datxInt32z(0,*dat) = val;
}
void datxNew(void **dat, long long val)
{
	*dat = realloc(*dat,sizeof(val)+sizeof(int));
	*(int*)*dat = sizeof(val);
	*datxNewz(0,*dat) = val;
}
void datxNum(void **dat, double val)
{
	*dat = realloc(*dat,sizeof(val)+sizeof(int));
	*(int*)*dat = sizeof(val);
	*datxNumz(0,*dat) = val;
}
void datxOld(void **dat, float val)
{
	*dat = realloc(*dat,sizeof(val)+sizeof(int));
	*(int*)*dat = sizeof(val);
	*datxOldz(0,*dat) = val;
}
#define BINARY_STR(LFT,RGT) strcmp(LFT,RGT)
#define BINARY_TRI(LFT,RGT) (LFT>RGT?1:(LFT<RGT?-1:0))
#define BINARY_ADD(LFT,RGT) (LFT+RGT)
#define BINARY_SUB(LFT,RGT) (LFT-RGT)
#define BINARY_MUL(LFT,RGT) (LFT*RGT)
#define BINARY_DIV(LFT,RGT) (LFT/RGT)
#define BINARY_REM(LFT,RGT) (LFT%RGT)
#define BINARY_MOD(LFT,RGT) fmod(LFT,RGT)
#define BINARY_FLM(LFT,RGT) fmodf(LFT,RGT)
#define BINARY_BIT(LFT,RGT) datxBitwise(LFT,RGT,exp->bit)
#define BINARY_BEGIN(FLD) {\
	void *dat0 = 0; void *dat1 = 0; int typ0 = 0; int typ1 = 0;\
	typ0 = datxEval(&dat0,&exp->FLD[0],typ);\
	typ1 = datxEval(&dat1,&exp->FLD[1],typ);\
	if (typ == -1) typ = typ0; if (typ0 != typ1 || typ != typ0) ERROR();
#define BINARY_DONE() ERROR();\
	free(dat0); free(dat1);}
#define BINARY_TYPE(TYPE,STR,GET,SET,OP)\
	if (typ0 == identType(STR)) {\
	TYPE lft = GET(0,dat0);\
	TYPE rgt = GET(0,dat1);\
	SET(dat,OP(lft,rgt));}
#define BINARY_BLOCK(OP,STR,FLD)\
	BINARY_BEGIN(FLD)\
	BINARY_TYPE(int,"Int",*datxIntz,datxInt,OP) else\
	BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,OP) else\
	BINARY_TYPE(double,"Num",*datxNumz,datxNum,OP) else\
	BINARY_TYPE(float,"Old",*datxOldz,datxOld,OP) else\
	BINARY_DONE()
#define BINARY_SET(DAT,VAL) datxInt(DAT,VAL)
#define BINARY_CMP(DAT,VAL) datxInt(DAT,datxComp(VAL,exp->cmp))
int datxBitwise(int lft, int rgt, enum Bitwise bit)
{
	switch (bit) {
	case (AndBit): return (lft&rgt);
	case (OrBit): return (lft|rgt);
	case (XorBit): return (lft^rgt);
	case (NandBit): return ~(lft&rgt);
	case (NorBit): return ~(lft|rgt);
	case (EquBit): return ~(lft^rgt);
	case (ShiftBit): return (lft<<rgt);
	case (PackBit): {
	int idx = 0; int res = 0;
	while (rgt) {
	while (rgt && !(rgt&0x1)) {
	lft >>= 1; rgt >>= 1;}
	while (rgt && (rgt&0x1)) {
	res |= ((lft&0x1)<<idx); idx += 1;
	lft >>= 1; rgt >>= 1;}}
	return res;}
	default: ERROR();}
	return 0;
}
int datxComp(int val, enum Compare cmp)
{
	switch (cmp) {
	case (LOCmp): return (val < 0);
	case (LCCmp): return (val <= 0);
	case (EOCmp): return (val != 0);
	case (ECCmp): return (val == 0);
	case (MOCmp): return (val > 0);
	case (MCCmp): return (val >= 0);
	default: ERROR();}
	return 0;
}
int datxRegcmp(const char *str)
{
	int val = 0; int idx = regsiz++;
	regexp = realloc(regexp,regsiz*sizeof(regex_t));
	val = regcomp(&regexp[idx],str,REG_EXTENDED);
	if (val != 0) {char buf[128];
	regerror(val,&regexp[idx],buf,128);
	fprintf(stderr,"%s\n",buf); exit(-1);}
	return idx;
}
int datxRegexe(const char *str, int idx)
{
	if (idx < 0 || idx >= regsiz) ERROR();
	return (regexec(&regexp[idx],str,0,0,0) == 0 ? 1 : 0);
}
int datxLoop(char chr)
{
	if (chr == '\\') return 1;
	return 0;
}
const char *datxEscape(char *tmp, const char *str)
{
	if (datxLoop(str[0]))
	switch (str[1]) {
	case ('n'): *tmp = '\n'; return tmp;
	default: *tmp = str[1]; return tmp;}
	return str;
}
int datxOpen(char chr)
{
	switch (chr) {case ('='): case ('<'): case ('>'): case ('*'): case ('|'): return 1; default: break;} return 0;
}
int datxClose(char chr)
{
	switch (chr) {case ('^'): return 1; default: break;} return 0;
}
int datxMatch(char chr)
{
	switch (chr) {case ('='): case ('<'): case ('>'): case ('*'): case ('|'): case ('^'): return 0; default: break;} return 1;
}
enum Order datxOrder(char chr)
{
	switch (chr) {default: break;
	case ('='): return BeginOrd;
	case ('<'): return PostOrd;
	case ('>'): return PreOrd;
	case ('*'): return PermOrd;
	case ('|'): return ForkOrd;}
	return Orders;
}
void datxIrreg(const char *str, struct Irrex *rgt, enum Order ord)
{
	int lvl = 0; int siz = 0; char tmp[2] = {0};
	if (ord == ChrOrd) {rgt->ord = ord; rgt->str = strndup(str,1); return;}
	for (const char *chr = str; *chr && lvl >= 0; chr += datxLoop(*chr)+1) {
	if (lvl == 0 && !datxClose(*chr)) siz++; if (datxOpen(*chr)) lvl++; if (datxClose(*chr)) lvl--;}
	rgt->ord = ord; rgt->siz = siz; allocIrrex(&rgt->sub,siz); lvl = 0; siz = 0;
	for (const char *chr = str; *chr && lvl >= 0; chr += datxLoop(*chr)+1) {
	if (lvl == 0 && datxOpen(*chr)) {datxIrreg(chr+1,rgt->sub+siz,datxOrder(*chr));}
	if (lvl == 0 && datxMatch(*chr)) {datxIrreg(datxEscape(tmp,chr),rgt->sub+siz,ChrOrd);}
	if (lvl == 0 && !datxClose(*chr)) siz++; if (datxOpen(*chr)) lvl++; if (datxClose(*chr)) lvl--;}
}
void datxIrrclr(struct Irrex *rgt)
{
	rgt->prm = 0; rgt->msk = 0;
	if (rgt->ord != ChrOrd) {free(rgt->str); rgt->str = 0;}
	for (int i = 0; i < rgt->siz; i++) datxIrrclr(rgt->sub+i);
}
int datxIrrfct(int len)
{
	if (len <= 0) return 1;
	return len*datxIrrfct(len-1);
}
int datxIrrcat(struct Irrex *exp);
int datxIrrdup(struct Irrex *exp)
{
	int val = 0;
	for (int i = 0; i < exp->siz; i++) {
		val += datxIrrcat(exp->sub+i);}
	exp->str = malloc(val+1); val = 0;
	for (int i = 0; i < exp->siz; i++) {
		strcpy(exp->str+val,exp->sub[i].str);
		val += strlen(exp->sub[i].str);}
	return val;
}
int datxIrrcat(struct Irrex *exp)
{
	int val = 0;
	if (exp->str) return strlen(exp->str);
	switch (exp->ord) {
	case (ChrOrd): ERROR(); // str should not be 0
	case (PreOrd): {char *tmp = 0;
		val = datxIrrdup(exp) - exp->prm;
		if (val < 0) ERROR();
		tmp = strndup(exp->str,val);
		free(exp->str); exp->str = tmp;} break;
	case (PostOrd): ERROR(); // TODO shift instead of truncate
	case (PermOrd): ERROR(); // TODO permute
	case (ForkOrd): {
		if (exp->prm < 0 || exp->prm >= exp->siz) ERROR();
		val = datxIrrcat(exp->sub+exp->prm);
		exp->str = strdup(exp->sub[exp->prm].str);} break;
	case (BeginOrd): {
		val = datxIrrdup(exp);} break;
	default: ERROR();}
	return val;
}
int datxIrrnxt(struct Irrex *exp);
int datxIrrbin(struct Irrex *exp)
{
	// recurse until success starting from last
	for (int i = 0; i < exp->siz; i++) {
	if (datxIrrnxt(exp->sub+exp->siz-i-1)) return 1;}
	return 0;
}
int datxIrrnxt(struct Irrex *exp)
{
	// assume str is valid; increment exp->prm; if exp->prm invalid reset exp->prm and return recurse
	if (!exp->str || exp->prm < 0) ERROR();
	switch (exp->ord) {
	case (ChrOrd): {
		if (exp->prm > 0) ERROR();
		return 0;} break;
	case (PreOrd): {int val = 0;
		for (int i = 0; i < exp->siz; i++) if (!exp->sub[i].str) ERROR();
		for (int i = 0; i < exp->siz; i++) val += strlen(exp->sub[i].str);
		exp->prm += 1; free(exp->str); exp->str = 0;
		if (exp->prm > val) ERROR();
		if (exp->prm == val) {exp->prm = 0; return datxIrrbin(exp);}} break;
	case (PostOrd): ERROR(); // TODO shift instead of truncate
	case (PermOrd): ERROR(); // TODO permute
	case (ForkOrd): {
		exp->prm += 1; free(exp->str); exp->str = 0;
		if (exp->prm > exp->siz) ERROR();
		while (exp->prm < exp->siz && ((1<<exp->prm) & exp->msk) == 1) exp->prm += 1;
		if (exp->prm == exp->siz) {exp->prm = 0; exp->msk = 0; if (!datxIrrbin(exp)) return 0;}
		exp->msk |= 1<<exp->prm;} break;
	case (BeginOrd): free(exp->str); exp->str = 0; return datxIrrbin(exp);
	default: ERROR();}
	return 1;
}
int datxIrrcmp(const char *str)
{
	int idx = irrsiz++;
	irrexp = realloc(irrexp,irrsiz*sizeof(struct Irrex));
	datxIrreg(str,&irrexp[idx],BeginOrd);
	return idx;
}
int datxIrrexe(const char *str, int idx)
{
	struct Irrex *ptr = 0;
	if (idx < 0 || idx >= irrsiz) ERROR();
	ptr = &irrexp[idx];
	datxIrrclr(ptr);
	while (1) {int val = 0;
	val = datxIrrcat(ptr);
	if (val == strlen(str) && strcmp(str,ptr->str) == 0) return 1;
	if (!datxIrrnxt(ptr)) break;}
	return 0;
}
int datxEval(void **dat, struct Express *exp, int typ)
{
	// {char *opr = 0; showOperate(exp->opr,&opr);
	// printf("datxEval %d %s\n",debug++,opr); free(opr);}
	switch (exp->opr) {
	case (AddOp): BINARY_BLOCK(BINARY_ADD,"add",opa) break;
	case (SubOp): BINARY_BLOCK(BINARY_SUB,"sub",opa) break;
	case (MulOp): BINARY_BLOCK(BINARY_MUL,"mul",opa) break;
	case (DivOp): BINARY_BLOCK(BINARY_DIV,"div",opa) break;
	case (RemOp): BINARY_BEGIN(opa)
		BINARY_TYPE(int,"Int",*datxIntz,datxInt,BINARY_REM) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,BINARY_REM) else
		BINARY_TYPE(double,"Num",*datxNumz,datxNum,BINARY_MOD) else
		BINARY_TYPE(float,"Old",*datxOldz,datxOld,BINARY_FLM) else
		BINARY_DONE() break;
	case (BitOp): BINARY_BEGIN(opb)
		BINARY_TYPE(int,"Int",*datxIntz,datxInt,BINARY_BIT) else
		BINARY_DONE() break;
	case (CmpOp): {
		void *dat0 = 0; int typ0 = -1; void *dat1 = 0; int typ1 = -1;
		typ0 = datxEval(&dat0,&exp->opc[0],typ0);
		typ1 = datxEval(&dat1,&exp->opc[1],typ1);
		if (exp->cmp == ReCmp || exp->cmp == IrCmp) {
		if (typ0 != identType("Str")) ERROR();
		if (typ1 != identType("Int")) ERROR();}
		else if (typ0 != typ1) ERROR();
		if (typ == -1) typ = identType("Int");
		if (typ != identType("Int")) ERROR();
		if (exp->cmp == ReCmp) datxInt(dat,datxRegexe(datxChrz(0,dat0),*datxIntz(0,dat1)));
		else if (exp->cmp == IrCmp) datxInt(dat,datxIrrexe(datxChrz(0,dat0),*datxIntz(0,dat1)));
		else BINARY_TYPE(int,"Int",*datxIntz,BINARY_CMP,BINARY_TRI)
		else BINARY_TYPE(int32_t,"Int32",*datxInt32z,BINARY_CMP,BINARY_TRI)
		else BINARY_TYPE(double,"Num",*datxNumz,BINARY_CMP,BINARY_TRI)
		else BINARY_TYPE(float,"Old",*datxOldz,BINARY_CMP,BINARY_TRI)
		else BINARY_TYPE(char *,"Str",datxChrz,BINARY_CMP,BINARY_STR)
		else ERROR();
		free(dat0); free(dat1);} break;
	case (CndOp): {
		if (exp->siz <= 0) ERROR();
		int idx = 0; while (1) {
		void *dat0 = 0; int typ0 = datxEval(&dat0,&exp->cnd[idx],-1);
		if (typ0 != identType("Int")) ERROR();
		int tmp = *datxIntz(0,dat0); free(dat0);
		if (tmp == 0) {typ = datxEval(dat,&exp->lst[idx],typ); break;}
		idx = (idx + tmp) % exp->siz;}} break;
	case (FesOp): {
		datxNone(dat); typ = identType("Dat");
		if (exp->num <= 0) ERROR();
		void *dat0 = 0; int typ0 = datxEval(&dat0,&exp->msk[0],-1);
		if (typ0 != identType("Int")) ERROR();
		int msk = *datxIntz(0,dat0); free(dat0);
		for (int i = ffs(msk)-1; msk; i = ffs(msk&=~(1<<i))-1) {
		void *dat1 = 0; int typ1 = datxEval(&dat1,&exp->mux[i],-1);
		if (typ1 != typ) ERROR(); free(dat1);}} break;
	case (RetOp): {
		if (!retptr) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,retptr(exp->cfg));} break;
	case (SetOp): {
		if (!setptr) ERROR();
		void *dat0 = 0; int typ1 = identType("Int");
		int typ0 = datxEval(&dat0,exp->set,typ1); if (typ0 != typ1) ERROR();
		setptr(*datxIntz(0,dat0),exp->cgs);
		datxNone(dat); typ1 = identType("Dat"); if (typ == -1) typ = typ1; if (typ != typ1) ERROR();} break;
	case (ValOp): {
		int typ0 = 0; void *key = 0;
		datxStr(&key,exp->key); typ0 = datxFind(dat,key); free(key);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (SavOp): {
		int typ0 = 0; void *dat0 = 0; void *key = 0;
		typ0 = datxEval(&dat0,exp->sav,-1); datxStr(&key,exp->kys); datxInsert(key,dat0,typ0); free(key); free(dat0);
		datxNone(dat); typ0 = identType("Dat"); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (RexOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxRegcmp(exp->rex));} break;
	case (IrxOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxIrrcmp(exp->irx));} break;
	case (GetOp): {
		if (!getptr) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxStr(dat,getptr());} break;
	case (PutOp): {
		if (!putptr) ERROR();
		int typ0 = datxEval(dat,exp->set,typ); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();
		if (typ0 != identType("Str")) ERROR();
		putptr(datxChrz(0,dat));} break;
	case (FldOp): {
		if (!fldptr) ERROR();
		void *dat0 = 0; int typ0 = datxEval(&dat0,&exp->fld[0],-1);
		void *dat1 = 0; int typ1 = datxEval(&dat1,&exp->fld[1],-1);
		void *dat2 = 0; int typ2 = datxEval(&dat2,&exp->fld[2],identType("Int")); if (typ2 != identType("Int")) ERROR();
		void *dat3 = 0; int typ3 = datxEval(&dat3,&exp->fld[3],identType("Int")); if (typ3 != identType("Int")) ERROR();
		typ = fldptr(dat,dat0,dat1,*datxIntz(0,dat2),*datxIntz(0,dat3),typ0,typ1);} break;
	case (ExtOp): {
		if (!extptr) ERROR();
		void *dat0 = 0; int typ0 = datxEval(&dat0,&exp->fld[0],-1);
		void *dat1 = 0; int typ1 = datxEval(&dat1,&exp->fld[1],identType("Int")); if (typ1 != identType("Int")) ERROR();
		void *dat2 = 0; int typ2 = datxEval(&dat2,&exp->fld[2],identType("Int")); if (typ2 != identType("Int")) ERROR();
		typ = extptr(dat,dat0,*datxIntz(0,dat1),*datxIntz(0,dat2),typ0);} break;
	case (ImmOp):
		if (!immptr) ERROR();
		typ = immptr(dat,exp->val); break;
	case (IntOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,exp->ivl);} break;
	case (StrOp): {
		if (typ == -1) typ = identType("Str"); if (typ != identType("Str")) ERROR();
		datxStr(dat,exp->svl);} break;
	case (CfgOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,exp->cvl);} break;
	case (MemOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,exp->mvl);} break;
	default: ERROR();}
	// debug--;
	return typ;
}
void datxPrefix(const char *str)
{
	datxStr(&prefix,str);
}
void datxChanged(rktype fnc)
{
	datxNoteFp = fnc;
}
void datxFnptr(retfp ret, setfp set, getfp get, putfp put, fldfp fld, extfp ext, immfp imm)
{
	retptr = ret;
	setptr = set;
	getptr = get;
	putptr = put;
	fldptr = fld;
	extptr = ext;
	immptr = imm;
}
