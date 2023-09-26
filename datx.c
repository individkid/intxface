#include "type.h"
#include "face.h"
#include "datx.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <regex.h>

int unique = 0;
void *prefix = 0;
rktype datxNoteFp = 0;
rltype datxCallFp = 0;
cgtype datxSetFp = 0;
tltype datxGetFp = 0;
gftype datxEmbFp = 0;
sftype datxDupFp = 0;
rptype datxInsFp = 0;
rqtype datxDelFp = 0;
hftype datxOutFp = 0;
void ***datx = 0;
int ndatx = 0;
int datxSubs = 0;
int datxSub0 = 0;
int datxSub1 = 0;
int datxSub2 = 0;
int datxIdx0 = 0;
int datxIdx1 = 0;
int datxIdx2 = 0;
void **datxDat0 = 0;
void **datxDat1 = 0;
void **datxDat2 = 0;
int sizs = 0;
int *typs = 0;
void **boxs = 0;
void **keys = 0;
regex_t *regexp = 0;
int regsiz = 0;
struct Irrex *irrexp = 0;
int irrsiz = 0;
struct Datex *datexp = 0;
int datsiz = 0;

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
void datxSingle()
{
	if (datxSubs) return;
	datxSubs = 3;
	datxSub0 = datxSub();
	datxSub1 = datxSub();
	datxSub2 = datxSub();
	datxIdx0 = puntInit(datxSub0,datxSub0,datxReadFp,datxWriteFp);
	datxIdx1 = puntInit(datxSub1,datxSub1,datxReadFp,datxWriteFp);
	datxIdx2 = puntInit(datxSub2,datxSub2,datxReadFp,datxWriteFp);
	datxDat0 = datxDat(datxSub0);
	datxDat1 = datxDat(datxSub1);
	datxDat2 = datxDat(datxSub2);
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
	if (prefix) datxJoin(&dat,prefix,key); else assignDat(&dat,key);
	while (siz > 0) {int cmp = 0;
	if (idx < 0 || idx >= sizs) ERROR();
	cmp = datxCompare(dat,keys[idx+siz/2]);
	if (cmp == 0) {assignDat(&boxs[idx+siz/2],val); typs[idx+siz/2] = typ; free(dat); return;}
	if (cmp < 0) {siz = siz/2;}
	if (cmp > 0) {idx = idx + siz/2 + 1; siz = siz - siz/2 - 1;}}
	sizs++; keys = realloc(keys,sizs*sizeof(void*)); boxs = realloc(boxs,sizs*sizeof(void*)); typs = realloc(typs,sizs*sizeof(int));
	for (int i = sizs-1; i > idx; i--) {assignDat(&keys[i],keys[i-1]); assignDat(&boxs[i],boxs[i-1]); typs[i] = typs[i-1];}
	if (datxNoteFp) datxNoteFp(dat);
	assignDat(&keys[idx],dat); assignDat(&boxs[idx],val); typs[idx] = typ; free(dat);
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
void datxReplace(char *str, int val)
{
	void *key = 0; void *dat = 0;
	datxStr(&key,str); datxInt(&dat,val);
	datxInsert(key,dat,identType("Int"));
	free(key); free(dat);
}
int datxLookup(char *str)
{
	void *key = 0; void *dat = 0; int typ = 0; int val = 0;
	datxStr(&key,str); typ = datxFind(&dat,key);
	if (typ != identType("Int")) {
	datxReplace(str,0); val = 0;} else
	val = *datxIntz(0,dat);
	free(key); free(dat);
	return val;
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
#define BINARY_BEGIN() {\
	void *dat0 = 0; void *dat1 = 0; int typ0 = 0; int typ1 = 0;\
	typ0 = datxEval(&dat1,&exp->opb[1],typ);\
	typ1 = datxEval(&dat0,&exp->opb[0],typ);\
	if (typ == -1) typ = typ0; if (typ0 != typ1 || typ != typ0) ERROR();
#define BINARY_DONE() ERROR();\
	free(dat0); free(dat1);}
#define BINARY_TYPE(TYPE,STR,GET,SET,OP)\
	if (typ0 == identType(STR)) {\
	TYPE lft = GET(0,dat0);\
	TYPE rgt = GET(0,dat1);\
	SET(dat,OP(lft,rgt));}
#define BINARY_BLOCK(OP,STR)\
	BINARY_BEGIN()\
	BINARY_TYPE(int,"Int",*datxIntz,datxInt,OP) else\
	BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,OP) else\
	BINARY_TYPE(double,"Num",*datxNumz,datxNum,OP) else\
	BINARY_TYPE(float,"Old",*datxOldz,datxOld,OP) else\
	BINARY_DONE()
#define BINARY_SET(DAT,VAL) val = VAL;
#define BINARY_CMP(DAT,VAL) val = datxComp(VAL,exp->cnd->cmp[j]);
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
int datxDatdim(int idx, void *exp)
{
	struct Homgen hom = {0}; int typ = 0; int val = 0;
	datxReplace("",idx);
	typ = datxEval(datxDat0,exp,identType("Homgen"));
	if (typ != identType("Homgen")) ERROR();
	readHomgen(&hom,datxIdx0);
	if (hom.tag != IntTag) ERROR();
	val = hom.siz;
	freeHomgen(&hom);
	return val;
}
void datxDatvec(int *vec, int idx, void *exp)
{
	struct Homgen hom = {0}; int typ = 0;
	datxReplace("",idx);
	typ = datxEval(datxDat0,exp,identType("Homgen"));
	if (typ != identType("Homgen")) ERROR();
	readHomgen(&hom,datxIdx0);
	if (hom.tag != IntTag) ERROR();
	for (int i = 0; i < hom.siz; i++)
	vec[i] = hom.vInt[i];
	freeHomgen(&hom);
}
int datxDatcmp(int siz, void *exp)
{
	struct Datex *ptr = 0; int *vec = 0; int idx = datsiz++;
	datexp = realloc(datexp,datsiz*sizeof(struct Datex));
	ptr = datexp+idx; ptr->idx = ptr->dim = 0; ptr->rsz = 1; ptr->siz = siz;
	for (int i = 0; i < siz; i++)
	if (datxDatdim(i,exp) > ptr->dim)
	ptr->dim = datxDatdim(i,exp);
	vec = malloc(ptr->dim*sizeof(int));
	ptr->fsz = siz*ptr->dim;
	allocInt(&ptr->fwd,ptr->fsz*sizeof(int));
	for (int i = 0; i < siz; i++) {
	datxDatvec(vec,i,exp);
	for (int j = 0; j < ptr->dim; j++)
	ptr->fwd[i*ptr->dim+j] = vec[j];}
	allocInt(&ptr->max,ptr->dim);
	for (int i = 0; i < ptr->dim; i++) ptr->max[i] = 0;
	for (int i = 0; i < siz; i++) {
	datxDatvec(vec,i,exp);
	for (int j = 0; j < ptr->dim; j++)
	if (vec[j] > ptr->max[j]) ptr->max[j] = vec[j];}
	allocInt(&ptr->min,ptr->dim);
	for (int i = 0; i < ptr->dim; i++) ptr->min[i] = ptr->max[i];
	for (int i = 0; i < siz; i++) {
	datxDatvec(vec,i,exp);
	for (int j = 0; j < ptr->dim; j++)
	if (vec[j] < ptr->min[j]) ptr->min[j] = vec[j];}
	for (int i = 0; i < ptr->dim; i++)
	ptr->rsz *= ptr->max[i]-ptr->min[i]+1;
	allocInt(&ptr->rev,ptr->rsz*sizeof(int));
	for (int i = 0; i < siz; i++) {
	int inc = 1; int sum = 0;
	datxDatvec(vec,i,exp);
	for (int j = 0; j < ptr->dim; j++) {
	sum += (vec[j]-ptr->min[j])*inc;
	inc *= ptr->max[j]-ptr->min[j]+1;}
	ptr->rev[sum] = i;}
	free(vec); return idx;
}
void datxDatexe(void *src, void *dst, int rev, int fwd, int idx, int jdx)
{
	struct Datex *ptr = 0; struct Datex *qtr = 0;
	if (rev < 0 || rev >= datsiz || fwd < 0 || fwd >= datsiz) ERROR();
	ptr = &datexp[rev]; qtr = &datexp[fwd];
	if (ptr->idx < 0 || qtr->idx < 0) ERROR();
	for (int i = 0; i < datxChrs(dst); i++) {
	int fnd = 1; int inc = 1; int sum = 0; int *vec = 0; int id = 0; int jd = 0;
	jd = qtr->idx+i; jd %= qtr->siz; jd *= qtr->dim;
	if (jd < 0 || jd >= qtr->fsz) ERROR();
	vec = qtr->fwd+jd; for (int j = 0; j < ptr->dim; j++)
	if (vec[j] < ptr->min[j] || vec[j] > ptr->max[j])
	fnd = 0; if (!fnd) continue;
	for (int j = 0; j < ptr->dim; j++) {
	sum += (vec[j]-ptr->min[j])*inc; inc *= ptr->max[j]-ptr->min[j]+1;}
	if (sum < 0 || sum >= ptr->rsz) ERROR();
	id = ptr->idx+ptr->rev[sum]; id %= ptr->siz;
	if (id >= datxChrs(src)) continue;
	*datxChrz(i,dst) = *datxChrz(id,src);}
	ptr->idx += idx; qtr->idx += jdx;
}
// int debug = 0;
int datxEval(void **dat, struct Express *exp, int typ)
{
	// {char *opr = 0; showOperate(exp->opr,&opr);
	// printf("datxEval %d %s\n",debug++,opr); free(opr);}
	switch (exp->opr) {
	case (AddOp): BINARY_BLOCK(BINARY_ADD,"add") break;
	case (SubOp): BINARY_BLOCK(BINARY_SUB,"sub") break;
	case (MulOp): BINARY_BLOCK(BINARY_MUL,"mul") break;
	case (DivOp): BINARY_BLOCK(BINARY_DIV,"div") break;
	case (RemOp):
		BINARY_BEGIN()
		BINARY_TYPE(int,"Int",*datxIntz,datxInt,BINARY_REM) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,BINARY_REM) else
		BINARY_TYPE(double,"Num",*datxNumz,datxNum,BINARY_MOD) else
		BINARY_TYPE(float,"Old",*datxOldz,datxOld,BINARY_FLM) else
		BINARY_DONE() break;
	case (CndOp): {
		void *dats[exp->cnd->len]; int typs[exp->cnd->len]; int typ0 = 0; int val = 0; int idx = 0;
		for (int i = 0; i < exp->cnd->len; i++) {
			int typ0 = identType(exp->cnd->typ[i]);
			dats[i] = 0; typs[i] = datxEval(&dats[i],&exp->cnd->var[i],typ0);
			if (typs[i] == -1 || typ0 != -1 && typs[i] != typ0) ERROR();}
		for (int i = 0; i < exp->cnd->siz; i++) {
			if (exp->cnd->dom[i].len > exp->cnd->len) ERROR();
			val = 1; for (int j = 0; j < exp->cnd->dom[i].len && val; j++) {
				void *dat0 = dats[j]; int typ0 = typs[j];
				switch (exp->cnd->cmp[j]) {
				case (ReCmp): {
					void *dat1 = 0; int typ1 = 0;
					if (typ0 != identType("Str")) ERROR();
					typ1 = datxEval(&dat1,&exp->cnd->dom[i].val[j],identType("Regex"));
					if (typ1 != identType("Regex")) ERROR();
					val = datxRegexe(datxChrz(0,dat0),*datxIntz(0,dat1));
					free(dat1);} break;
				case (IrCmp): {
					void *dat1 = 0; int typ1 = 0;
					if (typ0 != identType("Str")) ERROR();
					typ1 = datxEval(&dat1,&exp->cnd->dom[i].val[j],identType("Irrex"));
					if (typ1 != identType("Int")) ERROR();
					val = datxIrrexe(datxChrz(0,dat0),*datxIntz(0,dat1));
					free(dat1);} break;
				case (EbCmp): {
					void *dat1 = 0; int typ1 = 0; char *arg = 0; char *scr = 0;
					typ1 = datxEval(&dat1,&exp->cnd->dom[i].val[j],identType("Str"));
					if (typ0 != identType("Str") || typ1 != identType("Str")) ERROR();
					arg = datxChrz(0,dat0); scr = datxChrz(0,dat1);
					if (datxEmbFp == 0) ERROR(); val = datxEmbFp(scr,arg);
					free(dat1);} break;
				default: {
					void *dat1 = 0; int typ1 = 0;
					typ1 = datxEval(&dat1,&exp->cnd->dom[i].val[j],typ0);
					if (typ0 != typ1) ERROR();
					BINARY_TYPE(int,"Int",*datxIntz,BINARY_CMP,BINARY_TRI) else
					BINARY_TYPE(int32_t,"Int32",*datxInt32z,BINARY_CMP,BINARY_TRI) else
					BINARY_TYPE(double,"Num",*datxNumz,BINARY_CMP,BINARY_TRI) else
					BINARY_TYPE(float,"Old",*datxOldz,BINARY_CMP,BINARY_TRI) else
					BINARY_TYPE(char *,"Str",datxChrz,BINARY_CMP,BINARY_STR) else
					ERROR(); free(dat1);} break;}}
			if (val) {idx = i; break;}} if (!val) ERROR();
		for (int i = 0; i < exp->cnd->len; i++) free(dats[i]);
		typ0 = datxEval(dat,&exp->cnd->rng[idx],typ);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (TotOp): {
		int typ0 = 0; datxSingle();
		typ0 = datxEval(dat,exp->tot,-1);
		if (typ == -1) typ = identType(exp->typ); if (typ != identType(exp->typ)) ERROR();
		if (typ == identType("Hetgen") && typ0 != identType("Hetgen")) {
		struct Hetgen het = {0}; het.siz = 1; allocGeneric(&het.gen,1);
		assignDat(datxDat0,*dat); readUnion(het.gen,typ0,datxIdx0);
		datxNone(datxDat0); writeHetgen(&het,datxIdx0); assignDat(dat,*datxDat0);
		freeHetgen(&het);}
		else if (typ != identType("Hetgen") && typ0 == identType("Hetgen")) {
		struct Hetgen het = {0};
		assignDat(datxDat0,*dat); readHetgen(&het,datxIdx0);
		datxNone(datxDat0); writeUnion(het.gen,datxIdx0);
		if (typ == -1) typ = identUnion(het.gen->tag); if (typ != identUnion(het.gen->tag)) ERROR();
		assignDat(dat,*datxDat0); freeHetgen(&het);}
		else if (typ == identType("Homgen") && typ0 != identType("Homgen")) {
		struct Homgen hom = {0}; struct Generic gen = {0}; allocUnion(&hom,typ0,1);
		assignDat(datxDat0,*dat); readUnion(&gen,typ0,datxIdx0);
		insertUnion(&hom,&gen,0); assignDat(dat,*datxDat0);
		freeHomgen(&hom); freeGeneric(&gen);}
		else if (typ != identType("Homgen") && typ0 == identType("Homgen")) {
		struct Homgen hom = {0}; struct Generic gen = {0};
		assignDat(datxDat0,*dat); readHomgen(&hom,datxIdx0); extractUnion(&gen,&hom,0);
		if (typ == -1) typ = identUnion(gen.tag); if (typ != identUnion(gen.tag)) ERROR();
		datxNone(datxDat0); writeUnion(&gen,datxIdx0); assignDat(dat,*datxDat0);
		freeHomgen(&hom); freeGeneric(&gen);}
		else if (typ == identType("Int") && typ0 == identType("Int32")) datxInt(dat,*datxInt32z(0,*dat));
		else if (typ == identType("Int") && typ0 == identType("Num")) datxInt(dat,*datxNumz(0,*dat));
		else if (typ == identType("Int") && typ0 == identType("Old")) datxInt(dat,*datxOldz(0,*dat));
		else if (typ == identType("Int32") && typ0 == identType("Int")) datxInt32(dat,*datxIntz(0,*dat));
		else if (typ == identType("Int32") && typ0 == identType("Num")) datxInt32(dat,*datxNumz(0,*dat));
		else if (typ == identType("Int32") && typ0 == identType("Old")) datxInt32(dat,*datxOldz(0,*dat));
		else if (typ == identType("Num") && typ0 == identType("Int")) datxNum(dat,*datxIntz(0,*dat));
		else if (typ == identType("Num") && typ0 == identType("Int32")) datxNum(dat,*datxInt32z(0,*dat));
		else if (typ == identType("Num") && typ0 == identType("Old")) datxNum(dat,*datxOldz(0,*dat));
		else if (typ == identType("Old") && typ0 == identType("Int")) datxOld(dat,*datxIntz(0,*dat));
		else if (typ == identType("Old") && typ0 == identType("Int32")) datxOld(dat,*datxInt32z(0,*dat));
		else if (typ == identType("Old") && typ0 == identType("Num")) datxOld(dat,*datxNumz(0,*dat));} break;
	case (GetOp): {
		if (datxGetFp == 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxGetFp(exp->cfg));} break;
	case (SetOp): {
		int typ0 = 0; if (datxSetFp == 0) ERROR();
		typ0 = datxEval(dat,exp->set,typ); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();
		if (typ0 != identType("Int")) ERROR();
		datxSetFp(*datxIntz(0,*dat),exp->cgs);} break;
	case (ValOp): {
		int typ0 = 0; void *key = 0;
		datxStr(&key,exp->key); typ0 = datxFind(dat,key); free(key);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (SavOp): {
		int typ0 = 0; void *dat0 = 0; void *key = 0;
		typ0 = datxEval(&dat0,exp->sav,-1); datxStr(&key,exp->kys); datxInsert(key,dat0,typ0); free(key); free(dat0);
		datxNone(dat); typ0 = identType("Dat"); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (OptOp): {struct OptExp *opt = exp->opt; // assume given Express does not go away
		if (!opt->vld) {int typ0 = 0; void *dat0 = 0;
		typ0 = datxEval(&opt->dat,opt->exp,typ); freeExpress(exp->opt->exp);
		opt->vld = 1; opt->typ = typ0; opt->dat = dat0;}
		if (typ == -1) typ = opt->typ; if (typ != opt->typ) ERROR();
		assignDat(dat,opt->dat);} break;
	case (RexOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxRegcmp(exp->rex));} break;
	case (IrxOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxIrrcmp(exp->irx));} break;
	case (CmpOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxDatcmp(exp->csz,exp->cmp));} break;
	case (DatOp): {
		if (typ == -1) typ = identType("Dat"); if (typ != identType("Dat")) ERROR();
		datxVoid(dat,exp->dsz); for (int i = 0; i < exp->dsz; i++) *datxChrz(i,dat) = exp->dvl;} break;
	case (MapOp): {
		if (typ == -1) typ = identType("Dat"); if (typ != identType("Dat")) ERROR();
		int typ0 = 0; void *dat0 = 0; void *src = 0; void *dst = 0; int rev = 0; int fwd = 0; int idx = 0; int jdx = 0;
		typ0 = datxEval(&dat0,exp->dup+0,identType("Dat")); if (typ0 != identType("Dat")) ERROR(); assignDat(&src,dat0);
		typ0 = datxEval(&dat0,exp->dup+1,identType("Dat")); if (typ0 != identType("Dat")) ERROR(); assignDat(dat,dat0);
		typ0 = datxEval(&dat0,exp->map+2,identType("Int")); if (typ0 != identType("Int")) ERROR(); rev = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->dup+3,identType("Int")); if (typ0 != identType("Int")) ERROR(); fwd = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->map+4,identType("Int")); if (typ0 != identType("Int")) ERROR(); idx = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->dup+5,identType("Int")); if (typ0 != identType("Int")) ERROR(); jdx = *datxIntz(0,dat0);
		datxDatexe(src,*dat,rev,fwd,idx,jdx);} break;
	case (InpOp): {
		int typ0 = 0; char *str = 0;
		if (!datxDupFp) ERROR(); datxDupFp(&str,-1,2,0); typ0 = identType("Str"); datxStr(dat,str); free(str);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (DupOp): {
		int typ0 = 0; void *dat0 = 0; char *str = 0; int len = 0; int idx = 0; int loc = 0;
		typ0 = datxEval(&dat0,exp->dup,identType("Int")); if (typ0 != identType("Int")) ERROR(); len = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->dup+1,identType("Int")); if (typ0 != identType("Int")) ERROR(); idx = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->dup+2,identType("Int")); if (typ0 != identType("Int")) ERROR(); loc = *datxIntz(0,dat0);
		if (!datxDupFp) ERROR(); datxDupFp(&str,len,idx,loc); typ0 = identType("Str"); datxStr(dat,str); free(str);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (InsOp): {
		int typ0 = 0; void *dat0 = 0; char *str = 0; int len = 0; int idx = 0; int loc = 0;
		typ0 = datxEval(&dat0,exp->ins,identType("Str")); if (typ0 != identType("Str")) ERROR(); assignStr(&str,datxChrz(0,dat0));
		typ0 = datxEval(&dat0,exp->ins+1,identType("Int")); if (typ0 != identType("Int")) ERROR(); len = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->ins+2,identType("Int")); if (typ0 != identType("Int")) ERROR(); idx = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->ins+3,identType("Int")); if (typ0 != identType("Int")) ERROR(); loc = *datxIntz(0,dat0);
		if (!datxInsFp) ERROR(); datxInsFp(str,len,idx,loc); free(str);
		datxNone(dat); typ0 = identType("Dat"); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (DelOp): {
		int typ0 = 0; void *dat0 = 0; int len = 0; int idx = 0; int loc = 0;
		typ0 = datxEval(&dat0,exp->del,identType("Int")); if (typ0 != identType("Int")) ERROR(); len = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->del+1,identType("Int")); if (typ0 != identType("Int")) ERROR(); idx = *datxIntz(0,dat0);
		typ0 = datxEval(&dat0,exp->del+2,identType("Int")); if (typ0 != identType("Int")) ERROR(); loc = *datxIntz(0,dat0);
		if (!datxDelFp) ERROR(); datxDelFp(len,idx,loc);
		datxNone(dat); typ0 = identType("Dat"); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (OutOp): {
		int typ0 = 0; void *dat0 = 0; char *str = 0;
		typ0 = datxEval(&dat0,exp->out,identType("Str")); if (typ0 != identType("Str")) ERROR(); assignStr(&str,datxChrz(0,dat0));
		if (!datxOutFp) ERROR(); datxOutFp(str); free(str);
		datxNone(dat); typ0 = identType("Dat"); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (FldOp): {
		struct Hetgen val = {0}; struct Homgen str = {0}; struct Homgen idx = {0};
		void *dat0 = 0; void *dat1 = 0; void *dat2 = 0; int typ0 = 0; int typ1 = 0; int typ2 = 0; datxSingle();
		typ0 = datxEval(dat,&exp->fld[0],typ); if (typ == -1) typ = typ0; if (typ0 != typ) ERROR();
		typ0 = datxEval(&dat0,&exp->fld[1],identType("Hetgen")); if (typ0 != identType("Hetgen")) ERROR();
		typ1 = datxEval(&dat1,&exp->fld[2],identType("Homgen")); if (typ1 != identType("Homgen")) ERROR();
		typ2 = datxEval(&dat2,&exp->fld[3],identType("Homgen")); if (typ2 != identType("Homgen")) ERROR();
		assignDat(datxDat0,dat0); assignDat(datxDat1,dat1); assignDat(datxDat2,dat2);
		readHetgen(&val,datxIdx0); readHomgen(&str,datxIdx1); readHomgen(&idx,datxIdx2);
		if (val.siz != str.siz || val.siz != idx.siz) ERROR(); if (str.tag != StrTag || idx.tag != IntTag) ERROR();
		for (int i = 0; i < val.siz; i++) {
			int fld = identField(typ,str.vStr[i]); if (fld == -1) ERROR();
			assignDat(datxDat0,*dat); datxNone(datxDat1); datxNone(datxDat2);
			writeUnion(&val.gen[i],datxIdx1); readField(typ,fld,idx.vInt[i],datxIdx0,datxIdx1,datxIdx2);
			assignDat(dat,*datxDat2);}
		free(dat0); free(dat1); free(dat2); freeHetgen(&val); freeHomgen(&str); freeHomgen(&idx);} break;
	case (ExtOp): {
		struct Hetgen val = {0}; struct Homgen str = {0}; struct Homgen idx = {0};
		void *dat0 = 0; void *dat1 = 0; void *dat2 = 0; int typ0 = 0; int typ1 = 0; int typ2 = 0; datxSingle();
		typ0 = datxEval(&dat0,&exp->ext[0],typ);
		typ1 = datxEval(&dat1,&exp->ext[1],identType("Homgen")); if (typ1 != identType("Homgen")) ERROR();
		typ2 = datxEval(&dat2,&exp->ext[2],identType("Homgen")); if (typ2 != identType("Homgen")) ERROR();
		assignDat(datxDat0,dat0); assignDat(datxDat1,dat1); assignDat(datxDat2,dat2);
		readHomgen(&str,datxIdx1); readHomgen(&idx,datxIdx2);
		if (str.siz != idx.siz) ERROR(); if (str.tag != StrTag || idx.tag != IntTag) ERROR();
		val.siz = idx.siz; allocGeneric(&val.gen,val.siz);
		for (int i = 0; i < val.siz; i++) {
			int pos = identField(typ0,str.vStr[i]); int fld = identSubtype(typ0,pos); datxNone(datxDat1);
			writeField(typ0,pos,idx.vInt[i],datxIdx0,datxIdx1); readUnion(&val.gen[i],fld,datxIdx1);}
		datxNone(datxDat2); writeHetgen(&val,datxIdx2);
		if (typ == -1) typ = identType("Hetgen"); if (typ != identType("Hetgen")) ERROR();
		assignDat(dat,*datxDat2);
		free(dat0); free(dat1); free(dat2); freeHetgen(&val); freeHomgen(&str); freeHomgen(&idx);} break;
	case (PrmOp): {
		struct Homgen idx = {0}; void *dat0 = 0; void *dat1 = 0; int typ0 = 0; int typ1 = 0; datxSingle();
		typ0 = datxEval(&dat0,&exp->prm[0],identType("Homgen")); if (typ0 != identType("Homgen")) ERROR();
		assignDat(datxDat0,dat0); readHomgen(&idx,datxIdx0); if (idx.tag != IntTag) ERROR();
		typ1 = datxEval(&dat1,&exp->prm[1],-1); assignDat(datxDat1,dat1);
		if (typ1 == identType("Homgen")) {
		struct Homgen val = {0}; struct Homgen prm = {0};
		readHomgen(&val,datxIdx1); allocUnion(&prm,val.tag,val.siz);
		for (int i = 0; i < prm.siz; i++) copyUnion(&prm,&val,idx.vInt[i]);
		freeHomgen(&val);}
		else if (typ1 == identType("Hetgen")) {
		struct Hetgen val = {0}; struct Hetgen prm = {0};
		readHetgen(&val,datxIdx1); prm.siz = idx.siz; allocGeneric(&prm.gen,prm.siz);
		for (int i = 0; i < prm.siz; i++) copyGeneric(&prm.gen[i],&val.gen[idx.vInt[i]]);
		datxNone(datxDat2); writeHetgen(&prm,datxIdx2);
		freeHetgen(&val);}
		else ERROR();
		if (typ == -1) typ = typ1; if (typ != typ1) ERROR();
		assignDat(dat,*datxDat2);
		free(dat0); free(dat1); freeHomgen(&idx);} break;
	case (LstOp): {
		void *dat0 = 0; void *dat1 = 0; int typ0 = 0; int typ1 = 0; datxSingle();
		typ0 = datxEval(&dat0,&exp->lst[0],-1);
		typ1 = datxEval(&dat1,&exp->lst[1],-1);
		if (typ0 != typ1) ERROR(); if (typ0 == identType("Homgen")) {
		struct Homgen hom0 = {0}; struct Homgen hom1 = {0}; struct Homgen hom2 = {0};
		if (typ == -1) typ = identType("Homgen"); if (typ != identType("Homgen")) ERROR();
		assignDat(datxDat0,dat0); assignDat(datxDat1,dat1);
		readHomgen(&hom0,datxIdx0); readHomgen(&hom1,datxIdx1);
		if (hom0.tag != hom1.tag) ERROR();
		allocUnion(&hom2,hom0.tag,hom0.siz+hom1.siz);
		for (int i = 0; i < hom0.siz; i++) copyUnion(&hom2,&hom0,i);
		for (int i = 0; i < hom1.siz; i++) copyUnion(&hom2,&hom1,i+hom0.siz);
		datxNone(datxDat2); writeHomgen(&hom2,datxIdx2);
		freeHomgen(&hom0); freeHomgen(&hom1); freeHomgen(&hom2);}
		else if (typ0 == identType("Hetgen")) {
		struct Hetgen het0 = {0}; struct Hetgen het1 = {0}; struct Hetgen het2 = {0};
		if (typ == -1) typ = identType("Hetgen"); if (typ != identType("Hetgen")) ERROR();
		het2.siz = het0.siz + het1.siz; allocGeneric(&het2.gen,het2.siz);
		for (int i = 0; i < het0.siz; i++) copyGeneric(&het2.gen[i],&het0.gen[i]);
		for (int i = 0; i < het1.siz; i++) copyGeneric(&het2.gen[i+het0.siz],&het1.gen[i]);
		datxNone(datxDat2); writeHetgen(&het2,datxIdx2);
		freeHetgen(&het0); freeHetgen(&het1); freeHetgen(&het2);}
		else ERROR();
		free(dat0); free(dat1); assignDat(dat,*datxDat2);} break;
	case (HetOp): {
		struct Hetgen val = {0}; datxSingle();
		val.siz = exp->siz;
		allocGeneric(&val.gen,val.siz);
		for (int i = 0; i < val.siz; i++) {
		void *dat0 = 0; int typ0 = 0; typ0 = 0;
		typ0 = datxEval(&dat0,&exp->exp[i],-1); assignDat(datxDat0,dat0); 
		readUnion(&val.gen[i],typ0,datxIdx0);
		free(dat0);}
		if (typ == -1) typ = identType("Hetgen"); if (typ != identType("Hetgen")) ERROR();
		datxNone(datxDat0); writeHetgen(&val,datxIdx0); assignDat(dat,*datxDat0);
		freeHetgen(&val);} break;
	case (HomOp): {
		struct Homgen val = {0}; datxSingle();
		for (int i = 0; i < exp->siz; i++) {
		struct Generic gen = {0}; void *dat0 = 0; int typ0 = 0;
		typ0 = datxEval(&dat0,&exp->exp[i],-1); assignDat(datxDat0,dat0);
		if (i == 0) {allocUnion(&val,typ0,exp->siz);}
		readUnion(&gen,typ0,datxIdx0); insertUnion(&val,&gen,i);
		free(dat0); freeGeneric(&gen);}
		if (typ == -1) typ = identType("Homgen"); if (typ != identType("Homgen")) ERROR();
		datxNone(datxDat0); writeHomgen(&val,datxIdx0); assignDat(dat,*datxDat0);
		freeHomgen(&val);} break;
	case (EmmOp): {
		if (typ == -1) typ = identType("Hetgen"); if (typ != identType("Hetgen")) ERROR();
		datxNone(datxDat0); writeHetgen(exp->het,datxIdx0); assignDat(dat,*datxDat0);} break;
	case (OmmOp): {
		if (typ == -1) typ = identType("Homgen"); if (typ != identType("Homgen")) ERROR();
		datxNone(datxDat0); writeHomgen(exp->hom,datxIdx0); assignDat(dat,*datxDat0);} break;
	// TODO Add way to put parenthesis in Str.
	// TODO Add RelOp IrlOp that split Str by Regex Irrex, respectively, into Homgen of Str.
	// TODO Add LenOp that returns length Int of Str.
	case (ImmOp): {
		if (typ == -1) typ = identUnion(exp->val->tag); if (typ != identUnion(exp->val->tag)) ERROR();
		datxSingle(); datxNone(datxDat0); writeUnion(exp->val,datxIdx0); assignDat(dat,*datxDat0);} break;
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
	case (UnqOp): {
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,unique++);} break;
	case (EmbOp): {
		if (datxEmbFp == 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxEmbFp(exp->scr,exp->arg));} break;
	case (NamOp): {
		int typ0 = 0; if (datxCallFp == 0) ERROR();
		typ0 = datxCallFp(dat,exp->nam);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (AccOp): {
		int typ0 = 0; if (datxCallFp == 0) ERROR();
		typ0 = datxCallFp(dat,0);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
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
void datxCaller(rltype fnc)
{
	datxCallFp = fnc;
}
void datxSetcfg(cgtype fnc)
{
	datxSetFp = fnc;
}
void datxGetcfg(tltype fnc)
{
	datxGetFp = fnc;
}
void datxEmbed(gftype fnc)
{
	datxEmbFp = fnc;
}
void datxDupstr(sftype fnc)
{
	datxDupFp = fnc;
}
void datxInsstr(rptype fnc)
{
	datxInsFp = fnc;
}
void datxDelstr(rqtype fnc)
{
	datxDelFp = fnc;
}
void datxOutstr(hftype fnc)
{
	datxOutFp = fnc;
}
