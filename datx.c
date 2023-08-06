#include "type.h"
#include "face.h"
#include "datx.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

int unique = 0;
void *prefix = 0;
dftype datxNoteFp = 0;
ghtype datxCallFp = 0;
dgtype datxSetFp = 0;
dhtype datxGetFp = 0;
fftype datxEmbFp = 0;
fftype datxOptFp = 0;
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
int datxReadFp(int fildes, void *buf, int nbyte)
{
	void **dat = datxDat(fildes);
	void *pre = 0;
	void *suf = 0;
	datxSplit(&pre,&suf,*dat,nbyte);
	if (*(int*)pre != nbyte) return 0;
	memcpy(buf,datxVoid(pre),nbyte);
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
	memcpy(datxVoid(suf),buf,nbyte);
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
void datxData(struct Data *dst, struct Data *src, struct DataExp *dat)
{
	for (int j = 0; j < src->siz; j++) {
	int val = datxLookup(src->str[j]);
	if (val >= 0) datxReplace(src->str[j],val+1);}
	for (int j = 0; j < datxChrs(src->dat); j++) {
	int val = datxLookup("");
	if (val >= 0) datxReplace("",val+1);
	switch (dat->act) {
	case (SubAct):
	for (int i = 0; i < dat->siz; i++)
	if (datxLookup(dat->key[i]) == dat->val[i])
	datxData(dst,src,&dat->sub[i]);
	break; case (NegAct):
	datxReplace(dat->str,-datxLookup(dat->str));
	break; case (ClrAct):
	datxReplace(dat->str,0);
	break; case (StrAct):
	if (*dat->str == 0) {
	void *tmp = 0; void *chr = 0;
	datxChr(&chr,*datxChrz(j,src->dat));
	datxJoin(&tmp,dst->dat,chr);
	assignDat(&dst->dat,tmp);
	free(tmp); free(chr);}
	else {
	allocStr(&dst->str,dst->siz+1);
	assignStr(&dst->str[dst->siz],dat->str);
	dst->siz += 1;}
	break; default: ERROR();}}
}
int datxPtrs(void *dat)
{
	if (!dat) return 0;
	return *(int*)dat;
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
void *datxVoid(void *dat)
{
	if (!dat) ERROR();
	return (void*)(((int*)dat)+1);
}
void *datxPtrz(int num, void *dat)
{
	if (num >= datxPtrs(dat)) ERROR();
	return (void*)(((char*)datxVoid(dat))+num);
}
char *datxChrz(int num, void *dat)
{
	if (num >= datxChrs(dat)) ERROR();
	return (char*)datxPtrz(num*sizeof(char),dat);
}
int *datxIntz(int num, void *dat)
{
	if (num >= datxInts(dat)) ERROR();
	return (int*)datxPtrz(num*sizeof(int),dat);
}
int32_t *datxInt32z(int num, void *dat)
{
	if (num >= datxInt32s(dat)) ERROR();
	return (int32_t*)datxPtrz(num*sizeof(int32_t),dat);
}
double *datxNumz(int num, void *dat)
{
	if (num >= datxNums(dat)) ERROR();
	return (double*)datxPtrz(num*sizeof(double),dat);
}
float *datxOldz(int num, void *dat)
{
	if (num >= datxNums(dat)) ERROR();
	return (float*)datxPtrz(num*sizeof(double),dat);
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
#define BINARY_CMP(DAT,VAL) val = datxEcmp(VAL,exp->cmp->cmp);
int datxEcmp(int val, enum Compare cmp)
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
	case (OptOp): {
		int val = 0; int i = 0; int typ0 = 0; if (datxOptFp == 0) ERROR();
		for (;i < exp->opt->siz && val == 0; i++) val = datxOptFp(exp->opt->str[i]);
		if (val < 0) typ0 = datxEval(dat,exp->opt->fer,typ);
		if (val == 0) {datxOptFp(0); typ0 = datxEval(dat,exp->opt->flt,typ);}
		if (val > 0) typ0 = datxEval(dat,&exp->opt->exp[i],typ);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (CndOp): {
		void *dats[exp->cnd->lft->siz]; int typs[exp->cnd->lft->siz]; int ret = 0; int idx = 0; int typ0 = 0;
		for (int i = 0; i < exp->cnd->lft->siz; i++) {
			int tmp = identUnion(exp->cnd->lft->typ[i]);
			dats[i] = 0; typs[i] = datxEval(&dats[i],&exp->cnd->lft->exp[i],tmp);
			if (typs[i] == -1 || tmp != -1 && typs[i] != tmp) ERROR();}
		for (int i = 0; i < exp->cnd->siz; i++) if (exp->cnd->lft->siz < exp->cnd->rgt[i].siz) ERROR();
		for (int i = 0; i < exp->cnd->siz; i++) {ret = 1; for (int j = 0; j < exp->cnd->rgt[i].siz; j++) {
			void *dat0 = 0; int typ0 = 0; void *dat1 = 0; int typ1 = 0; int val = 0;
			int tmp = identUnion(exp->cnd->rgt[i].typ[j]);
			dat0 = dats[j]; typ0 = typs[j];
			typ1 = datxEval(&dat1,&exp->cnd->rgt[i].exp[j],tmp);
			if (typ1 == -1 || tmp != -1 && typ1 != tmp) ERROR();
			if (typ0 != typ1) ERROR();
			BINARY_TYPE(int,"Int",*datxIntz,BINARY_SET,BINARY_TRI) else
			BINARY_TYPE(int32_t,"Int32",*datxInt32z,BINARY_SET,BINARY_TRI) else
			BINARY_TYPE(double,"Num",*datxNumz,BINARY_SET,BINARY_TRI) else
			BINARY_TYPE(float,"Old",*datxOldz,BINARY_SET,BINARY_TRI) else
			BINARY_TYPE(char*,"Str",datxChrz,BINARY_SET,BINARY_STR) else ERROR();
			if (val != 0) ret = 0;
			free(dat1); if (ret == 0) break;} if (ret == 1) {idx = i; break;}}
		for (int i = 0; i < exp->cnd->lft->siz; i++) free(dats[i]);
		if (ret == 0) ERROR();
		typ0 = datxEval(dat,&exp->cnd->exp[idx],typ);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR(); } break;
	case (CmpOp): {
		void *dat0 = 0; int typ0 = 0; void *dat1 = 0; int typ1 = 0; int val = 0;
		typ0 = datxEval(&dat0,exp->cmp->lft,-1);
		typ1 = datxEval(&dat1,exp->cmp->rgt,-1);
		if (typ0 != typ1) ERROR();
		BINARY_TYPE(int,"Int",*datxIntz,BINARY_CMP,BINARY_TRI) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,BINARY_CMP,BINARY_TRI) else
		BINARY_TYPE(double,"Num",*datxNumz,BINARY_CMP,BINARY_TRI) else
		BINARY_TYPE(float,"Old",*datxOldz,BINARY_CMP,BINARY_TRI) else
		BINARY_TYPE(char*,"Str",datxChrz,BINARY_CMP,BINARY_STR) else ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,val); free(dat0); free(dat1);} break;
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
		datxGetFp(dat,exp->cfg);} break;
	case (SetOp): {
		int typ0 = 0; if (datxSetFp == 0) ERROR();
		typ0 = datxEval(dat,exp->set,typ); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();
		if (typ0 != identType("Int")) ERROR();
		datxSetFp(*dat,exp->cgs);} break;
	case (ValOp): {
		int typ0 = 0; void *key = 0;
		datxStr(&key,exp->key); typ0 = datxFind(dat,key); free(key);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (SavOp): {
		int typ0 = 0; void *dat0 = 0; void *key = 0;
		typ0 = datxEval(dat,exp->sav,typ); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();
		datxStr(&key,exp->kys); datxInsert(key,*dat,typ0); free(key);} break;
	case (InsOp): {
		struct Hetgen val = {0}; struct Homgen str = {0}; struct Homgen idx = {0};
		void *dat0 = 0; void *dat1 = 0; void *dat2 = 0; int typ0 = 0; int typ1 = 0; int typ2 = 0; datxSingle();
		typ0 = datxEval(dat,&exp->ins[0],typ); if (typ == -1) typ = typ0; if (typ0 != typ) ERROR();
		typ0 = datxEval(&dat0,&exp->ins[1],identType("Hetgen")); if (typ0 != identType("Hetgen")) ERROR();
		typ1 = datxEval(&dat1,&exp->ins[2],identType("Homgen")); if (typ1 != identType("Homgen")) ERROR();
		typ2 = datxEval(&dat2,&exp->ins[3],identType("Homgen")); if (typ2 != identType("Homgen")) ERROR();
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
	case (CatOp): {
		void *dat0 = 0; void *dat1 = 0; int typ0 = 0; int typ1 = 0; datxSingle();
		typ0 = datxEval(&dat0,&exp->cat[0],-1);
		typ1 = datxEval(&dat1,&exp->cat[1],-1);
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
		void *save = 0; if (datxEmbFp == 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,datxEmbFp(exp->str));} break;
	case (NamOp): {
		int typ0 = 0; if (datxCallFp == 0) ERROR();
		typ0 = datxCallFp(dat,exp->str);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (AccOp): {
		int typ0 = 0; if (datxCallFp == 0) ERROR();
		typ0 = datxCallFp(dat,0);
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();} break;
	case (DatOp): {
		struct Data src = {0}; struct Data dst = {0}; void *dat0 = 0; int typ0 = 0;
		if (typ == -1) typ = identType("Data"); if (typ != identType("Data")) ERROR();
		typ0 = datxEval(&dat0,exp->dat,identType("Data")); if (typ0 != identType("Data")) ERROR();
		assignDat(datxDat0,dat0); readData(&src,datxIdx0); datxData(&dst,&src,exp->dtp);
		datxNone(datxDat0); writeData(&dst,datxIdx0); assignDat(dat,*datxDat0);
		free(dat0); freeData(&src); freeData(&dst);} break;
	default: ERROR();}
	// debug--;
	return typ;
}
void datxPrefix(const char *str)
{
	datxStr(&prefix,str);
}
void datxChanged(dftype fnc)
{
	datxNoteFp = fnc;
}
void datxCaller(ghtype fnc)
{
	datxCallFp = fnc;
}
void datxSetter(dgtype fnc)
{
	datxSetFp = fnc;
}
void datxGetter(dhtype fnc)
{
	datxGetFp = fnc;
}
void datxEmbed(fftype fnc)
{
	datxEmbFp = fnc;
}
void datxOption(fftype fnc)
{
	datxOptFp = fnc;
}
