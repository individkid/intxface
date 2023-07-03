#include "type.h"
#include "face.h"
#include "datx.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

int unique = 0;
void *prefix = 0;
dftype datxCallFp = 0;
dgtype datxSetFp = 0;
dhtype datxGetFp = 0;
fftype datxEmbFp = 0;
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

struct Node;
struct Box {
	struct Node *ptr;
	int idx;
};
struct Node {
	int siz;
	int typ[4];
	void *box[4];
	void *key[4];
	struct Node *ptr[4];
	struct Box ref;
	int dep;
} tree = {0};

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
	memcpy(buf,datxData(pre),nbyte);
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
	memcpy(datxData(suf),buf,nbyte);
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
struct Box datxFindF(void *key)
{
	struct Box box = {0}; box.ptr = &tree;
	if (box.ptr->siz == 0) return box;
	while (box.ptr->dep > 0 || datxCompare(box.ptr->key[box.idx],key) < 0) {
	box.idx++;
	if (box.idx == box.ptr->siz || datxCompare(box.ptr->key[box.idx],key) > 0) {
	box.idx--;
	if (box.ptr->dep == 0) break;
	box.ptr = box.ptr->ptr[box.idx];
	box.idx = 0;}}
	return box;
}
int datxFind(void **val, void *key)
{
	struct Box box = {0}; void *dat = 0; int typ = -1;
	if (prefix) datxJoin(dat,prefix,key); else assignDat(&dat,key);
	box = datxFindF(dat);
	if (box.idx < box.ptr->siz && datxCompare(box.ptr->key[box.idx],dat) == 0) {
		assignDat(val,box.ptr->box[box.idx]); typ = box.ptr->typ[box.idx];
		free(dat); return typ;}
	free(dat); free(*val); *val = 0;
	return typ;
}
void datxInsertF(void *key, void *val, int typ, void *ptr, struct Box box)
{
	for (int i = box.ptr->siz; i > box.idx; i--) {
		box.ptr->typ[i] = box.ptr->typ[i-1];
		box.ptr->key[i] = box.ptr->key[i-1];
		box.ptr->ptr[i] = box.ptr->ptr[i-1];
		box.ptr->box[i] = box.ptr->box[i-1];}
	box.ptr->siz++;
	box.ptr->typ[box.idx] = typ;
	box.ptr->key[box.idx] = key;
	box.ptr->ptr[box.idx] = ptr;
	box.ptr->box[box.idx] = val;
}
void datxInsert(void *key, void *val, int typ)
{
	struct Box box = {0}; void *dat = 0;
	if (prefix) datxJoin(&dat,prefix,key); else assignDat(&dat,key); key = dat;
	box = datxFindF(key);
	if (box.idx < box.ptr->siz && datxCompare(box.ptr->key[box.idx],key) == 0) {
		assignDat(&box.ptr->box[box.idx],val); free(key); return;}
	dat = 0; assignDat(&dat,val); val = dat;
	datxInsertF(key,val,typ,0,box);
	while (box.ptr->siz > 3) {
		struct Node tmp = {0};
		struct Node *ptr = malloc(sizeof(struct Node));
		*ptr = tmp;
		ptr->siz = box.ptr->siz/2;
		box.ptr->siz = box.ptr->siz-ptr->siz;
		for (int i = 0; i < ptr->siz; i++) {
		ptr->typ[i] = box.ptr->typ[i+box.ptr->siz]; box.ptr->typ[i+box.ptr->siz] = 0;
		ptr->key[i] = box.ptr->key[i+box.ptr->siz]; box.ptr->key[i+box.ptr->siz] = 0;
		ptr->ptr[i] = box.ptr->ptr[i+box.ptr->siz]; box.ptr->ptr[i+box.ptr->siz] = 0;}
		ptr->ref.ptr = box.ptr->ref.ptr;
		ptr->ref.idx = box.ptr->ref.idx+1;
		ptr->dep = box.ptr->dep;
		box = box.ptr->ref;
		datxInsertF(ptr->key[0],0,0,ptr,box);}
	if (datxCallFp) {void *save = 0; assignDat(&save,prefix); datxCallFp(key); assignDat(&prefix,save);}
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
void datxDatae(struct Data *dst, struct Data *src, struct Datae *dat)
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
	datxDatae(dst,src,&dat->sub[i]);
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
void *datxData(void *dat)
{
	if (!dat) ERROR();
	return (void*)(((int*)dat)+1);
}
void *datxPtrz(int num, void *dat)
{
	if (num >= datxPtrs(dat)) ERROR();
	return (void*)(((char*)datxData(dat))+num);
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
int datxType(void **dat, struct Express *exp, int typ)
{
	int typ0 = datxEval(dat,exp,typ);
	if (typ == -1) typ = typ0;
	if (typ != typ0) ERROR();
	return typ0;
}
#define BINARY_CMP(LFT,RGT) strcmp(LFT,RGT)
#define BINARY_TRI(LFT,RGT) (LFT>RGT?1:(LFT<RGT?-1:0))
#define BINARY_ADD(LFT,RGT) (LFT+RGT)
#define BINARY_SUB(LFT,RGT) (LFT-RGT)
#define BINARY_MUL(LFT,RGT) (LFT*RGT)
#define BINARY_DIV(LFT,RGT) (LFT/RGT)
#define BINARY_REM(LFT,RGT) (LFT%RGT)
#define BINARY_MOD(LFT,RGT) fmod(LFT,RGT)
#define BINARY_FLM(LFT,RGT) fmodf(LFT,RGT)
#define BINARY_BEGIN(TYP) {\
	void *dat0 = 0; void *dat1 = 0;\
	if (exp->siz != 2) ERROR();\
	TYP = datxType(&dat1,&exp->exp[1],datxType(&dat0,&exp->exp[0],TYP));
#define BINARY_DONE() ERROR();\
	free(dat0); free(dat1);}
#define BINARY_TYPE(TYPE,TYP,GET,SET,OP)\
	if (typ == identType(TYP)) {\
	TYPE lft = GET(0,dat0);\
	TYPE rgt = GET(0,dat1);\
	SET(dat,OP(lft,rgt));}
#define BINARY_BLOCK(OP,STR)\
	BINARY_BEGIN(typ)\
	BINARY_TYPE(int,"Int",*datxIntz,datxInt,OP) else\
	BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,OP) else\
	BINARY_TYPE(double,"Num",*datxNumz,datxNum,OP) else\
	BINARY_TYPE(float,"Old",*datxOldz,datxOld,OP) else\
	BINARY_DONE()
#define BINARY_SET(DAT,VAL) val = datxEcmp(VAL,exp->flt[i].cmp[j]);
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
int datxEval(void **dat, struct Express *exp, int typ)
{
	switch (exp->opr) {
	case (AddOp): BINARY_BLOCK(BINARY_ADD,"add") break; // siz = 2;
	case (SubOp): BINARY_BLOCK(BINARY_SUB,"sub") break; // 2;
	case (MulOp): BINARY_BLOCK(BINARY_MUL,"mul") break; // 2;
	case (DivOp): BINARY_BLOCK(BINARY_DIV,"div") break; // 2;
	case (RemOp): // 2;
		BINARY_BEGIN(typ)
		BINARY_TYPE(int,"Int",*datxIntz,datxInt,BINARY_REM) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,BINARY_REM) else
		BINARY_TYPE(double,"Num",*datxNumz,datxNum,BINARY_MOD) else
		BINARY_TYPE(float,"Old",*datxOldz,datxOld,BINARY_FLM) else
		BINARY_DONE() break;
	case (CmpOp): { // n; compare tree
		const char *str = ""; int found = 0;
		for (int i = 0; i < exp->siz; i++) {
		if (exp->flt[i].loc == Branch) {
		void *dat0 = 0; int typ0 = 0;
		typ0 = datxEval(&dat0,&exp->exp[i],-1);
		for (int j = 0; j < exp->flt[i].siz; j++) {
		void *dat1 = 0; int val = 0;
		if (typ0 != datxEval(&dat1,&exp->flt[i].exp[j],-1)) ERROR();
		BINARY_TYPE(int,"Int",*datxIntz,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(double,"Num",*datxNumz,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(float,"Old",*datxOldz,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(char*,"Str",datxChrz,BINARY_SET,BINARY_CMP) else ERROR();
		if (val && strncmp(str,exp->flt[i].str[j],strlen(str)) == 0) {
		str = exp->flt[i].str[j];}}} else {
		for (int j = 0; j < exp->flt[i].siz; j++) {
		if (strcmp(str,exp->flt[i].str[j]) == 0) {
		// printf("CmpOp %s\n",str);
		typ = datxType(dat,&exp->exp[i],typ); found = 1;}}}}
		if (!found) ERROR();} break;
	case (TotOp): { // 1; cast to type
		int tmp = 0; if (exp->siz != 1) ERROR();
		tmp = datxEval(dat,&exp->exp[0],-1);
		if (typ == -1) typ = identType(exp->typ); if (typ != identType(exp->typ)) ERROR();
		if (typ == identType("Int") && tmp == identType("Int32")) datxInt(dat,*datxInt32z(0,dat));
		else if (typ == identType("Int") && tmp == identType("Num")) datxInt(dat,*datxNumz(0,dat));
		else if (typ == identType("Int") && tmp == identType("Old")) datxInt(dat,*datxOldz(0,dat));
		else if (typ == identType("Int32") && tmp == identType("Int")) datxInt32(dat,*datxIntz(0,dat));
		else if (typ == identType("Int32") && tmp == identType("Num")) datxInt32(dat,*datxNumz(0,dat));
		else if (typ == identType("Int32") && tmp == identType("Old")) datxInt32(dat,*datxOldz(0,dat));
		else if (typ == identType("Num") && tmp == identType("Int")) datxNum(dat,*datxIntz(0,dat));
		else if (typ == identType("Num") && tmp == identType("Int32")) datxNum(dat,*datxInt32z(0,dat));
		else if (typ == identType("Num") && tmp == identType("Old")) datxNum(dat,*datxOldz(0,dat));
		else if (typ == identType("Old") && tmp == identType("Int")) datxOld(dat,*datxIntz(0,dat));
		else if (typ == identType("Old") && tmp == identType("Int32")) datxOld(dat,*datxInt32z(0,dat));
		else if (typ == identType("Old") && tmp == identType("Num")) datxOld(dat,*datxNumz(0,dat));} break;
	case (ImmOp): { // 0; built in value
		datxSingle(); if (exp->siz != 0) ERROR();
		if (typ == -1) typ = identUnion(exp->val); if (typ != identUnion(exp->val)) ERROR();
		datxNone(datxDat0); writeUnion(exp->val,datxIdx0); assignDat(dat,*datxDat0);} break;
	case (GetOp): { // 0; value from callback
		void *sav = 0; if (exp->siz != 0 || datxGetFp == 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		assignDat(&sav,prefix); datxGetFp(dat,exp->cfg); assignDat(&prefix,sav);
		free(sav);} break;
	case (SetOp): { // 1; callback with value
		void *sav = 0; if (exp->siz != 1 || datxSetFp == 0) ERROR();
		typ = datxType(dat,&exp->exp[0],typ); if (typ != identType("Int")) ERROR();
		assignDat(&sav,prefix); datxSetFp(*dat,exp->cfg); assignDat(prefix,sav);
		free(sav);} break;
	case (InsOp): { // 1+; fields to struct
		datxSingle(); if (exp->siz < 1) ERROR();
		typ = datxType(dat,&exp->exp[0],typ);
		if (typ != identType(exp->fld[0])) ERROR();
		for (int i = 1; i < exp->siz; i++) {
		int typ0 = identField(typ,exp->fld[i]);
		assignDat(datxDat0,dat); datxEval(datxDat1,&exp->exp[i],typ0);
		datxNone(datxDat2); readField(typ,typ0,exp->idx[i],datxIdx0,datxIdx1,datxIdx2);
		assignDat(dat,*datxDat2);}} break;
	case (ExtOp): { // 1+; field from struct
		datxSingle(); if (exp->siz < 1) ERROR();
		typ = datxType(dat,&exp->exp[0],typ);
		if (typ != identType(exp->fld[0])) ERROR();
		for (int i = 1; i < exp->siz; i++) {
		int typ0 = identField(typ,exp->fld[i]);
		assignDat(datxDat0,dat); datxEval(datxDat1,&exp->exp[i],typ0);
		datxNone(datxDat2); writeField(typ,typ0,exp->idx[i],datxIdx0,datxIdx1);
		assignDat(dat,*datxDat1);}} break;
	case (FldOp): { // n: fold expressions
		int typ0 = 0; void *key = 0;
		datxStr(&key,exp->key);
		for (int i = 0; i < exp->siz; i++) {
		typ0 = datxEval(dat,&exp->exp[i],-1);
		datxInsert(key,*dat,typ0);}
		typ0 = datxFind(dat,key); if (typ0 < 0) ERROR();
		if (typ == -1) typ = typ0; if (typ != typ0) ERROR();
		free(key);} break;
	case (IntOp): { // 0: ImmOp sugar
		if (exp->siz != 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,exp->ivl);} break;
	case (StrOp): { // 0: ImmOp sugar
		if (exp->siz != 0) ERROR();
		if (typ == -1) typ = identType("Str"); if (typ != identType("Str")) ERROR();
		datxStr(dat,exp->svl);} break;
	case (CfgOp): { // 0: ImmOp sugar
		if (exp->siz != 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,exp->cvl);} break;
	case (MemOp): { // 0: ImmOp sugar
		if (exp->siz != 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,exp->mvl);} break;
	case (UnqOp): { // 0; magic number
		if (exp->siz != 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxInt(dat,unique++);} break;
	case (EmbOp): { // 1: script embed
		if (exp->siz != 1 || datxEmbFp == 0) ERROR();
		if (typ == -1) typ = identType("Int"); if (typ != identType("Int")) ERROR();
		datxEval(datxDat0,&exp->exp[0],identType("Str"));
		datxInt(dat,datxEmbFp(datxChrz(0,datxDat0)));} break;
	case (DatOp): { // 0; stream data
		struct Data src = {0}; struct Data dst = {0};
		if (exp->siz != 1) ERROR();
		if (typ == -1) typ = identType("Data"); if (typ != identType("Data")) ERROR();
		datxEval(datxDat0,&exp->exp[0],identType("Data")); readData(&src,datxIdx0);
		datxDatae(&dst,&src,exp->dat); freeData(&src);
		datxNone(datxDat0); writeData(&dst,datxIdx0);
		assignDat(dat,*datxDat0); freeData(&dst);} break;
	default: ERROR();}
	return typ;
}
void datxPrefix(const char *str)
{
	datxStr(&prefix,str);
}
void datxCallback(dftype fnc)
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
