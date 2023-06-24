#include "type.h"
#include "face.h"
#include "datx.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

struct Data *base[NUMOPEN] = {0}; // last read
int *next[NUMOPEN] = {0}; // bytes per sub
int totl[NUMOPEN] = {0}; // bytes since open
int last[NUMOPEN] = {0}; // bytes at last read
int *todo[NUMOPEN] = {0}; // opcodes to run
int done[NUMOPEN] = {0}; // opcodes total
int *limt[NUMOPEN] = {0}; // bytes per sub
int *jump[NUMOPEN] = {0}; // opcode per sub
int lmts[NUMOPEN] = {0}; // subs total
int mark[NUMOPEN] = {0}; // opcode to jump
int strt[NUMOPEN] = {0}; // opcode to restart
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
void datxOpen(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (base[idx] == 0) {
		allocData(&base[idx],1);
		readData(base[idx],idx);
		allocInt(&next[idx],base[idx]->siz);}
}
void datxClose(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (base[idx] != 0) allocData(&base[idx],0);
	if (next[idx] != 0) allocInt(&next[idx],0);
	if (todo[idx] != 0) allocInt(&todo[idx],0);
	if (limt[idx] != 0) allocInt(&limt[idx],0);
	if (jump[idx] != 0) allocInt(&jump[idx],0);
	totl[idx] = 0; last[idx] = 0; done[idx] = 0;lmts[idx] = 0;
}
void datxProg(int sub, int idx)
{
	struct Data *dat = 0;
	int *nxt = 0;
	enum Logic *opc = 0;
	int *lim = 0;
	int *jmp = 0;
	int *loc = 0;
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	dat = base[idx];
	nxt = next[idx];
	opc = dat->opc;
	lim = dat->lim;
	jmp = &dat->jmp[sub];
	loc = &dat->loc[sub];
	while (*jmp >= 0 && *jmp < dat->len) {
	int opd = opc[*jmp]>>4;
	switch ((enum Logic)(opc[*jmp]&0xf)) {
	case (WrpJmp): if (nxt[opd] == lim[opd]) {*jmp = *loc; continue;} break;
	case (WrpYld): if (nxt[opd] == lim[opd]) {*jmp = *loc; return;} break;
	case (RunJmp): if (nxt[opd] < lim[opd]) {*jmp = *loc; continue;} break;
	case (RunYld): if (nxt[opd] < lim[opd]) {*jmp = *loc; return;} break;
	case (ClrVal): nxt[opd] = 0; break;
	case (ClrJmp): nxt[opd] = 0; *jmp = *loc; continue;
	case (ClrYld): nxt[opd] = 0; *jmp = *loc; return;
	case (SkpVal): nxt[opd] = lim[opd]; break;
	case (SkpJmp): nxt[opd] = lim[opd]; *jmp = *loc; continue;
	case (SkpYld): nxt[opd] = lim[opd]; *jmp = *loc; return;
	case (ImmJmp): *jmp = opd; continue;
	case (ImmYld): *jmp = opd; return;
	case (SetJmp): *loc = opd; break;
	default: ERROR();}
	*jmp += 1;}
}
void datxCopy(void **ptr, void *dat, int loc, int siz)
{
	void *pre = 0;
	void *suf = 0;
	void *tmp = 0;
	datxSplit(&pre,&tmp,dat,loc);
	datxSplit(ptr,&suf,tmp,siz);
	free(pre);
	free(suf);
	free(tmp);
}
void datxRead(void **ptr, int sub, int num, int idx)
{
	void *ret = 0;
	struct Data *dat = 0;
	int *nxt = 0;
	int tot = 0;
	int lst = 0;
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	dat = base[idx];
	nxt = next[idx];
	tot = totl[idx]; // total number copied
	lst = last[idx]; // start of current dat
	while (num > 0) {
		int inc = (*(int*)(dat->dat))-(tot-lst);
		if (inc <= 0) break;
		for (int i = 0; i < dat->siz; i++) {
			int dif = dat->lim[i]-nxt[i];
			if (dif > 0 && dif < inc) inc = dif;}
		for (int i = 0; i < dat->siz; i++) {
			if (nxt[i] < dat->lim[i]) nxt[i] += inc;}
		for (int i = 0; i < dat->siz; i++) {
			datxProg(idx,i);}
		if (nxt[sub] == 0) num--;
		tot += inc;}
	if (num > 0) ERROR();
	datxCopy(ptr,dat->dat,totl[idx]-lst,tot-totl[idx]);
	totl[idx] = tot;
	if (tot-lst >= *(int*)(dat->dat)) {
		struct Data *tmp = 0;
		allocData(&tmp,1);
		readData(tmp,idx);
		if (tmp->siz == 0) {
			assignDat(dat->dat,tmp->dat);
			allocData(&tmp,0);}
		else {
			allocData(&base[idx],0);
			base[idx] = tmp;}
		last[idx] = tot;}
}
void datxInit(int sub, int lim, int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (done[idx] != 0) ERROR();
	if (lmts[idx] <= sub) {
	resizeInt(&limt[idx],sub,sub+1);
	resizeInt(&jump[idx],sub,sub+1);
	lmts[idx] = sub+1;}
	limt[idx][sub] = lim; jump[idx][sub] = done[idx]; strt[idx] = done[idx]; mark[idx] = done[idx];
	appendInt(&todo[idx],Logics,&done[idx]);
}
void datxStep(int sub, int opc, int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (sub < 0 || sub >= lmts[idx]) ERROR();
	if (opc < 0 || opc >= ImmJmp) ERROR();
	appendInt(&todo[idx],((sub<<4)|opc),&done[idx]);
}
void datxMark(int idx)
{
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (todo[idx][mark[idx]] != Logics) ERROR();
	todo[idx][mark[idx]] = ((done[idx]<<4)|SetJmp);
	mark[idx] = done[idx];
	appendInt(&todo[idx],Logics,&done[idx]);
}
void datxWrite(void *dat, int idx)
{
	struct Data tmp = {0};
	if (idx < 0 || idx >= NUMOPEN) ERROR();
	if (done[idx] != 0) {
	if (todo[idx][mark[idx]] != Logics) ERROR();
	todo[idx][mark[idx]] = ((done[idx]<<4)|SetJmp);
	appendInt(&todo[idx],((strt[idx]<<4)|ImmYld),&done[idx]);
	allocInt(&(tmp.lim),lmts[idx]);
	allocInt(&(tmp.jmp),lmts[idx]);
	allocLogic(&(tmp.opc),done[idx]);
	for (int i = 0; i < lmts[idx]; i++) tmp.lim[i] = limt[idx][i];
	for (int i = 0; i < lmts[idx]; i++) tmp.jmp[i] = jump[idx][i];
	for (int i = 0; i < done[idx]; i++) tmp.opc[i] = todo[idx][i];
	allocInt(&limt[idx],0);
	allocInt(&jump[idx],0);
	allocInt(&todo[idx],0);
	tmp.siz = lmts[idx]; tmp.len = done[idx];
	done[idx] = 0; lmts[idx] = 0;}
	assignDat(&tmp.dat,dat);
	writeData(&tmp,idx);
	freeData(&tmp);
}
int datxMeta(int sub, int idx)
{
	datxOpen(idx);
	if (sub < 0 || sub >= base[idx]->siz) ERROR();
	return next[idx][sub];
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
	if (box.ptr->siz == 0) {box.idx = -1; return box;}
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
	if (box.idx >= box.ptr->siz) ERROR();
	if (box.idx >= 0 && datxCompare(box.ptr->key[box.idx],dat) == 0) {
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
#define BINARY_CMP(LFT,RGT) strcmp(LFT,RGT)
#define BINARY_TRI(LFT,RGT) (LFT>RGT?1:(LFT<RGT?-1:0))
#define BINARY_ADD(LFT,RGT) (LFT+RGT)
#define BINARY_SUB(LFT,RGT) (LFT-RGT)
#define BINARY_MUL(LFT,RGT) (LFT*RGT)
#define BINARY_DIV(LFT,RGT) (LFT/RGT)
#define BINARY_REM(LFT,RGT) (LFT%RGT)
#define BINARY_MOD(LFT,RGT) fmod(LFT,RGT)
#define BINARY_FLM(LFT,RGT) fmodf(LFT,RGT)
#define BINARY_BEGIN(TYP)\
	void *dat0 = 0; void *dat1 = 0;\
	int typ0 = 0; int typ1 = 0;\
	if (exp->siz != 2) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}\
	typ0 = datxEval(&dat0,&exp->exp[0],TYP);\
	typ1 = datxEval(&dat1,&exp->exp[1],TYP);\
	if (typ0 != typ1) {fprintf(stderr,"wrong type of argument %d %d\n",typ0,typ1); ERROR();}
#define BINARY_DONE()\
	free(dat0); free(dat1);
#define BINARY_TYPE(TYPE,TYP,GET,SET,OP)\
	if (typ0 == identType(TYP)) {\
	TYPE lft = GET(0,dat0);\
	TYPE rgt = GET(0,dat1);\
	SET(dat,OP(lft,rgt));}
#define BINARY_BLOCK(OP,STR) {\
	BINARY_BEGIN(typ)\
	if (typ < 0) typ = typ0;\
	if (typ != typ0) {fprintf(stderr,"inconsistent type %d\n",typ); ERROR();}\
	BINARY_TYPE(int,"Int",*datxIntz,datxInt,OP) else\
	BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,OP) else\
	BINARY_TYPE(double,"Num",*datxNumz,datxNum,OP) else\
	BINARY_TYPE(float,"Old",*datxOldz,datxOld,OP) else\
	{fprintf(stderr,"unsupported "STR" type %d\n",typ); ERROR();}}
#define BINARY_SET(DAT,VAL) val = datxEcmp(VAL,exp->flt[i].neq[j]);
int datxEcmp(int val, enum Ineq neq)
{
	switch (neq) {
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
	case (RemOp): { // 2;
		BINARY_BEGIN(typ)
		if (typ < 0) typ = typ0;
		if (typ != typ0) {fprintf(stderr,"inconsistent type %d\n",typ); ERROR();}
		BINARY_TYPE(int,"Int",*datxIntz,datxInt,BINARY_REM) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,datxInt32,BINARY_REM) else
		BINARY_TYPE(double,"Num",*datxNumz,datxNum,BINARY_MOD) else
		BINARY_TYPE(float,"Old",*datxOldz,datxOld,BINARY_FLM) else
		{fprintf(stderr,"unsupported rem type %d\n",typ); ERROR();}
		BINARY_DONE()} break;
	case (CmpOp): {const char *str = ""; int typ0 = -1;
		for (int i = 0; i < exp->siz; i++) {
		if (exp->flt[i].cmp == Branch) {
		void *dat0 = 0; int typ0 = 0;
		typ0 = datxEval(&dat0,&exp->exp[i],-1);
		for (int j = 0; j < exp->flt[i].siz; j++) {
		void *dat1 = 0; int typ1 = 0; int val = 0;
		typ1 = datxEval(&dat1,&exp->flt[i].exp[j],-1);
		if (typ0 != typ1) {fprintf(stderr,"wrong type of argument %d %d\n",typ0,typ1); ERROR();}
		BINARY_TYPE(int,"Int",*datxIntz,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(int32_t,"Int32",*datxInt32z,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(double,"Num",*datxNumz,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(float,"Old",*datxOldz,BINARY_SET,BINARY_TRI) else
		BINARY_TYPE(char*,"Str",datxChrz,BINARY_SET,BINARY_CMP) else
		{fprintf(stderr,"unsupported cmp type %d\n",typ); ERROR();}
		if (val && strncmp(str,exp->flt[i].str[j],strlen(str)) == 0) {
		str = exp->flt[i].str[j];}}} else {
		for (int j = 0; j < exp->flt[i].siz; j++) {
		if (strcmp(str,exp->flt[i].str[j]) == 0) {
		typ0 = datxEval(dat,&exp->exp[i],typ);}}}}
		if (typ < 0) typ = typ0;
		if (typ != typ0) {fprintf(stderr,"no match found %d\n",typ); ERROR();}} break;
	case (TotOp): { // 1; cast to type
		int tmp = 0;
		if (exp->siz != 1) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		tmp = datxEval(dat,&exp->exp[0],-1);
		if (typ == -1) typ = tmp;
		else if (typ == identType("Int") && tmp == identType("Int32")) datxInt(dat,*datxInt32z(0,dat));
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
		else if (typ == identType("Old") && tmp == identType("Num")) datxOld(dat,*datxNumz(0,dat));
		else if (typ != tmp) {fprintf(stderr,"cannot cast from %d to %d\n",tmp,typ); ERROR();}} break;
	case (ImmOp): { // 0; built in value
		datxSingle();
		if (exp->siz != 0) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		if (typ == -1) typ = identUnion(exp->val);
		if (typ != identUnion(exp->val)) {fprintf(stderr,"wrong type of argument %d %d\n",typ,identUnion(exp->val)); ERROR();}
		datxNone(datxDat0);
		writeUnion(exp->val,datxIdx0);
		assignDat(dat,*datxDat0);} break;
	case (ValOp): { // 0; restore from named
		void *dat0 = 0; int typ0 = 0;
		if (exp->siz != 2) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		datxEval(&dat0,&exp->exp[0],-1); typ0 = datxFind(dat,dat0);
		if (*dat == 0) typ0 = datxEval(dat,&exp->exp[1],typ);
		if (typ == -1) typ = typ0;
		if (typ0 != typ) {fprintf(stderr,"wrong type of value %d\n",typ0); ERROR();}
		free(dat0);} break;
	case (SavOp): { // 3; save to named
		void *dat0 = 0; void *dat1 = 0; int typ1 = 0;
		if (exp->siz != 3) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		datxEval(&dat0,&exp->exp[0],-1); typ1 = datxEval(&dat1,&exp->exp[1],-1); datxInsert(dat0,dat1,typ1);
		datxEval(dat,&exp->exp[2],typ);
		free(dat0); free(dat1);} break;
	case (GetOp): { // 0; value from callback
		void *save = 0;
		if (exp->siz != 0) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		if (typ == -1) typ = identType("Int");
		if (typ != identType("Int")) {fprintf(stderr,"wrong type for GetOp %d\n",typ); ERROR();}
		assignDat(&save,prefix);
		if (datxGetFp == 0) {fprintf(stderr,"getter not set\n"); ERROR();}
		datxGetFp(dat,exp->cfg);
		assignDat(&prefix,save);} break;
	case (SetOp): { // 2; callback with value
		void *dat0 = 0; int typ0 = 0; void *save = 0;
		if (exp->siz != 2) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		typ0 = datxEval(&dat0,&exp->exp[0],-1);
		assignDat(&save,prefix);
		if (datxSetFp == 0) {fprintf(stderr,"setter not set\n"); ERROR();}
		datxSetFp(dat0,exp->cfg);
		datxEval(dat,&exp->exp[1],typ);
		assignDat(prefix,save); free(dat0);} break;
	case (InsOp): { // 2; field to struct
		int typ0 = 0; int typ1 = 0; datxSingle();
		if (exp->siz != 2) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		typ0 = datxEval(datxDat0,&exp->exp[0],typ);
		typ1 = datxEval(datxDat1,&exp->exp[1],identField(typ0,exp->fld));
		datxNone(datxDat2); readField(typ0,typ1,exp->idx,datxIdx0,datxIdx1,datxIdx2);
		assignDat(dat,*datxDat2);} break;
	case (ExtOp): { // 1; field from struct
		int typ0 = 0; datxSingle();
		if (exp->siz != 1) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		typ0 = datxEval(datxDat0,&exp->exp[0],typ);
		datxNone(datxDat1); writeField(typ0,identField(typ0,exp->fld),exp->idx,datxIdx0,datxIdx1);
		assignDat(dat,*datxDat1);} break;
	case (UnqOp): { // 0; magic number
		if (exp->siz != 0) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		if (typ != identType("Int")) {fprintf(stderr,"wrong type of argument %d\n",typ); ERROR();}
		datxInt(dat,unique++);} break;
	case (EmbOp): { // 1: script embed
		if (exp->siz != 1) {fprintf(stderr,"wrong number of arguments %d\n",exp->siz); ERROR();}
		if (datxEmbFp == 0) {fprintf(stderr,"no interpreter embedded\n"); ERROR();}
		if (typ != identType("Int")) {fprintf(stderr,"wrong type of result %d\n",typ); ERROR();}
		datxEval(datxDat0,&exp->exp[0],identType("Str"));
		datxInt(dat,datxEmbFp(datxChrz(0,datxDat0)));} break;
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
