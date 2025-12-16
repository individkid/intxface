#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "sugx.h"
#include "stlx.h"
#include "type.h"

DECLARE_DEQUE(struct Express *,Expr)
enum Closure {
	OpClo,
	BitClo,
	CmpClo,
	LimClo,
};
void sugarFront(struct Express *dst, void *src)
{
	struct Express *ptr = frontExpr(src);
	copyExpress(dst,ptr);
	freeExpress(ptr);
	allocExpress(&ptr,0);
	dropExpr(src);
}
void sugarBack(struct Express *dst, void *src)
{
	struct Express *ptr = backExpr(src);
	copyExpress(dst,ptr);
	freeExpress(ptr);
	allocExpress(&ptr,0);
	popExpr(src);
}
void sugarCopy(struct Express *dst, struct Express **src)
{
	copyExpress(dst,*src);
	freeExpress(*src);
	allocExpress(src,0);
}
enum Closure sugarRecurse(void *lst, int lim, const char *str, int *idx);
enum Closure sugarBinary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *ret = 0;
	while (sizeExpr(nst) > 0) {
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->opa,2);
	if (ret == 0) sugarFront(&exp->opa[0],nst);
	else sugarCopy(&exp->opa[0],&ret);
	sugarFront(&exp->opa[1],nst);
	ret = exp;}
	freeExpr(nst); pushExpr(ret,lst);
	return clo;
}
enum Closure sugarBitwise(void *lst, enum Bitwise bit, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *ret = 0;
	while (sizeExpr(nst) > 0) {
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = BitOp; exp->bit = bit; allocExpress(&exp->opb,2);
	if (ret == 0) sugarFront(&exp->opb[0],nst);
	else sugarCopy(&exp->opb[0],&ret);
	sugarFront(&exp->opb[1],nst);
	ret = exp;}
	freeExpr(nst); pushExpr(ret,lst);
	return clo;
}
enum Closure sugarCompare(void *lst, enum Compare cmp, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *ret = 0;
	while (sizeExpr(nst) > 0) {
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = CmpOp; exp->cmp = cmp; allocExpress(&exp->opc,2);
	if (ret == 0) sugarFront(&exp->opc[0],nst);
	else sugarCopy(&exp->opc[0],&ret);
	sugarFront(&exp->opc[1],nst);
	ret = exp;}
	freeExpr(nst); pushExpr(ret,lst);
	return clo;
}
enum Closure sugarUnary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->put,1);
	for (int i = 0; i < 1; i++)
	sugarFront(&exp->put[i],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarTrinary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,3,str,idx);
	if (sizeExpr(nst) != 3) ERROR();
	struct Express *exp = 0; allocExpress(&exp,3);
	exp->opr = opr; allocExpress(&exp->ext,3);
	for (int i = 0; i < 3; i++)
	sugarFront(&exp->ext[i],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarQuadary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,4,str,idx);
	if (sizeExpr(nst) != 4) ERROR();
	struct Express *exp = 0; allocExpress(&exp,4);
	exp->opr = opr; allocExpress(&exp->fld,4);
	for (int i = 0; i < 4; i++)
	sugarFront(&exp->fld[i],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarInfix(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->opa,2);
	sugarBack(&exp->opa[0],lst);
	sugarFront(&exp->opa[1],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarBitfix(void *lst, enum Bitwise bit, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = BitOp; exp->bit = bit; allocExpress(&exp->opb,2);
	sugarBack(&exp->opb[0],lst);
	sugarFront(&exp->opb[1],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarCmpfix(void *lst, enum Compare cmp, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = CmpOp; exp->cmp = cmp; allocExpress(&exp->opc,2);
	sugarBack(&exp->opc[0],lst);
	sugarFront(&exp->opc[1],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarAssign(void *lst, enum Operate opr, const char *key, int len, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (len == 0 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->kys = malloc(len+1); allocExpress(&exp->sav,1);
	strncpy(exp->kys,key,len); exp->kys[len] = 0;
	sugarFront(&exp->sav[0],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarSetcfg(void *lst, enum Operate opr, enum Configure cfg, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->cgs = cfg; allocExpress(&exp->set,1);
	sugarFront(&exp->set[0],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarCondit(void *lst, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst)%2) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = CndOp; exp->siz = sizeExpr(nst)/2;
	allocExpress(&exp->cnd,exp->siz); allocExpress(&exp->lst,exp->siz);
	for (int i = 0; i < exp->siz; i++) {
	sugarFront(&exp->cnd[i*2],nst);
	sugarFront(&exp->cnd[i*2+1],nst);}
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Closure sugarForeach(void *lst, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 1) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = FesOp; exp->num = sizeExpr(nst)-1;
	allocExpress(&exp->msk,1); allocExpress(&exp->mux,exp->num);
	sugarFront(&exp->msk[0],nst);
	for (int i = 0; i < exp->num; i++) {
	sugarFront(&exp->mux[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
	return clo;
}
enum Parse {
	IdBefPar,
	IdMidPar,
	IdAftPar,
	OpIntPar,
	OpStrPar,
	IbIntPar,
	ImIntPar,
	IbStrPar,
	ImStrPar,
};
void sugarIdPar(const char **key, int *len, enum Parse *par, const char *str, int lft, int rgt)
{
	switch (*par) {default:
	break; case (IdMidPar): case (IdAftPar):
	*len = rgt-lft; *key = str+lft; *par = IdBefPar;}
}
void sugarWsPar(int *ptr, enum Parse *par, int rgt)
{
	switch (*par) {default:
	break; case (IdMidPar): *ptr = rgt; *par = IdAftPar;}
}
void sugarNwsPar(int *ptr, enum Parse *par, int lft)
{
	switch (*par) {default:
	break; case (IdBefPar): *ptr = lft; *par = IdMidPar;
	break; case (IbIntPar): *ptr = lft; *par = ImIntPar;
	break; case (IbStrPar): *ptr = lft; *par = ImStrPar;}
}
void sugarOpPar(char **ptr, int *num, enum Parse *par, const char *str, int lft, int rgt)
{
	switch (*par) {default:
	break; case (OpIntPar):
	sscanf(str+lft," %d",num);
	break; case (OpStrPar):
	*ptr = malloc(rgt-lft+1);
	strncpy(*ptr,str+lft,rgt-lft);
	(*ptr)[rgt-lft] = 0;}
	*par = IdBefPar;
}
void sugarImPar(char **ptr, int *num, enum Parse *par, const char *str, int lft, int rgt)
{
	switch (*par) {default:
	break; case (ImIntPar):
	sscanf(str+lft," %d",num);
	break; case (ImStrPar):
	*ptr = malloc(rgt-lft+1);
	strncpy(*ptr,str+lft,rgt-lft);
	(*ptr)[rgt-lft] = 0;}
	*par = IdBefPar;
}
enum Closure sugarRecurse(void *lst, int lim, const char *str, int *idx)
{
	// if (lim<0) go until Op; else don't expect Op
	enum Parse par = IdBefPar;
	char **ptr = 0;
	int *num = 0;
	int lft = *idx;
	int rgt = *idx;
	while (lim) {
	if (strncmp(str+*idx,"Op",2)==0) {*idx += 2;
		sugarOpPar(ptr,num,&par,str,lft,*idx-2);
		return OpClo;}
	if (strncmp(str+*idx,"Bit",3)==0) {*idx += 3;
		sugarOpPar(ptr,num,&par,str,lft,*idx-3);
		return BitClo;}
	if (strncmp(str+*idx,"Cmp",3)==0) {*idx += 3;
		sugarOpPar(ptr,num,&par,str,lft,*idx-3);
		return CmpClo;}
	if (strncmp(str+*idx,"Add",3)==0) {*idx += 3;
		if (sugarBinary(lst,AddOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"+",1)==0) {*idx += 1;
		if (sugarInfix(lst,AddOp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Sub",3)==0) {*idx += 3;
		if (sugarBinary(lst,SubOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"-",1)==0) {*idx += 1;
		if (sugarInfix(lst,SubOp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Mul",3)==0) {*idx += 3;
		if (sugarBinary(lst,MulOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"*",1)==0) {*idx += 1;
		if (sugarInfix(lst,MulOp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Div",3)==0) {*idx += 3;
		if (sugarBinary(lst,DivOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"/",1)==0) {*idx += 1;
		if (sugarInfix(lst,DivOp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Rem",3)==0) {*idx += 3;
		if (sugarBinary(lst,RemOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"%",1)==0) {*idx += 1;
		if (sugarInfix(lst,RemOp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"And",3)==0) {*idx += 3;
		if (sugarBitwise(lst,AndBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"&",1)==0) {*idx += 1;
		if (sugarBitfix(lst,AndBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Or",2)==0) {*idx += 2;
		if (sugarBitwise(lst,OrBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"|",1)==0) {*idx += 1;
		if (sugarBitfix(lst,OrBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Xor",3)==0) {*idx += 3;
		if (sugarBitwise(lst,XorBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"^",1)==0) {*idx += 1;
		if (sugarBitfix(lst,XorBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Nand",4)==0) {*idx += 4;
		if (sugarBitwise(lst,NandBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!&",2)==0) {*idx += 2;
		if (sugarBitfix(lst,NandBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Nor",3)==0) {*idx += 3;
		if (sugarBitwise(lst,NorBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!|",2)==0) {*idx += 2;
		if (sugarBitfix(lst,NorBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Nxor",4)==0) {*idx += 4;
		if (sugarBitwise(lst,NxorBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!^",2)==0) {*idx += 2;
		if (sugarBitfix(lst,NxorBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Shl",3)==0) {*idx += 3;
		if (sugarBitwise(lst,ShlBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"<<",2)==0) {*idx += 2;
		if (sugarBitfix(lst,ShlBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Fns",3)==0) {*idx += 3;
		if (sugarBitwise(lst,FnsBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,">>",2)==0) {*idx += 2;
		if (sugarBitfix(lst,FnsBit,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"LO",2)==0) {*idx += 2;
		if (sugarCompare(lst,LOCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (str[*idx] == '<' && str[*idx+1] != '<' && str[*idx+1] != '=') {*idx += 1;
		if (sugarCmpfix(lst,LOCmp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"LC",2)==0) {*idx += 2;
		if (sugarCompare(lst,LCCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"<=",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,LCCmp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"EO",2)==0) {*idx += 2;
		if (sugarCompare(lst,EOCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!=",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,EOCmp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"EC",2)==0) {*idx += 2;
		if (sugarCompare(lst,ECCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"==",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,ECCmp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"MO",2)==0) {*idx += 2;
		if (sugarCompare(lst,MOCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (str[*idx] == '>' && str[*idx+1] != '>' && str[*idx+1] != '=') {*idx += 1;
		if (sugarCmpfix(lst,MOCmp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"MC",2)==0) {*idx += 2;
		if (sugarCompare(lst,MCCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,">=",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,MCCmp,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Cnd",3)==0) {*idx += 3;
		if (sugarCondit(lst,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Fes",3)==0) {*idx += 3;
		if (sugarForeach(lst,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Ret",3)==0) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		enum Configure cfg;
		if (!hideConfigure(&cfg,str,idx)) ERROR();
		exp->opr = RetOp; exp->cfg = cfg;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Set",3)==0) {*idx += 3;
		enum Configure cfg;
		if (!hideConfigure(&cfg,str,idx)) ERROR();
		if (sugarSetcfg(lst,SetOp,cfg,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Wos",3)==0) {*idx += 3;
		enum Configure cfg;
		if (!hideConfigure(&cfg,str,idx)) ERROR();
		if (sugarSetcfg(lst,WosOp,cfg,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Woc",3)==0) {*idx += 3;
		enum Configure cfg;
		if (!hideConfigure(&cfg,str,idx)) ERROR();
		if (sugarSetcfg(lst,WocOp,cfg,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Raw",3)==0) {*idx += 3;
		enum Configure cfg;
		if (!hideConfigure(&cfg,str,idx)) ERROR();
		if (sugarSetcfg(lst,RawOp,cfg,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Val",3)==0) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->key; exp->opr = ValOp; par = OpStrPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"$",1)==0) {*idx += 1;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->key; exp->opr = ValOp; par = IbStrPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Sav",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->kys; exp->opr = SavOp; par = OpStrPar;
		pushExpr(exp,lst);
		continue;}
	if (str[*idx] == '=' && str[*idx+1] != '=') {*idx += 1;
		const char *key; int len;
		sugarIdPar(&key,&len,&par,str,lft,rgt);
		if (sugarAssign(lst,SavOp,key,len,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Rex",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->rex; exp->opr = RexOp; par = OpStrPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Irx",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->irx; exp->opr = IrxOp; par = OpStrPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Get",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		exp->opr = GetOp;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Put",3)==3) {*idx += 3;
		if (sugarUnary(lst,PutOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Fld",3)==3) {*idx += 3;
		if (sugarQuadary(lst,FldOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Ext",3)==3) {*idx += 3;
		if (sugarTrinary(lst,ExtOp,str,idx) != OpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Imm",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->val; exp->opr = ImmOp; par = OpStrPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Int",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		num = &exp->ivl; exp->opr = IntOp; par = OpIntPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"#",1)==0) {*idx += 1;
		struct Express *exp = 0; allocExpress(&exp,1);
		num = &exp->ivl; exp->opr = IntOp; par = IbIntPar;
		pushExpr(exp,lst);
		continue;}
	if (strncmp(str+*idx,"Str",3)==3) {*idx += 3;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->svl; exp->opr = StrOp; par = OpStrPar;
		pushExpr(exp,lst);
		continue;}
	if (isspace(str[*idx])) {*idx += 1;
		sugarWsPar(&rgt,&par,*idx-1);
		sugarImPar(ptr,num,&par,str,lft,*idx-1);
		continue;}
	*idx += 1;
	sugarNwsPar(&lft,&par,*idx-1);}
	return LimClo; // TODO decrement lim above
}
void sugarExpand(char **ptr, const char *str)
{
	void *lst = allocExpr(); int idx = 0;
	if (sugarRecurse(lst, 1, str, &idx) != OpClo || sizeExpr(lst) != 1) ERROR();
	free(*ptr); *ptr = 0;
	struct Express *exp = frontExpr(lst);
	showExpress(exp,ptr);
	freeExpress(exp);
	allocExpress(&exp,0);
	dropExpr(lst);
	freeExpr(lst);
}