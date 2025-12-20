#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "sugx.h"
#include "stlx.h"
#include "type.h"

DECLARE_DEQUE(struct Express *,Expr)
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
void sugarShow(char **ptr, void *lst)
{
	free(*ptr); *ptr = 0;
	struct Express *exp = frontExpr(lst);
	showExpress(exp,ptr);
	freeExpress(exp);
	allocExpress(&exp,0);
	dropExpr(lst);
}
void sugarRecurse(void *lst, int lim, const char *str, int *idx);
void sugarBinary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,-1,str,idx);
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
}
void sugarBitwise(void *lst, enum Operate opr, enum Bitwise bit, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *ret = 0;
	while (sizeExpr(nst) > 0) {
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->bit = bit; allocExpress(&exp->opb,2);
	if (ret == 0) sugarFront(&exp->opb[0],nst);
	else sugarCopy(&exp->opb[0],&ret);
	sugarFront(&exp->opb[1],nst);
	ret = exp;}
	freeExpr(nst); pushExpr(ret,lst);
}
void sugarCompare(void *lst, enum Operate opr, enum Compare cmp, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *ret = 0;
	while (sizeExpr(nst) > 0) {
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->cmp = cmp; allocExpress(&exp->opc,2);
	if (ret == 0) sugarFront(&exp->opc[0],nst);
	else sugarCopy(&exp->opc[0],&ret);
	sugarFront(&exp->opc[1],nst);
	ret = exp;}
	freeExpr(nst); pushExpr(ret,lst);
}
void sugarUnary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->put,1);
	for (int i = 0; i < 1; i++) {
	sugarFront(&exp->put[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarTrinary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,3,str,idx);
	if (sizeExpr(nst) != 3) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->ext,3);
	for (int i = 0; i < 3; i++) {
	sugarFront(&exp->ext[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarQuadary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,4,str,idx);
	if (sizeExpr(nst) != 4) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->fld,4);
	for (int i = 0; i < 4; i++) {
	sugarFront(&exp->fld[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarInfix(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; allocExpress(&exp->opa,2);
	sugarBack(&exp->opa[0],lst);
	sugarFront(&exp->opa[1],nst);
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarBitfix(void *lst, enum Operate opr, enum Bitwise bit, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->bit = bit; allocExpress(&exp->opb,2);
	sugarBack(&exp->opb[0],lst);
	sugarFront(&exp->opb[1],nst);
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarCmpfix(void *lst, enum Operate opr, enum Compare cmp, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->cmp = cmp; allocExpress(&exp->opc,2);
	sugarBack(&exp->opc[0],lst);
	sugarFront(&exp->opc[1],nst);
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarCondit(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst)%2) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->siz = sizeExpr(nst)/2;
	allocExpress(&exp->cnd,exp->siz); allocExpress(&exp->lst,exp->siz);
	for (int i = 0; i < exp->siz; i++) {
	sugarFront(&exp->cnd[i*2],nst);
	sugarFront(&exp->cnd[i*2+1],nst);}
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarForeach(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 1) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr; exp->num = sizeExpr(nst)-1;
	allocExpress(&exp->msk,1); allocExpress(&exp->mux,exp->num);
	sugarFront(&exp->msk[0],nst);
	for (int i = 0; i < exp->num; i++) {
	sugarFront(&exp->mux[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarGetcfg(void *lst, enum Operate opr, enum Configure cfg, const char *str, int *idx)
{
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr; exp->cfg = cfg;
	pushExpr(exp,lst);
}
void sugarSetcfg(void *lst, enum Operate opr, enum Configure cfg, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->cgs = cfg;
	allocExpress(&exp->set,1);
	sugarFront(&exp->set[0],nst);
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarGetval(void *lst, enum Operate opr, char *key, const char *str, int *idx)
{
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr; exp->key = key;
	pushExpr(exp,lst);
}
void sugarSetval(void *lst, enum Operate opr, char *key, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->lhs = key;
	allocExpress(&exp->rhs,1);
	sugarFront(&exp->rhs[0],nst);
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarZeroary(void *lst, enum Operate opr, const char *str, int *idx)
{
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr;
	pushExpr(exp,lst);
}
void sugarGetnum(void *lst, enum Operate opr, int val, const char *str, int *idx)
{
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr; exp->val = val;
	pushExpr(exp,lst);
}
void leftSugar(const char *str, int *idx)
{
	while (str[*idx] && isspace(str[*idx])) *idx += 1;
}
int compSugar(const char *str, int *idx)
{
	if (strncmp(str+*idx,"Op",2)==0) return 1;
	if (strncmp(str+*idx,"Bit",3)==0) return 1;
	if (strncmp(str+*idx,"Cmp",3)==0) return 1;
	return 0;
}
void rightSugar(const char *str, int *idx)
{
	while (str[*idx] && !isspace(str[*idx]) && compSugar(str,idx)) *idx += 1;
}
void copySugar(char **idt, int lft, int rgt, const char *str)
{
	int len = rgt-lft;
	*idt = realloc(*idt,len+1);
	strncpy(*idt,str+lft,len);
	(*idt)[len] = 0;
}
void hideSugar(char **idt, const char *str, int *idx) // find next alphanumeric
{
	leftSugar(str,idx);
	int lft = *idx;
	rightSugar(str,idx);
	int rgt = *idx;
	copySugar(idt,lft,rgt,str);
}
void backSugar(char **idt, const char *str, int *idx) // find last alphanumeric
{
	int sub = *idx - 1;
	while (sub >= 0 && isspace(str[sub])) sub -= 1;
	int rgt = sub + 1;
	while (sub >= 0 && !isspace(str[sub])) sub -= 1;
	int lft = sub + 1;
	if (lft==rgt) ERROR();
	int len = rgt-lft;
	*idt = realloc(*idt,len+1);
	strncpy(*idt,str+lft,len);
	(*idt)[len] = 0;
}
void scanSugar(int *num, const char *str, int *idx) // find next number
{
	leftSugar(str,idx);
	int lft = *idx;
	int ofs = 0;
	if (sscanf(str+lft,"%d%n",num,&ofs)!=1 || ofs==0) ERROR();
	*idx = lft+ofs;
}
void skipSugar(const char *opr, const char *str, int *idx)
{
	int len = strlen(opr);
	while (str[*idx]) {
	if (strncmp(str+*idx,opr,len)==0) {
		*idx += len;
		return;}
	*idx += 1;}
	ERROR();
}
void moreSugar(char **raw, const char *str, int *idx) // find all til Op
{
	int lft = *idx;
	while (str[*idx] && compSugar(str,idx)) *idx += 1;
	int rgt = *idx;
	copySugar(raw,lft,rgt,str);
}
int debug = 0;
void sugarRecurse(void *lst, int lim, const char *str, int *idx)
{
	debug += 1;
	int siz = sizeExpr(lst);
	while (str[*idx] && (lim < 0 || sizeExpr(lst)-siz < lim)) {
	fprintf(stderr,"Recurse ");
	for (int i = 0; i < debug; i++) fprintf(stderr,"-");
	fprintf(stderr,"- %s -- %d\n",str+*idx,*idx);
	if (strncmp(str+*idx,"Op",2)==0) break;
	if (strncmp(str+*idx,"Bit",3)==0) break;
	if (strncmp(str+*idx,"Cmp",3)==0) break;
	if (strncmp(str+*idx,"Add",3)==0) {
		*idx += 3;
		sugarBinary(lst,AddOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"+",1)==0) {
		*idx += 1;
		sugarInfix(lst,AddOp,str,idx);
		continue;}
	if (strncmp(str+*idx,"Sub",3)==0) {
		*idx += 3;
		sugarBinary(lst,SubOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"-",1)==0) {
		*idx += 1;
		sugarInfix(lst,SubOp,str,idx);
		continue;}
	if (strncmp(str+*idx,"Mul",3)==0) {
		*idx += 3;
		sugarBinary(lst,MulOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"*",1)==0) {
		*idx += 1;
		sugarInfix(lst,MulOp,str,idx);
		continue;}
	if (strncmp(str+*idx,"Div",3)==0) {
		*idx += 3;
		sugarBinary(lst,DivOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"/",1)==0) {
		*idx += 1;
		sugarInfix(lst,DivOp,str,idx);
		continue;}
	if (strncmp(str+*idx,"Rem",3)==0) {
		*idx += 3;
		sugarBinary(lst,RemOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"%",1)==0) {
		*idx += 1;
		sugarInfix(lst,RemOp,str,idx);
		continue;}
	if (strncmp(str+*idx,"And",3)==0) {
		*idx += 3;
		sugarBitwise(lst,BitOp,AndBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"&",1)==0) {
		*idx += 1;
		sugarBitfix(lst,BitOp,AndBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Or",2)==0) {
		*idx += 2;
		sugarBitwise(lst,BitOp,OrBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"|",1)==0) {
		*idx += 1;
		sugarBitfix(lst,BitOp,OrBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Xor",3)==0) {
		*idx += 3;
		sugarBitwise(lst,BitOp,XorBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"^",1)==0) {
		*idx += 1;
		sugarBitfix(lst,BitOp,XorBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Nand",4)==0) {
		*idx += 4;
		sugarBitwise(lst,BitOp,NandBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"!&",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,NandBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Nor",3)==0) {
		*idx += 3;
		sugarBitwise(lst,BitOp,NorBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"!|",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,NorBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Nxor",4)==0) {
		*idx += 4;
		sugarBitwise(lst,BitOp,NxorBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"!^",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,NxorBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Shl",3)==0) {
		*idx += 3;
		sugarBitwise(lst,BitOp,ShlBit,str,idx);
		fprintf(stderr,"Shl %s\n",str+*idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,"<<",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,ShlBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"Fns",3)==0) {
		*idx += 3;
		sugarBitwise(lst,BitOp,FnsBit,str,idx);
		skipSugar("Bit",str,idx);
		continue;}
	if (strncmp(str+*idx,">>",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,FnsBit,str,idx);
		continue;}
	if (strncmp(str+*idx,"LO",2)==0) {
		*idx += 2;
		sugarCompare(lst,CmpOp,LOCmp,str,idx);
		skipSugar("Cmp",str,idx);
		continue;}
	if (str[*idx] == '<' && str[*idx+1] != '<' && str[*idx+1] != '=') {
		*idx += 1;
		sugarCmpfix(lst,CmpOp,LOCmp,str,idx);
		continue;}
	if (strncmp(str+*idx,"LC",2)==0) {
		*idx += 2;
		sugarCompare(lst,CmpOp,LCCmp,str,idx);
		skipSugar("Cmp",str,idx);
		continue;}
	if (strncmp(str+*idx,"<=",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,LCCmp,str,idx);
		continue;}
	if (strncmp(str+*idx,"EO",2)==0) {
		*idx += 2;
		sugarCompare(lst,CmpOp,EOCmp,str,idx);
		skipSugar("Cmp",str,idx);
		continue;}
	if (strncmp(str+*idx,"!=",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,EOCmp,str,idx);
		continue;}
	if (strncmp(str+*idx,"EC",2)==0) {
		*idx += 2;
		sugarCompare(lst,CmpOp,ECCmp,str,idx);
		skipSugar("Cmp",str,idx);
		continue;}
	if (strncmp(str+*idx,"==",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,ECCmp,str,idx);
		continue;}
	if (strncmp(str+*idx,"MO",2)==0) {
		*idx += 2;
		sugarCompare(lst,CmpOp,MOCmp,str,idx);
		skipSugar("Cmp",str,idx);
		continue;}
	if (str[*idx] == '>' && str[*idx+1] != '>' && str[*idx+1] != '=') {
		*idx += 1;
		sugarCmpfix(lst,CmpOp,MOCmp,str,idx);
		continue;}
	if (strncmp(str+*idx,"MC",2)==0) {
		*idx += 2;
		sugarCompare(lst,CmpOp,MCCmp,str,idx);
		skipSugar("Cmp",str,idx);
		continue;}
	if (strncmp(str+*idx,">=",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,MCCmp,str,idx);
		continue;}
	if (strncmp(str+*idx,"Cnd",3)==0) {
		*idx += 3;
		sugarCondit(lst,CndOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Fes",3)==0) {
		*idx += 3;
		sugarForeach(lst,FesOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Ret",3)==0) {
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarGetcfg(lst,RetOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Set",3)==0) {
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,SetOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Wos",3)==0) {
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,WosOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Woc",3)==0) {
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,WocOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Raw",3)==0) {
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,RawOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Val",3)==0) {
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,ValOp,idt,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"$",1)==0) {
		*idx += 1;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,ValOp,idt,str,idx);
		continue;}
	if (strncmp(str+*idx,"Sav",3)==0) {
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarSetval(lst,SavOp,idt,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (str[*idx] == '=' && str[*idx+1] != '=') {
		*idx += 1;
		char *idt = 0; backSugar(&idt,str,idx);
		sugarSetval(lst,SavOp,idt,str,idx);
		continue;}
	if (strncmp(str+*idx,"Rex",3)==0) {
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,RexOp,idt,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Irx",3)==0) {
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,IrxOp,idt,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Get",3)==0) {
		*idx += 3;
		sugarZeroary(lst,GetOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Put",3)==0) {
		*idx += 3;
		sugarUnary(lst,PutOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Fld",3)==0) {
		*idx += 3;
		sugarQuadary(lst,FldOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Ext",3)==0) {
		*idx += 3;
		sugarTrinary(lst,ExtOp,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Imm",3)==0) {
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,ImmOp,idt,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"Int",3)==0) {
		*idx += 3;
		int num; scanSugar(&num,str,idx);
		sugarGetnum(lst,IntOp,num,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	if (strncmp(str+*idx,"#",1)==0) {
		*idx += 1;
		int num; scanSugar(&num,str,idx);
		sugarGetnum(lst,IntOp,num,str,idx);
		continue;}
	if (strncmp(str+*idx,"Str",3)==0) {
		*idx += 3;
		char *raw = 0; moreSugar(&raw,str,idx);
		sugarGetval(lst,ImmOp,raw,str,idx);
		skipSugar("Op",str,idx);
		continue;}
	*idx += 1;}
}
void sugarExpand(char **ptr, const char *str)
{
	void *lst = allocExpr(); int idx = 0;
	sugarRecurse(lst, 1, str, &idx);
	if (sizeExpr(lst) != 1) ERROR();
	sugarShow(ptr,lst);
	fprintf(stderr,"%s\n",*ptr);
	freeExpr(lst);
}