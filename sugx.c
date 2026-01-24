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
void sugarRecurse(void *lst, int lim, const char *str, int *idx);
int sugarHide(struct Express ***ptr, const char *str)
{
	void *lst = allocExpr(); int idx = 0;
	sugarRecurse(lst,-1,str,&idx);
	int dim = sizeExpr(lst);
	free(*ptr); *ptr = malloc(dim*sizeof(struct Express *)); dim = 0;
	while (sizeExpr(lst) > 0) {
	(*ptr)[dim] = frontExpr(lst);
	dim += 1;
	dropExpr(lst);}
	freeExpr(lst);
	return dim;
}
void sugarShow(char **ptr, const char *str)
{
	struct Express **exp = 0;
	int dim = sugarHide(&exp,str);
	char **ary = malloc(dim*sizeof(char *));
	int tot = 0;
	for (int i = 0; i < dim; i++) {
	ary[i] = 0; showExpress(exp[i],&ary[i]);
	freeExpress(exp[i]);
	allocExpress(&exp[i],0); exp[i] = 0;
	tot += strlen(ary[i]);}
	free(exp); exp = 0;
	free(*ptr); *ptr = malloc(tot+1); tot = 0;
	for (int i = 0; i < dim; i++) {
	strcpy(*ptr+tot,ary[i]);
	tot += strlen(ary[i]);
	free(ary[i]); ary[i] = 0;}
	free(ary); ary = 0;
}
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
struct Express *sugarTrinary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,1,str,idx);
	if (sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr;
	allocExpress(&exp->ext,1);
	sugarFront(&exp->ext[0],nst);
	freeExpr(nst); pushExpr(exp,lst);
	return exp;
}
struct Express *sugarQuadary(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,2,str,idx);
	if (sizeExpr(nst) != 2) ERROR();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr;
	allocExpress(&exp->fld,2);
	for (int i = 0; i < 2; i++) {
	sugarFront(&exp->fld[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
	return exp;
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
struct Express *sugarTrifix(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr;
	allocExpress(&exp->ext,1);
	sugarBack(&exp->ext[0],lst);
	pushExpr(exp,lst);
	return exp;
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
	sugarFront(&exp->cnd[i],nst);
	sugarFront(&exp->lst[i],nst);}
	freeExpr(nst); pushExpr(exp,lst);
}
void sugarForeach(void *lst, enum Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	sugarRecurse(nst,-1,str,idx);
	struct Express *exp = 0; allocExpress(&exp,1);
	exp->opr = opr; exp->num = sizeExpr(nst);
	allocExpress(&exp->non,exp->num);
	for (int i = 0; i < exp->num; i++) {
	sugarFront(&exp->non[i],nst);}
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
void sugarZeroary(void *lst, enum Operate opr)
{
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr;
	pushExpr(exp,lst);
}
void sugarGetnum(void *lst, enum Operate opr, int val)
{
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr; exp->val = val;
	pushExpr(exp,lst);
}
int compSugar(const char *str, int idx)
{
	if (strncmp(str+idx,"Op",2)==0) return 1;
	if (strncmp(str+idx,"Bit",3)==0) return 1;
	if (strncmp(str+idx,"Cmp",3)==0) return 1;
	return 0;
}
void copySugar(char **idt, int lft, int rgt, const char *str)
{
	int len = rgt-lft;
	*idt = realloc(*idt,len+1);
	strncpy(*idt,str+lft,len);
	(*idt)[len] = 0;
}
void leftSugar(const char *str, int *idx)
{
	while (str[*idx] && isspace(str[*idx])) *idx += 1;
}
void rightSugar(const char *str, int *idx)
{
	while (str[*idx] && !isspace(str[*idx]) && !compSugar(str,*idx)) *idx += 1;
}
void hideSugar(char **idt, const char *str, int *idx) // find next alphanumeric
{
	leftSugar(str,idx);
	int lft = *idx;
	rightSugar(str,idx);
	int rgt = *idx;
	copySugar(idt,lft,rgt,str);
}
void backSugar(int *lft, int *rgt, const char *str, int idx)
{
	int sub = idx - 1;
	while (sub >= 0 && isspace(str[sub])) sub -= 1;
	*rgt = sub + 1;
	while (sub >= 0 && !isspace(str[sub])) sub -= 1;
	*lft = sub + 1;
}
void lastSugar(char **idt, const char *str, int idx) // find last alphanumeric
{
	int lft,rgt; backSugar(&lft,&rgt,str,idx);
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
	while (str[*idx] && !compSugar(str,*idx)) *idx += 1;
	int rgt = *idx;
	copySugar(raw,lft,rgt,str);
}
int identSugar(const char *str, int sub)
{
	while (str[sub] && isspace(str[sub])) sub += 1;
	while (str[sub] && !isspace(str[sub])) sub += 1;
	while (str[sub] && isspace(str[sub])) sub += 1;
	return sub;
}
// int sugarDebug = 0;
void sugarRecurse(void *lst, int lim, const char *str, int *idx)
{
	// sugarDebug += 1;
	int siz = sizeExpr(lst);
	int idt = *idx; // lookahead
	int sav = *idx; // fallbehind
	while (str[*idx]) {
	// fprintf(stderr,"Recurse ");
	// for (int i = 0; i < sugarDebug; i++) fprintf(stderr,"-");
	// fprintf(stderr,"- %s -- %d\n",str+*idx,*idx);
	if (idt <= *idx) idt = identSugar(str,*idx);
	if (strncmp(str+idt,":=",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		char *ptr = 0; int tmp = 0; enum Configure cfg;
		lastSugar(&ptr,str,idt);
		hideConfigure(&cfg,ptr,&tmp);
		free(ptr);
		*idx = idt+2;
		sugarSetcfg(lst,SetOp,cfg,str,idx);
		sav = *idx;
		continue;}
	if (str[idt] == '=' && str[idt+1] != '=') {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		char *ptr = 0; lastSugar(&ptr,str,idt);
		*idx = idt+1;
		sugarSetval(lst,SavOp,ptr,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Add",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBinary(lst,AddOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"+",1)==0) {
		*idx += 1;
		sugarInfix(lst,AddOp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Sub",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBinary(lst,SubOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"-",1)==0) {
		*idx += 1;
		sugarInfix(lst,SubOp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Mul",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBinary(lst,MulOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"*",1)==0) {
		*idx += 1;
		sugarInfix(lst,MulOp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Div",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBinary(lst,DivOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"/",1)==0) {
		*idx += 1;
		sugarInfix(lst,DivOp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Rem",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBinary(lst,RemOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"%",1)==0) {
		*idx += 1;
		sugarInfix(lst,RemOp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"And",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBitwise(lst,BitOp,AndBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"&",1)==0) {
		*idx += 1;
		sugarBitfix(lst,BitOp,AndBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Or",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarBitwise(lst,BitOp,OrBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"|",1)==0) {
		*idx += 1;
		sugarBitfix(lst,BitOp,OrBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Xor",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBitwise(lst,BitOp,XorBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"^",1)==0) {
		*idx += 1;
		sugarBitfix(lst,BitOp,XorBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Nand",4)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 4;
		sugarBitwise(lst,BitOp,NandBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"!&",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,NandBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Nor",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBitwise(lst,BitOp,NorBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"!|",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,NorBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Nxor",4)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 4;
		sugarBitwise(lst,BitOp,NxorBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"!^",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,NxorBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Shl",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBitwise(lst,BitOp,ShlBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"<<",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,ShlBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Fns",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBitwise(lst,BitOp,FnsBit,str,idx);
		skipSugar("Bit",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,">>",2)==0) {
		*idx += 2;
		sugarBitfix(lst,BitOp,FnsBit,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"LO",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarCompare(lst,CmpOp,LOCmp,str,idx);
		skipSugar("Cmp",str,idx);
		sav = *idx;
		continue;}
	if (str[*idx] == '<' && str[*idx+1] != '<' && str[*idx+1] != '=') {
		*idx += 1;
		sugarCmpfix(lst,CmpOp,LOCmp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"LC",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarCompare(lst,CmpOp,LCCmp,str,idx);
		skipSugar("Cmp",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"<=",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,LCCmp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"EO",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarCompare(lst,CmpOp,EOCmp,str,idx);
		skipSugar("Cmp",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"!=",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,EOCmp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"EC",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarCompare(lst,CmpOp,ECCmp,str,idx);
		skipSugar("Cmp",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"==",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,ECCmp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"MO",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarCompare(lst,CmpOp,MOCmp,str,idx);
		skipSugar("Cmp",str,idx);
		sav = *idx;
		continue;}
	if (str[*idx] == '>' && str[*idx+1] != '>' && str[*idx+1] != '=') {
		*idx += 1;
		sugarCmpfix(lst,CmpOp,MOCmp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"MC",2)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 2;
		sugarCompare(lst,CmpOp,MCCmp,str,idx);
		skipSugar("Cmp",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,">=",2)==0) {
		*idx += 2;
		sugarCmpfix(lst,CmpOp,MCCmp,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Cnd",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarCondit(lst,CndOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Non",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarForeach(lst,NonOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Ret",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarGetcfg(lst,RetOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"?",1)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 1;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarGetcfg(lst,RetOp,cfg,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Top",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarGetcfg(lst,TopOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Set",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,SetOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Wos",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,WosOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Woc",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,WocOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Raw",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		enum Configure cfg; hideConfigure(&cfg,str,idx);
		sugarSetcfg(lst,RawOp,cfg,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Val",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,ValOp,idt,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"@",1)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 1;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,ValOp,idt,str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Sav",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarSetval(lst,SavOp,idt,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Rex",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,RexOp,idt,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Irx",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		char *idt = 0; hideSugar(&idt,str,idx);
		sugarGetval(lst,IrxOp,idt,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Get",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarZeroary(lst,GetOp);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Put",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarForeach(lst,PutOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Fld",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		struct Express *exp = sugarQuadary(lst,FldOp,str,idx);
		char *idt = 0; hideSugar(&idt,str,idx);
		int sub; scanSugar(&sub,str,idx);
		exp->fid = idt; exp->fub = sub;
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Ext",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		struct Express *exp = sugarTrinary(lst,ExtOp,str,idx);
		char *idt = 0; hideSugar(&idt,str,idx);
		int sub; scanSugar(&sub,str,idx);
		exp->eid = idt; exp->eub = sub;
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,".",1)==0) {
		*idx += 1;
		struct Express *exp = sugarTrifix(lst,ExtOp,str,idx);
		char *idt = 0; hideSugar(&idt,str,idx);
		int sub; scanSugar(&sub,str,idx);
		exp->eid = idt; exp->eub = sub;
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Tim",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarZeroary(lst,TimOp);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Cst",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarBinary(lst,CstOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Imm",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		sugarUnary(lst,ImmOp,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Int",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		int num; scanSugar(&num,str,idx);
		sugarGetnum(lst,IntOp,num);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"#",1)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 1;
		int num; scanSugar(&num,str,idx);
		sugarGetnum(lst,IntOp,num);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Str",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		char *raw = 0; moreSugar(&raw,str,idx);
		sugarGetval(lst,StrOp,raw,str,idx);
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"End",3)==0) {
		if (lim >= 0 && sizeExpr(lst)-siz >= lim) break;
		*idx += 3;
		char *raw = malloc(2);
		raw[0] = '\n'; raw[1] = 0;
		sugarGetval(lst,StrOp,raw,str,idx);		
		skipSugar("Op",str,idx);
		sav = *idx;
		continue;}
	if (strncmp(str+*idx,"Op",2)==0) break;
	if (strncmp(str+*idx,"Bit",3)==0) break;
	if (strncmp(str+*idx,"Cmp",3)==0) break;
	*idx += 1;}
	*idx = sav;
	// sugarDebug -= 1;
}

int sugarSkip(const char **str)
{
	char *bas = strchr(*str,'(');
	char *lim = strchr(*str,')');
	if (!lim) ERROR();
	if (bas && bas < lim) {*str = bas+1; return 1;}
	if (!bas || lim < bas) {*str = lim+1; return -1;}
	return 0;
}
void sugarNest(int *dim, char ***keep, char ***expr, const char *str, char chr)
{
	char open[3] = {chr,'(',0}; *dim = 0;
	for (const char *lim = str;
	strstr(lim,open) && *(lim = strstr(lim,open)) && *(lim += 2);
	*dim += 1) {
	for (int nst = 1; nst; nst += sugarSkip(&lim)) {}}
	*keep = malloc((*dim+1)*sizeof(char*));
	*expr = malloc((*dim)*sizeof(char*));
	const char *lst = str; *dim = 0;
	for (const char *lim = str;
	strstr(lim,open) && *(lim = strstr(lim,open)) && *(lim += 2);
	*dim += 1, lst = lim) {
	const char *exp = lim;
	int len = lim-lst-2; (*keep)[*dim] = malloc(len+1);
	strncpy((*keep)[*dim],lst,len); (*keep)[*dim][len] = 0;
	for (int nst = 1; nst; nst += sugarSkip(&lim)) {}
	int lth = lim-exp-1; (*expr)[*dim] = malloc(lth+1);
	strncpy((*expr)[*dim],exp,lth); (*expr)[*dim][lth] = 0;}
	int fin = strlen(lst); (*keep)[*dim] = malloc(fin+1);
	strncpy((*keep)[*dim],lst,fin); (*keep)[*dim][fin] = 0;
}
void sugarRepl(char **ptr, char chr)
{
	if (!*ptr) return;
	int dim = 0; char **keep = 0; char **expr = 0;
	sugarNest(&dim,&keep,&expr,*ptr,chr);

	char **repl = malloc(dim*sizeof(char*)); int tot = 0;
	for (int i = 0; i < dim; i++) {
	repl[i] = 0; sugarShow(&repl[i],expr[i]);
	free(expr[i]); expr[i] = 0; tot += strlen(keep[i]) + strlen(repl[i]);}
	tot += strlen(keep[dim]);

	free(*ptr); *ptr = malloc(tot+1); tot = 0;
	for (int i = 0; i < dim; i++) {
	strcpy(&(*ptr)[tot],keep[i]); tot += strlen(keep[i]);
	strcpy(&(*ptr)[tot],repl[i]); tot += strlen(repl[i]);
	free(keep[i]); keep[i] = 0; free(repl[i]); repl[i] = 0;}
	strcpy(&(*ptr)[tot],keep[dim]); tot += strlen(keep[dim]);
	free(keep[dim]); keep[dim] = 0; (*ptr)[tot] = 0;

	free(expr); free(keep); free(repl);
}
void sugarEval(sftype exe, const char *str, char chr)
{
	if (!exe) return;
	int dim = 0; char **keep = 0; char **expr = 0;
	sugarNest(&dim,&keep,&expr,str,chr);
	for (int i = 0; i < dim; i++) {
	exe(expr[i]);
	free(expr[i]); expr[i] = 0;
	free(keep[i]); keep[i] = 0;}
	free(keep[dim]); keep[dim] = 0;
	free(expr); free(keep);
}
void sugarFilt(char **ptr, char chr)
{
	if (!*ptr) return;
	int dim = 0; char **keep = 0; char **expr = 0;
	sugarNest(&dim,&keep,&expr,*ptr,chr);
	int tot = 0;
	for (int i = 0; i < dim; i++) {
	free(expr[i]); expr[i] = 0; tot += strlen(keep[i]);}
	tot += strlen(keep[dim]);
	free(*ptr); *ptr = malloc(tot+1); tot = 0;
	for (int i = 0; i < dim; i++) {
	strcpy(&(*ptr)[tot],keep[i]); tot += strlen(keep[i]);
	free(keep[i]); keep[i] = 0;}
	strcpy(&(*ptr)[tot],keep[dim]); tot += strlen(keep[dim]);
	free(expr); free(keep);
}
