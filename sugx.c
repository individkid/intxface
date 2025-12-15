#include "sugr.h"
#include "stlx.h"

DECLARE_DEQUE(struct Express *,Express)
enum Closure {
	OpClo,
	BitClo,
	CmpClo,
	LimClo,
}
enum Closure sugarRecurse(void *lst, int lim, const char *str, int *idx);
void sugarBinary(void *lst, Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr;
	struct Express *opa = exp;
	while (sizeExpr(nst) > 1) {
	opa->opa[0] = *frontExpr(nst); dropExpr(nst);
	opa = &opa->opa[1]; *opa = 0;
	allocExpress(&opa,1); opa->opr = opr;}
	opa->opa[1] = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarBitwise(void *lst, Bitwise bit, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = BitOp; exp->bit = bit;
	struct Express *opb = exp;
	while (sizeExpr(nst) > 1) {
	opb->opb[0] = *frontExpr(nst); dropExpr(nst);
	opb = &opb->opb[1]; *opb = 0;
	allocExpress(&opb,1); opb->opr = BitOp; opb->bit = bit;}
	opb->opb[1] = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarCompare(void *lst, Compare cmp, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,-1,str,idx);
	if (sizeExpr(nst) < 2) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = CmpOp; exp->cmp = cmp;
	struct Express *opc = exp;
	while (sizeExpr(nst) > 1) {
	opc->opc[0] = *frontExpr(nst); dropExpr(nst);
	opc = &opc->opc[1]; *opc = 0;
	allocExpress(&opc,1); opc->opr = BitOp; opc->cmp = cmp;}
	opc->opc[1] = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarInfix(void *lst, Operate opr, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr;
	exp->opa[0] = *backExpr(lst); popExpr(lst);
	exp->opa[1] = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarBitfix(void *lst, Bitwise bit, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = BitOp; exp->bit = bit;
	exp->opb[0] = *backExpr(lst); popExpr(lst);
	exp->opb[1] = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarCmpfix(void *lst, Compare cmp, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (sizeExpr(lst) < 1 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = CmpOp; exp->cmp = cmp;
	exp->opc[0] = *backExpr(lst); popExpr(lst);
	exp->opc[1] = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarAssign(void *lst, Operate opr, const char *key, int len, const char *str, int *idx)
{
	void *nst = allocExpr();
	enum Closure clo = sugarRecurse(nst,1,str,idx);
	if (len == 0 || sizeExpr(nst) != 1) ERROR();
	struct Express *exp = 0;
	allocExpress(&exp,1); exp->opr = opr;
	exp->kys = malloc(len+1);
	strncpy(exp->kys,str+idl,len); exp->kys[len] = 0; idl = idr;
	exp->sav = *frontExpr(nst); dropExpr(nst);
	freeExpr(nst); pushExpr(sizeof(struct Express*),exp,lst);
	return clo;
}
void sugarConsume(char **ptr, int *num, Parse par, const char *str)
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
/*Operate = { -- little language expressions
	"CndOp", -- touring machine
	"FesOp", -- for each set
	"RetOp", -- config from callback
	"SetOp", -- callback with config
	"WosOp", -- callback setbits config
	"WocOp", -- callback clearbits config
	"RawOp", -- callback read and clear config
	"ValOp", -- value from lookup
	"SavOp", -- value for lookup
	"RexOp", -- regex sugar
	"IrxOp", -- irrex sugar
	"GetOp", -- string from callback
	"PutOp", -- callback with string
	"FldOp", -- fields to struct
	"ExtOp", -- fields from struct
	"ImmOp", -- built in value
	"IntOp", -- ImmOp sugar
	"StrOp", -- ImmOp sugar
}
Compare = { -- little language comparisons
	-- Less Equal More
	-- Open Closed
	"LOCmp",
	"LCCmp",
	"EOCmp",
	"ECCmp",
	"MOCmp",
	"MCCmp",
	"ReCmp", -- regular expression
	"IrCmp", -- irregular expression
}*/
enum Closure sugarRecurse(void *lst, int lim, const char *str, int *idx)
{
	// if (lim<0) go until Op; else don't expect Op
	Parse par = IdBefPar;
	char **ptr = 0;
	int *num = 0;
	int lft = *idx;
	int rgt = *idx;
	while (lim) {
	if (strncmp(str+*idx,"Op",2)==0) {*idx += 2;
		switch (par) {default:
		break; case (OpIntPar):
		sscanf(str+lft," %d",num);
		break; case (OpStrPar):
		*str = malloc(*idx-2-lft+1);
		strncpy(*ptr,str+lft,*idx-2-lft);
		(*str)[*idx-2-lft] = 0;}
		par = IdBefPar;
		return OpClo;}
	if (strncmp(str+*idx,"Bit",3)==0) {*idx += 3;
		return BitClo;}
	if (strncmp(str+*idx,"Cmp",3)==0) {*idx += 3;
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
		if (sugarBitfix(lst,AndBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Or",2)==0) {*idx += 2;
		if (sugarBitwise(lst,OrBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"|",1)==0) {*idx += 1;
		if (sugarBitfix(lst,OrBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Xor",3)==0) {*idx += 3;
		if (sugarBitwise(lst,XorBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"^",1)==0) {*idx += 1;
		if (sugarBitfix(lst,XorBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Nand",4)==0) {*idx += 4;
		if (sugarBitwise(lst,NandBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!&",2)==0) {*idx += 2;
		if (sugarBitfix(lst,NandBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Nor",3)==0) {*idx += 3;
		if (sugarBitwise(lst,NorBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!|",2)==0) {*idx += 2;
		if (sugarBitfix(lst,NorBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Nxor",4)==0) {*idx += 4;
		if (sugarBitwise(lst,NxorBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!^",2)==0) {*idx += 2;
		if (sugarBitfix(lst,NxorBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Shl",3)==0) {*idx += 3;
		if (sugarBitwise(lst,ShlBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"<<",2)==0) {*idx += 2;
		if (sugarBitfix(lst,ShlBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"Fns",3)==0) {*idx += 3;
		if (sugarBitwise(lst,FnsBit,str,idx) != BitClo) ERROR();
		continue;}
	if (strncmp(str+*idx,">>",2)==0) {*idx += 2;
		if (sugarBitfix(lst,FnsBit,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"LO",2)==0) {*idx += 2;
		if (sugarCompare(lst,LOCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (str[*idx] == '<' && str[*idx+1] != '<' && str[*idx+1] != '=') {*idx += 1;
		if (sugarCmpfix(lst,LOCmp,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"LC",2)==0) {*idx += 2;
		if (sugarCompare(lst,LCCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"<=",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,LCCmp,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"EO",2)==0) {*idx += 2;
		if (sugarCompare(lst,EOCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"!=",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,EOCmp,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"EC",2)==0) {*idx += 2;
		if (sugarCompare(lst,ECCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"==",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,ECCmp,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"MO",2)==0) {*idx += 2;
		if (sugarCompare(lst,MOCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (str[*idx] == '>' && str[*idx+1] != '>' && str[*idx+1] != '=') {*idx += 1;
		if (sugarCmpfix(lst,MOCmp,str,str,idx) != LimClo) ERROR();
		continue;}
	if (strncmp(str+*idx,"MC",2)==0) {*idx += 2;
		if (sugarCompare(lst,MCCmp,str,idx) != CmpClo) ERROR();
		continue;}
	if (strncmp(str+*idx,">=",2)==0) {*idx += 2;
		if (sugarCmpfix(lst,MCCmp,str,str,idx) != LimClo) ERROR();
		continue;}
	// TODO CndOp
	// TODO FesOp
	// TODO RetOp
	// TODO SetOp
	// TODO WosOp
	// TODO WocOp
	// TODO RawOp
	// TODO ValOp
	// TODO SavOp
	// TODO ValOp
	if (strncmp(str+*idx,"$",1)==0) {*idx += 1;
		struct Express *exp = 0; allocExpress(&exp,1);
		ptr = &exp->key; exp->opr = ValOp; par = IbStrPar;
		continue;}
	// TODO SavOp
	if (str[*idx] == '=' && str[*idx+1] != '=') {*idx += 1;
		if (par != IdMidPar && par != IdAftPar) ERROR();
		int len = rgt-lft; const char *key = str+lft;
		sugarAssign(lst,SavOp,key,len,str,idx);
		continue;}
	// TODO RexOp
	// TODO IrxOp
	// TODO GetOp
	// TODO PutOp
	// TODO FldOp
	// TODO ExtOp
	// TODO ImmOp
	// TODO IntOp
	if (strncmp(str+*idx,"#",1)==0) {*idx += 1;
		struct Express *exp = 0; allocExpress(&exp,1);
		num = &exp->ivl; exp->opr = IntOp; par = IbIntPar;
		continue;}
	// TODO StrOp
	if (isspace(str[*idx])) {*idx += 1;
		switch (par) {default:
		break; case (IdMidPar): rgt = *idx; par = IdAftPar;
		break; case (ImIntPar): par = IdBefPar;
		sscanf(str+lft," %d",num);
		break; case (ImStrPar): par = IdBefPar;
		*str = malloc(*idx-2-lft+1);
		strncpy(*ptr,str+lft,*idx-2-lft);
		(*str)[*idx-2-lft] = 0;}
		continue;}
	switch (par) {default:
	break; case (IdBefPar): lft = *idx; par = IdMidPar;
	break; case (IbIntPar): lft = *idx; par = ImIntPar;
	break; case (IbStrPar): lft = *idx; par = ImStrPar;}
	*idx += 1;}
	return LimClo; // TODO decrement lim above
}
char *sugarExpand(const char *str)
{
	// TODO
}