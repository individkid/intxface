#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <libgen.h>
#include <stdarg.h>

char *exestr = 0;
char *exetmp = 0;
char *msgstr = 0;
char *msgtmp = 0;
struct Closure argbuf = {0};

struct Function protoTypeCf(cftype fnc)
{
	struct Function ret = {.ft = Cftype, {.cf = fnc}}; return ret;
}
struct Function protoTypeCg(cgtype fnc)
{
	struct Function ret = {.ft = Cgtype, {.cg = fnc}}; return ret;
}
struct Function protoTypeCh(cgtype fnc)
{
	struct Function ret = {.ft = Chtype, {.ch = fnc}}; return ret;
}
struct Function protoTypeHg(hgtype fnc)
{
	struct Function ret = {.ft = Hgtype, {.hg = fnc}}; return ret;
}
struct Function protoTypeFf(fftype fnc)
{
	struct Function ret = {.ft = Fftype, {.ff = fnc}}; return ret;
}
struct Function protoTypeGf(gftype fnc)
{
	struct Function ret = {.ft = Gftype, {.gf = fnc}}; return ret;
}
struct Function protoTypeOf(oftype fnc)
{
	struct Function ret = {.ft = Oftype, {.of = fnc}}; return ret;
}
struct Function protoTypeRf(rftype fnc)
{
	struct Function ret = {.ft = Rftype, {.rf = fnc}}; return ret;
}
struct Function protoTypeRg(rgtype fnc)
{
	struct Function ret = {.ft = Rgtype, {.rg = fnc}}; return ret;
}
struct Function protoTypeRh(rhtype fnc)
{
	struct Function ret = {.ft = Rhtype, {.rh = fnc}}; return ret;
}
struct Function protoTypeAf(aftype fnc)
{
	struct Function ret = {.ft = Aftype, {.af = fnc}}; return ret;
}
struct Function protoTypeBf(bftype fnc)
{
	struct Function ret = {.ft = Bftype, {.bf = fnc}}; return ret;
}
struct Function protoTypeBg(bgtype fnc)
{
	struct Function ret = {.ft = Bgtype, {.bg = fnc}}; return ret;
}
struct Function protoTypeNf(nftype fnc)
{
	struct Function ret = {.ft = Nftype, {.nf = fnc}}; return ret;
}
struct Function protoTypeMf(mftype fnc)
{
	struct Function ret = {.ft = Mftype, {.mf = fnc}}; return ret;
}
struct Function protoTypeDf(dftype fnc)
{
	struct Function ret = {.ft = Dftype, {.df = fnc}}; return ret;
}
struct Function protoTypeIf(iftype fnc)
{
	struct Function ret = {.ft = Iftype, {.it = fnc}}; return ret;
}
struct Function protoTypeIg(igtype fnc)
{
	struct Function ret = {.ft = Igtype, {.ig = fnc}}; return ret;
}
struct Function protoTypeJf(jftype fnc)
{
	struct Function ret = {.ft = Jftype, {.jf = fnc}}; return ret;
}
struct Function protoTypeKf(kftype fnc)
{
	struct Function ret = {.ft = Kftype, {.kf = fnc}}; return ret;
}
struct Function protoTypeTf(tftype fnc)
{
	struct Function ret = {.ft = Tftype, {.tf = fnc}}; return ret;
}
struct Function protoTypeLf(lftype fnc)
{
	struct Function ret = {.ft = Lftype, {.lf = fnc}}; return ret;
}

void protoMake(struct Argument *arg)
{
	switch (arg->at) {
	case (Iatype): arg->ia = 0; break;
	case (Satype): free (arg->sa); arg->sa = 0; break;
	case (Latype): free (arg->pa); arg->pa = 0; arg->la = 0; break;
	case (Patype): arg->pa = 0; break;
	default: break;}
}
void protoMakeIf(struct Argument *arg, int val)
{
	protoMake(arg);
	arg->at = Iatype;
	arg->ia = val;
}
void protoMakeSf(struct Argument *arg, const char *val)
{
	protoMake(arg);
	arg->at = Satype;
	arg->sa = malloc(strlen(val)+1);
	strcpy(arg->sa,val);
}
void protoMakeLf(struct Argument *arg, const void *val, int len)
{
	protoMake(arg);
	arg->at = Latype;
	arg->sa = malloc(len);
	memcpy(arg->sa,val,len);
	arg->la = len;
}
void protoMakePf(struct Argument *arg, void *val)
{
	protoMake(arg);
	arg->at = Patype;
	arg->pa = val;
}

const struct Closure *protoClose(int na, int nb)
{
	struct Argument zero = {0};
	for (int i = 0; i < argbuf.na; i++) protoMake(argbuf.aa+i);
	free(argbuf.aa); argbuf.aa = malloc(sizeof(argbuf.aa)*na); argbuf.na = na;
	for (int i = 0; i < argbuf.na; i++) argbuf.aa[i] = zero;
	for (int i = 0; i < argbuf.nb; i++) protoMake(argbuf.ab+i);
	free(argbuf.ab); argbuf.ab = malloc(sizeof(argbuf.ab)*nb); argbuf.nb = nb;
	for (int i = 0; i < argbuf.nb; i++) argbuf.ab[i] = zero;
	return &argbuf;
}
const struct Closure *protoClosePf(int idx, int nbyte)
{
	protoClose(2,2);
	protoMakeIf(&argbuf.aa[0],idx);
	protoMakeIf(&argbuf.aa[1],nbyte);
	return &argbuf;
}
const struct Closure *protoCloseQf(int idx, const void *buf, int nbyte)
{
	protoClose(3,1);
	protoMakeIf(&argbuf.aa[0],idx);
	protoMakeLf(&argbuf.aa[1],buf,nbyte);
	protoMakeIf(&argbuf.aa[2],nbyte);
	return &argbuf;
}
const struct Closure *protoCloseRf(int arg)
{
	protoClose(1,1);
	protoMakeIf(&argbuf.aa[0],arg);
	return &argbuf;
}
const struct Closure *protoCloseRg()
{
	protoClose(0,1);
	return &argbuf;
}
const struct Closure *protoCloseBf(const char *arg)
{
	protoClose(1,1);
	protoMakeSf(&argbuf.aa[0],arg);
	return &argbuf;
}
const struct Closure *protoCloseBg()
{
	protoClose(0,1);
	return &argbuf;
}
const struct Closure *protoCloseBh()
{
	protoClose(0,1);
	return &argbuf;
}
int protoResultPf(void *buf)
{
	memcpy(buf,argbuf.ab[1].pa,argbuf.ab[1].la);
	return argbuf.ab[0].ia;
}
int protoResultQf()
{
	return argbuf.ab[0].ia;
}
int protoResultRf()
{
	return argbuf.ab[0].ia;
}
int protoResultRg()
{
	return argbuf.ab[0].ia;
}
const char *protoResultBf()
{
	return argbuf.ab[0].sa;
}
const char *protoResultBg()
{
	return argbuf.ab[0].sa;
}
const char *protoResultBh(int *len)
{
	*len = argbuf.ab[0].la;
	return argbuf.ab[0].sa;
}

void exitErr(const char *str, int num, int idx)
{
	fprintf(stderr,"exitErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid()); exit(-1);
}
void protoSet(const char *str)
{
	int len = 0;
	if (exestr) {
		exestr = realloc(exestr,strlen(exestr)+1+strlen(str)+1);
		strcat(exestr,":");
		strcat(exestr,str);}
	else {
		exestr = malloc(strlen(str)+1);
		strcpy(exestr,str);}
	exetmp = realloc(exetmp,len = strlen(exestr));
	strcpy(exetmp,exestr);
	for (int i = 0; i < len; i++) if (exestr[i] == ':') exetmp[i] = 0;
}
const char *protoGet(int i)
{
	int len = 0;
	int j = 0;
	if (i == 0) return "";
	if (exestr == 0) return 0;
	len = strlen(exestr);
	for (j = 0, i--; j < len && i > 0; j++) if (exestr[j] == ':') i--;
	return (i == 0 ? dirname(exetmp+j) : 0);
}
void protoErr(const char *fmt, ...)
{
	char *temp = 0;
	va_list args = {0};
	va_start(args,fmt);
	vasprintf(&temp,fmt,args);
	va_end(args);
	if (msgstr) {
		msgstr = realloc(msgstr,strlen(msgstr)+strlen(temp)+1);
		strcat(msgstr,temp);}
	else {
		msgstr = malloc(strlen(temp)+1);
		strcpy(msgstr,temp);}
	free(temp);
}
const char *protoMsg()
{
	if (msgtmp) free(msgtmp);
	msgtmp = msgstr;
	msgstr = 0;
	return msgtmp;
}
int protoPath(const char *exp)
{
	const char *str = 0;
	char *temp = 0;
	int val = 0;
	for (int i = 0; (str = protoGet(i)); i++) {
	asprintf(&temp,"%s%s",str,exp);
	val = access(temp,R_OK);
	free(temp);
	if (val == 0) return i;}
	return -1;
}
