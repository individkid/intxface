#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <libgen.h>
#include <stdarg.h>
#include <execinfo.h>

char *exestr = 0;
char *exetmp = 0;
char *msgstr = 0;
char *msgtmp = 0;
struct Closure argbuf = {0};

#define PROTOTYPE(UC,LC)\
struct Function protoType##UC(LC##type fnc)\
{\
	struct Function ret = {.ft = UC##type, {.LC = fnc}}; return ret;\
}

PROTOTYPE(Cf,cf)
PROTOTYPE(Cg,cg)
PROTOTYPE(Ch,ch)
PROTOTYPE(Df,df)
PROTOTYPE(Dg,dg)
PROTOTYPE(Dh,dh)
PROTOTYPE(Ef,ef)
PROTOTYPE(Eg,eg)
PROTOTYPE(Eh,eh)
PROTOTYPE(Pf,pf)
PROTOTYPE(Qf,qf)
PROTOTYPE(Ff,ff)
PROTOTYPE(Fg,fg)
PROTOTYPE(Fh,fh)
PROTOTYPE(Gf,gf)
PROTOTYPE(Gg,gg)
PROTOTYPE(Of,of)
/*
PROTOTYPE(Af,af)
PROTOTYPE(Bf,bf)
PROTOTYPE(If,if)
PROTOTYPE(Jf,jf)
PROTOTYPE(Kf,kf)
*/

PROTOTYPE(Tf,tf)
PROTOTYPE(Tg,tg)
PROTOTYPE(Th,th)
PROTOTYPE(Ti,ti)
PROTOTYPE(Tj,tj)
PROTOTYPE(Tk,tk)
PROTOTYPE(Tl,tl)
PROTOTYPE(Tm,tm)
PROTOTYPE(Tn,tn)

PROTOTYPE(Sf,sf)
PROTOTYPE(Sg,sg)
PROTOTYPE(Sh,sh)
PROTOTYPE(Si,si)
PROTOTYPE(Sj,sj)
PROTOTYPE(Sk,sk)
PROTOTYPE(Sl,sl)
PROTOTYPE(Sm,sm)
PROTOTYPE(Sn,sn)

PROTOTYPE(Lf,lf)
PROTOTYPE(Lg,lg)
PROTOTYPE(Lh,lh)
PROTOTYPE(Li,li)
PROTOTYPE(Lj,lj)
PROTOTYPE(Lk,lk)
PROTOTYPE(Ll,ll)
PROTOTYPE(Lm,lm)
PROTOTYPE(Ln,ln)

PROTOTYPE(Mf,mf)
PROTOTYPE(Mh,mh)
PROTOTYPE(Mi,mi)
PROTOTYPE(Mj,mj)
PROTOTYPE(Mk,mk)
PROTOTYPE(Ml,ml)
PROTOTYPE(Mm,mm)
PROTOTYPE(Mn,mn)
PROTOTYPE(Mo,mo)
PROTOTYPE(Mp,mp)
PROTOTYPE(Mq,mq)

PROTOTYPE(Nf,nf)
PROTOTYPE(Nh,nh)
PROTOTYPE(Ni,ni)
PROTOTYPE(Nj,nj)
PROTOTYPE(Nk,nk)
PROTOTYPE(Nl,nl)
PROTOTYPE(Nm,nm)
PROTOTYPE(Nn,nn)
PROTOTYPE(No,no)
PROTOTYPE(Np,np)
PROTOTYPE(Nq,nq)

PROTOTYPE(Rf,rf)
PROTOTYPE(Rg,rg)
PROTOTYPE(Rh,rh)
PROTOTYPE(Ri,ri)
PROTOTYPE(Rj,rj)
PROTOTYPE(Rk,rk)
PROTOTYPE(Rl,rl)
PROTOTYPE(Rm,rm)
PROTOTYPE(Rn,rn)

void protoMake(struct Parameter *arg)
{
	switch (arg->at) {
	case (Iatype): arg->ia = 0; break;
	case (Satype): free (arg->sa); arg->sa = 0; break;
	case (Latype): free (arg->pa); arg->pa = 0; arg->la = 0; break;
	case (Patype): arg->pa = 0; break;
	default: break;}
}
void protoMakeIf(struct Parameter *arg, int val)
{
	protoMake(arg);
	arg->at = Iatype;
	arg->ia = val;
}
void protoMakeSf(struct Parameter *arg, const char *val)
{
	protoMake(arg);
	arg->at = Satype;
	arg->sa = malloc(strlen(val)+1);
	strcpy(arg->sa,val);
}
void protoMakeLf(struct Parameter *arg, const void *val, int len)
{
	protoMake(arg);
	arg->at = Latype;
	arg->sa = malloc(len);
	memcpy(arg->sa,val,len);
	arg->la = len;
}
void protoMakePf(struct Parameter *arg, void *val)
{
	protoMake(arg);
	arg->at = Patype;
	arg->pa = val;
}

const struct Closure *protoClose(int na, int nb)
{
	struct Parameter zero = {0};
	for (int i = 0; i < argbuf.na; i++) protoMake(argbuf.aa+i);
	free(argbuf.aa); argbuf.aa = malloc(sizeof(struct Parameter)*na); argbuf.na = na;
	for (int i = 0; i < argbuf.na; i++) argbuf.aa[i] = zero;
	for (int i = 0; i < argbuf.nb; i++) protoMake(argbuf.ab+i);
	free(argbuf.ab); argbuf.ab = malloc(sizeof(struct Parameter)*nb); argbuf.nb = nb;
	for (int i = 0; i < argbuf.nb; i++) argbuf.ab[i] = zero;
	return &argbuf;
}
const struct Closure *protoCloseCf(int idx)
{
	protoClose(1,0);
	protoMakeIf(&argbuf.aa[0],idx);
	return &argbuf;
}
const struct Closure *protoCloseEf(const char *str, int num, int idx)
{
	protoClose(3,0);
	protoMakeSf(&argbuf.aa[0],str);
	protoMakeIf(&argbuf.aa[1],num);
	protoMakeIf(&argbuf.aa[2],idx);
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
const struct Closure *protoCloseGf(const char *one, const char *oth)
{
	protoClose(2,1);
	protoMakeSf(&argbuf.aa[0],one);
	protoMakeSf(&argbuf.aa[1],oth);
	return &argbuf;
}
const struct Closure *protoCloseGg(int rfd, int wfd)
{
	protoClose(2,1);
	protoMakeIf(&argbuf.aa[0],rfd);
	protoMakeIf(&argbuf.aa[1],wfd);
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
void protoResultCf()
{
}
void protoResultEf()
{
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
int protoResultGf()
{
	return argbuf.ab[0].ia;
}
int protoResultGg()
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

void stackErr()
{
	void *array[10];
	size_t size;
	// get void*'s for all entries on the stack
	size = backtrace(array, 10);
	// print out all the frames to stderr
	backtrace_symbols_fd(array, size, STDERR_FILENO);
}
void exitErr(const char *file, int line)
{
	stackErr();
	fprintf(stderr,"exitErr %s(%d): %d %lld\n",file,line,errno,(long long)getpid());
	exit(-1);
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
