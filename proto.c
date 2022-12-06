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

struct Function protoTypeF(fftype fnc)
{
	struct Function ret = {.ft = Fftype, {.ff = fnc}}; return ret;
}
struct Function protoTypeG(gftype fnc)
{
	struct Function ret = {.ft = Gftype, {.gf = fnc}}; return ret;
}
struct Function protoTypeO(oftype fnc)
{
	struct Function ret = {.ft = Oftype, {.of = fnc}}; return ret;
}
struct Function protoTypeN(nftype fnc)
{
	struct Function ret = {.ft = Nftype, {.nf = fnc}}; return ret;
}
struct Function protoTypeM(mftype fnc)
{
	struct Function ret = {.ft = Mftype, {.mf = fnc}}; return ret;
}
struct Function protoTypeD(dftype fnc)
{
	struct Function ret = {.ft = Dftype, {.df = fnc}}; return ret;
}
struct Function protoTypeA(aftype fnc)
{
	struct Function ret = {.ft = Aftype, {.af = fnc}}; return ret;
}
struct Function protoTypeI(iftype fnc)
{
	struct Function ret = {.ft = Iftype, {.it = fnc}}; return ret;
}
struct Function protoTypeJ(jftype fnc)
{
	struct Function ret = {.ft = Jftype, {.jf = fnc}}; return ret;
}
struct Function protoTypeK(kftype fnc)
{
	struct Function ret = {.ft = Kftype, {.kf = fnc}}; return ret;
}
struct Function protoTypeT(tftype fnc)
{
	struct Function ret = {.ft = Tftype, {.tf = fnc}}; return ret;
}
struct Function protoTypeL(lftype fnc)
{
	struct Function ret = {.ft = Lftype, {.lf = fnc}}; return ret;
}

struct Closure protoCloseB(const char *arg)
{
	struct Closure ret = {0};
	return ret; // TODO
}
struct Closure protoCloseR(int arg)
{
	struct Closure ret = {0};
	return ret; // TODO
}
struct Closure protoCloseP(int idx, int nbyte)
{
	struct Closure ret = {0};
	return ret; // TODO
}
struct Closure protoCloseQ(int idx, const char *buf, int nbyte)
{
	struct Closure ret = {0};
	return ret; // TODO
}
void protoResultB(char *val)
{
	// TODO
}
void protoResultR(int *val)
{
	// TODO
}
int protoResultP(char *buf)
{
	return 0; // TODO
}
int protoResultQ()
{
	return 0; // TODO
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
