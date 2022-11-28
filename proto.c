#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <libgen.h>
#include <stdarg.h>

char *exestr = 0;
char *tmpstr = 0;

struct Prototype protoTypeF(fftype fnc)
{
	struct Prototype ret = {.ft = Fftype, {.ff = fnc}}; return ret;
}
struct Prototype protoTypeG(gftype fnc)
{
	struct Prototype ret = {.ft = Gftype, {.gf = fnc}}; return ret;
}
struct Prototype protoTypeO(oftype fnc)
{
	struct Prototype ret = {.ft = Oftype, {.of = fnc}}; return ret;
}
struct Prototype protoTypeN(nftype fnc)
{
	struct Prototype ret = {.ft = Nftype, {.nf = fnc}}; return ret;
}
struct Prototype protoTypeM(mftype fnc)
{
	struct Prototype ret = {.ft = Mftype, {.mf = fnc}}; return ret;
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
	tmpstr = realloc(tmpstr,len = strlen(exestr));
	strcpy(tmpstr,exestr);
	for (int i = 0; i < len; i++) if (exestr[i] == ':') tmpstr[i] = 0;
}
const char *protoGet(int i)
{
	int len = strlen(exestr);
	int j = 0;
	if (i == 0) return "";
	for (j = 0, i--; j < len && i > 0; j++) if (exestr[j] == ':') i--;
	return (i == 0 ? dirname(tmpstr+j) : 0);
}
int protoForm(fftype fnc, const char *fmt, ...)
{
	char *temp = 0;
	va_list args = {0};
	int val = 0;
	va_start(args,fmt);
	asprintf(&temp,fmt,args);
	va_end(args);
	val = fnc(temp);
	free(temp);
	return val;
}
int protoPathF(const char *exp)
{
	return access(exp,R_OK);
}
int protoPath(const char *exp)
{
	const char *str = 0;
	for (int i = 0; (str = protoGet(i)); i++)
	if (protoForm(protoPathF,"%s%s",str,exp) == 0) return i;
	return -1;
}
