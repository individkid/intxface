#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <libgen.h>

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
void setExestr(const char *str)
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
const char *getExedir(int i)
{
	int len = strlen(exestr);
	int j = 0;
	if (i == 0) return "";
	for (j = 0, i--; j < len && i > 0; j++) if (exestr[j] == ':') i--;
	return (i == 0 ? dirname(tmpstr+j) : 0);
}
