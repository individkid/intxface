#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>

const char *exestr = 0;

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
	exestr = str;
}
const char *getExestr()
{
	return exestr;
}
