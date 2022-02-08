#include "util.h"
#include "face.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>

int uargc = 0; // line
int ulstc = 0; // pass
int uglbc = 0; // global
int *uovrc = 0; // overload
int *ubndc = 0; // binding
union UtilUnion **uovrv = 0;
union UtilUnion **ubndv = 0;
union UtilUnion *uglbv = 0;
UtilFunc *ufnc = 0;
jmp_buf uenv[4] = {0};
int uenc = 0;

extern int inp[];
extern int out[];
extern int len;

void utilError(const char *str, ...)
{
	if (uenc) longjmp(uenv[--uenc],1);
	va_list args;
	va_start (args,str);
	vfprintf(stderr,str,args);
	va_end (args);
	exit(-1);
}
void utilAlloc1(int siz, int **buf)
{
	if (!buf) utilError("invalid int reference\n");
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc2(int siz, union UtilUnion **buf)
{
	if (!buf) utilError("invalid union reference\n");
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc3(int siz, UtilFunc **buf)
{
	if (!buf) utilError("invalid function reference\n");
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc4(int now, int siz, int ***buf)
{
	if (!buf) utilError("invalid int pointer reference\n");
	if (*buf) for (int i = 0; i < now; i++) free((*buf)[i]);
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc5(int now, int siz, union UtilUnion ***buf)
{
	if (!buf) utilError("invalid struct pointer reference\n");
	if (*buf) for (int i = 0; i < now; i++) free((*buf)[i]);
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc(int lstc, int argc, int glbc)
{
	if (lstc < 0 || argc < 0) utilError("invalid alloc size\n");
	utilAlloc1(argc,&uovrc);
	utilAlloc1(lstc,&ubndc);
	utilAlloc5(uargc,argc,&uovrv);
	utilAlloc5(ulstc,lstc,&ubndv);
	utilAlloc2(glbc,&uglbv);
	utilAlloc3(lstc,&ufnc);
	ulstc = lstc;
	uargc = argc;
	uglbc = glbc;
}
void utilArgc(int arg, int siz)
{
	if (arg < 0 || arg >= uargc) utilError("invalid argc index\n");
	utilAlloc2(siz,uovrv+arg);
	uovrc[arg] = siz;
	if (siz > 0) uovrv[arg][0] = utilUnionI(siz);
	if (siz > 1) uovrv[arg][1] = utilUnionI(arg);
}
void utilArgv(int arg, int idx, union UtilUnion val)
{
	if (arg < 0 || arg >= uargc || idx < 0 || idx >= uovrc[arg]) utilError("invalid argv index\n");
	uovrv[arg][idx] = val;
}
void utilLstc(int lst, int siz)
{
	if (lst < 0 || lst >= ulstc) utilError("invalid lstc index\n");
	utilAlloc2(siz,ubndv+lst);
	ubndc[lst] = siz;
	if (siz > 0) ubndv[lst][0] = utilUnionI(siz);
	if (siz > 1) ubndv[lst][1] = utilUnionI(lst);
}
void utilLstv(int lst, int idx, union UtilUnion val)
{
	if (lst < 0 || lst >= ulstc || idx < 0 || idx >= uovrc[lst]) utilError("invalid lstv index\n");
	ubndv[lst][idx] = val;
}
void utilFunc(int lst, UtilFunc func)
{
	if (lst < 0 || lst >= ulstc) utilError("invalid lstc index\n");
	ufnc[lst] = func;
}
void utilGlbv(int glb, union UtilUnion val)
{
	uglbv[glb] = val;
}
void utilMerge(int size, int *index, UtilComp func)
{
	int lsiz = size/2;
	int rsiz = size/2+size%2;
	int *left;
	int *right;
	if (size <= 1) return;
	left = calloc(lsiz,sizeof(*left));
	right = calloc(rsiz,sizeof(*right));
	for (int i = 0; i < lsiz; i++) left[i] = index[i];
	for (int i = 0; i < rsiz; i++) right[i] = index[lsiz+i];
	utilMerge(lsiz,left,func);
	utilMerge(rsiz,right,func);
	for (int i = 0, j = 0, k = 0; i < size/2 || j < size/2+size%2; k++) {
		if (i < size/2 && func(left[i],right[j]) < 0) {
			index[k] = left[i]; i++;
		} else {
			index[k] = right[j]; j++;
		}
	}
	free(left);
	free(right);
}
void utilCheck0(int glb, const char *str)
{
	if (glb < 0 || glb >= uglbc) utilError("invalid %s glb\n",str);
}
void utilCheck1(int lst, const char *str)
{
	if (lst < 0 || lst >= ulstc) utilError("invalid %s lst\n",str);
}
void utilCheck2(int arg, const char *str)
{
	if (arg < 0 || arg >= uargc) utilError("invalid %s arg\n",str);
}
void utilCheck3(int opt, const char *str)
{
}
void utilCheck13(int lst, int opt, const char *str)
{
	utilCheck1(lst,str);
	utilCheck3(opt,str);
}
void utilCheck12(int lst, int arg, const char *str)
{
	utilCheck1(lst,str);
	utilCheck2(arg,str);
}
void utilCheck123(int lst, int arg, int opt, const char *str)
{
	utilCheck1(lst,str);
	utilCheck2(arg,str);
	utilCheck3(opt,str);
}
union UtilUnion utilOver(int arg, int idx)
{
	utilCheck2(arg,"over");
	if (idx < 0 || idx >= uovrc[arg]) utilError("invalid over idx\n");
	return uovrv[arg][idx];
}
union UtilUnion utilBind(int lst, int idx)
{
	utilCheck1(lst,"bind");
	if (idx < 0 || idx >= ubndc[lst]) utilError("invalid bind idx\n");
	return ubndv[lst][idx];
}
union UtilUnion *utilGlob(int glb)
{
	utilCheck0(glb,"glob");
	return uglbv+glb;
}
int utilListMin(int lst) // -> arg
{
	return utilPartMin(lst,utilMinPart(lst));
}
int utilListMax(int lst) // -> arg
{
	return utilPartMax(lst,utilMaxPart(lst));
}
int utilLast(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"last");
	if (arg-1 == -1) return -1;
	if (utilEquiv(lst,arg,arg-1)) return arg-1;
	return utilLastMax(lst,arg);
}
int utilNext(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"next");
	if (arg+1 == uargc) return -1;
	if (utilEquiv(lst,arg,arg+1)) return arg+1;
	return utilNextMin(lst,arg);
}
int utilLastMin(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"lastmin");
	for (int i = 0; i < arg; i++) if (utilEquiv(lst,arg,i)) return i;
	return arg;
}
int utilNextMax(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"nextmax");
	for (int i = uargc-1; i > arg; i--) if (utilEquiv(lst,arg,i)) return i;
	return arg;
}
int utilLastMax(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"lastmax");
	int opt = 0;
	int lim = utilPart(lst,arg);
	for (int i = 0; i < uargc; i++) {
		int tmp = utilPart(lst,i);
		if (opt < tmp && lim > tmp) opt = tmp;
	}
	return utilPartMax(lst,opt);
}
int utilNextMin(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"lastmax");
	int opt = uargc-1;
	int lim = utilPart(lst,arg);
	for (int i = 0; i < uargc; i++) {
		int tmp = utilPart(lst,i);
		if (opt > tmp && lim < tmp) opt = tmp;
	}
	return utilPartMin(lst,opt);
}
int utilMinPart(int lst) // -> opt
{
	utilCheck1(lst,"minpart");
	int opt = uargc-1;
	for (int i = 0; i != uargc; i++) {
		int tmp = utilPart(lst,i);
		if (opt > tmp) opt = tmp;
	}
	return utilPartMin(lst,opt);
}
int utilMaxPart(int lst) // -> opt
{
	utilCheck1(lst,"maxpart");
	int opt = 0;
	for (int i = 0; i != uargc; i++) {
		int tmp = utilPart(lst,i);
		if (opt < tmp) opt = tmp;
	}
	return utilPartMax(lst,opt);
}
int utilPartMin(int lst, int opt) // -> arg
{
	utilCheck13(lst,opt,"partmin");
	for (int i = 0; i < uargc; i++) {
		int tmp = utilPart(lst,i);
		if (opt == tmp) return i;
	}
	return -1;
}
int utilPartMax(int lst, int opt) // -> arg
{
	utilCheck13(lst,opt,"partmax");
	for (int i = uargc-1; i != -1; i--) {
		int tmp = utilPart(lst,i);
		if (opt == tmp) return i;
	}
	return -1;
}
int utilPartLast(int lst, int arg, int opt) // -> arg
{
	utilCheck123(lst,arg,opt,"partlast");
	for (int i = arg; i != -1; i--) {
		int tmp = utilPart(lst,i);
		if (opt == tmp) return i;
	}
	return -1;
}
int utilPartNext(int lst, int arg, int opt) // -> arg
{
	utilCheck123(lst,arg,opt,"partnext");
	for (int i = arg; i != uargc; i++) {
		int tmp = utilPart(lst,i);
		if (opt == tmp) return i;
	}
	return -1;
}
int utilIsList(int arg) // -> bool
{
	if (arg == -1) return 0;
	utilCheck2(arg,"islist");
	return 1;
}
int utilIsPart(int lst, int opt) // -> bool
{
	if (opt == -1) return 0;
	return utilIsList(utilPartMin(lst,opt));
}
int utilEquiv(int lst, int one, int oth) // -> bool
{
	if (one == -1 || oth == -1) return 0;
	return (utilPart(lst,one) == utilPart(lst,oth));
}
int utilPart(int lst, int arg) // -> opt
{
	utilCheck12(lst,arg,"part");
	return ufnc[lst](lst,arg).i;
}
union UtilUnion utilHash(int lst, int arg)
{
	utilCheck12(lst,arg,"hash");
	return ufnc[lst](lst,arg).u;
}
union UtilUnion utilUnion(int glb)
{
	return uglbv[glb];
}
union UtilUnion utilUnionI(int i)
{
	union UtilUnion u; u.i = i; return u;
}
union UtilUnion utilUnionL(long long l)
{
	union UtilUnion u; u.l = l; return u;
}
union UtilUnion utilUnionS(const char *s)
{
	union UtilUnion u; u.s = s; return u;
}
struct UtilStruct utilStructI(int i, int u)
{
	struct UtilStruct s; s.i = i; s.u = utilUnionI(u); if (uenc) uenc--; return s;
}
struct UtilStruct utilStructL(int i, long long u)
{
	struct UtilStruct s; s.i = i; s.u = utilUnionI(u); if (uenc) uenc--; return s;
}
struct UtilStruct utilStructS(int i, const char *u)
{
	struct UtilStruct s; s.i = i; s.u = utilUnionS(u); if (uenc) uenc--; return s;
}
int utilMatch(const char *lst, const char *arg)
{
	for (int i = 0; lst[i]; i++) {
		char str[3]; str[0] = '-'; str[1] = lst[i]; str[2] = 0;
		if (strcmp(str,arg) == 0) return i;
	}
	if (uenc) longjmp(uenv[--uenc],1);
	return strlen(lst);
}
const char *utilGetenv(const char *wrd)
{
	const char *str = getenv(wrd);
	if (str == 0 && uenc) longjmp(uenv[--uenc],1);
	return str;
}
int utilAtoi(const char *str)
{
	errno = 0;
	int val = atoi(str);
	if (errno && uenc) longjmp(uenv[--uenc],1);
	return val;
}
struct UtilStruct utilFlagFunc(int lst, int arg)
{
	const char *wrd = utilOver(arg,0).s;
	const char *pat = utilBind(lst,0).s;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
	int ident = utilMatch(pat,wrd);
	return utilStructI(ident,-1);
}
struct UtilStruct utilEnvFunc(int lst, int arg)
{
	const char *wrd = utilOver(arg,0).s;
	const char *pat = utilBind(lst,0).s;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
	int last = utilLast(lst,arg);
	const char *word = utilOver(last,0).s;
	int ident = utilMatch(pat,word);
	const char *str = utilGetenv(wrd);
	int num = utilAtoi(str);
	return utilStructI(ident,num);
}
struct UtilStruct utilPipeFunc(int lst, int arg)
{
	const char *wrd = utilOver(arg,0).s;
	const char *pat = utilBind(lst,0).s;
	int iol = utilBind(lst,1).i;
	int ipt = utilBind(lst,2).i;
	int opt = utilBind(lst,3).i;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
	int last = utilLast(lst,arg);
	const char *word = utilOver(last,0).s;
	int ident = utilMatch(pat,word);
	int prev = utilPartLast(lst,last,ident);
	int num = utilHash(lst,prev).i;
	int iarg = utilPartLast(iol,arg,ipt);
	int inum = utilHash(iol,iarg).i;
	const char *istr = utilOver(iarg,0).s;
	int oarg = utilPartLast(iol,arg,opt);
	int onum = utilHash(iol,oarg).i;
	const char *ostr = utilOver(oarg,0).s;
	if (num == -1 || inp[num] != inum || out[num] != onum) num = pipeInit(istr,ostr);
	return utilStructI(ident,num);
}
void utilFlag(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilFlagFunc);
}
void utilEnv(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilEnvFunc);
	*utilGlob(0) = utilUnionI(lst);
	*utilGlob(1) = utilUnionI(strstr(str,utilGlob(1)->s)-str);
	*utilGlob(2) = utilUnionI(strstr(str,utilGlob(2)->s)-str);
}
void utilPipe(int lst, const char *str)
{
	utilLstc(lst,4);
	utilLstv(lst,0,utilUnionS(str));
	utilLstv(lst,1,*utilGlob(0));
	utilLstv(lst,2,*utilGlob(1));
	utilLstv(lst,3,*utilGlob(2));
	utilFunc(lst,utilPipeFunc);
}
