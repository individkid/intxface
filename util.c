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
	struct UtilStruct s; s.i = i; s.u = utilUnionI(u); return s;
}
struct UtilStruct utilStructL(int i, long long u)
{
	struct UtilStruct s; s.i = i; s.u = utilUnionI(u); return s;
}
struct UtilStruct utilStructS(int i, const char *u)
{
	struct UtilStruct s; s.i = i; s.u = utilUnionS(u); return s;
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
void utilCheck12(int lst, int arg, const char *str)
{
	utilCheck1(lst,str);
	utilCheck2(arg,str);
}
int utilComp(int lst, int arg, int opt)
{
	utilCheck12(lst,arg,"comp");
	int ident = (opt == -1 ? opt : ufnc[lst](lst,arg).i);
	if (ident < opt) return -1;
	if (ident > opt) return 1;
	return 0;
}
int utilFind(int lst, int arg, int opt, int cnt, int cmp, const char *str)
{
	utilCheck12(lst,arg,str);
	int i,j;
	if (cnt > 0) {
		for (i = arg, j = cnt; i < uargc && j > 0; i++) if (utilComp(lst,i,opt) == cmp) j--;
		if (i == uargc) utilError("invalid %s %d %d %d %d %d\n",str,lst,arg,opt,cnt,cmp);
	} else if (cnt < 0) {
		for (i = uargc-1, j = cnt; i > -1 && j < 0; i--) if (utilComp(lst,i,opt) == cmp) j++;
		if (i == -1) utilError("invalid %s %d %d %d %d %d\n",str,lst,arg,opt,cnt,cmp);
	} else {
		i = arg;
	}
	return i;
}
union UtilUnion utilOver(int lst, int arg, int opt, int cnt, int cmp, int idx)
{
	int i = utilFind(lst,arg,opt,cnt,cmp,"over");
	if (idx < 0 || idx >= uovrc[i]) utilError("invalid over idx\n");
	return uovrv[i][idx];
}
union UtilUnion utilHash(int lst, int arg, int opt, int cnt, int cmp)
{
	utilCheck1(lst,"hash");
	return ufnc[lst](lst,utilFind(lst,arg,opt,cnt,cmp,"hash")).u;
}
union UtilUnion utilBind(int lst, int idx)
{
	utilCheck1(lst,"bind");
	if (idx < 0 || idx >= ubndc[lst]) utilError("invalid bind idx\n");
	return ubndv[lst][idx];
}
union UtilUnion utilSelf(int arg, int idx)
{
	utilCheck2(arg,"self");
	if (idx < 0 || idx >= uovrc[arg]) utilError("invalid self idx\n");
	return uovrv[arg][idx];
}
union UtilUnion utilGlob(int glb)
{
	utilCheck0(glb,"glob");
	return uglbv[glb];
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
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
	int ident = utilMatch(pat,wrd);
	uenc--; return utilStructI(ident,-1);
}
struct UtilStruct utilRawFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	if (setjmp(uenv[uenc++])) {
		if (setjmp(uenv[uenc++])) return utilStructS(STR,wrd);
		int num = utilAtoi(wrd);
		uenc--; return utilStructI(NUM,num);
	}
	const char *word = utilOver(lst,arg,WLD,LST,EQU,WRD).s;
	utilMatch(pat,word);
	uenc--; return utilStructS(OPT,wrd);
}
struct UtilStruct utilEnvFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
	const char *word = utilOver(lst,arg,WLD,LST,EQU,WRD).s;
	int ident = utilMatch(pat,word);
	const char *str = utilGetenv(wrd);
	int num = utilAtoi(str);
	uenc--; return utilStructI(ident,num);
}
struct UtilStruct utilPipeFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	int raw = utilGlob(RAW).i;
	int env = utilGlob(ENV).i;
	int iprt = strstr(utilBind(env,PAT).s,ICH)-utilBind(env,PAT).s;
	int oprt = strstr(utilBind(env,PAT).s,OCH)-utilBind(env,PAT).s;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
	const char *word = utilOver(lst,arg,WLD,LST,EQU,WRD).s;
	int ident = utilMatch(pat,word);
	int num = utilHash(lst,arg,ident,-1,0).i;
	int inum; if (setjmp(uenv[uenc++])) {inum = utilHash(raw,MIN,NUM,INP,EQU).i;}
	else {inum = utilHash(env,arg,iprt,LST,EQU).i; uenc--;}
	const char *istr; if (setjmp(uenv[uenc++])) {istr = utilOver(raw,MIN,NUM,INP,EQU,WRD).s;}
	else {istr = utilOver(env,arg,iprt,LST,EQU,WRD).s; uenc--;}
	int onum; if (setjmp(uenv[uenc++])) {onum = utilHash(raw,MIN,NUM,OUT,EQU).i;}
	else {onum = utilHash(env,arg,oprt,LST,EQU).i; uenc--;}
	const char *ostr; if (setjmp(uenv[uenc++])) {ostr = utilOver(raw,MIN,NUM,OUT,EQU,WRD).s;}
	else {ostr = utilOver(env,arg,oprt,LST,EQU,WRD).s; uenc--;}
	if (num == -1 || inp[num] != inum || out[num] != onum) num = pipeInit(istr,ostr);
	uenc--; return utilStructI(ident,num);
}
void utilFlag(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilFlagFunc);
}
void utilRaw(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilRawFunc);
	utilGlbv(0,utilUnionI(lst));
}
void utilEnv(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilEnvFunc);
	utilGlbv(1,utilUnionI(lst));
}
void utilPipe(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilPipeFunc);
}
