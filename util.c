#include "util.h"
#include "face.h"
#include "type.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <sys/errno.h>

int uargc = 0; // line
int ulstc = 0; // pass
int uglbc = 0; // global
int *uovrc = 0; // overload
int *ubndc = 0; // binding
union UtilUnion **uovrv = 0;
union UtilUnion **ubndv = 0;
union UtilUnion *uglbv = 0;
union UtilUnion **uvalv = 0;
int **uidtv = 0;
int **ucntv = 0;
jmp_buf uenv[JMP] = {0};
int uenc = 0;
int uholc = 0;

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
void utilAlloc4(int now, int siz, int ***buf)
{
	if (!buf) utilError("invalid int pointer reference\n");
	if (*buf) for (int i = 0; i < now; i++) free((*buf)[i]);
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc5(int now, int siz, union UtilUnion ***buf)
{
	if (!buf) utilError("invalid union pointer reference\n");
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
	utilAlloc5(ulstc,lstc,&uvalv);
	utilAlloc4(ulstc,lstc,&uidtv);
	utilAlloc4(ulstc,lstc,&ucntv);
	for (int i = 0; i < lstc; i++) {
		utilAlloc2(argc,uvalv+i);
		utilAlloc1(argc,uidtv+i);
		utilAlloc1(argc,ucntv+i);
	}
	ulstc = lstc;
	uargc = argc;
	uglbc = glbc;
}
void utilArgc(int arg, int siz)
{
	if (arg < 0 || arg >= uargc) utilError("invalid argc index\n");
	utilAlloc2(siz,uovrv+arg);
	uovrc[arg] = siz;
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
}
void utilLstv(int lst, int idx, union UtilUnion val)
{
	if (lst < 0 || lst >= ulstc || idx < 0 || idx >= uovrc[lst]) utilError("invalid lstv index\n");
	ubndv[lst][idx] = val;
}
void utilFunc(int lst, UtilFunc func)
{
	if (lst < 0 || lst >= ulstc) utilError("invalid func index\n");
	for (int i = 0; i < uargc; i++) ucntv[lst][i] = -1;
	for (int i = 0; i < uargc; i++) {
		struct UtilStruct val = func(lst,i);
		uidtv[lst][i] = val.i;
		uvalv[lst][i] = val.u;
		ucntv[lst][i] = 0;
		for (int j = 0; j < i; j++) {
			if (uidtv[lst][i] < uidtv[lst][j]) ucntv[lst][j]++;
			if (uidtv[lst][i] > uidtv[lst][j]) ucntv[lst][i]++;
		}
	}
}
void utilGlbv(int glb, union UtilUnion val)
{
	uglbv[glb] = val;
}
void utilMerge(int lst, int size, int *index, UtilComp func)
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
	utilMerge(lst,lsiz,left,func);
	utilMerge(lst,rsiz,right,func);
	for (int i = 0, j = 0, k = 0; i < size/2 || j < size/2+size%2; k++) {
		if (i < size/2 && func(lst,left[i],right[j]) < 0) {
			index[k] = left[i]; i++;
		} else {
			index[k] = right[j]; j++;
		}
	}
	free(left);
	free(right);
}
int utilComp(int lst, int left, int right)
{
	if (lst < 0 || lst >= ulstc) utilError("invalid comp index\n");
	if (left < 0 || left >= uargc) utilError("invalid comp left\n");
	if (right < 0 || right >= uargc) utilError("invalid comp right\n");
	if (uidtv[lst][left] < uidtv[lst][right]) return -1;
	if (uidtv[lst][left] > uidtv[lst][right]) return 1;
	return 0;
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
int utilEqual(int lst, int lft, int rgt, int opt)
{
	return (opt == -1 || (rgt != -1 && uidtv[lst][lft] == uidtv[lst][rgt]));
}
int utilFind(int lst, int arg, int opt, int cnt, int cmp, const char *str)
{
	utilCheck12(lst,arg,str);
	int i,j,k;
	for (i = 0, j = -1; opt != -1 && i != uargc && j == -1; i++) if (ucntv[lst][i] != -1 && uidtv[lst][i] == opt) j = i;
	for (i = 0, k = -1; j != -1 && i != uargc && k == -1; i++) if (ucntv[lst][i] != -1 && ucntv[lst][i] == ucntv[lst][j] + cmp) k = i;
	if (cnt > 0) {
		for (i = arg, j = cnt; i != uargc; i++) if (utilEqual(lst,i,k,opt) && --j == 0) break;
		if (i == uargc) utilError("invalid %s %d %d %d %d %d\n",str,lst,arg,opt,cnt,cmp);
	} else if (cnt < 0) {
		for (i = arg, j = cnt; i != -1; i--) if (utilEqual(lst,i,k,opt) && ++j == 0) break;
		if (i == -1) utilError("invalid %s %d %d %d %d %d\n",str,lst,arg,opt,cnt,cmp);
	} else {
		i = arg;
		if (!utilEqual(lst,i,k,opt)) utilError("invalid %s %d %d %d %d %d\n",str,lst,arg,opt,cnt,cmp);
	}
	return i;
}
int utilTest(int lst, int arg, int opt, int cnt, int cmp)
{
	if (setjmp(uenv[uenc++])) return 0;
		utilFind(lst,arg,opt,cnt,cmp,"test");
	uenc--;
	return 1;
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
	return uvalv[lst][utilFind(lst,arg,opt,cnt,cmp,"hash")];
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
	utilError("invalid match str\n");
	return strlen(lst);
}
const char *utilGetenv(const char *wrd)
{
	const char *str = getenv(wrd);
	if (str == 0) utilError("invalid getenv str\n");
	return str;
}
int utilAtoi(const char *str)
{
	errno = 0;
	int val = atoi(str);
	if (errno) utilError("invalid atoi str\n");
	return val;
}
int utilStrstr(int hay, int ndl, int ndx)
{
	const char *str = utilBind(hay,PAT).s;
	const char pat[] = {utilBind(ndl,PAT).s[ndx],0};
	return strstr(str,pat)-str;
}
struct UtilStruct utilUsageFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	int ident = utilMatch(pat,wrd);
	return utilStructI(ident,-1);
}
struct UtilStruct utilFlagFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
		int ident = utilMatch(pat,wrd);
	uenc--;
	return utilStructI(ident,-1);
}
struct UtilStruct utilRawFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char *pat = utilBind(lst,PAT).s;
	if (setjmp(uenv[uenc++])) {
		if (setjmp(uenv[uenc++])) return utilStructS(STR,wrd);
			int num = utilAtoi(wrd);
		uenc--; 
		return utilStructI(NUM,num);}
		const char *word = utilOver(lst,arg,WLD,LST,EQU,WRD).s;
		utilMatch(pat,word);
	uenc--;
	return utilStructS(OPT,wrd);
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
	uenc--;
	return utilStructI(ident,num);
}
struct UtilStruct utilPipeFunc(int lst, int arg)
{
	const char *wrd = utilSelf(arg,WRD).s;
	const char pat[] = {utilBind(lst,PAT).s[FCH],0};
	int raw = utilGlob(RAW).i;
	int env = utilGlob(ENV).i;
	int iprt = utilStrstr(env,lst,ICH);
	int oprt = utilStrstr(env,lst,OCH);
	if (setjmp(uenv[uenc++])) return utilStructI(strlen(pat),-1);
		const char *word = utilOver(lst,arg,WLD,LST,EQU,WRD).s;
		int ident = utilMatch(pat,word);
	uenc--;
	if (setjmp(uenv[uenc++])) {uenc--; return utilStructI(ident,forkExec("file"));}
		int pnum; if (setjmp(uenv[uenc++])) {pnum = -1;}
		else {pnum = utilHash(lst,arg,ident,LST,EQU).i; uenc--;}
		int inum; if (setjmp(uenv[uenc++])) {inum = utilHash(raw,MIN,NUM,INP,EQU).i;}
		else {inum = utilHash(env,arg,iprt,LST,EQU).i; uenc--;}
		const char *istr; if (setjmp(uenv[uenc++])) {istr = utilOver(raw,MIN,NUM,INP,EQU,WRD).s;}
		else {istr = utilOver(env,arg,iprt,LST,EQU,WRD).s; uenc--;}
		int onum; if (setjmp(uenv[uenc++])) {onum = utilHash(raw,MIN,NUM,OUT,EQU).i;}
		else {onum = utilHash(env,arg,oprt,LST,EQU).i; uenc--;}
		const char *ostr; if (setjmp(uenv[uenc++])) {ostr = utilOver(raw,MIN,NUM,OUT,EQU,WRD).s;}
		else {ostr = utilOver(env,arg,oprt,LST,EQU,WRD).s; uenc--;}
	uenc--;
	if (pnum == -1 || inp[pnum] != inum || out[pnum] != onum) pnum = pipeInit(istr,ostr);
	return utilStructI(ident,pnum);
}
struct UtilStruct utilFileFunc(int lst, int arg)
{
	int sub = utilGlob(SUB).i;
	if (setjmp(uenv[uenc++])) return utilStructI(INV,-1);
		int pnum = utilHash(sub,arg,THD,EQU,EQU).i;
		const char *name = utilOver(sub,arg,THD,EQU,EQU,WRD).s;
	uenc--;
	char buf[strlen(name)+1]; strcpy(buf,name);
	int fnum = uholc++;
	struct File file; file.act = NewHub; file.idx = fnum; file.str = buf;
	writeFile(&file,pnum);
	return utilStructI(THD,fnum);
}
void utilUsage(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilFunc(lst,utilUsageFunc);
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
	utilGlbv(RAW,utilUnionI(lst));
	utilFunc(lst,utilRawFunc);
}
void utilEnv(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilGlbv(ENV,utilUnionI(lst));
	utilFunc(lst,utilEnvFunc);
}
void utilPipe(int lst, const char *str)
{
	utilLstc(lst,1);
	utilLstv(lst,0,utilUnionS(str));
	utilGlbv(SUB,utilUnionI(lst));
	utilFunc(lst,utilPipeFunc);
}
void utilFile(int lst, const char *str)
{
	utilLstc(lst,0);
	utilGlbv(IDX,utilUnionI(lst));
	utilFunc(lst,utilFileFunc);
}
