#include "util.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int *min = 0; // ident
int **last2 = 0; // ident back
int **last1 = 0; // equiv back
int **last = 0; // go back
int **ident = 0; // ident
int **next = 0; // go fore
int **next1 = 0; // equiv fore
int **next2 = 0; // ident fore
int *max = 0; // ident
union UtilHash **hash = 0; // info per part
const char **uargv = 0; // string per step
const char **ulstv = 0; // string per pass
int uargc = 0; // number of steps
int uoptc = 0; // number of parts
int ulstc = 0; // number of passes

void utilAlloc1(int siz, int **buf)
{
	if (!buf) {fprintf(stderr,"invalid alloc1 reference\n"); exit(-1);}
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc2(int now, int siz1, int siz2, int ***buf)
{
	if (!buf) {fprintf(stderr,"invalid alloc2 reference\n"); exit(-1);}
	if (*buf) for (int i = 0; i < now; i++) free((*buf)[i]);
	free(*buf); *buf = calloc(siz1,sizeof(*buf));
	for (int i = 0; i < siz1; i++) (*buf)[i] = calloc(siz2,sizeof(***buf));
}
void utilAlloc3(int siz, const char ***buf)
{
	if (!buf) {fprintf(stderr,"invalid alloc3 reference\n"); exit(-1);}
	free(*buf); *buf = calloc(siz,sizeof(**buf));
}
void utilAlloc4(int now, int siz1, int siz2, union UtilHash ***buf)
{
	if (!buf) {fprintf(stderr,"invalid alloc4 reference\n"); exit(-1);}
	if (*buf) for (int i = 0; i < now; i++) free((*buf)[i]);
	free(*buf); *buf = calloc(siz1,sizeof(*buf));
	for (int i = 0; i < siz1; i++) (*buf)[i] = calloc(siz2,sizeof(***buf));
}
void utilAlloc(int argc, int optc, int lstc)
{
	utilAlloc1(lstc,&min);
	utilAlloc2(ulstc,lstc,optc,&last2);
	utilAlloc2(ulstc,lstc,argc,&last1);
	utilAlloc2(ulstc,lstc,argc,&last);
	utilAlloc2(ulstc,lstc,argc,&ident);
	utilAlloc2(ulstc,lstc,argc,&next);
	utilAlloc2(ulstc,lstc,argc,&next1);
	utilAlloc2(ulstc,lstc,optc,&next2);
	utilAlloc1(lstc,&max);
	utilAlloc4(ulstc,lstc,optc,&hash);
	utilAlloc3(argc,&uargv);
	utilAlloc3(lstc,&ulstv);
	uargc = argc; uoptc = optc; ulstc = lstc;
}
void utilArg(int arg, const char *str)
{
	if (arg < 0 || arg >= uargc) {fprintf(stderr,"invalid given arg\n"); exit(-1);}
	uargv[arg] = str;
}
void utilList(int lst, const char *str)
{
	if (lst < 0 || lst >= ulstc) {fprintf(stderr,"invalid given lst\n"); exit(-1);}
	ulstv[lst] = str;
}
void utilMerge(int size, int *index, UtilCompBT func)
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
union UtilHash utilUnionI(int val)
{
	union UtilHash retval; retval.i = val; return retval;
}
union UtilHash utilUnionL(long long val)
{
	union UtilHash retval; retval.l = val; return retval;
}
union UtilHash utilCall(int lst, int one, int oth, int alt, struct UtilFunc func)
{
	if (alt == 0) {
		if (func.ovl == UtilASTag && func.hash.as) return func.hash.as(lst,uargv[one]);
		else if (func.ovl == UtilBSTag && func.hash.bs) return func.hash.bs(uargv[one]);
		else if (func.ovl == UtilATTag && func.hash.at) return func.hash.at(lst,one);
		else if (func.ovl == UtilBTTag && func.hash.bt) return func.hash.bt(one);
		else {fprintf(stderr,"invalid hash overload\n"); exit(-1);}
	} else if (alt == 1) {
		if (func.ovl == UtilASTag && func.dflt.as) return utilUnionI(func.dflt.as(lst));
		else if (func.ovl == UtilBSTag && func.dflt.bs) return utilUnionI(func.dflt.bs());
		else if (func.ovl == UtilATTag && func.dflt.at) return utilUnionI(func.dflt.at(lst));
		else if (func.ovl == UtilBTTag && func.dflt.bt) return utilUnionI(func.dflt.bt());
		else {fprintf(stderr,"invalid dflt overload\n"); exit(-1);}
	} else if (alt == 2 || func.tag == UtilIdentTag) {
		if (func.ovl == UtilASTag && func.ident.as) return utilUnionI(func.ident.as(lst,uargv[one]));
		else if (func.ovl == UtilBSTag && func.ident.bs) return utilUnionI(func.ident.bs(uargv[one]));
		else if (func.ovl == UtilATTag && func.ident.at) return utilUnionI(func.ident.at(lst,one));
		else if (func.ovl == UtilBTTag && func.ident.bt) return utilUnionI(func.ident.bt(one));
		else {fprintf(stderr,"invalid ident overload\n"); exit(-1);}
	} else if (alt == 3 || func.tag == UtilCompTag) {
		if (func.ovl == UtilASTag && func.comp.as) return utilUnionI(func.comp.as(lst,uargv[one],uargv[oth]));
		else if (func.ovl == UtilBSTag && func.comp.bs) return utilUnionI(func.comp.bs(uargv[one],uargv[oth]));
		else if (func.ovl == UtilATTag && func.comp.at) return utilUnionI(func.comp.at(lst,one,oth));
		else if (func.ovl == UtilBTTag && func.comp.bt) return utilUnionI(func.comp.bt(one,oth));
		else {fprintf(stderr,"invalid comp overload\n"); exit(-1);}
	} else {fprintf(stderr,"invalid func tag\n"); exit(-1);}
	return utilUnionL(-1);
}
int utilComp(int lst, int one, int oth, struct UtilFunc func)
{
	return utilCall(lst,one,oth,3,func).i;
}
int utilIdent(int lst, int one, struct UtilFunc func)
{
	return utilCall(lst,one,-1,2,func).i;
}
union UtilHash utilUnion(int lst, int one, struct UtilFunc func)
{
	return utilCall(lst,one,-1,0,func);
}
int utilDflt(int lst, struct UtilFunc func)
{
	return utilCall(lst,-1,-1,1,func).i;
}
int utilHole(int lst)
{
	int id = -1;
	for (int k = 0; k < uoptc && id == -1; k++) {
		if (last2[lst][k] == -1) id = k;
	}
	if (id == -1) {fprintf(stderr,"no free ident\n"); exit(-1);}
	return id;
}
int utilFind(int lst, int arg, struct UtilFunc func)
{
	int id = -1;
	for (int k = 0; last2[lst][k] != -1 && id == -1; k++) {
		if (k == uoptc) {fprintf(stderr,"no type in comp\n"); exit(-1);}
		if (utilComp(lst,arg,last2[lst][k],func) == 0) id = k;
	}
	return id;
}
int utilBoth(int lst, int arg, struct UtilFunc func)
{
	int id = -1;
	for (int k = 0; k < uoptc && last2[lst][k] != -1 && id == -1; k++) {
		if (utilComp(lst,arg,last2[lst][k],func) == 0) id = k;
	}
	if (id == -1) id = utilIdent(lst,arg,func);
	return id;
}
void utilLink(int lst, struct UtilFunc func)
{
	if (lst < 0 || lst >= ulstc) {fprintf(stderr,"invalid lst\n"); exit(-1);}
	min[lst] = -1;
	max[lst] = -1;
	for (int i = 0; i < uoptc; i++) {
		last2[lst][i] = -1;
		next2[lst][i] = -1;
		hash[lst][i].l = -1;
	}
	for (int i = 0; i < uargc; i++) {
		int id = -1;
		if (func.tag == UtilCompTag && func.arg == UtilArgTag) id = utilFind(lst,i,func);
		else if (func.tag == UtilCompTag && func.arg == UtilPreTag && i > 0) id = utilFind(lst,i-1,func);
		else if (func.tag == UtilIdentTag && func.arg == UtilArgTag) id = utilIdent(lst,i,func);
		else if (func.tag == UtilIdentTag && func.arg == UtilPreTag && i > 0) id = utilIdent(lst,i-1,func);
		else if (func.tag == UtilBothTag && func.arg == UtilArgTag) id = utilBoth(lst,i,func);
		else if (func.tag == UtilBothTag && func.arg == UtilPreTag && i > 0) id = utilBoth(lst,i-1,func);
		if (id < 0 || id >= uoptc) {
			if (func.def == UtilCustTag) id = utilDflt(lst,func);
			else if (func.def == UtilHoleTag) id = utilHole(lst);
			else if (func.def == UtilOnceTag && i == 0) id = utilDflt(lst,func);
			else if (func.def == UtilOnceTag && i != 0) id = ident[lst][i-1];
			else if (func.def == UtilZeroTag && i == 0) id = 0;
			else if (func.def == UtilZeroTag && i != 0) id = ident[lst][i-1];
			if (id < 0 || id >= uoptc) {fprintf(stderr,"invalid id\n"); exit(-1);}
			if (func.act == UtilEveryTag) hash[lst][id] = utilUnion(lst,i,func);
		} else {
			if (func.act == UtilEveryTag) hash[lst][id] = utilUnion(lst,i,func);
			else if (func.act == UtilValidTag) hash[lst][id] = utilUnion(lst,i,func);
			else if (func.act == UtilFirstTag && last2[lst][id] == -1) hash[lst][id] = utilUnion(lst,i,func);
		}
		ident[lst][i] = id;
		if (last2[lst][id] == -1 && min[lst] == -1) {
			last[lst][i] = next[lst][i] = -1;
			last2[lst][id] = next2[lst][id] = i;
			max[lst] = min[lst] = id;
		} else if (last2[lst][id] == -1) {
			if (max[lst] == -1) {fprintf(stderr,"invalid max\n"); exit(-1);}
			if (next2[lst][max[lst]] == -1) {fprintf(stderr,"invalid next2 of max\n"); exit(-1);}
			last[lst][i] = next2[lst][max[lst]];
			next[lst][next2[lst][max[lst]]] = i;
			next[lst][i] = -1;
			last2[lst][id] = next2[lst][id] = i;
			max[lst] = id;
		} else {
			if (next2[lst][id] == -1) {fprintf(stderr,"invalid next2\n"); exit(-1);}
			last[lst][i] = next2[lst][id];
			next[lst][next2[lst][id]] = i;
			next[lst][i] = -1;
			next2[lst][id] = i;
		}
	}
	for (int i = 0; i < uargc; i++) {
		for (int j = i; j > 0 && ident[lst][j-1] == ident[lst][i]; j--) last1[lst][i] = j;
		for (int j = i; j < uargc-1 && ident[lst][j+1] == ident[lst][i]; j++) next1[lst][i] = j;
		if (last1[lst][i] != last2[lst][ident[lst][i]]) {fprintf(stderr,"invalid last\n"); exit(-1);}
		if (next1[lst][i] != next2[lst][ident[lst][i]]) {fprintf(stderr,"invalid next\n"); exit(-1);}
	}
}
int utilASIdent(int lst, const char *arg)
{
	const char *str = (ulstv[lst] ? ulstv[lst] : "");
	for (int i = 0; str[i]; i++) {
		char str[3]; str[0] = '-'; str[1] = str[i]; str[2] = 0;
		if (strcmp(str,arg) == 0) return i;
	}
	return -1;
}
int utilASDflt(int lst)
{
	const char *str = (ulstv[lst] ? ulstv[lst] : "");
	return strlen(str);
}
union UtilHash utilEIHash(int lst, const char *arg)
{
	const char *str = getenv(arg);
	if (!str) return utilUnionL(-1);
	return utilUnionI(atoi(str));
}
struct UtilFunc utilASFact(enum UtilHowTag arg, enum UtilWhenTag act, enum UtilDfltTag def, UtilCompAS comp, UtilIdentAS ident, UtilHashAS hash, UtilDfltAS dflt)
{
	struct UtilFunc retval;
	if (comp&&ident) retval.tag = UtilBothTag;
	else if (comp) retval.tag = UtilCompTag;
	else retval.tag = UtilIdentTag;
	retval.ovl = UtilASTag;
	retval.arg = arg;
	retval.act = act;
	retval.def = def;
	retval.comp.as = comp;
	retval.ident.as = ident;
	retval.hash.as = hash;
	retval.dflt.as = dflt;
	return retval;
}
void utilFlag(int lst, const char *str)
{
	utilList(lst,str);
	utilLink(lst,utilASFact(UtilArgTag,UtilNeverTag,UtilCustTag,0,utilASIdent,0,utilASDflt));
}
void utilEnvInt(int lst, const char *str)
{
	utilList(lst,str);
	utilLink(lst,utilASFact(UtilPreTag,UtilFirstTag,UtilOnceTag,0,utilASIdent,utilEIHash,utilASDflt));
}
void utilCheck1(int lst, const char *str)
{
	if (lst < 0 || lst >= ulstc) {fprintf(stderr,"invalid %s lst\n",str); exit(-1);}
}
void utilCheck2(int arg, const char *str)
{
	if (arg < 0 || arg >= uargc) {fprintf(stderr,"invalid %s arg\n",str); exit(-1);}
}
void utilCheck3(int opt, const char *str)
{
	if (opt < 0 || opt >= uoptc) {fprintf(stderr,"invalid %s opt\n",str); exit(-1);}
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
	return last[lst][arg];
}
int utilNext(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"next");
	return next[lst][arg];
}
int utilLastMin(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"lastmin");
	return last1[lst][arg];
}
int utilNextMax(int lst, int arg) // -> arg
{
	utilCheck12(lst,arg,"nextmax");
	return next1[lst][arg];
}
int utilLastMax(int lst, int arg) // -> arg
{
	return utilLast(lst,utilLastMin(lst,arg));
}
int utilNextMin(int lst, int arg) // -> arg
{
	return utilNext(lst,utilNextMax(lst,arg));
}
int utilPart(int lst, int arg) // -> opt
{
	utilCheck12(lst,arg,"part");
	return ident[lst][arg];
}
int utilMinPart(int lst) // -> opt
{
	utilCheck1(lst,"minpart");
	return min[lst];
}
int utilMaxPart(int lst) // -> opt
{
	utilCheck1(lst,"maxpart");
	return max[lst];
}
int utilPartMin(int lst, int opt) // -> arg
{
	utilCheck13(lst,opt,"partmin");
	return last2[lst][opt];
}
int utilPartMax(int lst, int opt) // -> arg
{
	utilCheck13(lst,opt,"partmax");
	return next2[lst][opt];
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
union UtilHash utilHash(int lst, int opt)
{
	utilCheck13(lst,opt,"partval");
	return hash[lst][opt];
}
