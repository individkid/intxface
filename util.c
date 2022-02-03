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
void utilOpt(const char *str, int lst)
{
	if (lst < 0 || lst >= ulstc) {fprintf(stderr,"too many opts\n"); exit(-1);}
	ulstv[lst] = str;
}
void utilArg(const char *str, int arg)
{
	if (arg < 0 || arg >= uargc) {fprintf(stderr,"too many args\n"); exit(-1);}
	uargv[arg] = str;
}
void utilMerge(UtilCompBT func, int size, int *index)
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
	utilMerge(func,lsiz,left);
	utilMerge(func,rsiz,right);
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
union UtilHash utilCallC(int val)
{
	union UtilHash retval; retval.c = val; return retval;
}
union UtilHash utilCallI(int val)
{
	union UtilHash retval; retval.i = val; return retval;
}
union UtilHash utilCallS(int val)
{
	union UtilHash retval; retval.s = val; return retval;
}
union UtilHash utilCallL(long long val)
{
	union UtilHash retval; retval.l = val; return retval;
}
union UtilHash utilCall(struct UtilFunc func, int lst, int one, int oth, int alt)
{
	if (alt != -1) {
		if (func.ovl == UtilASTag && func.hash.as) return func.hash.as(lst,uargv[one]);
		else if (func.ovl == UtilBSTag && func.hash.bs) return func.hash.bs(uargv[one]);
		else if (func.ovl == UtilATTag && func.hash.at) return func.hash.at(lst,one);
		else if (func.ovl == UtilBTTag && func.hash.bt) return func.hash.bt(one);
		else {fprintf(stderr,"invalid hash overload\n"); exit(-1);}
	} else if (func.tag == UtilCompTag) {
		if (func.ovl == UtilASTag && func.comp.as) return utilCallC(func.comp.as(lst,uargv[one],uargv[oth]));
		else if (func.ovl == UtilBSTag && func.comp.bs) return utilCallC(func.comp.bs(uargv[one],uargv[oth]));
		else if (func.ovl == UtilATTag && func.comp.at) return utilCallC(func.comp.at(lst,one,oth));
		else if (func.ovl == UtilBTTag && func.comp.bt) return utilCallC(func.comp.bt(one,oth));
		else {fprintf(stderr,"invalid comp overload\n"); exit(-1);}
	} else if (func.tag == UtilIdentTag) {
		if (func.ovl == UtilASTag && func.ident.as) return utilCallI(func.ident.as(lst,uargv[one]));
		else if (func.ovl == UtilBSTag && func.ident.bs) return utilCallI(func.ident.bs(uargv[one]));
		else if (func.ovl == UtilATTag && func.ident.at) return utilCallI(func.ident.at(lst,one));
		else if (func.ovl == UtilBTTag && func.ident.bt) return utilCallI(func.ident.bt(one));
		else {fprintf(stderr,"invalid ident overload\n"); exit(-1);}
	} else if (func.tag == UtilStepTag || func.tag == UtilSetupTag) {
		if (func.ovl == UtilASTag && func.test.as) return utilCallS(func.test.as(lst,uargv[one]));
		else if (func.ovl == UtilBSTag && func.test.bs) return utilCallS(func.test.bs(uargv[one]));
		else if (func.ovl == UtilATTag && func.test.at) return utilCallS(func.test.at(lst,one));
		else if (func.ovl == UtilBTTag && func.test.bt) return utilCallS(func.test.bt(one));
		else {fprintf(stderr,"invalid test overload\n"); exit(-1);}
	} else {fprintf(stderr,"invalid func tag\n"); exit(-1);}
	return utilCallL(-1);
}
int utilCall1(struct UtilFunc func, int lst, int one, int oth)
{
	return utilCall(func,lst,one,oth,-1).i;
}
int utilCall2(struct UtilFunc func, int lst, int one)
{
	return utilCall(func,lst,one,-1,-1).i;
}
union UtilHash utilCall3(struct UtilFunc func, int lst, int one)
{
	return utilCall(func,lst,one,-1,0);
}
void utilLink(struct UtilFunc func, int lst)
{
	if (lst < 0 || lst >= ulstc || uargc == 0) return;
	min[lst] = -1;
	max[lst] = -1;
	for (int i = 0; i < uargc; i++) {
		last2[lst][i] = -1;
		next2[lst][i] = -1;
		hash[lst][i].l = -1;
	}
	for (int i = 0; i < uargc; i++) {
		int id = -1;
		if (func.tag == UtilCompTag) {
			for (int k = 0; last2[lst][k] != -1 && id == -1; k++) {
				if (k == uargc) {fprintf(stderr,"no type in comp\n"); exit(-1);}
				if (utilCall1(func,lst,i,last2[lst][k]) == 0) id = k;
			}
		} else if (func.tag == UtilIdentTag) {
			id = utilCall2(func,lst,i);
			if (id < 0 || id >= uargc) id = -1;
		} else if (func.tag == UtilStepTag) {
			if (i != 0 && !utilCall2(func,lst,i)) id = ident[lst][i-1];
		} else if (func.tag == UtilSetupTag) {
			if (i == 0 || !utilCall2(func,lst,i-1)) id = 0;
		}
		if (id == -1) {
			for (int k = 0; k < uargc && id == -1; k++) {
				if (last2[lst][k] == -1) id = k;
			}
			if (id == -1) {fprintf(stderr,"no free ident\n"); exit(-1);}
		}
		ident[lst][i] = id;
		if (last2[lst][id] == -1) {
			last2[lst][id] = i;
			if (func.act) hash[lst][id] = utilCall3(func,lst,i);
		}
		if (min[lst] == -1 || id < min[lst]) min[lst] = id;
		if (max[lst] == -1 || id > max[lst]) max[lst] = id;
		if (next2[lst][id] == -1 && id == 0) {
			last[lst][i] = -1;
		} else if (next2[lst][id] == -1) {
			if (next2[lst][id-1] < 0) {fprintf(stderr,"no previous type ident\n"); exit(-1);}
			last[lst][i] = next2[lst][id-1];
			next[lst][next2[lst][id-1]] = i;
		} else {
			if (next2[lst][id] < 0) {fprintf(stderr,"invalid previous type ident\n"); exit(-1);}
			last[lst][i] = next2[lst][id];
			next[lst][next2[lst][id]] = i;
		}
		next2[lst][id] = i;
		next[lst][i] = -1;
	}
	for (int i = 0; i < uargc; i++) {
		for (int j = i; j > 0 && ident[lst][j-1] == ident[lst][i]; j--) last1[lst][i] = j;
		for (int j = i; j < uargc-1 && ident[lst][j+1] == ident[lst][i]; j++) next1[lst][i] = j;
	}
}
int utilFlagIdent(int lst, const char *arg)
{
	const char *str = (ulstv[lst] ? ulstv[lst] : "");
	for (int i = 0; str[i]; i++) {
		char tmp[3]; tmp[0] = '-'; tmp[1] = str[i]; tmp[2] = 0;
		if (strcmp(str,arg) == 0) return i;
	}
	return strlen(str);
}
struct UtilFunc utilFlagFact()
{
	struct UtilFunc retval;
	retval.tag = UtilIdentTag;
	retval.ovl = UtilASTag;
	retval.act = 0;
	retval.ident.as = utilFlagIdent;
	retval.hash.as = 0;
	return retval;
}
void utilCheck1(int lst, const char *str)
{
	if (lst < 0 || lst >= ulstc) {fprintf(stderr,"invalid %s lst\n",str); exit(-1);}
}
void utilCheck2(int arg, const char *str)
{
	if (arg < 0 || arg >= uargc) {fprintf(stderr,"invalid %s arg\n",str); exit(-1);}
}
void utilCheck3(int lst, int opt, const char *str)
{
	utilCheck1(lst,str);
	if (opt < 0 || opt >= uoptc) {fprintf(stderr,"invalid %s opt\n",str); exit(-1);}
}
void utilCheck(int lst, int arg, const char *str)
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
	utilCheck(lst,arg,"last");
	return last[lst][arg];
}
int utilNext(int lst, int arg) // -> arg
{
	utilCheck(lst,arg,"next");
	return next[lst][arg];
}
int utilLastMin(int lst, int arg) // -> arg
{
	utilCheck(lst,arg,"lastmin");
	return last1[lst][arg];
}
int utilNextMax(int lst, int arg) // -> arg
{
	utilCheck(lst,arg,"nextmax");
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
	utilCheck(lst,arg,"part");
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
	utilCheck3(lst,opt,"partmin");
	return last2[lst][opt];
}
int utilPartMax(int lst, int opt) // -> arg
{
	utilCheck3(lst,opt,"partmax");
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
	utilCheck3(lst,opt,"partval");
	return hash[lst][opt];
}
