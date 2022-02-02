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
const char **uoptv = 0; // string per pass
int uargc = 0; // number of steps
int uprtc = 0; // number of parts
int uoptc = 0; // number of passes

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
void utilAlloc(int argc, int prtc, int cmpc)
{
	utilAlloc1(cmpc,&min);
	utilAlloc2(uoptc,cmpc,prtc,&last2);
	utilAlloc2(uoptc,cmpc,argc,&last1);
	utilAlloc2(uoptc,cmpc,argc,&last);
	utilAlloc2(uoptc,cmpc,argc,&ident);
	utilAlloc2(uoptc,cmpc,argc,&next);
	utilAlloc2(uoptc,cmpc,argc,&next1);
	utilAlloc2(uoptc,cmpc,prtc,&next2);
	utilAlloc1(cmpc,&max);
	utilAlloc4(uoptc,cmpc,prtc,&hash);
	utilAlloc3(argc,&uargv);
	utilAlloc3(cmpc,&uoptv);
	uargc = argc; uprtc = prtc; uoptc = cmpc;
}
void utilOpt(const char *str, int opt)
{
	if (opt < 0 || opt >= uoptc) {fprintf(stderr,"too many opts\n"); exit(-1);}
	uoptv[opt] = str;
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
union UtilHash utilCall(struct UtilFunc func, int opt, int one, int oth, int alt)
{
	if (alt != -1) {
		if (func.ovl == UtilASTag && func.hash.as) return func.hash.as(opt,uargv[one]);
		else if (func.ovl == UtilBSTag && func.hash.bs) return func.hash.bs(uargv[one]);
		else if (func.ovl == UtilATTag && func.hash.at) return func.hash.at(opt,one);
		else if (func.ovl == UtilBTTag && func.hash.bt) return func.hash.bt(one);
		else {fprintf(stderr,"invalid hash overload\n"); exit(-1);}
	} else if (func.tag == UtilCompTag) {
		if (func.ovl == UtilASTag && func.comp.as) return utilCallC(func.comp.as(opt,uargv[one],uargv[oth]));
		else if (func.ovl == UtilBSTag && func.comp.bs) return utilCallC(func.comp.bs(uargv[one],uargv[oth]));
		else if (func.ovl == UtilATTag && func.comp.at) return utilCallC(func.comp.at(opt,one,oth));
		else if (func.ovl == UtilBTTag && func.comp.bt) return utilCallC(func.comp.bt(one,oth));
		else {fprintf(stderr,"invalid comp overload\n"); exit(-1);}
	} else if (func.tag == UtilIdentTag) {
		if (func.ovl == UtilASTag && func.ident.as) return utilCallI(func.ident.as(opt,uargv[one]));
		else if (func.ovl == UtilBSTag && func.ident.bs) return utilCallI(func.ident.bs(uargv[one]));
		else if (func.ovl == UtilATTag && func.ident.at) return utilCallI(func.ident.at(opt,one));
		else if (func.ovl == UtilBTTag && func.ident.bt) return utilCallI(func.ident.bt(one));
		else {fprintf(stderr,"invalid ident overload\n"); exit(-1);}
	} else if (func.tag == UtilStepTag || func.tag == UtilSetupTag) {
		if (func.ovl == UtilASTag && func.test.as) return utilCallS(func.test.as(opt,uargv[one]));
		else if (func.ovl == UtilBSTag && func.test.bs) return utilCallS(func.test.bs(uargv[one]));
		else if (func.ovl == UtilATTag && func.test.at) return utilCallS(func.test.at(opt,one));
		else if (func.ovl == UtilBTTag && func.test.bt) return utilCallS(func.test.bt(one));
		else {fprintf(stderr,"invalid test overload\n"); exit(-1);}
	} else {fprintf(stderr,"invalid func tag\n"); exit(-1);}
	return utilCallL(-1);
}
int utilCall1(struct UtilFunc func, int opt, int one, int oth)
{
	return utilCall(func,opt,one,oth,-1).i;
}
int utilCall2(struct UtilFunc func, int opt, int one)
{
	return utilCall(func,opt,one,-1,-1).i;
}
union UtilHash utilCall3(struct UtilFunc func, int opt, int one)
{
	return utilCall(func,opt,one,-1,0);
}
void utilLink(struct UtilFunc func, int opt)
{
	if (opt < 0 || opt >= uoptc || uargc == 0) return;
	for (int i = 0; i < uargc; i++) {
		last2[opt][i] = -1;
		next2[opt][i] = -1;
		hash[opt][i].l = -1;
	}
	for (int i = 0; i < uargc; i++) {
		int id = -1;
		if (func.tag == UtilCompTag) {
			for (int k = 0; last2[opt][k] != -1 && id == -1; k++) {
				if (k == uargc) {fprintf(stderr,"no type in comp\n"); exit(-1);}
				if (utilCall1(func,opt,i,last2[opt][k]) == 0) id = k;
			}
		} else if (func.tag == UtilIdentTag) {
			id = utilCall2(func,opt,i);
			if (id < 0 || id >= uargc) id = -1;
		} else if (func.tag == UtilStepTag) {
			if (i != 0 && !utilCall2(func,opt,i)) id = ident[opt][i-1];
		} else if (func.tag == UtilSetupTag) {
			if (i == 0 || !utilCall2(func,opt,i-1)) id = 0;
		}
		if (id == -1) {
			for (int k = 0; k < uargc && id == -1; k++) {
				if (last2[opt][k] == -1) id = k;
			}
			if (id == -1) {fprintf(stderr,"no free ident\n"); exit(-1);}
		}
		ident[opt][i] = id;
		if (last2[opt][id] == -1) {
			last2[opt][id] = i;
			if (func.act) hash[opt][id] = utilCall3(func,opt,i);
		}
		if (id < min[opt]) min[opt] = id;
		if (id > max[opt]) max[opt] = id;
		if (next2[opt][id] == -1 && id == 0) {
			last[opt][i] = -1;
		} else if (next2[opt][id] == -1) {
			if (next2[opt][id-1] < 0) {fprintf(stderr,"no previous type ident\n"); exit(-1);}
			last[opt][i] = next2[opt][id-1];
			next[opt][next2[opt][id-1]] = i;
		} else {
			if (next2[opt][id] < 0) {fprintf(stderr,"invalid previous type ident\n"); exit(-1);}
			last[opt][i] = next2[opt][id];
			next[opt][next2[opt][id]] = i;
		}
		next2[opt][id] = i;
		next[opt][i] = -1;
	}
	for (int i = 0; i < uargc; i++) {
		for (int j = i; j > 0 && ident[opt][j-1] == ident[opt][i]; j--) last1[opt][i] = j;
		for (int j = i; j < uargc-1 && ident[opt][j+1] == ident[opt][i]; j++) next1[opt][i] = j;
	}
}
int utilFlagIdent(int opt, const char *arg)
{
	const char *str = (uoptv[opt] ? uoptv[opt] : "");
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
void utilCheck1(int opt, const char *str)
{
	if (opt < 0 || opt >= uoptc) {fprintf(stderr,"invalid %s opt\n",str); exit(-1);}	
}
void utilCheck2(int arg, const char *str)
{
	if (arg < 0 || arg >= uargc) {fprintf(stderr,"invalid %s arg\n",str); exit(-1);}
}
void utilCheck3(int opt, int typ, const char *str)
{
	utilCheck1(opt,str);
	if (typ < 0 || typ >= uprtc) {fprintf(stderr,"invalid %s typ\n",str); exit(-1);}
}
void utilCheck(int opt, int arg, const char *str)
{
	utilCheck1(opt,str);
	utilCheck2(arg,str);
}
int utilListMin(int opt) // -> arg
{
	return utilPartMin(opt,utilMinPart(opt));
}
int utilListMax(int opt) // -> arg
{
	return utilPartMax(opt,utilMaxPart(opt));
}
int utilLast(int opt, int arg) // -> arg
{
	utilCheck(opt,arg,"last");
	return last[opt][arg];
}
int utilNext(int opt, int arg) // -> arg
{
	utilCheck(opt,arg,"next");
	return next[opt][arg];
}
int utilLastMin(int opt, int arg) // -> arg
{
	utilCheck(opt,arg,"lastmin");
	return last1[opt][arg];
}
int utilNextMax(int opt, int arg) // -> arg
{
	utilCheck(opt,arg,"nextmax");
	return next1[opt][arg];
}
int utilLastMax(int opt, int arg) // -> arg
{
	return utilLast(opt,utilLastMin(opt,arg));
}
int utilNextMin(int opt, int arg) // -> arg
{
	return utilNext(opt,utilNextMax(opt,arg));
}
int utilPart(int opt, int arg) // -> typ
{
	utilCheck(opt,arg,"part");
	return ident[opt][arg];
}
int utilMinPart(int opt) // -> typ
{
	utilCheck1(opt,"minpart");
	return min[opt];
}
int utilMaxPart(int opt) // -> typ
{
	utilCheck1(opt,"maxpart");
	return max[opt];
}
int utilPartMin(int opt, int typ) // -> arg
{
	utilCheck3(opt,typ,"partmin");
	return last2[opt][typ];
}
int utilPartMax(int opt, int typ) // -> arg
{
	utilCheck3(opt,typ,"partmax");
	return next2[opt][typ];
}
int utilIsList(int arg) // -> bool
{
	if (arg == -1) return 0;
	utilCheck2(arg,"islist");
	return 1;
}
int utilIsPart(int opt, int typ) // -> bool
{
	if (typ == -1) return 0;
	return utilIsList(utilPartMin(opt,typ));
}
int utilEquiv(int opt, int one, int oth) // -> bool
{
	if (one == -1 || oth == -1) return 0;
	return (utilPart(opt,one) == utilPart(opt,oth));
}
union UtilHash utilHash(int opt, int typ)
{
	utilCheck3(opt,typ,"partval");
	return hash[opt][typ];
}
