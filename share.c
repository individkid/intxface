#include "face.h"
#include "type.h"
#include "datx.h"
#include "luax.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <stdint.h>

int note = 0;
struct Wrap {
	enum Stream tag;
	int nxt; // next if this is on wake list
	int sub; // dst subscript
	int siz; // dst size
	struct Wrap **dst; // where to write for tag==Fanout,Combine
	int vld; // tag==Fanout,Buffer whether idx valid
	int idx; // tag==Fanout,Buffer reads from here
	int inp; // type to read from here
	int out; // type to write to here
	struct Express *exp; // for tag==Combine
	char *str; // for tag==Buffer
} *wrap = 0; // per argv that is a Valve
int args = 0;
int **back = 0; // per value lists to add to wake
int *refs = 0; // per value list length
int vals = 0; // number of values in expressions
int wake = 0; // first to process before waiting for readable pipe
int vlds = 0;
int sub0 = 0;
int sub1 = 0;
int idx0 = 0;
int idx1 = 0;
void **dat0 = 0;
void **dat1 = 0;

void shareNote(int idx)
{
	note = 1;
}
int shareExec(const char *exe, struct Argument *arg)
{
	int idx = openFork();
	char *str = 0;
	struct stat new;
	if (openCheck(idx) == -1) return idx;
	arg->inp = openRdfd(idx);
	arg->out = openWrfd(idx);
	datxNone(dat0); writeArgument(arg,idx0);
	showType(&str,identType("Argument"),idx0);
	return openExec(exe,str);
}
void shareArgs(int sub, const char *str)
{
	args++;
}
void shareVals(int sub, const char *str)
{
	struct Wrap *ptr = &wrap[sub];
	struct Valve arg = {0}; int len = 0; hideValve(&arg,str,&len);
	switch (arg.tag) {
	case (Fanout): {
		datxInt(dat1,sub);
		datxInserts("P",arg.str,*dat1,identType("Int"));
		ptr->vld |= 1; ptr->inp = identType(arg.typ);
		ptr->siz = ptr->siz; ptr->dst = malloc(arg.siz*sizeof(struct Wrap *));
		assignStr(&ptr->str,arg.str);
		break;}
	case (Combine): if (sub+1 == args) {
		fprintf(stderr,"ERROR: argument after Combine should be Fanout or Buffer\n");
		exit(-1);} else {
		for (int i = 0; i < arg.num; i++) {int typ = 0;
		typ = datxFinds(dat1,"R",arg.dep[i]);
		if (*dat1 == 0) {datxInt(dat1,vals++); datxInserts("R",arg.dep[i],*dat1,typ);}}
		ptr->siz = 1; ptr->dst = malloc(sizeof(struct Wrap *));
		break;}
	case (Buffer): {
		datxInt(dat1,sub);
		datxInserts("P",arg.str,*dat1,identType("Int"));
		ptr->vld |= 1; ptr->inp = identType(arg.typ);
		assignStr(&ptr->str,arg.str);
		break;}
	case (Execute): if (sub+1 == args) {
		fprintf(stderr,"ERROR: argument after Execute should be Fanout or Buffer\n");
		exit(-1);} else {
		struct Wrap *nxt = &wrap[sub+1];
		nxt->idx = shareExec(arg.url,arg.arg);
		*userIdent(nxt->idx) = (void*)(intptr_t)(sub+1);
		nxt->vld |= 6; nxt->out = identType(arg.typ);
		ptr->exp = arg.exp; arg.exp = 0;

		break;}
	default: ERROR();}
	freeValve(&arg);
}
void shareRefs(int sub, const char *str)
{
	struct Wrap *ptr = &wrap[sub];
	struct Valve arg = {0}; int len = 0; hideValve(&arg,str,&len);
	switch (arg.tag) {
	case (Fanout): {if (ptr->vld != 1 && ptr->vld != 7) {
		ERROR();} else if (ptr->vld == 1) {
		ptr->vld |= 6; ptr->idx = openPipe(); ptr->out = identType(arg.typ);
		*userIdent(ptr->idx) = (void*)(intptr_t)sub;}
		for (int i = 0; i < ptr->siz; i++) {int typ = 0;
		typ = datxFinds(dat1,"P",arg.dst[i]);
		if (typ != identType("Int")) ERROR();
		ptr->dst[i] = &wrap[*datxIntz(0,*dat1)];}
		break;}
	case (Combine): if (ptr->vld != 0) {
		fprintf(stderr,"ERROR: argument after Execute should be Fanout or Buffer\n");
		exit(-1);} else {
		ptr->dst[0] = &wrap[sub+1];
		if (ptr->dst[0]->vld == 0) {
		fprintf(stderr,"ERROR: argument after Combine should be Fanout or Buffer\n");
		exit(-1);} else {
		for (int i = 0; i < arg.num; i++) {int typ = 0;
		typ = datxFinds(dat1,"R",arg.dep[i]);
		if (typ != identType("Int")) ERROR();
		refs[*datxIntz(0,*dat1)] += 1;}
		break;}}
	case (Buffer): {if (ptr->vld != 1 && ptr->vld != 7) {
		ERROR();} else if (ptr->vld == 1) {
		ptr->vld |= 6; ptr->idx = openPipe(); ptr->out = identType(arg.typ);
		*userIdent(ptr->idx) = (void*)(intptr_t)sub;}
		break;}
	case (Execute): if (ptr->vld != 0) {
		fprintf(stderr,"ERROR: argument after Execute should be Fanout or Buffer\n");
		exit(-1);} else {
		break;}
	default: ERROR();}
	freeValve(&arg); vlds++;
}
void shareBack(int sub, const char *str)
{
	struct Wrap *ptr = &wrap[sub];
	struct Valve arg = {0}; int len = 0; hideValve(&arg,str,&len);
	switch (arg.tag) {
	case (Fanout): break;
	case (Combine): {
		for (int i = 0; i < arg.num; i++) {int typ = 0;
		typ = datxFinds(dat1,"R",arg.dep[i]);
		if (typ != identType("Int")) ERROR();
		back[*datxIntz(0,*dat1)][refs[*datxIntz(0,*dat1)]] = sub;
		refs[*datxIntz(0,*dat1)] += 1;}
		break;}
	case (Buffer): break;
	case (Execute): break;
	default: ERROR();}
	freeValve(&arg);
}
void shareNone(int typ, const char *str)
{
}
void shareConst(int typ, const char *str)
{ // note argument constants cannot be Valve
	if (vlds+1 == args || !wrap[vlds+1].vld) {
	fprintf(stderr,"ERROR: argument after Constant should be Fanout or Buffer\n");
	exit(-1);} else {
	int len = 0; hideType(str,&len,typ,wrap[vlds+1].idx);}
}
void shareSyntax(int len, const char *str)
{
	fprintf(stderr,"ERROR: invalid type: %s\n",str);
	fprintf(stderr,"---------------------");
	for (int j = 0; j < len; j++) fprintf(stderr,"-");
	fprintf(stderr,"^\n");
	exit(-1);
}
void shareError(int len, const char *str)
{
	ERROR();
}
int sharePeek(const char *str, int *len)
{
	for (int typ = 0; identSubtype(typ,0)!=-1; typ++) {
		int tmp = 0;
		note = 0;
		hideType(str,&tmp,typ,idx0);
		flushBuf(idx0);
		if (tmp > *len) *len = tmp;
		if (note == 0) return typ;}
	return -1;
}
void shareParse(int argc, char **argv, hktype err, hktype arg, hktype stg)
{
	for (int i = 1, sub = 0; i < argc; i++) {
		int len = 0; int typ = 0;
		if ((typ = sharePeek(argv[i],&len)) < 0) err(len,argv[i]);
		else if (typ != identType("Valve")) arg(typ,argv[i]);
		else stg(sub,argv[i]); sub += 1;}
}
void shareCallback(void *key)
{
	void *dat = 0; int ref = 0; int typ = 0;
	typ = datxFinds(&dat,"R",datxChrz(0,key));
	if (dat == 0) return;
	if (typ != identType("Int")) ERROR();
	ref = *datxIntz(0,dat);
	for (int i = 0; i < refs[ref]; i++) {
	int sub = back[ref][i];
	struct Wrap *ptr = &wrap[sub];
	if (ptr->nxt == args) {ptr->nxt = wake; wake = sub;}}
}
void shareLoop(int src, int dst, int stp, int dtp)
{
	if (stp == dtp) {loopType(stp,src,dst); return;}
	if (stp == identType("Dat") && dtp == identType("Str")) ERROR();
	if (stp == identType("Dat") && dtp == identType("Generic")) ERROR();
	if (stp == identType("Dat")) {readDat(dat0,src); loopType(dtp,idx0,dst); return;}
	if (stp == identType("Str") && dtp == identType("Dat")) {char *str = 0; int len = 0; int typ = 0; readStr(&str,src); datxNone(dat0); typ = sharePeek(str,&len); len = 0; hideType(str,&len,typ,idx0); writeDat(*dat0,dst); free(str); return;}
	if (stp == identType("Str") && dtp == identType("Generic")) {struct Generic gen = {0}; char *str = 0; int len = 0; int typ = 0; readStr(&str,src); typ = sharePeek(str,&len); hideUnion(&gen,typ,str,&len); writeGeneric(&gen,dst); freeGeneric(&gen); free(str); return;}
	if (stp == identType("Str")) {char *str = 0; int len = 0; readStr(&str,src); hideType(str,&len,dtp,dst); free(str); return;}
	if (stp == identType("Generic") && dtp == identType("Dat")) {struct Generic gen = {0}; readGeneric(&gen,src); datxNone(dat0); writeUnion(&gen,idx0); writeDat(*dat0,dst); freeGeneric(&gen); return;}
	if (stp == identType("Generic") && dtp == identType("Str")) {struct Generic gen = {0}; char *str = 0; readGeneric(&gen,src); showUnion(&gen,&str); writeStr(str,dst); freeGeneric(&gen); free(str); return;}
	if (stp == identType("Generic")) {struct Generic gen = {0}; readGeneric(&gen,src); if (identUnion(gen.tag) != dtp) ERROR(); writeUnion(&gen,dst); freeGeneric(&gen); return;}
	if (dtp == identType("Dat")) {datxNone(dat0); loopType(stp,src,idx0); writeDat(*dat0,dst); return;}
	if (dtp == identType("Str")) {char *str = 0; showType(&str,stp,src); writeStr(str,dst); free(str); return;}
	if (dtp == identType("Generic")) {struct Generic gen = {0}; readUnion(&gen,stp,src); writeGeneric(&gen,dst); freeGeneric(&gen); return;}
	ERROR();
}
void shareWrap(struct Wrap *ptr)
{
	switch (ptr->tag) {
	case (Fanout): {
		note = 0; shareLoop(ptr->idx,ptr->dst[ptr->sub]->idx,ptr->inp,ptr->dst[ptr->sub]->out);
		if (note == 0) {if (++ptr->sub == ptr->siz) ptr->sub = 0;} else {
		datxStr(dat1,""); datxStr(dat0,ptr->str); datxInsert(*dat1,*dat0,identType("Str"));}
		break;}
	case (Combine): {
		datxEval(dat0,ptr->exp,ptr->dst[0]->out);
		shareLoop(idx0,ptr->dst[0]->idx,identType("Dat"),ptr->dst[0]->out);
		break;}
	case (Buffer): {
		datxNone(dat0); note = 0; loopType(ptr->inp,ptr->idx,idx0);
		if (note == 0) {datxStr(dat1,ptr->str); datxInsert(*dat1,*dat0,ptr->inp);} else {
		datxStr(dat1,""); datxStr(dat0,ptr->str); datxInsert(*dat1,*dat0,identType("Str"));}
		break;}
	case (Execute): ERROR();
	default: ERROR();}
}
void shareAppend(char **buf, char **nxt, int *lim, int *tot, int siz)
{
	int goon = 1; while (goon) {
	goon = nestPass();
	for (int i = 0; i < siz; i++) {
	const char *str = nestRepl(i);
	int len = strlen(str)+1;
	int ofs = *nxt-*buf;
	while (ofs+len > *lim) {
	if (*lim == 0) *lim = 1; else *lim *= 2;
	*buf = realloc(*buf,*lim); *nxt = *buf + ofs;}
	strcpy(*nxt,str); *nxt += len; *tot += 1;}}
}
void shareArgv(int *argc, char **argv)
{
	int chunks = 0;
	int *sizes = 0; // given strings per chunk
	char *buffer = 0; // reallocatable packed of nestRepl
	char *space = 0; // usable portion of buffer
	int limit = 0; // total size of buffer
	int total = 0; // number of strings in buffer
	char *save = argv[0];
	for (int i = 1; i < *argc; i++) if (strcmp(argv[i],"--") == 0) chunks += 1; chunks += 1;
	sizes = malloc(chunks*sizeof(int)); memset(sizes,0,chunks*sizeof(int)); chunks = 0;
	for (int i = 1; i < *argc; i++) if (strcmp(argv[i],"--") == 0) chunks += 1; else sizes[chunks] += 1; chunks = 0; nestInit(sizes[0]);
	for (int i = 1; i < *argc; i++) if (strcmp(argv[i],"--") == 0) {
	shareAppend(&buffer,&space,&limit,&total,sizes[chunks]); chunks += 1; nestInit(sizes[chunks]); sizes[chunks] = 0;} else {
	nestElem(sizes[chunks],argv[i]); sizes[chunks] += 1;}
	buffer = realloc(buffer,space-buffer); *argc = total + 1;
	*argv = malloc(*argc*sizeof(char*)); argv[0] = save; space = buffer;
	for (int i = 1; i < *argc; i++) {argv[i] = space; space += strlen(space)+1;}
}

int main(int argc, char **argv)
{
	sub0 = datxSub();
	sub1 = datxSub();
	idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp);
	idx1 = puntInit(sub1,sub1,datxReadFp,datxWriteFp);
	dat0 = datxDat(sub0);
	dat1 = datxDat(sub1);
	shareArgv(&argc,argv);
	shareParse(argc,argv,shareSyntax,shareNone,shareArgs); // initialize args
	wrap = malloc((args+1)*sizeof(struct Wrap)); memset(wrap,0,(args+1)*sizeof(struct Wrap));
	wrap[args].idx = openPipe(); wrap[args].out = identType("Str");
	*userIdent(wrap[args].idx) = (void*)(intptr_t)args;
	datxInt(dat1,args); datxInserts("P","",*dat1,identType("Int"));
	shareParse(argc,argv,shareError,shareNone,shareVals); // map strings to subscripts; open filters
	back = malloc(vals*sizeof(int*)); refs = malloc(vals*sizeof(int));
	for (int i = 0; i < vals; i++) {back[i] = 0; refs[i] = 0;}
	shareParse(argc,argv,shareError,shareConst,shareRefs); // count back list sizes; open pipes
	for (int i = 0; i < vals; i++) {back[i] = malloc(refs[i]*sizeof(int)); refs[i] = 0;}
	shareParse(argc,argv,shareError,shareNone,shareBack); // fill in back lists
	datxPrefix("V"); datxChanged(shareCallback);
	noteFunc(shareNote); wake = args; for (int i = 0; i < args; i++) wrap[i].nxt = args;
	while (1) {int sub = 0; int idx = 0;
	if (wake < args) {sub = wake; wake = wrap[sub].nxt; wrap[sub].nxt = args;} else {
	idx = waitRead(0,-1); if (idx == wrap[args].idx) {
	char *str = 0; readStr(&str,wrap[args].idx); printf("%s",str); break;}
	sub = (int)(intptr_t)*userIdent(idx);}
	shareWrap(&wrap[sub]);}
	return 0;
}
