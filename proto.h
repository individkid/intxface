#ifndef PROTO_H
#define PROTO_H
#include <stdint.h>
#define ERROR() exitErr(__FILE__,__LINE__)
#define SATURATE 1000000000000.0
#define SEC2NANO 1000000000.0
#define NANO2SEC 0.000000001
#define BACKOFF 0.001
#define CALLRATE 44100
#define FILESIZE 4096
#define NUMPOOL 1024
#define NUMOPEN 1024
#define NUMARGX 256
#define NUMFILE 64
#define BUFSIZE 64
#define NUMINET 16
#define NUMPEND 10
// SATURATE largest stock value
// SEC2NANO for pselect delay
// NANO2SEC for timewheel stamp
// BACKOFF file process livelock
// CALLRATE portaudio sample rate
// FILESIZE rough limit to helper file
// NUMPOOL number of pending stock changes
// NUMOPEN total number of file descriptors
// NUMARGX number of argument types
// NUMFILE limit on number of file threads
// BUFSIZE amount read from file at a time
// NUMINET open address port pair limit
// NUMPEND connection request queue length

typedef int (*pftype)(int fildes, void *buf, int nbyte);
typedef int (*qftype)(int fildes, const void *buf, int nbyte);
typedef void (*eftype)(const char *str, int num, int idx);
typedef int (*fgtype)(const char *str, int len, int idx);
typedef void (*cgtype)(int idx0, int idx1);
typedef void (*hktype)(int len, const char *val);
typedef int (*gftype)(const char *one, const char *oth);

typedef void (*chtype)();
typedef void (*hmtype)(char val);
typedef void (*hgtype)(int val);
typedef void (*hltype)(int32_t val);
typedef void (*hitype)(long long val);
typedef void (*hjtype)(float val);
typedef void (*hhtype)(double val);
typedef void (*hftype)(const char *val);

typedef int (*tmtype)();
typedef int (*tltype)(int arg);
typedef int (*fftype)(const char *str);

typedef int (*ggtype)(int rfd, int wfd);
typedef int (*tftype)(double dly, int msk);
typedef int (*thtype)(long long loc, long long siz, int idx);
typedef void (*tjtype)(long long loc, long long siz, int idx);

typedef void (*setype)(char **str, int idx);
typedef void (*sftype)(char **str, int len, int idx, int loc);
typedef void (*sgtype)(char **str, long long loc, int idx);
typedef void (*shtype)(void **dat, int idx);

typedef char (*sitype)(int idx);
typedef int (*sjtype)(int idx);
typedef int32_t (*sktype)(int idx);
typedef double (*sltype)(int idx);
typedef long long (*smtype)(int idx);
typedef float (*sntype)(int idx);

typedef void (*lftype)(const char *arg, int idx);
typedef void (*lgtype)(const char *arg, long long loc, int idx);
typedef void (*lhtype)(const void *arg, int idx);
typedef void (*litype)(char arg, int idx);
typedef void (*ljtype)(int arg, int idx);
typedef void (*lktype)(int32_t arg, int idx);
typedef void (*lltype)(double arg, int idx);
typedef void (*lmtype)(long long arg, int idx);
typedef void (*lntype)(float arg, int idx);

typedef void (*mftype)(const char* val, char **str);
typedef void (*mhtype)(const void* val, char **str);
typedef void (*mitype)(char val, char **str);
typedef void (*mjtype)(int val, char **str);
typedef void (*mktype)(int32_t val, char **str);
typedef void (*mltype)(double val, char **str);
typedef void (*mmtype)(long long val, char **str);
typedef void (*mntype)(float val, char **str);
typedef void (*motype)(const char *typ, const char* val, char **str);
typedef void (*mptype)(const char* val, char **str);
typedef void (*mqtype)(char **str);

typedef int (*nftype)(char **val, const char *str, int *len);
typedef int (*nhtype)(void **val, const char *str, int *siz);
typedef int (*nitype)(char *val, const char *str, int *siz);
typedef int (*njtype)(int *val, const char *str, int *siz);
typedef int (*nktype)(int32_t *val, const char *str, int *siz);
typedef int (*nltype)(double *val, const char *str, int *siz);
typedef int (*nmtype)(long long *val, const char *str, int *siz);
typedef int (*nntype)(float *val, const char *str, int *siz);
typedef int (*notype)(const char* typ, const char *val, const char *str, int *siz);
typedef int (*nptype)(const char *val, const char *str, int *siz);
typedef int (*nqtype)(const char *str, int *siz);

typedef const char *(*rjtype)(int i);
typedef void (*rktype)(void *dat);
typedef int (*rltype)(void **dat, const char *str);
typedef void (*rmtype)(void *arg, int idx);
typedef int (*rntype)(int idx, const char *str);
typedef void (*rptype)(const char *src, int len, int idx, int loc);
typedef void (*rqtype)(int len, int idx, int loc);
typedef void (*rrtype)(char **val, const char *key);
typedef void (*rstype)(const char *key, const char *val);

struct Function {
	enum {

		Pftype, // 0
		Qftype,
		Eftype,
		Fgtype,
		Cgtype,
		Hktype,
		Gftype, // 6

		Chtype, // 7
		Hmtype,
		Hgtype,
		Hltype,
		Hitype,
		Hjtype,
		Hhtype,
		Hftype, // 14

		Tmtype, // 15
		Tltype,
		Fftype, // 17

		Ggtype, // 18
		Tftype,
		Thtype,
		Tjtype, // 21

		Setype, // 22
		Sftype,
		Sgtype,
		Shtype,
		Sitype,
		Sjtype,
		Sktype,
		Sltype,
		Smtype,
		Sntype, // 31

		Lftype, // 32
		Lgtype,
		Lhtype,
		Litype,
		Ljtype,
		Lktype,
		Lltype,
		Lmtype,
		Lntype, // 40

		Mftype, // 41
		Mhtype,
		Mitype,
		Mjtype,
		Mktype,
		Mltype,
		Mmtype,
		Mntype,
		Motype,
		Mptype,
		Mqtype, // 51

		Nftype, // 52
		Nhtype,
		Nitype,
		Njtype,
		Nktype,
		Nltype,
		Nmtype,
		Nntype,
		Notype,
		Nptype,
		Nqtype, // 62

		Rjtype, // 63
		Rktype,
		Rltype,
		Rmtype,
		Rntype,
		Rptype,
		Rqtype,
		Rrtype,
		Rstype,

	} ft;
	union {

		pftype pf;
		qftype qf;
		eftype ef;
		fgtype fg;
		cgtype cg;
		hktype hk;
		gftype gf;

		chtype ch;
		hmtype hm;
		hgtype hg;
		hltype hl;
		hitype hi;
		hjtype hj;
		hhtype hh;
		hftype hf;

		tmtype tm;
		tltype tl;
		fftype ff;

		ggtype gg;
		tftype tf;
		thtype th;
		tjtype tj;

		setype se;
		sftype sf;
		sgtype sg;
		shtype sh;
		sitype si;
		sjtype sj;
		sktype sk;
		sltype sl;
		smtype sm;
		sntype sn;

		lftype lf;
		lgtype lg;
		lhtype lh;
		litype li;
		ljtype lj;
		lktype lk;
		lltype ll;
		lmtype lm;
		lntype ln;

		mftype mf;
		mhtype mh;
		mitype mi;
		mjtype mj;
		mktype mk;
		mltype ml;
		mmtype mm;
		mntype mn;
		motype mo;
		mptype mp;
		mqtype mq;

		nftype nf;
		nhtype nh;
		nitype ni;
		njtype nj;
		nktype nk;
		nltype nl;
		nmtype nm;
		nntype nn;
		notype no;
		nptype np;
		nqtype nq;

		rjtype rj;
		rktype rk;
		rltype rl;
		rmtype rm;
		rntype rn;
		rptype rp;
		rqtype rq;
		rrtype rr;
		rstype rs;

		void *vp;
	};
};

struct Function protoTypePf(pftype fnc);
struct Function protoTypeQf(qftype fnc);
struct Function protoTypeEf(eftype fnc);
struct Function protoTypeFg(fgtype fnc);
struct Function protoTypeCg(cgtype fnc);
struct Function protoTypeHk(hktype fnc);
struct Function protoTypeGf(gftype fnc);

struct Function protoTypeCh(chtype fnc);
struct Function protoTypeHm(hmtype fnc);
struct Function protoTypeHg(hgtype fnc);
struct Function protoTypeHl(hltype fnc);
struct Function protoTypeHi(hitype fnc);
struct Function protoTypeHj(hjtype fnc);
struct Function protoTypeHh(hhtype fnc);
struct Function protoTypeHf(hftype fnc);

struct Function protoTypeTm(tmtype fnc);
struct Function protoTypeTl(tltype fnc);
struct Function protoTypeFf(fftype fnc);

struct Function protoTypeGg(ggtype fnc);
struct Function protoTypeTf(tftype fnc);
struct Function protoTypeTh(thtype fnc);
struct Function protoTypeTj(tjtype fnc);

struct Function protoTypeSe(setype fnc);
struct Function protoTypeSf(sftype fnc);
struct Function protoTypeSg(sgtype fnc);
struct Function protoTypeSh(shtype fnc);
struct Function protoTypeSi(sitype fnc);
struct Function protoTypeSj(sjtype fnc);
struct Function protoTypeSk(sktype fnc);
struct Function protoTypeSl(sltype fnc);
struct Function protoTypeSm(smtype fnc);
struct Function protoTypeSn(sntype fnc);

struct Function protoTypeLf(lftype fnc);
struct Function protoTypeLg(lgtype fnc);
struct Function protoTypeLh(lhtype fnc);
struct Function protoTypeLi(litype fnc);
struct Function protoTypeLj(ljtype fnc);
struct Function protoTypeLk(lktype fnc);
struct Function protoTypeLl(lltype fnc);
struct Function protoTypeLm(lmtype fnc);
struct Function protoTypeLn(lntype fnc);

struct Function protoTypeMf(mftype fnc);
struct Function protoTypeMh(mhtype fnc);
struct Function protoTypeMi(mitype fnc);
struct Function protoTypeMj(mjtype fnc);
struct Function protoTypeMk(mktype fnc);
struct Function protoTypeMl(mltype fnc);
struct Function protoTypeMm(mmtype fnc);
struct Function protoTypeMn(mntype fnc);
struct Function protoTypeMo(motype fnc);
struct Function protoTypeMp(mptype fnc);
struct Function protoTypeMq(mqtype fnc);

struct Function protoTypeNf(nftype fnc);
struct Function protoTypeNh(nhtype fnc);
struct Function protoTypeNi(nitype fnc);
struct Function protoTypeNj(njtype fnc);
struct Function protoTypeNk(nktype fnc);
struct Function protoTypeNl(nltype fnc);
struct Function protoTypeNm(nmtype fnc);
struct Function protoTypeNn(nntype fnc);
struct Function protoTypeNo(notype fnc);
struct Function protoTypeNp(nptype fnc);
struct Function protoTypeNq(nqtype fnc);

struct Function protoTypeRj(rjtype fnc);
struct Function protoTypeRk(rktype fnc);
struct Function protoTypeRl(rltype fnc);
struct Function protoTypeRm(rmtype fnc);
struct Function protoTypeRn(rntype fnc);
struct Function protoTypeRp(rptype fnc);
struct Function protoTypeRq(rqtype fnc);
struct Function protoTypeRr(rrtype fnc);
struct Function protoTypeRs(rstype fnc);

struct Parameter {
	enum {
		Iatype,
		Satype,
		Latype,
		Patype,
	} at;
	union {
		int ia;
		char *sa;
		void *pa;
	};
	int la;
};
void protoMake(struct Parameter *arg);
void protoMakeIf(struct Parameter *arg, int val);
void protoMakeSf(struct Parameter *arg, const char *val);
void protoMakeLf(struct Parameter *arg, const void *val, int len);
void protoMakePf(struct Parameter *arg, void *val);

struct Closure {
	int na,nb;
	struct Parameter *aa,*ab;
};
const struct Closure *protoClose(int na, int nb);
const struct Closure *protoCloseCf(int idx);
const struct Closure *protoCloseEf(const char *str, int num, int idx);
const struct Closure *protoClosePf(int idx, int nbyte);
const struct Closure *protoCloseQf(int idx, const void *buf, int nbyte);
const struct Closure *protoCloseGf(const char *one, const char *oth);
const struct Closure *protoCloseFf(const char *str);
const struct Closure *protoCloseGg(int rfd, int wfd);
const struct Closure *protoCloseRi();
const struct Closure *protoCloseRo();
void protoResultCf();
void protoResultEf();
int protoResultPf(void *buf);
int protoResultQf();
int protoResultGf();
int protoResultFf();
int protoResultGg();
int protoResultRi();
const char *protoResultRo();

void stackErr();
void exitErr(const char *file, int line);
void protoSet(const char *str);
const char *protoGet(int i);
void protoErr(const char *fmt, ...);
const char *protoMsg();
int protoPath(const char *exp);
#endif
