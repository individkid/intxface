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

typedef void (*cftype)(int idx); // thread callback
typedef void (*cgtype)(int idx0, int idx1);
typedef void (*chtype)();
typedef void (*eftype)(const char *str, int num, int idx); // error throws
typedef void (*sftype)(int idx, const char *str); // add string
typedef void (*hftype)(const char *val); // haskell wrapper
typedef void (*hgtype)(int val); // haskell wrapper
typedef void (*hhtype)(double val); // haskell wrapper
typedef void (*hitype)(long long val); // haskell wrapper
typedef void (*hjtype)(float val); // haskell wrapper
typedef void (*hktype)(int len, const char *val); // haskell wrapper
typedef void (*hltype)(int32_t val); // haskell wrapper
typedef int (*pftype)(int fildes, void *buf, int nbyte); // stream to punt to
typedef int (*qftype)(int fildes, const void *buf, int nbyte); // stream to punt to
typedef int (*fgtype)(const char *str, int len, int idx);
typedef int (*fhtype)(int idx, const char *str);
typedef int (*fftype)(const char *str);
typedef int (*gftype)(const char *one, const char *oth);
typedef int (*oftype)(void *arg);
typedef int (*rftype)(int arg); // permutation
typedef int (*rgtype)();
typedef const char *(*rhtype)(int i);
typedef const char *(*aftype)(void *mem);
typedef const char *(*bftype)(const char *arg); // dictionary
typedef const char *(*bgtype)();
typedef const char *(*bhtype)(int *len);
typedef void (*nftype)(void **use, const char *str);
typedef void (*mftype)(void **run, void *use);
typedef void (*dftype)(void **mem);
typedef void (*iftype)(void **mem, int key);
typedef void (*igtype)(void **mem, void *giv, int key);
typedef void *(*jftype)(void *giv, void *key);
typedef void *(*kftype)(void *giv, int key);
typedef void *(*tftype)(int idx);
typedef int (*lftype)(void **mem);

struct Function {
	enum {
		Cftype,
		Cgtype,
		Chtype,
		Eftype,
		Sftype,
		Hftype,
		Pftype,
		Qftype,
		Fgtype,
		Fhtype,
		Fftype,
		Gftype,
		Oftype,
		Rftype,
		Rgtype,
		Rhtype,
		Aftype,
		Bftype,
		Bgtype,
		Nftype,
		Mftype,
		Dftype,
		Iftype,
		Igtype,
		Jftype,
		Kftype,
		Tftype,
		Lftype,
	} ft;
	union {
		cftype cf;
		cgtype cg;
		chtype ch;
		eftype ef;
		sftype sf;
		hftype hf;
		pftype pf;
		qftype qf;
		fgtype fg;
		fhtype fh;
		fftype ff;
		gftype gf;
		oftype of;
		rftype rf;
		rgtype rg;
		rhtype rh;
		aftype af;
		bftype bf;
		bgtype bg;
		nftype nf;
		mftype mf;
		dftype df;
		iftype it;
		igtype ig;
		jftype jf;
		kftype kf;
		tftype tf;
		lftype lf;
		void *vp;
	};
};
struct Function protoTypeCf(cftype fnc);
struct Function protoTypeCg(cgtype fnc);
struct Function protoTypeCh(chtype fnc);
struct Function protoTypeSf(sftype fnc);
struct Function protoTypeHf(hftype fnc);
struct Function protoTypeFg(fgtype fnc);
struct Function protoTypeFh(fhtype fnc);
struct Function protoTypeFf(fftype fnc);
struct Function protoTypeGf(gftype fnc);
struct Function protoTypeOf(oftype fnc);
struct Function protoTypeRf(rftype fnc);
struct Function protoTypeRg(rgtype fnc);
struct Function protoTypeRh(rhtype fnc);
struct Function protoTypeAf(aftype fnc);
struct Function protoTypeBf(bftype fnc);
struct Function protoTypeBg(bgtype fnc);
struct Function protoTypeNf(nftype fnc);
struct Function protoTypeMf(mftype fnc);
struct Function protoTypeDf(dftype fnc);
struct Function protoTypeIf(iftype fnc);
struct Function protoTypeIg(igtype fnc);
struct Function protoTypeJf(jftype fnc);
struct Function protoTypeKf(kftype fnc);
struct Function protoTypeTf(tftype fnc);
struct Function protoTypeLf(lftype fnc);

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
const struct Closure *protoCloseRf(int arg);
const struct Closure *protoCloseRg();
const struct Closure *protoCloseBf(const char *arg);
const struct Closure *protoCloseBg();
const struct Closure *protoCloseBh();
void protoResultCf();
void protoResultEf();
int protoResultPf(void *buf);
int protoResultQf();
int protoResultGf();
int protoResultRf();
int protoResultRg();
const char *protoResultBf();
const char *protoResultBg();
const char *protoResultBh(int *len);

void stackErr();
void exitErr(const char *file, int line);
void protoSet(const char *str);
const char *protoGet(int i);
void protoErr(const char *fmt, ...);
const char *protoMsg();
int protoPath(const char *exp);
#endif
