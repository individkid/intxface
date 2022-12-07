#ifndef PROTO_H
#define PROTO_H
#define ERROR(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}
#define NOTICE(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}
#define SATURATE 1000000000000.0
#define SEC2NANO 1000000000.0
#define NANO2SEC 0.000000001
#define BACKOFF 0.001
#define CALLRATE 44100
#define FILESIZE 4096
#define NUMOPEN 1024
#define NUMPOOL 1024
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
// NUMOPEN total number of file descriptors
// NUMPOOL number of pending stock changes
// NUMFILE limit on number of file threads
// BUFSIZE amount read from file at a time
// NUMINET open address port pair limit
// NUMPEND connection request queue length

typedef void (*cftype)(int idx); // thread callback
typedef void (*eftype)(const char *str, int num, int idx); // error throws
typedef void (*sftype)(const char *str, int trm, int idx, void *arg); // string callback
typedef void (*hftype)(const char *str, int trm); // haskell string wrapper
typedef int (*pftype)(int fildes, void *buf, int nbyte); // stream to punt to
typedef int (*qftype)(int fildes, const void *buf, int nbyte); // stream to punt to
typedef int (*fftype)(const char *str);
typedef int (*gftype)(const char *one, const char *oth);
typedef int (*oftype)(void *arg);
typedef int (*rftype)(int arg); // permutation
typedef const char *(*aftype)(void *mem);
typedef const char *(*bftype)(const char *arg); // dictionary
typedef void (*nftype)(void **use, const char *str);
typedef void (*mftype)(void **run, void *use);
typedef void (*dftype)(void **mem);
typedef void (*iftype)(void **mem, int key);
typedef void (*jftype)(void **mem, void *giv, void *key);
typedef void (*kftype)(void **mem, void *giv, int key);
typedef void *(*tftype)(int idx);
typedef int (*lftype)(void **mem);

struct Function {
	enum {
		Cftype,
		Eftype,
		Sftype,
		Hftype,
		Pftype,
		Qftype,
		Fftype,
		Gftype,
		Oftype,
		Nftype,
		Mftype,
		Dftype,
		Rftype,
		Aftype,
		Bftype,
		Iftype,
		Jftype,
		Kftype,
		Tftype,
		Lftype,
	} ft;
	union {
		cftype cf;
		eftype ef;
		sftype sf;
		hftype hf;
		pftype pf;
		qftype qf;
		fftype ff;
		gftype gf;
		oftype of;
		nftype nf;
		mftype mf;
		dftype df;
		rftype rf;
		aftype af;
		bftype bf;
		iftype it;
		jftype jf;
		kftype kf;
		tftype tf;
		lftype lf;
		void *vp;
	};
};
struct Function protoTypeF(fftype fnc);
struct Function protoTypeG(gftype fnc);
struct Function protoTypeO(oftype fnc);
struct Function protoTypeN(nftype fnc);
struct Function protoTypeM(mftype fnc);
struct Function protoTypeD(dftype fnc);
struct Function protoTypeA(aftype fnc);
struct Function protoTypeI(iftype fnc);
struct Function protoTypeJ(jftype fnc);
struct Function protoTypeK(kftype fnc);
struct Function protoTypeT(tftype fnc);
struct Function protoTypeL(lftype fnc);

struct Argument {
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
void protoMake(struct Argument *arg);
void protoMakeI(struct Argument *arg, int val);
void protoMakeS(struct Argument *arg, const char *val);
void protoMakeL(struct Argument *arg, const void *val, int len);
void protoMakeP(struct Argument *arg, void *val);

struct Closure {
	int na,nb;
	struct Argument *aa,*ab;
};
const struct Closure *protoClose(int na, int nb);
const struct Closure *protoCloseR(int arg);
const struct Closure *protoCloseB(const char *arg);
const struct Closure *protoCloseP(int idx, int nbyte);
const struct Closure *protoCloseQ(int idx, const void *buf, int nbyte);
int protoResultR();
const char *protoResultB();
int protoResultP(void *buf);
int protoResultQ();

void exitErr(const char *str, int num, int idx);
void protoSet(const char *str);
const char *protoGet(int i);
void protoErr(const char *fmt, ...);
const char *protoMsg();
int protoPath(const char *exp);
#endif
