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
typedef int (*lftype)(int *val, const char *typ, const char *str, int *siz);
typedef int (*fftype)(const char *str);
typedef int (*gftype)(const char *one, const char *oth);
typedef int (*oftype)(void *arg);
typedef void (*nftype)(void **use, const char *str);
typedef void (*mftype)(void **run, void *use);

struct Prototype {
	enum {
		Cftype,
		Eftype,
		Sftype,
		Hftype,
		Pftype,
		Qftype,
		Lftype,
		Fftype,
		Gftype,
		Oftype,
		Nftype,
		Mftype,
	} ft;
	union {
		cftype cf;
		eftype ef;
		sftype sf;
		hftype hf;
		pftype pf;
		qftype qf;
		lftype lf;
		fftype ff;
		gftype gf;
		oftype of;
		nftype nf;
		mftype mf;
		void *vp;
	};
};
struct Prototype protoTypeF(fftype fnc);
struct Prototype protoTypeG(gftype fnc);
struct Prototype protoTypeO(oftype fnc);
struct Prototype protoTypeN(nftype fnc);
struct Prototype protoTypeM(mftype fnc);

void exitErr(const char *str, int num, int idx);
void setExestr(const char *str);
const char *getExedir(int i);
#endif
