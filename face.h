#define ERROR(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}
#define NOTICE(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}
#define INVALID 1.0e30
#define ANGLE 0.0333
#define LENGTH 10.0
#define POWER 0.01
#define SATURATE 1000000000000.0
#define INFINITE 1000000000ull
#define SEC2NANO 1000000000.0
#define NANO2SEC 0.000000001
#define BACKOFF 0.001
#define CALLRATE 44100
#define FILESIZE 4096
#define NUMOPEN 1024
#define NUMPOOL 1024
#define WINLONG 1024
#define WINWIDE 512
#define WINHIGH 512
#define WINDEEP 512
#define WINSTOP 128
#define TIPDEEP 128
#define NUMFILE 64
#define BUFSIZE 64
#define NUMINET 16
#define NUMPEND 10
// INFINITE longer than any given file
// FILESIZE rough limit to helper file
// NUMOPEN total number of file descriptors
// NUMFILE limit on number of file threads
// BUFSIZE amount read from file at a time
// NUMINET open address port pair limit
// NUMPEND connection request queue length

struct Text {
	char **str;
	int trm;
};
typedef void (*wftype)(int);
typedef void (*eftype)(const char*,int,int);
typedef void (*hftype)(const char*,int);
typedef void (*cftype)(const char*,int,int,void*);
void debugStr(const char *str);
void exitErr(const char *str, int num, int idx);
void readNote(eftype exc, int idx);
void readJump(eftype err, int idx);
void writeJump(eftype err, int idx);
void closeIdent(int idx);
void moveIdent(int idx0, int idx1);
int findIdent(const char *str);
int openPipe();
int openFifo(const char *str);
int openAtom(const char *str);
int openFile(const char *str);
int openInet(const char *adr, const char *num);
int forkExec(const char *exe);
int pipeInit(const char *av1, const char *av2);
int waitAny();
int pauseAny(double dly);
void waitAll();
void callInit(wftype fnc, int idx);
int pollPipe(int idx);
int pollFile(int idx);
void seekFile(long long arg, int idx);
void truncFile(int idx);
long long checkFile(int idx);
int pollInet(const char *adr, const char *num);
int checkInet(const char *adr, const char *num);
int rdlkFile(long long loc, long long siz, int idx);
int wrlkFile(long long loc, long long siz, int idx);
void unlkFile(long long loc, long long siz, int idx);
void rdlkwFile(long long loc, long long siz, int idx);
void wrlkwFile(long long loc, long long siz, int idx);
int checkRead(int idx);
int checkWrite(int idx);
void sleepSec(double sec);
void allocMark();
void allocKeep();
void allocDrop();
void allocMem(void **ptr, int siz);
void allocChr(char **ptr, int siz);
void allocInt(int **ptr, int siz);
void allocNew(long long **ptr, int siz);
void allocNum(double **ptr, int siz);
void allocOld(float **ptr, int siz);
void allocStr(char **ptr, const char *str);
void callStr(const char *str, int trm, int idx, void *arg);
void textStr(const char *str, int trm, int idx, void *arg);
void readStr(cftype fnc, void *arg, int idx);
void preadStr(cftype fnc, void *arg, long long loc, int idx);
void readStrHs(hftype fnc, int idx);
char readChr(int idx);
int readInt(int idx);
long long readNew(int idx);
double readNum(int idx);
float readOld(int idx);
int writeBuf(const void *arg, long long siz, int idx);
void flushBuf(int idx);
void writeStr(const char *arg, int trm, int idx);
void pwriteStr(const char *arg, int trm, long long loc, int idx);
void writeChr(char arg, int idx);
void writeInt(int arg, int idx);
void writeNum(double arg, int idx);
void writeNew(long long arg, int idx);
void writeOld(float arg, int idx);
void showEnum(const char *typ, const char* val, char **str, int *len);
void showStruct(const char* bef, int val, const char *aft, char **str, int *len);
void showField(const char* val, char **str, int *len);
void showOpen(const char* val, char **str, int *len);
void showClose(char **str, int *len);
void showChr(char val, char **str, int *len);
void showInt(int val, char **str, int *len);
void showNew(long long val, char **str, int *len);
void showNum(double val, char **str, int *len);
void showOld(float val, char **str, int *len);
void showStr(const char* val, char **str, int *len);
int hideEnum(const char* typ, const char *val, const char *str, int *len);
int hideStruct(const char* bef, int val, const char *aft, const char *str, int *len);
int hideField(const char *val, const char *str, int *len);
int hideOpen(const char *val, const char *str, int *len);
int hideClose(const char *str, int *len);
int hideChr(char *val, const char *str, int *len);
int hideInt(int *val, const char *str, int *len);
int hideNew(long long *val, const char *str, int *len);
int hideNum(double *val, const char *str, int *len);
int hideOld(float *val, const char *str, int *len);
int hideStr(char* *val, const char *str, int *len);
