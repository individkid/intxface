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
struct Text {
	char **str;
	int trm;
};
void debugStr(const char *str);
void exitErr(const char *str, int num, int idx);
void readNote(eftype exc, int idx);
void readJump(eftype err, int idx);
void writeJump(eftype err, int idx);
void closeIdent(int idx);
void moveIdent(int idx0, int idx1);
int findIdent(const char *str);
int inetIdent(const char *adr, const char *num);
int openPipe();
int openFifo(const char *str);
int openAtom(const char *str);
int openFile(const char *str);
int openInet(const char *adr, const char *num);
int forkExec(const char *exe);
int pipeInit(const char *av1, const char *av2);
int puntInit(int rdx, int wdx, pftype rpf, qftype wpf);
int waitAny();
int pauseAny(double dly);
void waitAll();
void callInit(cftype fnc, int idx);
int pollPipe(int idx);
int pollFile(int idx);
void seekFile(long long arg, int idx);
void truncFile(int idx);
long long checkFile(int idx);
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
void allocStr(char* **ptr, int siz);
void assignStr(char **ptr, const char *str);
void callStr(const char *str, int trm, int idx, void *arg);
void textStr(const char *str, int trm, int idx, void *arg);
void readStr(sftype fnc, void *arg, int idx);
void preadStr(sftype fnc, void *arg, long long loc, int idx);
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
int hideIdent(const char *val, const char *str, int *len);
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
