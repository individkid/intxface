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
#define CMDSIZE 4
// INFINITE longer than any given file
// FILESIZE rough limit to helper file
// NUMOPEN total number of file descriptors
// NUMFILE limit on number of file threads
// BUFSIZE amount read from file at a time
// NUMINET open address port pair limit
// NUMPEND connection request queue length
// CMDSIZE field buffer array limit

typedef void (*wftype)(int);
typedef void (*eftype)(const char*,int,int);
typedef void (*hftype)(const char*,int);
typedef void (*cftype)(const char*,int,void*);
void readNote(eftype exc, int idx);
void readJump(eftype err, int idx);
void writeJump(eftype err, int idx);
void bothJump(eftype err, int idx);
void closeIdent(int idx);
void moveIdent(int idx0, int idx1);
int openPipe();
int openFifo(const char *str);
int openFile(const char *str);
int openInet(const char *adr, const char *num);
int forkExec(const char *exe);
int pipeInit(const char *av1, const char *av2);
int waitAny();
int pauseAny(double dly);
void callInit(wftype fnc, int idx);
int pollPipe(int idx);
int pollFile(int idx);
void seekFile(long long arg, int idx);
void truncFile(int idx);
long long checkFile(int idx);
int pollInet(const char *adr, const char *num);
int checkInet(const char *adr, const char *num);
int rdlkFile(long long arg0, long long arg1, int idx);
int wrlkFile(long long arg0, long long arg1, int idx);
void unlkFile(long long arg0, long long arg1, int idx);
void rdlkwFile(long long arg0, long long arg1, int idx);
void wrlkwFile(long long arg0, long long arg1, int idx);
int checkRead(int idx);
int checkWrite(int idx);
void sleepSec(int sec);
void readStr(cftype fnc, void *arg, int idx);
void readStrHs(hftype fnc, int idx);
char readChr(int idx);
int readInt(int idx);
long long readNew(int idx);
double readNum(int idx);
float readOld(int idx);
void writeStr(const char *arg, int trm, int idx);
void writeChr(char arg, int idx);
void writeInt(int arg, int idx);
void writeNum(double arg, int idx);
void writeNew(long long arg, int idx);
void writeOld(float arg, int idx);
void allocChr(char **ptr, int siz);
void allocInt(int **ptr, int siz);
void allocNew(long long **ptr, int siz);
void allocNum(double **ptr, int siz);
void allocOld(float **ptr, int siz);
void allocStr(char **ptr, const char *str);
void callStr(const char* str, int trm, void*arg);
void allocPtr(void ***ptr, int siz);
void showChr(char **str, char val);
void showInt(char **str, int val);
void showNew(char **str, long long val);
void showNum(char **str, double val);
void showOld(char **str, float val);
void showStr(char **str, char* val);
int hideChr(const char *str, char *val);
int hideInt(const char *str, int *val);
int hideNew(const char *str, long long *val);
int hideNum(const char *str, double *val);
int hideOld(const char *str, float *val);
int hideStr(const char *str, char* *val);
