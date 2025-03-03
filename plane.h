enum Configure;
enum Thread;
struct Center;
typedef void (*mftype)(enum Thread tag, int idx);
typedef void (*wftype)(struct Center *ptr, int sub);
typedef void (*xftype)(enum Configure cfg, int sav, int val);
typedef void (*nftype)(enum Configure cfg, xftype back);
typedef void (*vftype)(enum Thread thd, int idx, mftype call, mftype done);
typedef int (*yftype)(int *ref, int val);
typedef int (*zftype)(enum Configure cfg, int val, yftype fnc);
typedef const char *(*oftype)(int arg);
void planeInit(wftype copy, nftype call, vftype fork, zftype info, zftype jnfo, zftype knfo, oftype cmnd);
void planeLoop();
void planeDone();
void planePass(struct Center *ptr, int sub);
void planeFail(struct Center *ptr, int sub);
