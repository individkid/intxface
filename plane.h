enum Configure;
enum Thread;
struct Center;
#ifdef MAYBE
#else
struct Response {
	int res;
	int mod;
	int idx;
	struct Center *ptr;
	void (*fnc)(struct Response);
};
#endif
typedef void (*mftype)(enum Thread tag, int idx);
#ifdef MAYBE
typedef void (*wftype)(struct Center *ptr);
#else
typedef void (*wftype)(struct Response);
#endif
typedef void (*xftype)(enum Configure cfg, int sav, int val);
typedef void (*nftype)(enum Configure cfg, xftype back);
typedef void (*vftype)(enum Thread thd, int idx, mftype call, mftype done);
typedef int (*yftype)(int *ref, int val);
typedef int (*zftype)(enum Configure cfg, int val, yftype fnc);
typedef const char *(*oftype)(int arg);
void planeInit(wftype copy, nftype call, vftype fork, zftype info, zftype jnfo, zftype knfo, oftype cmnd);
void planeLoop();
void planeDone();
