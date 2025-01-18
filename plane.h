enum Configure;
enum Thread;
struct Center;
struct Response {
	int res;
	int idx;
	struct Center *ptr;
	void (*fnc)(struct Response);
};
typedef void (*xftype)(enum Configure, int sav, int val);
typedef void (*mftype)(enum Thread tag, int idx);
typedef void (*wftype)(struct Response);
typedef void (*oftype)(enum Configure cfg, xftype back);
typedef void (*vftype)(enum Thread thd, int idx, mftype call, mftype done);
typedef int (*yftype)(int *ref, int val);
typedef int (*zftype)(enum Configure cfg, int val, yftype fnc);
void planeInit(wftype copy, oftype call, vftype fork, wftype pass, zftype info, zftype jnfo, zftype knfo);
void planeLoop();
void planeDone();
