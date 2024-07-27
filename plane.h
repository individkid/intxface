enum Micro;
enum Configure;
enum Phase;
enum Thread;
enum Memory;
struct Center;
typedef void (*vftype)(); // init safe boot main loop
typedef int (*zftype)(); // check block
typedef void (*yftype)(enum Thread proc, enum Phase wait); // wait
typedef void (*uftype)(struct Center **center); // copy
typedef struct Center *(*rftype)(enum Memory mem); // ready
typedef void (*xftype)(struct Center *ptr); // done
typedef void (*sftype)(enum Configure hint); // wake
typedef int (*tftype)(enum Configure cfg); // info
float *planeWindow(float *mat);
float *planeMatrix(float *mat);
void planeInit(vftype init, vftype safe, vftype boot, vftype main, zftype loop, zftype block,//)
	yftype wait, uftype copy, rftype ready, xftype done, sftype wake, tftype info);
void planeBoot();
void planeMain();
int planeLoop();
void planeWake(enum Configure hint);
void planraWake(enum Configure hint);
void planeSafe(enum Thread proc, enum Phase wait, enum Configure hint);
void planeCopy(struct Center **ptr);
void planeReady(struct Center *ptr);
void planeDone(struct Center *ptr);
void planePutstr(const char *str);
float *planeMatrix(float *mat);
float *planraMatrix(float *mat);
void planraCenter();
