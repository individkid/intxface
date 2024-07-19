enum Micro;
enum Configure;
enum Wait;
enum Thread;
enum Memory;
struct Center;
typedef void (*vftype)(); // init safe
typedef int (*zftype)(); // boot
typedef void (*yftype)(enum Thread proc, enum Wait wait); // main
typedef void (*uftype)(struct Center **center); // copy
typedef struct Center *(*rftype)(enum Memory mem); // ready
typedef void (*xftype)(struct Center *ptr); // done
typedef void (*sftype)(enum Configure hint); // wake
typedef int (*tftype)(enum Configure cfg); // info
float *planeWindow(float *mat);
float *planeMatrix(float *mat);
void planeInit(vftype init, vftype safe, vftype boot, yftype main, uftype copy, rftype ready, xftype done, sftype wake, tftype info);
void planeSafe(enum Thread proc, enum Wait wait, enum Configure hint);
void planeBoot();
int planeMain();
void planeCopy(struct Center **ptr);
void planeReady(struct Center *ptr);
void planeDone(struct Center *ptr);
void planeWake(enum Configure hint);
void planraWake(enum Configure hint);
void planePutstr(const char *str);
float *planraMatrix(float *mat);
void planraCenter();
