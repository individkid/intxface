enum Configure;
enum Phase;
enum Thread;
enum Memory;
struct Center;
struct Pierce;
typedef void (*vftype)(); // init boot main block safe
typedef int (*zftype)(); // loop
typedef void (*yftype)(enum Thread proc, enum Phase wait); // phase
typedef void (*uftype)(struct Center **center); // copy TODO change to Center *center
typedef void (*wftype)(int pass, struct Center *center); // done
typedef void (*sftype)(enum Configure hint); // wake
typedef int (*tftype)(enum Configure cfg); // info
void planeInit(vftype init, vftype boot, vftype main, zftype loop, zftype block, sftype wake, yftype phase, vftype safe, uftype copy, tftype info);
void planeBoot();
void planeMain();
int planeLoop();
void planeBlock();
void planeWake(enum Configure hint);
void planeSafe(enum Thread proc, enum Phase wait, enum Configure hint);
void planeCopy(struct Center **ptr);
void planeDone(struct Center *ptr);
void planePutstr(const char *str);
float *planeWindow(float *mat);
float *planeMatrix(float *mat);
float *planraMatrix(float *mat);
