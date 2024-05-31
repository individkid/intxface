enum Micro;
enum Configure;
enum Wait;
enum Thread;
struct Pierce;
struct Center;
typedef void (*zftype)(); // init
typedef void (*vftype)(); // safe
typedef void (*yftype)(enum Thread proc, enum Wait wait); // main
typedef void (*uftype)(struct Center *center); // dma
typedef void (*bftype)(int loc, int siz, void *ptr); // bind
typedef void (*wftype)(enum Micro shader, int base, int limit); // draw
typedef void *(*rftype)(int *siz, int *tag); // ready
typedef void (*xftype)(int tag); // done
typedef void (*sftype)(enum Configure hint); // wake
typedef int (*tftype)(enum Configure cfg); // info
float *planeWindow(float *mat);
float *planeMatrix(float *mat);
void planeInit(zftype init, vftype safe, yftype main, uftype dma, wftype draw, bftype bind, rftype ready, xftype done, sftype wake, tftype info, zftype boot);
void planePutstr(const char *str);
void planeDone(struct Center *ptr);
void planeSafe(enum Thread proc, enum Wait wait, enum Configure hint);
void planeMain();
void planeWake(enum Configure hint);
void planeBoot();
void planraWake(enum Configure hint);
void planraBoot();
float *planraMatrix(float *mat);
