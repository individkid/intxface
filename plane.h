enum Micro;
enum Configure;
enum Wait;
enum Thread;
enum Memory;
struct Center;
typedef void (*vftype)(); // init safe boot
typedef void (*yftype)(enum Thread proc, enum Wait wait); // main
typedef void (*uftype)(struct Center *center); // dma
typedef void (*wftype)(enum Micro shader, int base, int limit); // draw
typedef struct Center *(*rftype)(enum Memory mem); // ready
typedef void (*xftype)(struct Center *ptr); // done
typedef void (*sftype)(enum Configure hint); // atom wake
typedef int (*tftype)(enum Configure cfg); // info
float *planeWindow(float *mat);
float *planeMatrix(float *mat);
void planeInit(vftype init, vftype safe, vftype boot, yftype main, uftype dma, wftype draw, rftype ready, xftype done, sftype atom, sftype wake, tftype info);
void planePutstr(const char *str);
void planeReady(struct Center *ptr);
void planeDone(struct Center *ptr);
void planeSafe(enum Thread proc, enum Wait wait, enum Configure hint);
void planeMain();
void planeWake(enum Configure hint);
void planeBoot();
void planraWake(enum Configure hint);
void planraBoot();
float *planraMatrix(float *mat);
