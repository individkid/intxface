enum Micro;
enum Configure;
enum Wait;
enum Proc;
struct Pierce;
struct Center;
typedef void (*zftype)(); // init
typedef void (*uftype)(struct Center *center); // dma
typedef void (*vftype)(); // safe
typedef void (*yftype)(enum Proc proc, enum Wait wait); // main
typedef int (*xftype)(enum Configure query); // info
typedef void (*wftype)(enum Micro shader, int start, int stop); // draw
void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw);
int planeSet(int idx, const char *str);
int planeConfig(enum Configure cfg);
void planeSafe(enum Proc, enum Wait wait, enum Configure hint);
void planeMain();
void planeReady(struct Pierce *pierce);
