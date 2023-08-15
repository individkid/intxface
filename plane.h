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
typedef void (*wftype)(enum Micro shader, int base, int limit); // draw
void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw);
void planeAddstr(const char *str);
int planeInfo(enum Configure cfg);
void planeSafe(enum Proc proc, enum Wait wait, enum Configure hint);
void planeMain();
void planeReady(struct Pierce *pierce);
