enum Micro;
enum Configure;
enum Wait;
struct Pierce;
struct Center;
typedef void (*zftype)(); // init
typedef void (*uftype)(struct Center *center); // dma
typedef void (*vftype)(enum Wait wait, enum Configure hint); // safe
typedef void (*yftype)(enum Wait wait); // user
typedef int (*xftype)(enum Configure query); // info
typedef void (*wftype)(enum Micro shader, int start, int stop); // draw
void planeInit(zftype init, uftype dma, vftype safe, yftype user, xftype info, wftype draw);
int planeSet(int idx, const char *str);
int planeConfig(enum Configure cfg);
void planeSafe(enum Wait wait, enum Configure hint);
void planeUser(enum Wait wait, enum Configure hint);
void planeReady(struct Pierce *pierce);
