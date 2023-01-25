enum Shader;
enum Configure;
enum Wait;
struct Pierce;
struct Center;
typedef void (*uftype)(struct Center *center); // dma
typedef void (*vftype)(enum Wait wait); // wait
typedef void (*yftype)(enum Configure hint); // wake
typedef int (*xftype)(enum Configure query); // info
typedef void (*wftype)(enum Shader shader, int start, int stop); // draw
void planeInit(uftype dma, vftype wait, yftype wake, xftype info, wftype draw);
int planeConfig(enum Configure cfg);
void planeWait(enum Wait wait);
void planeWake(enum Configure hint);
void planeReady(struct Pierce *pierce);
