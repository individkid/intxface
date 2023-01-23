enum Shader;
enum Configure;
struct Pierce;
struct Center;
typedef void (*vftype)(); // init run
typedef void (*uftype)(struct Center *center); // dma
typedef void (*yftype)(enum Configure hint); // wake
typedef int (*xftype)(enum Configure query); // info
typedef void (*wftype)(enum Shader shader, int start, int stop); // draw
void planeInit(vftype init, vftype run, vftype stop, uftype dma, yftype wake, xftype info, wftype draw);
int planeConfig(enum Configure cfg);
void planeWake(enum Configure hint);
void planeReady(struct Pierce *pierce);
