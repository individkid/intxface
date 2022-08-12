enum Shader;
enum Configure;
struct Pierce;
struct Client;
typedef void (*vftype)(); // init and run
typedef void (*uftype)(struct Client *client); // dma
typedef float (*rftype)(enum Configure query); // info
typedef void (*wftype)(enum Shader shader, int start, int stop); // draw
void planeInit(vftype init, vftype run, uftype dma, vftype wake, rftype info, wftype draw);
void planeArgument(const char *str);
float planeConfig(enum Configure cfg);
void planeWake(enum Configure hint);
void planeReady(struct Pierce *pierce, int size);
