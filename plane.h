enum Configure {
	PictureMinX,
	PictureMinY,
	PictureWide,
	PictureHigh,
	WindowWide,
	WindowHigh,
};
struct Client;
enum Shader;
struct Ranje;
struct Pierce;
typedef void (*vftype)();
typedef void (*wftype)(enum Shader shader);
typedef void (*uftype)(struct Client *client);
void planeInit(vftype init, uftype dma, wftype draw);
float planeConfig(enum Configure cfg);
void planeWake(int count);
void planeReady(struct Pierce *pierce, int size);
void planeEmpty();
