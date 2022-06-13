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
typedef void (*vftype)();
typedef void (*wftype)(enum Shader cospace, struct Ranje *range);
typedef void (*uftype)(struct Client *client);
void planeInit(vftype init, uftype dma, wftype draw);
float planeConfig(enum Configure cfg);
