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
typedef void (*vftype)();
typedef void (*wftype)(enum Shader cospace);
typedef void (*uftype)(struct Client *client);
void planeInit(vftype init, uftype dma, wftype draw, wftype prep, wftype comp, wftype test);
float planeConfig(enum Configure cfg);
