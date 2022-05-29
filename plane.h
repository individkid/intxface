enum Configure {
	PictureMinX,
	PictureMinY,
	PictureWide,
	PictureHigh,
	WindowWide,
	WindowHigh,
};
struct Client;
typedef void (*vftype)();
void planeInit(vftype dma, vftype prep, vftype draw);
float planeConfig(enum Configure cfg);
struct Client *planeClient();