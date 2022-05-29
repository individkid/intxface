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
typedef void (*dftype)(struct Client *client);
void planeInit(dftype dma, vftype prep, vftype draw);
float planeConfig(enum Configure cfg);
