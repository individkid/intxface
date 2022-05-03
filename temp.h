enum Shader {
	Display,Track,Shaders
};
enum Config {
	PictureMinX,PictureMinY,PictureMinZ,
	PictureWide,PictureHigh,PictureDeep,
	DefaultWide,DefaultHigh,
	DefaultDeep,DefaultLong,DefaultStop,
	DefaultUnit,DefaultPole,DefaultBase,
	ScreenWide,ScreenHigh,LeverDeep,
};

struct Callback {
	void (*err)(const char *str, int num, int arg);
	// kvm to share; read from state; write to pipe
	void (*move)(double xpos, double ypos);
	void (*roll)(double xoffset, double yoffset);
	void (*click)(double xpos, double ypos, int isright);
	void (*drag)(double xmid, double ymid, double xmax, double ymax);
	void (*write)(struct Vector *point, struct Vector *normal, int object);
	// share to kvm; read from state; write to gpu
	double (*conf)(enum Config config);
	void (*warp)(double xpos, double ypos);
	void (*dma)(enum Memory mem, int idx, int siz);
	void (*draw)(enum Shader shader);
	// loop to kvm
	int (*full)();
	// loop to share; read from pipe; write to state
	void (*proc)();
	int (*read)();
	// main to loop
	void (*start)();
	void (*call)();
	// share to loop
	void (*wake)();
	// main to kvm
	void (*done)();
	int hub;
	int tub;
	int zub;
	int esc;
	struct Client *state[Memorys];
};

extern struct Callback cb;

void shareArg(const char *arg);
void shareInit();
void shareDone();
void threadInit();
void threadDone();
void clientFacet(enum Memory mem, int idx, int siz, int len, struct Facet *ptr, enum Function *fnc);
void clientVertex(enum Memory mem, int idx, int siz, int len, struct Vertex *ptr, enum Function *fnc);
void clientIndex(enum Memory mem, int idx, int siz, int len, struct Index *ptr, enum Function *fnc);
void clientInt(enum Memory mem, int idx, int siz, int len, int *ptr, enum Function *fnc);
void clientArray(enum Memory mem, int idx, int siz, int len, struct Array *ptr, enum Function *fnc);
void clientLinear(enum Memory mem, int idx, int siz, int len, struct Linear *ptr, enum Function *fnc);
void clientAffine(enum Memory mem, int idx, int siz, int len, struct Affine *ptr, enum Function *fnc);
void clientVector(enum Memory mem, int idx, int siz, int len, struct Vector *ptr, enum Function *fnc);
void clientResult(enum Memory mem, int idx, int siz, int len, struct Result *ptr, enum Function *fnc);
void clientMode(enum Memory mem, int idx, int siz, int len, struct Mode *ptr, enum Function *fnc);
void clientChr(enum Memory mem, int idx, int siz, int len, char *ptr, enum Function *fnc);
void clientClient(enum Memory mem, int idx, int siz, int len, struct Client *ptr, enum Function *fnc);
void clientMetric(struct Client *ptr);
void atomicFacet(enum Memory mem, int idx, int siz, int len, struct Facet *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicVertex(enum Memory mem, int idx, int siz, int len, struct Vertex *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicIndex(enum Memory mem, int idx, int siz, int len, struct Index *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicInt(enum Memory mem, int idx, int siz, int len, int *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicArray(enum Memory mem, int idx, int siz, int len, struct Array *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicLinear(enum Memory mem, int idx, int siz, int len, struct Linear *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicAffine(enum Memory mem, int idx, int siz, int len, struct Affine *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicVector(enum Memory mem, int idx, int siz, int len, struct Vector *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicResult(enum Memory mem, int idx, int siz, int len, struct Result *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicMode(enum Memory mem, int idx, int siz, int len, struct Mode *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicChr(enum Memory mem, int idx, int siz, int len, char *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
void atomicClient(enum Memory mem, int idx, int siz, int len, struct Client *ptr, enum Function *fnc, int num, struct Client *ary, void (*func)(int num, struct Client *ptr));
