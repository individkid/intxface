#define NUMNEST 256
#define NUMJUMP 16
#define NUMMODE 16
typedef void (*nftype)(int idx, int *typ, int *siz, void **dat);
typedef void (*mftype)(int idx, int *siz, void **dat);
typedef const char *(*gftype)(const char *str, int arg, int rep);
enum ArgxCode {
	FlowArgx, // data flow stream
	NoneArgx, // just execute script
	WaitArgx, // pselect on inputs
	BackArgx, // goto number of steps
	JumpArgx, // save restore location
	LoopArgx, // loop break nest
	NestArgx, // nesting control
	ArgxArgx
};
struct ArgxNest {
	enum ArgxCode opc; // type of data flow control step
	int cnt; // nesting or jump count
	int typ; // type to pass to stream
	int idx; // file descriptor for stream
	nftype fnc; // str fnc or gnc if FlowArgx
	mftype gnc; // str fnc or gnc if FlowArgx
	const char *str; // punt stream or nesting count
};
typedef struct ArgxNest (*fftype)(const char *arg, int *mod, int lim);
struct ArgxCnst {
	const char *str; // zero if value is constant idx
	int cnt; // ignored if str is not zero
};
gftype setScript(gftype fnc);
fftype setFactory(fftype fnc);
int setArg(int val, int idx);
int addFlags(const char *str, int idx);
int addConst(const char *str, int idx);
int addMode(const char *str);
int addMulti(const char *str);
int addElem(int dim, int idx, int arg);
int useArgument(const char *str);
void runProgram();
void initLua();
