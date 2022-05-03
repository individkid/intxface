#define NUMNEST 256
#define NUMJUMP 16
#define NUMMODE 16
typedef void (*nftype)(int idx, int *typ, int *siz, void **dat);
typedef const char *(*mftype)(const char *str, int arg, int rep);
enum ArgxCode {
	FlowArgx, // data flow stream
	NestArgx, // nesting control
	JumpArgx, // save restore location
	PushArgx, // push pop location
	LoopArgx, // loop break nest
	BackArgx, // goto number of steps
	NoneArgx, // just execute script
	ArgxArgx
};
struct ArgxNest {
	enum ArgxCode opc; // type of data flow control step
	nftype fnc; // valid for FlowArgx if str zero
	int idx; // file descriptor or nesting count
	const char *str; // punt stream or nesting count
};
typedef struct ArgxNest (*fftype)(const char *arg, int *mod, int lim);
struct ArgxCnst {
	const char *str; // zero if value is constant idx
	int idx; // ignored if str is not zero
};
const char *setScript(const char *str);
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
struct ArgxNest streamFactory(const char *arg, int *mod, int lim);
void readmeInit();
