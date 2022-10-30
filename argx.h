#define NUMNEST 256
#define NUMMODE 16
#define NUMDFLT 64
typedef void (*nftype)(int idx, int *typ, int *siz, void **dat);
typedef void (*mftype)(int idx, int *siz, void **dat);
enum Argx {
	FlowArgx, // data flow stream
	NoneArgx, // just execute script
	WaitArgx, // pselect on inputs
	JumpArgx, // loop break nest
	NestArgx, // nesting control
	ArgxArgx
};
struct ArgxNest {
	enum Argx opc; // type of data flow control step
	int cnt; // nesting or jump count
	int typ; // type to pass to stream
	int idx; // file descriptor for stream
	nftype fnc; // str fnc or gnc if FlowArgx
	mftype gnc; // str fnc or gnc if FlowArgx
	const char *str; // punt stream or nesting count
};
typedef struct ArgxNest (*fftype)(const char *arg, int *mod);
struct ArgxCnst {
	const char *str; // zero if value is constant idx
	int cnt; // ignored if str is not zero
};
fftype setFactory(fftype fnc);
int setArg(int val, int idx);
const char *setMode(const char *str);
int addFlags(const char *str, int idx);
int addConst(const char *str, int idx);
int addMulti(const char *str, int mod);
int addElem(int dim, int idx, int arg);
int useArgument(const char *str);
void runProgram();
void initLua();
enum Share {
	WrapShare,
	FlowShare,
	GateShare,
	TypeShare,
	FieldShare,
	SkipShare,
	ShareShare
};
struct ArgxNest argxFactory(const char *arg, int *mod);
enum Dflt {
	ArgxDflt, // set arg to default
	JumpDflt, // continue or break
	NestDflt, // open or close nest
	DfltDflt
};