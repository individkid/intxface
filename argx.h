#define NUMNEST 256
#define NUMJUMP 16
#define NUMMODE 16
typedef void (*lftype)(void *lua);
typedef void (*nftype)(int idx, int *typ, int *siz, void **dat);
enum ArgxCode {
	FlowArgx, // data flow stream
	NestArgx, // nesting control
	JumpArgx, // save restore location
	PushArgx, // push pop location
	LoopArgx, // loop break nest
	BackArgx, // goto number of steps
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