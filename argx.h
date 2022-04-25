#define NUMNEST 128
#define NUMMODE 32
typedef void (*sftype)(int idx, int *typ, int *siz, void **dat);
struct ArgxFlow {
	sftype fnc; // stream function
	int idx; // stream index
};
typedef struct ArgxFlow (*fftype)(const char *arg, int *mod, int lim);
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
	int idx; // file descriptor or nesting count
	sftype fnc; // valid for FlowArgx
	const char *str; // in place of idx if nonzero
};
struct ArgxCnst {
	const char *str; // zero if value is constant idx
	int idx; // ignored if str is not zero
};