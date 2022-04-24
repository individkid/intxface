#include "argx.h"
#include <lua.h>

extern lua_State *luaptr; // global context for expressions
struct {
	enum {
		FlowArgx, // data flow stream
		NestArgx, // nesting control
		JumpArgx, // save restore location
		PushArgx, // push pop location
		LoopArgx, // loop break nest
		BackArgx, // goto number of steps
	} opc; // type of data flow control step
	int idx; // file descriptor or nesting count
	sftype fnc; // valid for FlowArgx
	const char *str; // in place of idx if nonzero
} nst[NUMNEST] = {0}; // data flow control steps
int nlm = 0; // number of steps
void *dat = 0; // data to flow between streams
int siz = 0; // size of data to flow
int typ = 0; // type of data to flow
int stp = 0; // index into program steps
int mod[NUMMODE] = {0}; // arguments passed to factory
char *str[NUMMODE] = {0}; // expression for nonconstants
sftype fnc[NUMMODE] = {0}; // function for custom streams
int mlm = 0; // changing way to handle undashed arguments
char *flg[NUMMODE] = {0}; // char position is mod value
int adx[NUMMODE] = {0}; // which mod to use
int alm = 0; // mututally exclusive way to change handling
char *nxt[NUMMODE] = {0}; // dash char for value from next
int bdx[NUMMODE] = {0}; // which mod or exp to use
int blm = 0; // next undashed is way to change handling
char *dbl[NUMMODE] = {0}; // name of custom streams
int cdx[NUMMODE] = {0}; // which fnc to use
int clm = 0; // double dash like custom undashed
// phase one is configuring conversions
// phase two is converting command line arguments to a program
// phase three is executing the program
int phs = 0; // phase to access from lua context
