#include "argx.h"
#include <lua.h>

extern lua_State *luaptr;
struct {
	enum {
		FlowArgx,
		NestArgx,
		JumpArgx,
		PushArgx,
		LoopArgx,
		BackArgx,
	} opc;
	int idx; // file descriptor or nesting count
	sftype fnc; // valid for FlowArgx
	const char *exp; // in place of idx if nonzero
} nst[NUMNEST] = {0};
int nlm = 0;
void *dat = 0;
int siz = 0;
int typ = 0;
int mod[NUMMODE] = {0};
int mlm = 0;