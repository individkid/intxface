#include "argx.h"

struct {
	enum {
		FlowArgx,
		NestArgx,
		JumpArgx,
		PushArgx,
		LoopArgx,
		BackArgx,
	} opc;
	int num;
	sftype fnc;
	const char *exp;
} nst[NUMNEST] = {0};
int nlm = 0;
void *buf = 0;
int num = 0;
int typ = 0;
int mod[NUMMODE] = {0};
int mlm = 0;