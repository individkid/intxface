extern "C" {
#include <lua.h>
void wrapWrap(lua_State *L, const char *str, const struct Close *arg);
void wrapFmty(lua_State *L);
void fmtxStbi(void **ptr, int *wid, int *hei, int *cha, const char *str);
}
#include "wrap.h"

void wrapFmty(lua_State *L)
{
	wrapWrap(L,"fmtxStbi",(new WrapClose([](const struct WrapClose *arg) -> void {fmtxStbi(&arg->q_(0),&arg->i_(1),&arg->i_(2),&arg->i_(3),arg->u(0));},1,4))->ua(0)->qb(0)->ib(1)->ib(2)->ib(3));
}
