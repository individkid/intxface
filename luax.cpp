extern "C" {
#include <lua.h>
#include "luax.h"
void wrapLuax(lua_State *L);
}
#include "wrap.h"

void wrapLuax(lua_State *L)
{
	(new WrapClose(L,"luaxSide",[](const struct WrapClose *arg) -> void {arg->i_(0)=luaxSide(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose(L,"nestInit",[](const struct WrapClose *arg) -> void {nestInit(arg->i(0));},1,0))->ia(0);
	(new WrapClose(L,"nestElem",[](const struct WrapClose *arg) -> void {nestElem(arg->i(0),arg->u(1));},2,0))->ia(0)->ua(1);
	(new WrapClose(L,"nestScan",[](const struct WrapClose *arg) -> void {nestScan();},0,0));
	(new WrapClose(L,"nestPass",[](const struct WrapClose *arg) -> void {arg->i_(0) = nestPass();},0,1))->ib(0);
	(new WrapClose(L,"nestRepl",[](const struct WrapClose *arg) -> void {arg->u_(0) = nestRepl(arg->i(0));},1,1))->ia(0)->ub(0);
}
