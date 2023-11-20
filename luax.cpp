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
	// (new WrapClose(L,"nestElem",[](const struct WrapClose *arg) -> void {nestElem();},0,0));
	// (new WrapClose(L,"nestScan",[](const struct WrapClose *arg) -> void {nestScan();},0,0));
	// (new WrapClose(L,"nestPass",[](const struct WrapClose *arg) -> void {nestPass();},0,0));
	// (new WrapClose(L,"nestRepl",[](const struct WrapClose *arg) -> void {nestRepl();},0,0));
// void nestInit(int siz); // allocate given number of strings
// void nestElem(int i, const char *str); // set given string
// void nestScan(); // get expressions from strings
// int nestPass(); // evaluate expressions and return if any yielded
// const char *nestRepl(int i); // get string with expressions replaced
}
