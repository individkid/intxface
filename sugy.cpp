extern "C" {
#include <lua.h>
void wrapWrap(lua_State *L, const char *str, const struct Close *arg);
void wrapSugy(lua_State *L);
void sugarRepl(char **ptr, char chr);
}
#include "wrap.h"

void wrapSugy(lua_State *L)
{
	wrapWrap(L,"sugarRepl",(new WrapClose([](const struct WrapClose *arg) -> void {sugarRepl(&arg->s_(0),arg->w(1));arg->s(0);},2,1))->ua(0)->wa(1)->vb(0));
}
