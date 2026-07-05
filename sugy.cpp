extern "C" {
#include <lua.h>
void wrapWrap(lua_State *L, const char *str, const struct Close *arg);
void wrapSugy(lua_State *L);
void sugarRepl(char **ptr, char chr);
}
#include "wrap.h"

void wrapSugy(lua_State *L)
{
	wrapWrap(L,"sugarRepl",(new WrapClose([](const struct WrapClose *arg) -> void {sugarRepl(0,0);},0,0)));
}
