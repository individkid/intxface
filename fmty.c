#include <lua.h>

void wrapFmty(lua_State *L);
int luaopen_fmty(lua_State *L)
{
	wrapFmty(L);
	return 0;
}
