#include <lua.h>

void wrapSugy(lua_State *L);
int luaopen_sugy(lua_State *L)
{
	wrapSugy(L);
	return 0;
}
