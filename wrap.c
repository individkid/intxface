#include <lua.h>
#include "proto.h"

void wrapCallback(const struct Close *arg);
int luaxUnwrap(lua_State *L)
{
	const struct Close *arg = lua_touserdata(L, lua_upvalueindex(1));
	for (int i = 0; i < arg->n; i++) switch (arg->a[i].t) {
		case (Itype): arg->a[i].i = lua_tointeger(L,i+1); break;
		case (Jtype): arg->a[i].j = lua_tointeger(L,i+1); break;
		case (Ktype): arg->a[i].k = lua_tointeger(L,i+1); break;
		case (Mtype): arg->a[i].m = lua_tonumber(L,i+1); break;
		case (Ntype): arg->a[i].n = lua_tonumber(L,i+1); break;
		case (Utype): arg->a[i].u = lua_tostring(L,i+1); break;
		default: ERROR();}
	wrapCallback(arg);
	lua_pop(L,arg->n);
	int vld = (arg->i < 0 || arg->i >= arg->m || arg->b[arg->i].i == 0 ? 1 : 0);
	if (!vld) lua_pushnil(L);
	for (int i = !vld; i < arg->m; i++) switch (arg->b[i].t) {
		case (Itype): lua_pushinteger(L,arg->b[i].i); break;
		case (Jtype): lua_pushinteger(L,arg->b[i].j); break;
		case (Ktype): lua_pushinteger(L,arg->b[i].k); break;
		case (Mtype): lua_pushnumber(L,arg->b[i].m); break;
		case (Ntype): lua_pushnumber(L,arg->b[i].n); break;
		case (Utype): lua_pushstring(L,arg->b[i].u); break;
		case (Vtype): lua_pushstring(L,arg->b[i].v); break;
		default: ERROR();}
	return arg->m;
}
void luaxWrap(lua_State *L, const char *str, const struct Close *arg)
{
	lua_pushlightuserdata(L, (void*)arg);
	lua_pushcclosure(L, luaxUnwrap, 1);
	lua_setglobal(L, str);
}
