extern "C" {
#include <lua.h>
void planeDupstr(char **ptr, int len, int idx, int loc);
void planeInsstr(const char *src, int len, int idx, int loc);
void planeDelstr(int len, int idx, int loc);
void planeOutstr(const char *str);
void planeAddarg(const char *str);
void planeSetcfg(int val, int sub);
int planeGetcfg(int sub);
void planeValstr(char **val, const char *key);
void planeSavstr(const char *val, const char *key);
void luaxWrap(lua_State *L, const char *str, const struct Close *arg);
void wrapPlane(lua_State *L);
}
#include "wrap.h"

void wrapPlane(lua_State *L)
{
	luaxWrap(L,"planeDupstr",(new WrapClose([](const struct WrapClose *arg) -> void {planeDupstr(&arg->v_(0),arg->i(0),arg->i(1),arg->i(2));},3,1))->ia(0)->ia(1)->ia(2)->vb(0));
	luaxWrap(L,"planeOutstr",(new WrapClose([](const struct WrapClose *arg) -> void {planeOutstr(arg->u(0));},1,0))->ua(0));
	luaxWrap(L,"planeInsstr",(new WrapClose([](const struct WrapClose *arg) -> void {planeInsstr(arg->u(0),arg->i(1),arg->i(2),arg->i(3));},4,0))->ua(0)->ia(1)->ia(2)->ia(3));
	luaxWrap(L,"planeDelstr",(new WrapClose([](const struct WrapClose *arg) -> void {planeDelstr(arg->i(0),arg->i(1),arg->i(2));},3,0))->ia(0)->ia(1)->ia(2));
	luaxWrap(L,"planeSetcfg",(new WrapClose([](const struct WrapClose *arg) -> void {planeSetcfg(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1));
	luaxWrap(L,"planeGetcfg",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0) = planeGetcfg(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"planeValstr",(new WrapClose([](const struct WrapClose *arg) -> void {planeValstr(&arg->v_(0),arg->u(0));},1,1))->ua(0)->vb(0));
	luaxWrap(L,"planeSavstr",(new WrapClose([](const struct WrapClose *arg) -> void {planeSavstr(arg->u(0),arg->u(1));},2,0))->ua(0)->ua(1));
}