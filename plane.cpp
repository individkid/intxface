extern "C" {
#include "luax.h" // TODO remove after replacing luaxAdd by WrapClose
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
void wrapPlane(lua_State *L);
}
#include "wrap.h"

void wrapPlane(lua_State *L)
{
	luaxAdd("planeDupstr",protoTypeSf(planeDupstr)); luaxAdd("planeOutstr",protoTypeHf(planeOutstr));
	luaxAdd("planeInsstr",protoTypeRp(planeInsstr)); luaxAdd("planeDelstr",protoTypeRq(planeDelstr));
	luaxAdd("planeSetcfg",protoTypeCg(planeSetcfg)); luaxAdd("planeGetcfg",protoTypeTl(planeGetcfg));
	luaxAdd("planeValstr",protoTypeRr(planeValstr)); luaxAdd("planeSavstr",protoTypeRs(planeSavstr));
// void planeDupstr(char **ptr, int len, int idx, int loc);
// void planeInsstr(const char *src, int len, int idx, int loc);
// void planeDelstr(int len, int idx, int loc);
// void planeOutstr(const char *str);
// void planeAddarg(const char *str);
// void planeSetcfg(int val, int sub);
// int planeGetcfg(int sub);
// void planeValstr(char **val, const char *key);
// void planeSavstr(const char *val, const char *key);
}
