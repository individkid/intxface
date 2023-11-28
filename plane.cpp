extern "C" {
#include <lua.h>
#include "datx.h"
void planeDupstr(char **ptr, int len, int idx, int loc);
void planeInsstr(const char *src, int len, int idx, int loc);
void planeDelstr(int len, int idx, int loc);
void planeOutstr(const char *str);
void planeAddarg(const char *str);
void planeSetcfg(int val, int sub);
int planeGetcfg(int sub);
void planeValstr(char **val, const char *key);
void planeSavstr(const char *val, const char *key);
void datxWrap(enum Callback cb, const struct Close *arg);
void wrapPlane();
}
#include "wrap.h"

void wrapPlane()
{
	datxWrap(DupstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeDupstr(&arg->v_(0),arg->i(0),arg->i(1),arg->i(2));},3,1))->ia(0)->ia(1)->ia(2)->vb(0));
	datxWrap(OutstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeOutstr(arg->u(0));},1,0))->ua(0));
	datxWrap(InsstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeInsstr(arg->u(0),arg->i(1),arg->i(2),arg->i(3));},4,0))->ua(0)->ia(1)->ia(2)->ia(3));
	datxWrap(DelstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeDelstr(arg->i(0),arg->i(1),arg->i(2));},3,0))->ia(0)->ia(1)->ia(2));
	datxWrap(SetcfgCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeSetcfg(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1));
	datxWrap(GetcfgCb,(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0) = planeGetcfg(arg->i(0));},1,1))->ia(0)->ib(0));
}
