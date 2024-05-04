extern "C" {
#include "datx.h"
char *planePopstr();
void planePutstr(const char *src);
void planeOutstr(const char *str);
void planeSetcfg(int val, int sub);
int planeGetcfg(int sub);
void datxWrap(enum Callback cb, const struct Close *arg);
void wrapPlane();
}
#include "wrap.h"

void wrapPlane()
{
	datxWrap(PutstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planePutstr(arg->u(0));},1,0))->ua(0));
	// FIXME datxWrap(PopstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {arg->s_(0) = planePopstr();},0,1))->sb(0));
	datxWrap(OutstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeOutstr(arg->u(0));},1,0))->ua(0));
	datxWrap(SetcfgCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeSetcfg(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1));
	datxWrap(GetcfgCb,(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0) = planeGetcfg(arg->i(0));},1,1))->ia(0)->ib(0));
}
