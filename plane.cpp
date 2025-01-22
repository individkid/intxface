extern "C" {
#include "datx.h"
void planePutstr(const char *src);
char *planeGetstr();
void planeSetcfg(int val, int sub);
int planeRetcfg(int sub);
void datxWrap(enum Callback cb, const struct Close *arg);
void wrapPlane();
}
#include "wrap.h"

void wrapPlane()
{
	datxWrap(PutstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {planePutstr(arg->u(0));},1,0))->ua(0));
	datxWrap(GetstrCb,(new WrapClose([](const struct WrapClose *arg) -> void {arg->r_(0) = planeGetstr();},0,1))->vb(0));
	datxWrap(SetcfgCb,(new WrapClose([](const struct WrapClose *arg) -> void {planeSetcfg(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1));
	datxWrap(RetcfgCb,(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0) = planeRetcfg(arg->i(0));},1,1))->ia(0)->ib(0));
}
