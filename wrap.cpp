extern "C" {
#include "luax.h"
#include "face.h"
}
#include <functional>

struct WrapClose;
typedef void wrapFunc(const struct WrapClose *arg);
struct WrapClose: Close
{
	std::function<wrapFunc> fnc;
	WrapClose(const char *_str, std::function<wrapFunc> _fnc, int _n, int _m) {
		str = strdup(_str);
		luaxWrap(_str, this);
		Close::n = _n; a = (struct Para *)calloc(Close::n,sizeof(struct Para));
		Close::m = _m; b = (struct Para *)calloc(Close::m,sizeof(struct Para));
		fnc = _fnc;
	}
	// TODO remove sizes from constructor, remove argument and add realloc to following
	WrapClose *ia(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Itype; return this;}
	WrapClose *ja(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Jtype; return this;}
	WrapClose *ka(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Ktype; return this;}
	WrapClose *ma(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Mtype; return this;}
	WrapClose *na(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Ntype; return this;}
	WrapClose *ua(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Utype; return this;}
	WrapClose *va(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Vtype; return this;}
	WrapClose *ib(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Itype; return this;}
	WrapClose *jb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Jtype; return this;}
	WrapClose *kb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Ktype; return this;}
	WrapClose *mb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Mtype; return this;}
	WrapClose *nb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Ntype; return this;}
	WrapClose *ub(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Utype; return this;}
	WrapClose *vb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Vtype; return this;}
	int i(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Itype) ERROR(); return a[sub].i;}
	int32_t j(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Jtype) ERROR(); return a[sub].j;}
	long long k(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Ktype) ERROR(); return a[sub].k;}
	double m(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Mtype) ERROR(); return a[sub].m;}
	float n(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Ntype) ERROR(); return a[sub].n;}
	const char *u(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Utype) ERROR(); return a[sub].u;}
	// TODO add setters and make Close private
	int &i_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Itype) ERROR(); return b[sub].i;}
};

void wrapCallback(const struct Close *arg)
{
	const WrapClose *ptr = static_cast<const WrapClose *>(arg);
	ptr->fnc(ptr);
}

void wrapFaceLuax()
{
	(new WrapClose("noteFunc",[](const struct WrapClose *arg) -> void {noteFuncLua(arg->u(0));},1,0))->ua(0);
	(new WrapClose("errFunc",[](const struct WrapClose *arg) -> void {errFuncLua(arg->u(0));},1,0))->ua(0);
	(new WrapClose("closeIdent",[](const struct WrapClose *arg) -> void {closeIdent(arg->i(0));},1,0))->ia(0);
	(new WrapClose("moveIdent",[](const struct WrapClose *arg) -> void {moveIdent(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1);
	(new WrapClose("findIdent",[](const struct WrapClose *arg) -> void {arg->i_(0) = findIdent(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose("inetIdent",[](const struct WrapClose *arg) -> void {arg->i_(0) = inetIdent(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose("openPipe",[](const struct WrapClose *arg) -> void {arg->i_(0) = openPipe();},0,1))->ib(0);
	(new WrapClose("openFile",[](const struct WrapClose *arg) -> void {arg->i_(0) = openFile(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose("openInet",[](const struct WrapClose *arg) -> void {arg->i_(0) = openInet(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose("forkExec",[](const struct WrapClose *arg) -> void {arg->i_(0) = forkExec(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose("pipeInit",[](const struct WrapClose *arg) -> void {arg->i_(0) = pipeInit(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose("openFork",[](const struct WrapClose *arg) -> void {arg->i_(0) = openFork();},0,1))->ib(0);
	(new WrapClose("openCheck",[](const struct WrapClose *arg) -> void {arg->i_(0) = openCheck(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("openRdfd",[](const struct WrapClose *arg) -> void {arg->i_(0) = openRdfd(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("openWrfd",[](const struct WrapClose *arg) -> void {arg->i_(0) = openWrfd(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("openExec",[](const struct WrapClose *arg) -> void {arg->i_(0) = openExec(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose("rdwrInit",[](const struct WrapClose *arg) -> void {arg->i_(0) = rdwrInit(arg->i(0),arg->i(1));},2,1))->ia(0)->ia(1)->ib(0);
	(new WrapClose("waitRead",[](const struct WrapClose *arg) -> void {arg->i_(0) = waitRead(arg->k(0),arg->i(1));},2,1))->ka(0)->ia(1)->ib(0);
	(new WrapClose("waitExit",[](const struct WrapClose *arg) -> void {arg->i_(0) = waitExit();},0,1))->ib(0);
	(new WrapClose("pollPipe",[](const struct WrapClose *arg) -> void {arg->i_(0) = pollPipe(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("pollFile",[](const struct WrapClose *arg) -> void {arg->i_(0) = pollFile(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("seekFile",[](const struct WrapClose *arg) -> void {seekFile(arg->k(0),arg->i(1));},2,0))->ia(0)->ka(1);
	(new WrapClose("truncFile",[](const struct WrapClose *arg) -> void {truncFile(arg->i(0));},1,0))->ia(0);
}
