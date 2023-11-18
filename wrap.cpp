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
	WrapClose(const char *str, std::function<wrapFunc> _fnc, int _n, int _m) {
		Close::n = _n; a = (struct Para *)calloc(Close::n,sizeof(struct Para));
		Close::m = _m; b = (struct Para *)calloc(Close::m,sizeof(struct Para));
		fnc = _fnc;
		luaxWrap(str, this);
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
	int32_t &j_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Jtype) ERROR(); return b[sub].j;}
	long long &k_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Ktype) ERROR(); return b[sub].k;}
	double &m_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Ntype) ERROR(); return b[sub].n;}
	float &n_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Mtype) ERROR(); return b[sub].m;}
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
	(new WrapClose("checkFile",[](const struct WrapClose *arg) -> void {arg->k_(0) = checkFile(arg->i(0));},1,1))->ia(0)->kb(0);
	(new WrapClose("rdlkFile",[](const struct WrapClose *arg) -> void {arg->i_(0) = rdlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,1))->ka(0)->ka(1)->ia(2)->ib(0);
	(new WrapClose("wrlkFile",[](const struct WrapClose *arg) -> void {arg->i_(0) = wrlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,1))->ka(0)->ka(1)->ia(2)->ib(0);
	(new WrapClose("unlkFile",[](const struct WrapClose *arg) -> void {unlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2);
	(new WrapClose("rdlkwFile",[](const struct WrapClose *arg) -> void {rdlkwFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2);
	(new WrapClose("wrlkwFile",[](const struct WrapClose *arg) -> void {wrlkwFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2);
	(new WrapClose("checkRead",[](const struct WrapClose *arg) -> void {arg->i_(0) = checkRead(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("checkWrite",[](const struct WrapClose *arg) -> void {arg->i_(0) = checkWrite(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose("sleepSec",[](const struct WrapClose *arg) -> void {sleepSec(arg->m(0));},1,0))->ma(0);
	(new WrapClose("readEof",[](const struct WrapClose *arg) -> void {readEof(arg->i(0));},1,0))->ia(0);

	// (new WrapClose("readStr",[](const struct WrapClose *arg) -> void {readStr();},0,0));
	// (new WrapClose("preadStr",[](const struct WrapClose *arg) -> void {preadStr();},0,0));
	// (new WrapClose("readDat",[](const struct WrapClose *arg) -> void {readDat();},0,0));
	// (new WrapClose("readChr",[](const struct WrapClose *arg) -> void {readChr();},0,0));
	// (new WrapClose("readInt",[](const struct WrapClose *arg) -> void {readInt();},0,0));
	// (new WrapClose("readInt32",[](const struct WrapClose *arg) -> void {readInt32();},0,0));
	// (new WrapClose("readNum",[](const struct WrapClose *arg) -> void {readNum();},0,0));
	// (new WrapClose("readNew",[](const struct WrapClose *arg) -> void {readNew();},0,0));
	// (new WrapClose("readOld",[](const struct WrapClose *arg) -> void {readOld();},0,0));
// void readStr(char **str, int idx);
// void preadStr(char **str, long long loc, int idx);
// void readDat(void **dat, int idx);
// char readChr(int idx);
// int readInt(int idx);
// int32_t readInt32(int idx);
// long long readNew(int idx);
// double readNum(int idx);
// float readOld(int idx);
}
