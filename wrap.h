extern "C" {
#include "proto.h"
void wrapCallback(const struct Close *arg);
}
#include <functional>
#ifdef __linux__
#include <cstring>
#endif

struct WrapClose; typedef void wrapFunc(const struct WrapClose *arg);
struct WrapClose : Close {
	std::function<wrapFunc> fnc;
	static char *str;
	static int val;
	WrapClose(std::function<wrapFunc> _fnc, int n, int m) {
		Close::n = n; a = (struct Para *)calloc(n,sizeof(struct Para));
		Close::m = m; b = (struct Para *)calloc(m,sizeof(struct Para));
		c = (int*)calloc(m,sizeof(int)); memset(c,0,m*sizeof(int));
		fnc = _fnc; str = 0; val = 0;
	}
	WrapClose *ia(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Itype; return this;}
	WrapClose *ja(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Jtype; return this;}
	WrapClose *ka(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Ktype; return this;}
	WrapClose *ma(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Mtype; return this;}
	WrapClose *na(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Ntype; return this;}
	WrapClose *ua(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Utype; return this;}
	WrapClose *va(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Vtype; return this;}
	WrapClose *wa(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Wtype; return this;}
	WrapClose *pa(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Ptype; return this;}
	WrapClose *qa(int sub) {if (sub < 0 || sub >= Close::n) ERROR(); a[sub].t = Para::Qtype; return this;}
	WrapClose *ib(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Itype; return this;}
	WrapClose *jb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Jtype; return this;}
	WrapClose *kb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Ktype; return this;}
	WrapClose *mb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Mtype; return this;}
	WrapClose *nb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Ntype; return this;}
	WrapClose *ub(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Utype; return this;}
	WrapClose *vb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Vtype; return this;}
	WrapClose *wb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Wtype; return this;}
	WrapClose *pb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Ptype; return this;}
	WrapClose *qb(int sub) {if (sub < 0 || sub >= Close::m) ERROR(); b[sub].t = Para::Qtype; return this;}
	int i(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Itype) ERROR(); return a[sub].i;}
	int32_t j(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Jtype) ERROR(); return a[sub].j;}
	long long k(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Ktype) ERROR(); return a[sub].k;}
	double m(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Mtype) ERROR(); return a[sub].m;}
	float n(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Ntype) ERROR(); return a[sub].n;}
	const char *u(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Utype) ERROR(); return a[sub].u;}
	char w(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Wtype) ERROR(); return a[sub].w;}
	const void *p(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Ptype) ERROR(); return a[sub].p;}
	int &i_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Itype) ERROR(); return b[sub].i;}
	int32_t &j_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Jtype) ERROR(); return b[sub].j;}
	long long &k_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Ktype) ERROR(); return b[sub].k;}
	double &m_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Mtype) ERROR(); return b[sub].m;}
	float &n_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Ntype) ERROR(); return b[sub].n;}
	const char *&u_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Utype) ERROR(); return b[sub].u;}
	char *&v_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Vtype) ERROR(); return b[sub].v;}
	char &w_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Wtype) ERROR(); return b[sub].w;}
	void *&q_(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Qtype) ERROR(); return b[sub].q;}
	char *&s_(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Utype) ERROR(); free(str); str = strdup(a[sub].u); return str;}
	void s(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Vtype) ERROR(); b[sub].v = str;}
	int &t_(int sub) const {if (sub < 0 || sub >= Close::n || a[sub].t != Para::Itype) ERROR(); val = a[sub].i; return val;}
	void t(int sub) const {if (sub < 0 || sub >= Close::m || b[sub].t != Para::Itype) ERROR(); b[sub].i = val;}
	int &r(int sub) const {if (sub < 0 || sub >= Close::m) ERROR(); return c[sub];}
};


