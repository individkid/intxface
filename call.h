#define NUMCALL 1024
#define NUMPATH 8
#define NUMEACH 8
#define NUMMASK 8
typedef void (*tftype)(void *bnd, void *ovr, long long val); // triggered by callTime
typedef void (*fftype)(void *bnd, void *ovr, int idx); // triggered by callFlow
typedef int (*nftype)(void *bnd, void *ovr); // triggered by callNest
typedef void (*eftype)(void *bnd, void *ovr, int src, int dst, int how, int who); // triggered by callEdge
void makeTime(void *ovr, tftype fnc, void *bnd, long long val);
void callTime(void *ovr, long long val);
int makeFlow(fftype fnc, void *bnd);
void callFlow(void *ovr, int idx);
int makeNest(nftype fnc, void *bnd, int lft, int rgt);
void callNest(void *ovr);
int makeEdge(eftype fnc, void *bnd, long long now, long long new, long long sel);
void callEdge(void *ovr, long long how, long long who);
