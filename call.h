#define NUMCALL 1024
typedef void (*tftype)(void *bnd, void *ovr, double clk); // triggered by callTime
typedef void (*fftype)(void *bnd, void *ovr, int idx); // triggered by callFlow
typedef int (*nftype)(void *bnd, void *ovr); // triggered by callNest
void callTime(void *ovr, int idx, double clk);
void callFlow(void *ovr, int idx);
void callNest(void *ovr);
int initCall(void *bnd, wftype fnc, fftype fnc, nftype fnc, int lft, int rgt);