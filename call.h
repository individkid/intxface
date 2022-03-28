#define NUMCALL 1024
#define NUMNODE 1024
#define NUMLEVEL 8
#define NUMEACH 8
typedef void (*tftype)(void *bnd, void *ovr, long long val); // triggered by callTime
typedef void (*fftype)(void *bnd, void *ovr, int idx); // triggered by callFlow
typedef int (*nftype)(void *bnd, void *ovr); // triggered by callNest
int initCall(void *arg, tftype tfn, fftype ffn, nftype nfn, int brg, int crg);
void saveTime(void *ovr, int box, long long val);
void callTime(void *ovr, long long val);
void callFlow(void *ovr, int idx);
void callNest(void *ovr);
