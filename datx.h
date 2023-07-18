#include <stdint.h>
int datxSub();
void **datxDat(int sub);
int datxReadFp(int fildes, void *buf, int nbyte);
int datxWriteFp(int fildes, const void *buf, int nbyte);
void datxSplit(void **pre, void **suf, const void *dat, int len);
void datxJoin(void **dat, const void *pre, const void *suf);
int datxFind(void **val, void *key);
void datxInsert(void *key, void *val, int typ);
void datxNone(void **dat);
void datxChr(void **dat, char val);
void datxStr(void **dat, const char *val);
void datxInt(void **dat, int val);
void datxInt32(void **dat, int32_t val);
void datxNum(void **dat, double val);
void datxOld(void **dat, float val);
int datxPtrs(void *dat);
int datxChrs(void *dat);
int datxInts(void *dat);
int datxInt32s(void *dat);
int datxNums(void *dat);
int datxOlds(void *dat);
void *datxVoid(void *dat);
void *datxPtrz(int num, void *dat);
char *datxChrz(int num, void *dat);
int *datxIntz(int num, void *dat);
int32_t *datxInt32z(int num, void *dat);
double *datxNumz(int num, void *dat);
float *datxOldz(int num, void *dat);
struct Express;
int datxEval(void **dat, struct Express *exp, int typ);
void datxPrefix(const char *str);
void datxCallback(dftype fnc);
void datxSetter(dgtype fnc);
void datxGetter(dhtype fnc);
void datxEmbed(fftype fnc);
