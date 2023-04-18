void datxOpen(int idx);
void datxClose(int idx);
void datxProg(int sub, int idx);
void datxRead(void **ptr, int sub, int num, int idx);
void datxInit(int sub, int lim, int idx);
void datxStep(int sub, int opc, int idx);
void datxMark(int idx);
void datxWrite(void *dat, int idx);
int datxMeta(int sub, int idx);
void datxStr(void **dat, const char *str);
void datxSplit(void **pre, void **suf, const void *dat, int len);
void datxJoin(void **dat, const void *pre, const void *suf);
int datxPtrs(void *dat);
int datxChrs(void *dat);
int datxInts(void *dat);
void *datxData(void *dat);
void *datxPtr(int num, void *dat);
char *datxChrz(int num, void *dat);
int *datxIntz(int num, void *dat);
char datxChr(int num, void *dat);
int datxInt(int num, void *dat);
