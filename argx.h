#define NUMNEST 128
#define NUMMODE 32
typedef void (*sftype)(int idx, int *typ, int *siz, void **dat);
typedef sftype (*fftype)(int *mod, int lim);
