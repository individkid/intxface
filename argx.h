#define NUMNEST 64
#define NUMMODE 8
typedef void (*sftype)(int idx, int *typ, int *siz, void **dat);
typedef sftype (*fftype)(int *mod, int lim);
