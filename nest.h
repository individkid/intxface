typedef int (*lftype)(int *val, const char *typ, const char *str, int *siz); // hide given enum type

int hidePercent(char **ret, const char *str, lftype fnc);
int hideScript(int *val, const char *str, int arg);
