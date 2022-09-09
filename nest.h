typedef int (*lftype)(int *val, const char *typ, const char *str, int *siz); // hide given enum type; lua function takes typ and str, returns retval, val, and siz.
int hidePercent(char **ret, const char *str, lftype fnc); // lua version takes str and function, returns retval and ret string
int hideScript(int *val, const char *str, int arg); // lua version takes str and arg, returns retval and val
