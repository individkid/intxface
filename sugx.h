struct Express;
int sugarHide(struct Express ***ptr, const char *str);
void sugarShow(char **ptr, const char *str);
void sugarRepl(char **ptr, char chr);
typedef void (*sftype)(const char *str);
void sugarEval(sftype exe, const char *str, char chr);
void sugarFilt(char **ptr, char chr);
