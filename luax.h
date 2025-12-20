#include "proto.h"
#include <lua.h>

lua_State *luaxInit();
int luaxSide(const char *exp); // call the lua script
void nestInit(int siz); // allocate given number of strings
void nestElem(int i, const char *str); // set given string
void nestScan(); // get expressions from strings
int nestPass(); // evaluate expressions and return if any yielded
const char *nestRepl(int i); // get string with expressions replaced
typedef void (*sftype)(char **ptr, const char *str);
void luaxSugar(sftype sug);
void nestSugar(char **ptr);
