#include "proto.h"
int luaxFile(const char *exp); // call dofile with path from getExestr
int luaxLib(const char *exp); // call require with path from getExestr
int luaxSide(const char *exp); // call the lua script
int luaxExpr(const char *exp, const struct Closure *fnc);
int luaxCall(const char *str, const struct Closure *fnc);
void luaxAdd(const char *str, struct Function fnc); // extend
void nestInit(int siz); // allocate given number of strings
void nestElem(int i, const char *str); // set given string
void nestScan(); // get expressions from strings
int nestPass(); // evaluate expressions and return if any yielded
const char *nestRepl(int i); // get string with expressions replaced
