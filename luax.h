#include "proto.h"
int luaxFile(const char *exp); // call dofile with path from getExestr
int luaxLib(const char *exp); // call require with path from getExestr
int luaxSide(const char *exp); // call the lua script
int luaxDict(char **val, const char *exp, const char *arg); // call the lua script with one arg and one return value
int luaxPerm(int *val, const char *exp, int arg); // call the lua script with one arg and one return value
void luaxFunc(const char *str, struct Prototype); // add function of the given type to global context
void nestInit(int siz); // allocate given number of strings
void nestElem(int i, const char *str); // set given string
void nestScan(); // get expressions from strings
int nestPass(); // evaluate expressions and return if any yielded
const char *nestRepl(int i); // get string with expressions replaced
