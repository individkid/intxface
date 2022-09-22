#include "proto.h"
int nestScript(int *val, const char *exp, int arg); // call the lua script with one arg and one return value
void nestFunc(const char *str, enum Prototype pro, union Proto typ); // add function of the given type to global context
void nestInit(int siz); // allocate given number of strings
void nestElem(int i, const char *str); // set given string
int nestScan(); // return number of expressions from strings
int nestEval(int i); // return whether expression evaluates to yield
int nestPass(); // evaluate expressions and return if any yielded
const char *nestRepl(int i); // get string with expressions replaced
