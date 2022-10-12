#include "proto.h"
int nestSide(const char *exp); // call the lua script
int nestDict(char **val, const char *exp, const char *arg); // call the lua script with one arg and one return value
int nestPerm(int *val, const char *exp, int arg); // call the lua script with one arg and one return value
void nestFunc(const char *str, enum Prototype pro, union Proto typ); // add function of the given type to global context
void nestInit(int siz); // allocate given number of strings
void nestElem(int i, const char *str); // set given string
void nestScan(); // get expressions from strings
int nestPass(); // evaluate expressions and return if any yielded
const char *nestRepl(int i); // get string with expressions replaced
