#include "proto.h"
void nestFunc(const char *str, enum Prototype pro, union Proto typ);
void nestDone(int siz);
void nestElem(const char *str);
void nestLine();
int nestPass();
const char *nestNext(int i);
int nestScript(int *val, const char *str, int arg);
