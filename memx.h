#include "proto.h"

void memxLuax(); // add memx functions to lua interpreter
int memxSize(void *mem); // get size
int memxInt(void *mem); // get int
int memxMask(void *mem); // mask from collection
const char *memxStr(void *mem); // get string
const char *memxRaw(void *mem, int *len); // get bytes
void memxInit(void **mem, const char *str); // convert from string
void memxForm(void **mem, const char *fmt, ...); // use vasprintf
void *memxTemp(int idx); // realloc indexed memory
int memxOpen(void **mem); // get pipe punted to given
void memxCopy(void **mem, void *giv); // replaces target with given
void memxList(void **mem, void *giv); // adds given to target in order
void memxKeep(void **mem, void *giv); // adds given to target unordered
void memxMake(void **mem, void *giv); // delete of mem also deletes giv
void memxDone(void **mem); // delete now
void memxCall(void **mem, void *giv, struct Function fnc); // call function on mem with giv
void memxBack(void **mem, void **giv, struct Function fnc); // nontrivial change causes call on giv with mem
void memxDflt(void **mem, void **giv, struct Function fnc); // trivial change causes call on mem with giv
void *memxSkip(void *mem, int key); // skip to given iterator
void memxAdd(void **mem, void *giv, int key); // insert at given location
void memxDel(void **mem, int key); // delete at given location
