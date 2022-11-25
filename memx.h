#include "proto.h"

int memxSize(void *ptr); // get size
int memxInt(void *ptr); // get int
int memxMask(void *ptr); // mask from collection
const char *memxStr(void *mem); // get string
void memxEval(void **mem, void *giv); // evaluate script
void memxInit(void **mem, const char *str); // convert from string
void memxForm(void **mem, const char *str, void *map); // map separators to domain
void *memxTemp(const char *str, int idx); // realloc indexed memory
void memxCopy(void **mem, void *giv); // replaces target with given
void memxList(void **mem, void *giv); // adds given to target in order
void memxKeep(void **mem, void *giv); // adds given to target unordered
void memxDone(void **mem); // deletes target
void memxCall(void **mem, void *giv, struct Prototype fnc); // call function on mem with giv
void memxBack(void **mem, void **giv, struct Prototype fnc); // nontrivial change causes call on giv with mem
void memxDflt(void **mem, void **giv, struct Prototype fnc); // trivial change causes call on mem with giv
void *memxFirst(void *giv); // get iterator from given
int memxLast(void *giv); // check iterator of given
void *memxNext(void *giv); // get iterator from given iterator
void *memxFind(void *giv, void *key); // find iterator with given as first
void *memxSkip(void *giv, int key); // skip to given iterator
