#include "proto.h"

void memxLuax(); // add memx functions to lua interpreter
int memxSize(void *ptr); // get size
int memxInt(void *ptr); // get int
int memxMask(void *ptr); // mask from collection
const char *memxStr(void *mem); // get string
void memxInit(void **mem, const char *str); // convert from string
void memxForm(void **mem, const char *fmt, ...); // use vasprintf
void *memxTemp(int idx); // realloc indexed memory
int memxOpen(void **ptr); // get pipe punted to given
void memxCopy(void **mem, void *giv); // replaces target with given
void memxList(void **mem, void *giv); // adds given to target in order
void memxKeep(void **mem, void *giv); // adds given to target unordered
void memxDone(void **mem); // deletes target
void memxCall(void **mem, void *giv, struct Function fnc); // call function on mem with giv
void memxBack(void **mem, void **giv, struct Function fnc); // nontrivial change causes call on giv with mem
void memxDflt(void **mem, void **giv, struct Function fnc); // trivial change causes call on mem with giv
void memxFirst(void **mem, void *giv); // get iterator from given
int memxTest(void *giv); // check iterator of given
void memxNext(void **mem, void *giv); // get iterator from given iterator
void memxFind(void **mem, void *giv, void *key); // find iterator with given as first
void memxSkip(void **mem, void *giv, int key); // skip to given iterator
void memxAdd(void **mem, void *giv, int key); // insert at given location
void memxDel(void **mem, int key); // delete at given location
