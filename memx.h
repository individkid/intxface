#include "proto.h"
int memxSize(void *ptr); // get size
int memxInt(void *ptr); // get int
void memxInit(void **mem, const char *str); // convert from string
void memxCopy(void **mem, void *giv); // replaces target with given
void memxList(void **mem, void *giv); // adds given to target in order
void memxKeep(void **mem, void *giv); // adds given to target unordered
void memxMark(void **mem); // saves and clears target
void memxDone(void **mem); // deletes target
void memxCall(void **mem, zftype fnc, void **giv); // causes given call on given with target as given
void *memxFirst(void *giv); // get iterator from given
int memxLast(void *giv); // check iterator of given
void *memxNext(void *giv); // get iterator from given iterator
void *memxFind(void *giv, void *key); // find iterator with given as first
void *memxSkip(void *giv, int key); // skip to given iterator
void *memxPast(void *giv, int key); // go back to given mark