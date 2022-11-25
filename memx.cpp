extern "C" {
#include "memx.h"
#include "type.h"
// TODO add lua interpreter
}

extern "C" int memxSize(void *ptr) {return 0;} // get size
extern "C" int memxInt(void *ptr) {return 0;} // get int
extern "C" int memxMask(void *ptr) {return 0;} // mask from collection
extern "C" const char *memxStr(void *mem) {return 0;} // get string
extern "C" void memxEval(void **mem, void *giv) {} // evaluate script
extern "C" void memxInit(void **mem, const char *str) {} // convert from string
extern "C" void memxForm(void **mem, const char *str, void *map) {} // map separators to domain
extern "C" void *memxTemp(const char *str, int idx) {return 0;} // realloc indexed memory
extern "C" void memxCopy(void **mem, void *giv) {} // replaces target with given
extern "C" void memxList(void **mem, void *giv) {} // adds given to target in order
extern "C" void memxKeep(void **mem, void *giv) {} // adds given to target unordered
extern "C" void memxDone(void **mem) {} // deletes target
extern "C" void memxCall(void **mem, void *giv, struct Prototype fnc) {} // call function on mem with giv
extern "C" void memxBack(void **mem, void **giv, struct Prototype fnc) {} // causes given call on given with target as given
extern "C" void memxDflt(void **mem, void **giv, struct Prototype fnc) {} // trivial change causes call on mem with giv
extern "C" void *memxFirst(void *giv) {return 0;} // get iterator from given
extern "C" int memxLast(void *giv) {return 0;} // check iterator of given
extern "C" void *memxNext(void *giv) {return 0;} // get iterator from given iterator
extern "C" void *memxFind(void *giv, void *key) {return 0;} // find iterator with given as first
extern "C" void *memxSkip(void *giv, int key) {return 0;} // skip to given iterator
