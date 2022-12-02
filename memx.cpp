extern "C" {
#include "memx.h"
#include "luax.h"
}

extern "C" void memxLuax()
{
	luaxFunc("memxSize",protoTypeO(memxSize));
	luaxFunc("memxInt",protoTypeO(memxInt));
	luaxFunc("memxMask",protoTypeO(memxMask));
	luaxFunc("memxStr",protoTypeA(memxStr));
	luaxFunc("memxInit",protoTypeN(memxInit));
	luaxFunc("memxCopy",protoTypeM(memxCopy));
	luaxFunc("memxList",protoTypeM(memxList));
	luaxFunc("memxKeep",protoTypeM(memxKeep));
	luaxFunc("memxDone",protoTypeD(memxDone));
	luaxFunc("memxFirst",protoTypeT(memxFirst));
	luaxFunc("memxLast",protoTypeO(memxLast));
	luaxFunc("memxNext",protoTypeM(memxNext));
	luaxFunc("memxFind",protoTypeJ(memxFind));
	luaxFunc("memxSkip",protoTypeK(memxSkip));
	luaxFunc("memxAdd",protoTypeK(memxAdd));
	luaxFunc("memxDel",protoTypeI(memxDel));
}
extern "C" int memxSize(void *ptr) {return 0;} // get size
extern "C" int memxInt(void *ptr) {return 0;} // get int
extern "C" int memxMask(void *ptr) {return 0;} // mask from collection
extern "C" const char *memxStr(void *mem) {return 0;} // get string
extern "C" void memxInit(void **mem, const char *str) {} // convert from string
extern "C" void *memxTemp(int idx) {return 0;} // realloc indexed memory
extern "C" void memxCopy(void **mem, void *giv) {} // replaces target with given
extern "C" void memxList(void **mem, void *giv) {} // adds given to target in order
extern "C" void memxKeep(void **mem, void *giv) {} // adds given to target unordered
extern "C" void memxDone(void **mem) {} // deletes target
extern "C" void memxCall(void **mem, void *giv, struct Prototype fnc) {} // call function on mem with giv
extern "C" void memxBack(void **mem, void **giv, struct Prototype fnc) {} // causes given call on given with target as given
extern "C" void memxDflt(void **mem, void **giv, struct Prototype fnc) {} // trivial change causes call on mem with giv
extern "C" void *memxFirst(void *giv) {return 0;} // get iterator from given
extern "C" int memxLast(void *giv) {return 0;} // check iterator of given
extern "C" void memxNext(void **mem, void *giv) {} // get iterator from given iterator
extern "C" void memxFind(void **mem, void *giv, void *key) {} // find iterator with given as first
extern "C" void memxSkip(void **mem, void *giv, int key) {} // skip to given iterator
extern "C" void memxAdd(void **mem, void *giv, int key) {} // insert at given location
extern "C" void memxDel(void **mem, int key) {} // delete at given location
