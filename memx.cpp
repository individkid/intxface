extern "C" {
#include "memx.h"
#include "luax.h"
}

extern "C" void memxLuax()
{
	luaxAdd("memxSize",protoTypeO(memxSize));
	luaxAdd("memxInt",protoTypeO(memxInt));
	luaxAdd("memxMask",protoTypeO(memxMask));
	luaxAdd("memxStr",protoTypeA(memxStr));
	luaxAdd("memxInit",protoTypeN(memxInit));
	luaxAdd("memxTemp",protoTypeT(memxTemp));
	luaxAdd("memxOpen",protoTypeL(memxOpen));
	luaxAdd("memxCopy",protoTypeM(memxCopy));
	luaxAdd("memxList",protoTypeM(memxList));
	luaxAdd("memxKeep",protoTypeM(memxKeep));
	luaxAdd("memxDone",protoTypeD(memxDone));
	luaxAdd("memxFirst",protoTypeM(memxFirst));
	luaxAdd("memxTest",protoTypeO(memxTest));
	luaxAdd("memxNext",protoTypeM(memxNext));
	luaxAdd("memxFind",protoTypeJ(memxFind));
	luaxAdd("memxSkip",protoTypeK(memxSkip));
	luaxAdd("memxAdd",protoTypeK(memxAdd));
	luaxAdd("memxDel",protoTypeI(memxDel));
}
extern "C" int memxSize(void *ptr) {return 0;} // get size
extern "C" int memxInt(void *ptr) {return 0;} // get int
extern "C" int memxMask(void *ptr) {return 0;} // mask from collection
extern "C" const char *memxStr(void *mem) {return 0;} // get string
extern "C" void memxInit(void **mem, const char *str) {} // interpret string
extern "C" void memxForm(void **mem, const char *fmt, ...) {} // use vasprintf
extern "C" void *memxTemp(int idx) {return 0;} // realloc indexed memory
extern "C" int memxOpen(void **ptr) {return 0;} // get pipe punted to given
extern "C" void memxCopy(void **mem, void *giv) {} // replaces target with given
extern "C" void memxList(void **mem, void *giv) {} // adds given to target in order
extern "C" void memxKeep(void **mem, void *giv) {} // adds given to target unordered
extern "C" void memxDone(void **mem) {} // deletes target
extern "C" void memxCall(void **mem, void *giv, struct Function fnc) {} // call function on mem with giv
extern "C" void memxBack(void **mem, void **giv, struct Function fnc) {} // causes given call on given with target as given
extern "C" void memxDflt(void **mem, void **giv, struct Function fnc) {} // trivial change causes call on mem with giv
extern "C" void memxFirst(void **mem, void *giv) {} // get iterator from given
extern "C" int memxTest(void *giv) {return 0;} // check iterator of given
extern "C" void memxNext(void **mem, void *giv) {} // get iterator from given iterator
extern "C" void memxFind(void **mem, void *giv, void *key) {} // find iterator with given as first
extern "C" void memxSkip(void **mem, void *giv, int key) {} // skip to given iterator
extern "C" void memxAdd(void **mem, void *giv, int key) {} // insert at given location
extern "C" void memxDel(void **mem, int key) {} // delete at given location
