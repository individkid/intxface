#include "proto.h"

enum ArgxType {
	ArgxInt,
	ArgxStr,
	ArgxTypes
};
union ArgxValue {
	int vad;
	const char *vas;
};

int memxSize(void *ptr); // get size
int memxInt(void *ptr); // get int
int memxMsk(void *ptr); // mask from collection
const char *memxStr(void *mem); // get string
void *memxRun(void *mem); // evaluate script
void memxInit(void **mem, const char *str); // convert from string
void memxForm(void **mem, enum ArgxType tag, union ArgxValue val); // format from type
void memxCopy(void **mem, void *giv); // replaces target with given
void memxList(void **mem, void *giv); // adds given to target in order
void memxKeep(void **mem, void *giv); // adds given to target unordered
void memxDone(void **mem); // deletes target
void memxCall(void **mem, zftype fnc, void **giv); // causes given call on given with target as given
void *memxFirst(void *giv); // get iterator from given
int memxLast(void *giv); // check iterator of given
void *memxNext(void *giv); // get iterator from given iterator
void *memxFind(void *giv, void *key); // find iterator with given as first
void *memxSkip(void *giv, int key); // skip to given iterator
