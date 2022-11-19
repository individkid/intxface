extern "C" {
#include "memx.h"
}

extern "C" int memxSize(void *ptr)
{
	return 0;
}
extern "C" int memxInt(void *ptr)
{
	return 0;
}
extern "C" void memxInit(void **mem, const char *str)
{
}
extern "C" void memxCopy(void **mem, void *giv)
{
}
extern "C" void memxList(void **mem, void *giv)
{
}
extern "C" void memxKeep(void **mem, void *giv)
{
}
extern "C" void memxMark(void **mem)
{
}
extern "C" void memxDone(void **mem)
{
}
extern "C" void memxCall(void **mem, void (*call)(void **mem, void *giv), void *giv)
{
}
extern "C" void *memxFirst(void *giv)
{
	return 0;
}
extern "C" int memxLast(void *giv)
{
	return 0;
}
extern "C" void *memxNext(void *giv)
{
	return 0;
}
extern "C" void *memxFind(void *giv, void *key)
{
	return 0;
}
extern "C" void *memxSkip(void *giv, int key)
{
	return 0;
}
extern "C" void *memxPast(void *giv, int key)
{
	return 0;
}
