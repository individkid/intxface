extern "C" {
#include "memx.h"
}
#include <iostream>

extern "C" int memxSize(void *ptr)
{
	return 0; // TODO
}
extern "C" void *memxElem(void *ptr, int idx)
{
	return 0; // TODO
}
extern "C" int memxInt(void *ptr)
{
	return 0; // TODO
}
extern "C" void *memxInit(const char *str)
{
	return 0; // TODO
}
extern "C" void *memxHist(void *ptr)
{
	return 0; // TODO
}
extern "C" void *memxAcum(void *ptr)
{
	return 0; // TODO
}
extern "C" void *memxNoop()
{
	return 0; // TODO
}

void hello()
{
	std::cout << "hello";
}
void ok()
{
	std::cout << "ok";
}
void again()
{
	std::cout << "again";
}
void space()
{
	std::cout << " ";
}
void endline()
{
	std::cout << std::endl;
}
extern "C" void helloOkAgain()
{
	hello();
	space();
	ok();
	space();
	again();
	endline();
}