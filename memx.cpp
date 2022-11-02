extern "C" {
#include "memx.h"
}
#include <iostream>

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