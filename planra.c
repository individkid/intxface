#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "luax.h"
#include "type.h"
#include <stdio.h>

int datxIrrex(const char *lft, struct Irrex *rgt);

int main()
{
	void *ptr = 0; struct Irrex zro = {0}; struct Irrex rex = {0};
	datxInt(&ptr,5); if (datxInts(ptr) != 1) ERROR(); if (*datxIntz(0,ptr) != 5) ERROR();
	rex = zro; assignStr(&rex.exp,">abcdefg"); if (datxIrrex("abc",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,">abcdefg"); if (datxIrrex("bcd",&rex) != 0) ERROR();
	rex = zro; assignStr(&rex.exp,"abc=def"); if (datxIrrex("abcdef",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def^^"); if (datxIrrex("def",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def^^"); if (datxIrrex("abc",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def^^"); if (datxIrrex("bcd",&rex) != 0) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def"); if (datxIrrex("def",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def"); if (datxIrrex("abc",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def"); if (datxIrrex("bcd",&rex) != 0) ERROR();
	rex = zro; assignStr(&rex.exp,"abc"); if (datxIrrex("abc",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def^^ghi"); if (datxIrrex("abcghi",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def^^ghi"); if (datxIrrex("defghi",&rex) != 1) ERROR();
	rex = zro; assignStr(&rex.exp,"|=abc^=def^^ghi"); if (datxIrrex("defgh",&rex) != 0) ERROR();
	rex = zro; assignStr(&rex.exp,"876|=543^=210^^|=abc^=def^^ghi"); if (datxIrrex("876210abcghi",&rex) != 1) ERROR();
	return 0;
}