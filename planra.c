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
	void *ptr = 0;
	struct Irrex rex = {0};
	datxInt(&ptr,5);
	if (datxInts(ptr) != 1) ERROR();
	if (*datxIntz(0,ptr) != 5) ERROR();
	assignStr(&rex.exp,">abcdefg");
	if (datxIrrex("abc",&rex) != 1) ERROR();
	return 0;
}