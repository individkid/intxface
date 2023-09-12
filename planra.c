#include "proto.h"
#include "datx.h"

int main()
{
	void *ptr = 0;
	datxInt(&ptr,5);
	if (datxInts(ptr) != 1) ERROR();
	if (*datxIntz(0,ptr) != 5) ERROR();
	return 0;
}