#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "luax.h"
#include "type.h"
#include <stdio.h>
#include <string.h>

int datxIrrcmp(const char *str);
int datxIrrexe(const char *str, int idx);
int datxIrrex(const char *lft, const char *rgt)
{
	return datxIrrexe(lft,datxIrrcmp(rgt));
}
int datxRegcmp(const char *str);
int datxRegexe(const char *str, int idx);
int datxRegex(const char *lft, const char *rgt)
{
	return datxRegexe(lft,datxRegcmp(rgt));
}

int main()
{
	void *ptr = 0;
	datxInt(&ptr,5); if (datxInts(ptr) != 1) ERROR(); if (*datxIntz(0,ptr) != 5) ERROR();
	if (datxIrrex("abc",">abcdefg") != 1) ERROR();
	if (datxIrrex("bcd",">abcdefg") != 0) ERROR();
	if (datxIrrex("abcdef","abc=def") != 1) ERROR();
	if (datxIrrex("abcde","abc=def") != 0) ERROR();
	if (datxIrrex("def","|=abc^=def^^") != 1) ERROR();
	if (datxIrrex("abc","|=abc^=def^^") != 1) ERROR();
	if (datxIrrex("bcd","|=abc^=def^^") != 0) ERROR();
	if (datxIrrex("def","|=abc^=def") != 1) ERROR();
	if (datxIrrex("abc","|=abc^=def") != 1) ERROR();
	if (datxIrrex("bcd","|=abc^=def") != 0) ERROR();
	if (datxIrrex("abc","abc") != 1) ERROR();
	if (datxIrrex("abcghi","|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("defghi","|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("defgh","|=abc^=def^^ghi") != 0) ERROR();
	if (datxIrrex("876210abcghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("876543abcghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("876210defghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("876543defghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxRegex("abdef","ab[cd]ef") != 1) ERROR();
	if (datxRegex("abcef","ab[cd]ef") != 1) ERROR();
	return 0;
}