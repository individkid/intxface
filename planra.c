#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "luax.h"
#include "type.h"
#include <stdio.h>
#include <string.h>

int datxIrrex(const char *lft, struct Irrex *rgt);
int datxRegex(const char *lft, struct Regex *rgt);

int main()
{
	void *ptr = 0; struct Irrex irx = {0}; struct Regex rex = {0};
	datxInt(&ptr,5); if (datxInts(ptr) != 1) ERROR(); if (*datxIntz(0,ptr) != 5) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,">abcdefg"); if (datxIrrex("abc",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,">abcdefg"); if (datxIrrex("bcd",&irx) != 0) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"abc=def"); if (datxIrrex("abcdef",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"abc=def"); if (datxIrrex("abcde",&irx) != 0) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def^^"); if (datxIrrex("def",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def^^"); if (datxIrrex("abc",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def^^"); if (datxIrrex("bcd",&irx) != 0) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def"); if (datxIrrex("def",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def"); if (datxIrrex("abc",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def"); if (datxIrrex("bcd",&irx) != 0) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"abc"); if (datxIrrex("abc",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def^^ghi"); if (datxIrrex("abcghi",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def^^ghi"); if (datxIrrex("defghi",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"|=abc^=def^^ghi"); if (datxIrrex("defgh",&irx) != 0) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"876|=543^=210^^|=abc^=def^^ghi"); if (datxIrrex("876210abcghi",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"876|=543^=210^^|=abc^=def^^ghi"); if (datxIrrex("876543abcghi",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"876|=543^=210^^|=abc^=def^^ghi"); if (datxIrrex("876210defghi",&irx) != 1) ERROR();
	memset(&irx,0,sizeof(struct Irrex)); assignStr(&irx.exp,"876|=543^=210^^|=abc^=def^^ghi"); if (datxIrrex("876543defghi",&irx) != 1) ERROR();
	memset(&rex,0,sizeof(struct Regex)); assignStr(&rex.str,"ab[cd]ef"); if (datxRegex("abdef",&rex) != 1) ERROR();
	memset(&rex,0,sizeof(struct Regex)); assignStr(&rex.str,"ab[cd]ef"); if (datxRegex("abcef",&rex) != 1) ERROR();
	return 0;
}