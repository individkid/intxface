#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>

struct Bus {
	int siz;
	int *typ;
	void **dat;
};

int idx = 0;
void *dat = 0;

int shareRead(int fildes, void *buf, int nbyte)
{
	memcpy(buf,datxData(dat),*(int*)dat);
	return 0;
}
int shareWrite(int fildes, const void *buf, int nbyte)
{
	dat = realloc(dat,nbyte+sizeof(int));
	*(int*)dat = nbyte;
	memcpy(datxData(dat),buf,nbyte);
	return 0;
}

int runStage(struct Bus *dst, const struct Bus *src, const struct Stage *ptr)
{
	return 0; // TODO return -1 for more in src; return 1 for stage done
}
void runPipe(struct Bus *dst, const struct Bus *src, const struct Stage *ptr)
{
	// TODO count links; allocate array of links; start with last; go back on -1;
	// use src for first; return dst on 1 from last; use dst as src of nxt on 1;
}

int main(int argc, char **argv)
{
	struct Stage *start = 0;
	struct Stage **next = &start;
	struct Bus dst = {0};
	struct Bus src = {0};
	idx = buntInit(0,0,shareRead,shareWrite);
	for (int i = 1; i < argc; i++) {
		int len = 0;
		// TODO deduce typ and writeType to src if not Stage
		allocStage(next,1);
		hideStage(*next,argv[i],&len);
		next = &(*next)->nxt;}
	runPipe(&dst,&src,start);
	// TODO for each of dst, deduce typ and readType to stdout
	return 0;
}