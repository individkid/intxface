#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
void exitErr(const char *str, int num, int idx)
{
	fprintf(stderr,"exitErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	exit(-1);
}
