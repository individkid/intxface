#include "plane.h"

int argc = 0;
char **argv = 0;
void planeArg(const char *arg)
{
	argc++;
	argv = realloc(argv,argc*sizeof(*argv));
	argv[argc-1] = malloc(strlen(arg)+1);
	strcpy(argv[argc-1],arg);
}
