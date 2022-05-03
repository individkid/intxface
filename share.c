#include "argx.h"

struct ArgxNest streamFactory(const char *arg, int *mod, int lim)
{
	struct ArgxNest nest = {0};
	// TODO see readmeInit for interpretation of mod arguments
	// TODO open stream with face.h if not already open and
	// TODO return arg as script or stream function from type.h
	return nest;
}
void readmeInit()
{
	addFlags("ab",0); // text or binary
	addFlags("efgh",1); // type of stream
	addFlags("io",2); // direction of stream
	addConst("c",3); // type streamed
	addMode("mjklnd"); // flow control
	setFactory(streamFactory); // for unconsumed arguments
	// TODO setScript for initial unconsumed arguments
	// TODO use addMulti and addElem for double dashes
}