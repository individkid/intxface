#include "argx.h"
#include "memx.h"

int main(int argc, char **argv)
{
	addFlags("ab",WrapShare); // text or binary
	addFlags("efghpq",FlowShare); // type of stream
	addFlags("io",GateShare); // direction of stream
	addConst("m",TypeShare); // type streamed
	addConst("n",FieldShare); // field streamd
	addConst("j",SkipShare); // file descriptor skip count
	setMode("cdkl"); // flow control
	setFactory(argxFactory); // for unconsumed arguments
	// TODO use addMulti and addElem for double dashes
	// TODO call addDflt for default behavior
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	helloOkAgain();
	return 0;
}
