#include "share.h"
#include "argx.h"
#include "face.h"

const char *shareScript(const char *str, int arg, int rep)
{
	// TODO assume bare numbers are initially input then output ports
	// TODO use mod[ShareShare] for state machine
	// TODO add call to setArgument("") if str is ""
	return 0;
}
const char *headName(const char *str)
{
	return 0;
}
const char *tailName(const char *str)
{
	return 0;
}
const char *maybeName(const char *str, int dir)
{
	return 0;
}
struct ArgxNest shareFactory(const char *str, int *arg)
{
	struct ArgxNest nest = {0};
	nest.opc = FlowArgx;
	nest.typ = arg[TypeShare];
	switch (arg[FlowShare]) {
		case (0/*e*/): nest.idx = forkExec(str); break;
		case (1/*f*/): nest.idx = openFile(str); break;
		case (2/*g*/): nest.idx = openInet(headName(str),tailName(str)); break;
		case (3/*h*/): nest.str = str; return nest;
		case (4/*p*/): nest.idx = pipeInit(maybeName(str,arg[GateShare]),maybeName(str,!arg[GateShare])); break;
		case (5/*q*/): nest.idx = openFifo(str); break;
		default: break;}
	if (arg[TypeShare]/*generte type identifier*/ == -1) {
		switch (arg[GateShare]) {
			case (0/*i*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.fnc = 0; break; // TODO generated read text switch
				case (1/*b*/): nest.fnc = 0; break; // TODO generated read binary switch
				default: break;} break;
			case (1/*o*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.fnc = 0; break; // TODO generated write text switch
				case (1/*b*/): nest.fnc = 0; break; // TODO generated write binary switch
				default: break;} break;
			default: break;}}
	else {
		switch (arg[GateShare]) {
			case (0/*i*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.gnc = 0; break; // TODO type generated read text
				case (1/*b*/): nest.gnc = 0; break; // TODO type generated read binary
				default: break;} break;
			case (1/*o*/): switch (arg[WrapShare]) {
				case (0/*a*/): nest.gnc = 0; break; // TODO type generated write text
				case (1/*b*/): nest.gnc = 0; break; // TODO type generated write binary
				default: break;} break;
			default: break;}}
	return nest;
}
void shareInit()
{
	addFlags("ab",WrapShare); // text or binary
	addFlags("efghpq",FlowShare); // type of stream
	addFlags("io",GateShare); // direction of stream
	addConst("n",TypeShare); // type streamed
	addMode("cdjklm"); // flow control
	setFactory(shareFactory); // for unconsumed arguments
	setScript(shareScript); // for default arguments
	// TODO to handle double dashes call setFactory in the caller with a wrapper of shareFactory
	// TODO use addMulti and addElem in the caller for double dashes
}
