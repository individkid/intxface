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
struct ArgxNest shareFactory(const char *arg, int *mod, int lim)
{
	struct ArgxNest nest = {0};
	if (lim < 4) return nest;
	nest.opc = FlowArgx;
	nest.typ = mod[TypeShare];
	switch (mod[FlowShare]) {
		case (0): nest.idx = forkExec(arg); break;
		case (1): nest.idx = openFile(arg); break;
		case (2): nest.idx = openInet(headName(arg),tailName(arg)); break;
		case (3): nest.str = arg; return nest;
		case (4): nest.idx = pipeInit(maybeName(arg,mod[GateShare]),maybeName(arg,!mod[GateShare])); break;
		case (5): nest.idx = openFifo(arg); break;
		default: break;}
	if (mod[TypeShare] == -1) {
		switch (mod[GateShare]) {
			case (0): switch (mod[WrapShare]) {
				case (0): nest.fnc = 0; break; // TODO generated read text switch
				case (1): nest.fnc = 0; break; // TODO generated read binary switch
				default: break;} break;
			case (1): switch (mod[WrapShare]) {
				case (0): nest.fnc = 0; break; // TODO generated write text switch
				case (1): nest.fnc = 0; break; // TODO generated write binary switch
				default: break;} break;
			default: break;}}
	else {
		switch (mod[GateShare]) {
			case (0): switch (mod[WrapShare]) {
				case (0): nest.gnc = 0; break; // TODO type generated read text
				case (1): nest.gnc = 0; break; // TODO type generated read binary
				default: break;} break;
			case (1): switch (mod[WrapShare]) {
				case (0): nest.gnc = 0; break; // TODO type generated write text
				case (1): nest.gnc = 0; break; // TODO type generated write binary
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
