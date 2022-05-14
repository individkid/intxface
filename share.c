#include "share.h"
#include "argx.h"
#include "face.h"

const char *shareScript(const char *str, int arg, int rep)
{
	// TODO assume bare numbers are initially input then output ports
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
	nest.typ = mod[3];
	switch (mod[1]) {
		case (0): nest.idx = forkExec(arg); break;
		case (1): nest.idx = openFile(arg); break;
		case (2): nest.idx = openInet(headName(arg),tailName(arg)); break;
		case (3): nest.str = arg; return nest;
		case (4): nest.idx = pipeInit(maybeName(arg,mod[2]),maybeName(arg,!mod[2])); break;
		case (5): nest.idx = openFifo(arg); break;
		default: break;}
	if (mod[3] == -1) {
		switch (mod[2]) {
			case (0): switch (mod[0]) {
				case (0): nest.fnc = 0; break; // TODO generated read text switch
				case (1): nest.fnc = 0; break; // TODO generated read binary switch
				default: break;} break;
			case (1): switch (mod[0]) {
				case (0): nest.fnc = 0; break; // TODO generated write text switch
				case (1): nest.fnc = 0; break; // TODO generated write binary switch
				default: break;} break;
			default: break;}}
	else {
		switch (mod[2]) {
			case (0): switch (mod[0]) {
				case (0): nest.gnc = 0; break; // TODO type generated read text
				case (1): nest.gnc = 0; break; // TODO type generated read binary
				default: break;} break;
			case (1): switch (mod[0]) {
				case (0): nest.gnc = 0; break; // TODO type generated write text
				case (1): nest.gnc = 0; break; // TODO type generated write binary
				default: break;} break;
			default: break;}}
	return nest;
}
void shareInit()
{
	addFlags("ab",0); // text or binary
	addFlags("efghpq",1); // type of stream
	addFlags("io",2); // direction of stream
	addConst("n",3); // type streamed
	addMode("cdjklm"); // flow control
	setFactory(shareFactory); // for unconsumed arguments
	setScript(shareScript); // for default arguments
	// TODO use addMulti and addElem for double dashes
}
