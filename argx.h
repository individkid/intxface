#include "proto.h"
#define NUMARGX 256
struct ArgxNest;
enum ArgxTag {
	FlowTag, // data flow stream
	JumpTag, // loop break nest
	NestTag, // nesting control
	NoopTag, // data container
	ArgxTags
};
struct ArgxNest {
	enum ArgxTag opc; // type of data flow control step
	char opt; // dash char from before str
	void *str; // string after dash option
	void *use; // script or constant
	void *run; // result or copy
	struct Prototype fnc; // for str to use
	struct Prototype gnc; // for use to run
};
int argxJump(void *jmp);
void argxCopy(void **run, void *use);
void argxKeep(void **run, void *use);
struct ArgxNest *argxGet(int idx);
int argxHere();
int getLocation();
int addOption(const char *opt, struct Prototype fnc, struct Prototype gnc);
int addJump(const char *opt, struct Prototype fnc, struct Prototype gnc);
int addNest(const char *opt, struct Prototype fnc, struct Prototype gnc);
int mapCallback(const char *str, int ref, struct Prototype fnc);
int mapDefault(const char *str, int ref, struct Prototype fnc);
int useArgument(const char *str);
void runProgram();
