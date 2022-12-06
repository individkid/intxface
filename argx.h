#include "proto.h"
#define NUMARGX 256
struct ArgxNest;
enum ArgxTag {
	FlowTag, // data flow
	FlagTag, // option flag
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
	struct Function fnc; // for str to use
	struct Function gnc; // for use to run
};
int argxJump(void *jmp);
void argxCopy(void **run, void *use);
void argxKeep(void **run, void *use);
struct ArgxNest *argxGet(int idx);
void *argxUse(int idx);
void *argxRun(int idx);
int argxHere();
int getLocation();
int addOption(const char *opt, struct Function fnc, struct Function gnc);
int addFlag(const char *opt, struct Function fnc, struct Function gnc);
int addJump(const char *opt, struct Function fnc, struct Function gnc);
int addNest(const char *opt, struct Function fnc, struct Function gnc);
int mapCallback(const char *str, int ref, struct Function fnc);
int mapDefault(const char *str, int ref, struct Function fnc);
int mapContext(const char *str, int ref, struct Function fnc);
int useArgument(const char *str);
void runProgram();
