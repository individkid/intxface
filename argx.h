#include "proto.h"
int argxJump(void *jmp);
void argxCopy(void **run, void *use);
void argxKeep(void **run, void *use);
void *argxUse(int idx);
void *argxRun(int idx);
int argxHere();
int addFlow(const char *opt, struct Function fnc, struct Function gnc);
int addFlag(const char *opt, struct Function fnc, struct Function gnc);
int addJump(const char *opt, struct Function fnc, struct Function gnc);
int addSide(const char *opt, struct Function use, struct Function run);
int addNest(const char *opt, struct Function fnc, struct Function gnc);
int mapCallback(const char *str, int ref, struct Function fnc);
int mapDefault(const char *str, int ref, struct Function fnc);
int mapContext(const char *str, int ref, struct Function fnc);
int getLocation();
int useArgument(const char *str);
void runProgram();
void stopProgram();
