#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

#include "type.h"
#include "face.h"

int main(int argc, const char **argv)
{
	int count = 0; for (int i = 1; i < argc; i++) if (argv[i][0] == '-') count += 1;
	if (count == 0) {
	int tests = 0;
	for (int i = 1; i < argc; i++) {
	struct Argument arg = {}; int len = 0; 
	if (hideArgument(&arg,argv[i],&len)) tests += 1;}
	struct Argument *args[tests] = {}; tests = 0;
	for (int i = 1; i < argc; i++) {
	int len = 0; allocArgument(&args[tests],1);
	if (hideArgument(args[tests],argv[i],&len)) {
	args[tests]->idx = rdwrInit(args[tests]->inp,args[tests]->out);
	tests += 1;}}
	for (int i = 0; i < tests; i++) {
	writeInt(args[i]->typ,args[i]->idx);
	writeInt(args[i]->oth,args[i]->idx);}
	for (int i = 0; i < tests; i++) {
	enum Program slf = readInt(args[i]->idx);
	enum Program oth = readInt(args[i]->idx);
	writeInt(args[i]->typ,args[i]->idx);
	char *st0 = 0; char *st1 = 0; char *st2 = 0; char *st3 = 0;
	showProgram(args[i]->typ,&st0);
	showProgram(slf,&st1);
	showProgram(args[i]->oth,&st2);
	showProgram(oth,&st3);
	printf("%s:%s==%s)\n",st0,st0,st1);
	printf("%s:%s==%s)\n",st0,st2,st3);}
	} else {
	int words[count] = {}; char **procs[count]; char *shows[count][count-1];
	int rdfd[count][count-1]; int wrfd[count][count-1]; enum Program enums[count];
	for (int i = 0; i < count; i++) words[i] = 0; count = 0;
	for (int i = 1; i < argc; i++) if (argv[i][0] == '-') count += 1; else if (count > 0) words[count-1] += 1;
	for (int i = 0; i < count; i++) procs[i] = malloc((words[i]+count-1)*sizeof(char*));
	for (int i = 0; i < count; i++) words[i] = 0; count = 0;
	for (int i = 1; i < argc; i++) if (argv[i][0] == '-') count += 1; else if (count > 0) {
	procs[count-1][words[count-1]] = malloc(strlen(argv[i])+1);
	strcpy(procs[count-1][words[count-1]],argv[i]); words[count-1] += 1;} count = 0;
	for (int i = 1; i < argc; i++) if (argv[i][0] == '-') {
	int len = 1; hideProgram(&enums[count],argv[i],&len); count += 1;}
	for (int i = 0; i < count; i++) for (int j = 0; j < count; j++) if (i != j) {
	int fdes[2]; if (pipe(fdes) < 0) return -1; rdfd[i][j-(j>i)] = fdes[0]; wrfd[i][j-(j>i)] = fdes[1];}
	for (int i = 0; i < count; i++) for (int j = 0; j < count; j++) if (i != j) {
	struct Argument arg = {enums[i],enums[j],rdfd[i][j-(j>i)],wrfd[i][j-(j>i)]};
	char *show = 0; showArgument(&arg,&show); shows[i][j-(j>i)] = show;}
	for (int i = 0; i < count; i++) for (int j = 0; j < count; j++) if (i != j) {
	procs[i][words[i]] = shows[i][j-(j>i)]; words[i] += 1;}
	for (int i = 0; i < count; i++) if (fork() == 0) execv(procs[i][0],procs[i]);
	while (wait(0) >= 0); if (errno != ECHILD) return -2;}
	return 0;
}
