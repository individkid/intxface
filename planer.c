#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <poll.h>

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
	for (int i = 0; i < tests; i++) if (args[i]->typ != args[i]->oth) {
	enum Program slf = readInt(args[i]->idx);
	enum Program oth = readInt(args[i]->idx);
	char *st0 = 0; char *st1 = 0; char *st2 = 0; char *st3 = 0;
	showProgram(args[i]->typ,&st0);
	showProgram(slf,&st1);
	showProgram(oth,&st2);
	printf("%s:%s:%s\n",st0,st1,st2);}
	} else {
	int words[count] = {}; char **procs[count]; char *shows[count][count]; enum Program enums[count];
	int rdfd[count][count] = {}; int wrfd[count][count] = {}; pid_t pids[count] = {};
	pid_t gpid = getpid(); setpgid(0,0);
	for (int i = 0; i < count; i++) words[i] = 0; count = 0;
	for (int i = 1; i < argc; i++) if (argv[i][0] == '-') count += 1; else if (count > 0) words[count-1] += 1;
	for (int i = 0; i < count; i++) procs[i] = malloc((words[i]+count+1)*sizeof(char*));
	for (int i = 0; i < count; i++) words[i] = 0; count = 0;
	for (int i = 1; i < argc; i++) if (argv[i][0] == '-') count += 1; else if (count > 0) {
	procs[count-1][words[count-1]] = malloc(strlen(argv[i])+1);
	strcpy(procs[count-1][words[count-1]],argv[i]); words[count-1] += 1;} count = 0;
	for (int i = 1; i < argc; i++) if (argv[i][0] == '-') {
	int len = 1; hideProgram(&enums[count],argv[i],&len); count += 1;}
	for (int i = 0; i < count; i++) for (int j = 0; j < count; j++) {
	int fdes[2]; if (pipe(fdes) < 0) return -1;
	rdfd[i][j] = fdes[0]; wrfd[j][i] = fdes[1];}
	for (int i = 0; i < count; i++) for (int j = 0; j < count; j++) {
	struct Argument arg = {enums[i],enums[j],rdfd[i][j],wrfd[i][j]};
	char *show = 0; showArgument(&arg,&show); shows[i][j] = show;}
	for (int i = 0; i < count; i++) for (int j = 0; j < count; j++) {
	procs[i][words[i]] = shows[i][j]; words[i] += 1;}
	for (int i = 0; i < count; i++) procs[i][words[i]] = 0;
	for (int i = 0; i < count; i++) {
	pids[i] = fork(); if (pids[i] < 0) return -1;
	if (pids[i] == 0) {
	execv(procs[i][0],procs[i]);
	fprintf(stderr,"%s: cannot execute file: %s\n",argv[0],procs[i][0]);
	kill(-gpid,SIGTERM); return -2;}}
	pid_t pid; while ((pid = wait(0)) >= 0) for (int i = 0; i < count; i++) if (pid == pids[i]) {
	struct pollfd pfd; pfd.fd = rdfd[i][i]; pfd.events = POLLIN; int ret = poll(&pfd,1,0);
	if (ret <= 0) {
	fprintf(stderr,"poll negative %d %d\n",ret,errno);
	kill(-gpid,SIGTERM); return -3;}
	break;} if (errno != ECHILD) return -4;}
	return 0;
}
