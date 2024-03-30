#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <libgen.h>
#include <stdarg.h>
#include <execinfo.h>

char *exestr = 0;
char *exetmp = 0;
char *msgstr = 0;
char *msgtmp = 0;
chtype intrfn = 0;
hgtype notice = 0;
eftype errfnc = 0;

void stackErr()
{
	void *array[10];
	size_t size;
	// get void*'s for all entries on the stack
	size = backtrace(array, 10);
	// print out all the frames to stderr
	backtrace_symbols_fd(array, size, STDERR_FILENO);
}
void exitErr(const char *file, int line)
{
	stackErr();
	fprintf(stderr,"exitErr %s(%d): %d %lld\n",file,line,errno,(long long)getpid());
	exit(-1);
}
void intrFunc(chtype fnc)
{
	intrfn = fnc;
}
void noteFunc(hgtype fnc)
{
	notice = fnc;
}
void errFunc(eftype fnc)
{
	errfnc = fnc;
}
void callIntr()
{
	if (intrfn) intrfn();
}
void callNote(const char *file, int line, int idx)
{
	if (notice) notice(idx); else callErr(file,line,idx);
}
void callErr(const char *file, int line, int idx)
{
	if (errfnc) errfnc(file,line,idx); else ERROR();
}
void protoSet(const char *str)
{
	int len = 0;
	if (exestr) {
		exestr = realloc(exestr,strlen(exestr)+1+strlen(str)+1);
		strcat(exestr,":");
		strcat(exestr,str);}
	else {
		exestr = malloc(strlen(str)+1);
		strcpy(exestr,str);}
	exetmp = realloc(exetmp,len = strlen(exestr));
	strcpy(exetmp,exestr);
	for (int i = 0; i < len; i++) if (exestr[i] == ':') exetmp[i] = 0;
}
const char *protoGet(int i)
{
	int len = 0;
	int j = 0;
	if (i == 0) return "";
	if (exestr == 0) return 0;
	len = strlen(exestr);
	for (j = 0, i--; j < len && i > 0; j++) if (exestr[j] == ':') i--;
	return (i == 0 ? dirname(exetmp+j) : 0);
}
void protoErr(const char *fmt, ...)
{
	char *temp = 0;
	va_list args = {0};
	va_start(args,fmt);
	vasprintf(&temp,fmt,args);
	va_end(args);
	if (msgstr) {
		msgstr = realloc(msgstr,strlen(msgstr)+strlen(temp)+1);
		strcat(msgstr,temp);}
	else {
		msgstr = malloc(strlen(temp)+1);
		strcpy(msgstr,temp);}
	free(temp);
}
const char *protoMsg()
{
	if (msgtmp) free(msgtmp);
	msgtmp = msgstr;
	msgstr = 0;
	return msgtmp;
}
int protoPath(const char *exp)
{
	const char *str = 0;
	char *temp = 0;
	int val = 0;
	for (int i = 0; (str = protoGet(i)); i++) {
	asprintf(&temp,"%s%s",str,exp);
	val = access(temp,R_OK);
	free(temp);
	if (val == 0) return i;}
	return -1;
}
