#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <stdarg.h>

char *msgstr = 0;
char *msgtmp = 0;
chtype intrfn = 0;
hgtype notice = 0;
eftype errfnc = 0;

void exitErr(const char *file, int line)
{
	*(int*)0=0;
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

