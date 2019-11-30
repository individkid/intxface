/*
*    face.h
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef FACE_H
#define FACE_H

#define ERROR(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}
#define NOTICE(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}
#define NUMOPEN 1024
#define BUFSIZE 64
#define NUMINET 16
#define NUMPEND 10

typedef void (*eftype)(const char*,int,int);
typedef void (*hftype)(const char*,int);
typedef void (*cftype)(const char*,int,void*);
void readNote(eftype exc, int idx);
void readJump(eftype err, int idx);
void writeJump(eftype err, int idx);
void bothJump(eftype err, int idx);
void closeIdent(int idx);
void moveIdent(int idx0, int idx1);
int openPipe();
int openFifo(const char *str);
int openFile(const char *str);
int openInet(const char *adr, const char *num);
int forkExec(const char *exe);
int pipeInit(const char *av1, const char *av2);
int waitAny();
int pollPipe(int idx);
int pollFile(int idx);
void seekFile(long long arg, int idx);
void truncFile(int idx);
long long checkFile(int idx);
int pollInet(const char *adr, const char *num);
int checkInet(const char *adr, const char *num);
int rdlkFile(long long arg0, long long arg1, int idx);
int wrlkFile(long long arg0, long long arg1, int idx);
void unlkFile(long long arg0, long long arg1, int idx);
void rdlkwFile(long long arg0, long long arg1, int idx);
void wrlkwFile(long long arg0, long long arg1, int idx);
int checkRead(int idx);
int checkWrite(int idx);
void sleepSec(int sec);
void readStr(cftype fnc, void *arg, int idx);
void readStrHs(hftype fnc, int idx);
int readInt(int idx);
long long readNew(int idx);
double readNum(int idx);
float readOld(int idx);
void writeStr(const char *arg, int trm, int idx);
void writeInt(int arg, int idx);
void writeNum(double arg, int idx);
void writeNew(long long arg, int idx);
void writeOld(float arg, int idx);

#endif
