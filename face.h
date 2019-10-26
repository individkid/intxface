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

#define ERROR(FNC,ARG) {fprintf(stderr,"%s(%d): %d\n",__FILE__,__LINE__,errno); if (FNC) FNC(ARG); else exit(-1);}
#define NUMOPEN 256

typedef void (*eftype)(int);
void readJump(eftype err, int idx);
void writeJump(eftype err, int idx);
void bothJump(eftype err, int idx);
void closeIdent(int idx);
int addPipe(int fd0, int fd1);
int addFile(int fd);
void setFile(int fd, int idx);
int addPipe(int fd0, int fd1);
int forkExec(const char *exe);
int pipeInit(const char *av1, const char *av2);
int waitAny();
int pollPipe(int idx);
int todoFile(int idx);
int checkRead(int idx);
int checkWrite(int idx);
void sleepSec(int sec);
const char *readStr(int idx);
int readInt(int idx);
long long readNew(int idx);
double readNum(int idx);
float readOld(int idx);
void writeStr(const char *arg, int idx);
void writeInt(int arg, int idx);
void writeNum(double arg, int idx);
void writeNew(long long arg, int idx);
void writeOld(float arg, int idx);

#endif
