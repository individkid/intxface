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

#include <lua.h>

void forkExec(const char *exe);
void pipeInit(int argc, char **argv);
int pollString(int *siz, char *val);
int pollInt(int *val);
int pollNum(double *val);
int pollInts(int siz, int *val);
int pollNums(int siz, double *val);
int pollStringLua(lua_State *lua);
int pollIntLua(lua_State *lua);
int pollNumLua(lua_State *lua);
int pollIntsLua(lua_State *lua);
int pollNumsLua(lua_State *lua);
void readString(int siz, char *val);
void readInt(int *val);
void readNum(double *val);
void readInts(int siz, int *val);
void readNums(int siz, double *val);
int readStringLua(lua_State *lua);
int readIntLua(lua_State *lua);
int readNumLua(lua_State *lua);
int readIntsLua(lua_State *lua);
int readNumsLua(lua_State *lua);
void writeString(const char *val);
void writeInt(int val);
void writeNum(double val);
void writeInts(int siz, const int *val);
void writeNums(int siz, const double *val);
int writeStringLua(lua_State *lua);
int writeIntLua(lua_State *lua);
int writeNumLua(lua_State *lua);
int writeIntsLua(lua_State *lua);
int writeNumsLua(lua_State *lua);
