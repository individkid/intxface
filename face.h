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
int forkExecLua(lua_State *lua);
void pipeInit(const char *av1, const char *av2);
int pipeInitLua(lua_State *lua);
int waitAny();
int waitAnyLua(lua_State *lua);
int checkRead(int idx);
int checkReadLua(lua_State *lua);
int checkWrite(int idx);
int checkWriteLua(lua_State *lua);
void sleepSec(int sec);
int sleepSecLua(lua_State *lua);
const char *readStr(int idx);
int readStrLua(lua_State *lua);
int readInt(int idx);
int readIntLua(lua_State *lua);
double readNum(int idx);
int readNumLua(lua_State *lua);
void writeStr(const char *arg, int idx);
int writeStrLua(lua_State *lua);
void writeInt(int arg, int idx);
int writeIntLua(lua_State *lua);
void writeNum(double arg, int idx);
int writeNumLua(lua_State *lua);
