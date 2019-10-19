#!/usr/bin/env lua
--[[
*    facer.lua
*    Copyright (C) 2019  Paul Coelho
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
--]]

require "face"
if (arg[1] and arg[2] and arg[3]) then
pipeInit(arg[1],arg[2])
int = readInt(0)
writeInt(int,0)
num = readNum(0)
writeNum(num,0)
str = readStr(0)
writeStr(str,0)
new = readNew(0)
writeNew(new,0)
old = readOld(0)
writeOld(old,0)
else
forkExec("a.out")
forkExec("b.out")
forkExec("facer.lua")
sleepSec(1)
expectInt = {0,1,2}
expectNum = {0.1,1.1,2.1}
expectStr = {"zero","one","two"}
expectNew = {10,11,12}
expectOld = {0.2,1.2,2.2}
for index=0,2,1 do
	sub = index+1
	writeInt(expectInt[sub],index)
	writeNum(expectNum[sub],index)
	writeStr(expectStr[sub],index)
	writeNew(expectNew[sub],index)
	writeOld(expectOld[sub],index)
end
done = {0,0,0}
index = waitAny()
while (index < 3) do
	sub = index+1
	if (done[sub] == 0) then
		value = readInt(index)
		if value ~= expectInt[sub] then print(string.format("mismatch %d %d",value,index)); os.exit() end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 1) then
		value = readNum(index)
		if value ~= expectNum[sub] then print(string.format("mismatch %f %d",value,index)); os.exit() end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 2) then
		value = readStr(index)
		if value ~= expectStr[sub] then print(string.format("mismatch %s %d",value,index)); os.exit() end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 3) then
		value = readNew(index)
		if value ~= expectNew[sub] then print(string.format("mismatch %d %d",value,index)); os.exit() end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 4) then
		value = readOld(index)
		if value ~= expectOld[sub] then print(string.format("mismatch %f %d",value,index)); os.exit() end
		done[sub] = done[sub] + 1
	else
		readInt(index)
		writeInt(-1,index)
	end
	index = waitAny()
end
print(string.format("facer.lua %d %d %d %d %d %d",
	checkRead(0),checkRead(1),checkRead(2),checkWrite(0),checkWrite(1),checkWrite(2)))
end
