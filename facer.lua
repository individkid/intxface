#!/usr/local/bin/lua
require "face"
if (arg[1] and arg[2] and arg[3]) then
print("start spoke" .. arg[3])
pipeInit(arg[1],arg[2])
int = readInt(0)
print(string.format("spoke%d %d",arg[3],int))
writeInt(int,0)
num = readNum(0)
print(string.format("spoke%d %f",arg[3],num))
writeNum(num,0)
str = readString(0)
print(string.format("spoke%d %s",arg[3],str))
writeString(str,0)
print("spoke" .. arg[3] .. " done")
else
print("start hub")
forkExec("a.out")
forkExec("b.out")
forkExec("facer.lua")
sleepSec(1)
writeInt(0,0)
writeInt(1,1)
writeInt(2,2)
writeNum(0.1,0)
writeNum(1.1,1)
writeNum(2.1,2)
writeString("zero",0)
writeString("one",1)
writeString("two",2)
print(string.format("hub %d %d %d",readInt(0),readInt(1),readInt(2)))
print(string.format("hub %f %f %f",readNum(0),readNum(1),readNum(2)))
print(string.format("hub %s %s %s",readString(0),readString(1),readString(2)))
readInt(0)
readInt(1)
readInt(2)
writeInt(-1,0)
writeInt(-1,1)
writeInt(-1,2)
print(string.format("hub done %d %d %d %d %d %d",checkRead(0),checkRead(1),checkRead(2),checkWrite(0),checkWrite(1),checkWrite(2)))
end
