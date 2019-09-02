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
str = readStr(0)
print(string.format("spoke%d %s",arg[3],str))
writeStr(str,0)
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
writeStr("zero",0)
writeStr("one",1)
writeStr("two",2)
done = {0,0,0}
index = waitAny()
while (index < 3) do
	sub = index+1
	if (done[sub] == 0) then
		print(string.format("hub %d %d",index,readInt(index)))
		done[sub] = done[sub] + 1
	elseif (done[sub] == 1) then
		print(string.format("hub %d %f",index,readNum(index)))
		done[sub] = done[sub] + 1
	elseif (done[sub] == 2) then
		print(string.format("hub %d %s",index,readStr(index)))
		done[sub] = done[sub] + 1
	else
		readInt(index)
		writeInt(-1,index)
		print(string.format("hub %d done",index))
	end
	index = waitAny()
end
print(string.format("hub done %d %d %d %d %d %d",
	checkRead(0),checkRead(1),checkRead(2),checkWrite(0),checkWrite(1),checkWrite(2)))
end
