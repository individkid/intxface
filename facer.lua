#!/usr/local/bin/lua
require "face"
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