require "face"

function errClose(file,line,idx)
	closeIdent(idx)
end
function noteClose(idx)
	closeIdent(idx)
end

-- MAIN
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
noteFunc("noteClose")
errFunc("errClose")
forkExec("facerC")
forkExec("facerLua")
sleepSec(1)
expectInt = {0,1,2}
expectNum = {0.1,1.1,2.1}
expectStr = {"zero","one","two"}
expectNew = {10,11,12}
expectOld = {0.2,1.2,2.2}
for index=0,1,1 do
	sub = index+1
	writeInt(expectInt[sub],index)
	writeNum(expectNum[sub],index)
	writeStr(expectStr[sub],index)
	writeNew(expectNew[sub],index)
	writeOld(expectOld[sub],index)
end
done = {0,0,0}
index = waitRead(0.0,-1)
while (index >= 0) do
	sub = index+1
	if (done[sub] == 0) then
		value = readInt(index)
		if value ~= expectInt[sub] then print(string.format("mismatch %d %d %d %d",value,expectInt[sub],index,done[sub])); assert(false) end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 1) then
		value = readNum(index)
		if value ~= expectNum[sub] then print(string.format("mismatch %f %f %d %d",value,expectNum[sub],index,done[sub])); assert(false) end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 2) then
		value = readStr(index)
		if value ~= expectStr[sub] then print(string.format("mismatch %s %s %d %d",value,expectStr[sub],index,done[sub])); assert(false) end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 3) then
		value = readNew(index)
		if value ~= expectNew[sub] then print(string.format("mismatch %d %d %d %d",value,expectNew[sub],index,done[sub])); assert(false) end
		done[sub] = done[sub] + 1
	elseif (done[sub] == 4) then
		value = readOld(index)
		diff = value-expectOld[sub]
		if diff < 0.0 then diff = -diff end
		if diff > 0.0001 then print(string.format("mismatch %f %f %d %d",value,expectOld[sub],index,done[sub])); assert(false) end
		done[sub] = done[sub] + 1
	else
		readInt(index)
	end
	index = waitRead(0.0,-1)
end
assert(checkRead(0) == 0)
assert(checkRead(1) == 0)
assert(checkWrite(0) == 0)
assert(checkWrite(1) == 0)
end
