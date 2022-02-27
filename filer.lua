dofile("type.lua")

prefix = "hello "
field1 = "ok"
field2 = "OK"
separator = " "
suffix = "again"
data1 = prefix..field1..separator..suffix
data2 = prefix..field2
data3 = suffix
name = "filer.--"

function checkRead(ident)
	file = readFile(ident)
	if (file["act"] == "ThdErr") then print("ThdErr "..file["str"].." "..file["idx"]); exit() end
	if (file["act"] == "HubErr") then print("HubErr "..file["str"]); exit() end
	return file
end

-- MAIN
ident1 = forkExec("file")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
debugStr("before writeFile 1")
writeFile(file,ident1)
debugStr("after writeFile 1")
file = {}
file["act"] = "CfgHub"
file["idx"] = 0
file["loc"] = 0
file["str"] = data1
debugStr("before writeFile 2")
writeFile(file,ident1)
debugStr("after writeFile 2")
file = checkRead(ident1)
debugStr("after checkRead 2")
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 1)
assert(file["str"] == data1)

ident2 = forkExec("file")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
debugStr("before writeFile 3")
writeFile(file,ident2)
debugStr("after writeFile 3")
file = checkRead(ident2)
debugStr("after checkRead 3")
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == data1)
file = {}
file["act"] = "CfgHub"
file["idx"] = 0
file["loc"] = string.len(prefix)
file["str"] = field2
debugStr("before writeFile 4")
writeFile(file,ident2)
debugStr("after writeFile 4")
file = checkRead(ident2)
debugStr("after checkRead 4")
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
debugStr("loc "..file["loc"].." "..string.len(prefix).." ("..file["str"]..")")
assert(file["loc"] == string.len(prefix))
assert(file["slf"] == 1)
assert(file["str"] == field2)
debugStr("before checkRead 5")
file = checkRead(ident1)
debugStr("after checkRead 5")
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(prefix))
assert(file["slf"] == 0)
assert(file["str"] == field2)

ident3 = forkExec("file")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writeFile(file,ident3)
file = checkRead(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == data2)
file = checkRead(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == data3)

closeIdent(ident1)
closeIdent(ident2)
closeIdent(ident3)
waitAll()
