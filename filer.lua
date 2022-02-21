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

-- MAIN
ident1 = forkExec("file")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = {name}
writeFile(file,ident1)
file = {}
file["act"] = "CfgHub"
file["idx"] = 0
file["loc"] = 0
file["str"] = data1
writeFile(file,ident1)
file = readFile(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 1)
assert(file["str"] == data1)

ident2 = forkExec("file")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = {name}
writeFile(file,ident2)
file = readFile(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == data1)
file = {}
file["act"] = "CfgHub"
file["idx"] = 0
file["loc"] = string.len(prefix)
file["ptr"] = field2
writeFile(file,ident2)
file = readFile(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(prefix))
assert(file["slf"] == 1)
assert(file["str"] == field2)
file = readFile(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(prefix))
assert(file["slf"] == 0)
assert(file["str"] == field2)

ident3 = forkExec("file")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = {name}
writeFile(file,ident3)
file = readFile(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == data2)
file = readFile(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == data3)

closeIdent(ident1)
closeIdent(ident2)
closeIdent(ident3)
waitAll()
