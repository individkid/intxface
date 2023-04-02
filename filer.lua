dofile("type.lua")

field1 = "Str(hello)"
field2a = "Str(ok)"
field2b = "Str(OK)"
field3 = "Str(again)"
name = "filer.--"

-- MAIN
ident1 = openFork()
if (openCheck(ident1)) then
	arg = showArgument({["pro"]="Filez",["inp"]=openRdfd(ident1),["out"]=openWrfd(ident1)})
	openExec("fileC",arg); io.stderr:write("filerLua: cannot execute file: fileC\n"); os.exit(-1)
end
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writeFile(file,ident1)
file = {}
file["act"] = "AppHub"
file["idx"] = 0
file["loc"] = 0
file["str"] = field1
writeFile(file,ident1)
file["str"] = field2a
writeFile(file,ident1)
file["str"] = field3
writeFile(file,ident1)
file = readFile(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 1)
assert(file["str"] == field1)
file = readFile(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 1)
assert(file["str"] == field2a)
file = readFile(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1)+string.len(field2a))
assert(file["slf"] == 1)
assert(file["str"] == field3)

ident2 = forkExec("fileC")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writeFile(file,ident2)
file = readFile(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == field1)
file = readFile(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 0)
assert(file["str"] == field2a)
file = readFile(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1)+string.len(field2a))
assert(file["slf"] == 0)
assert(file["str"] == field3)
file = {}
file["act"] = "CfgHub"
file["idx"] = 0
file["loc"] = string.len(field1)
file["str"] = field2b
writeFile(file,ident2)
file = readFile(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 1)
assert(file["str"] == field2b)
file = readFile(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 0)
assert(file["str"] == field2b)

ident3 = forkExec("fileC")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writeFile(file,ident3)
file = readFile(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == field1)
file = readFile(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 0)
assert(file["str"] == field2b)
file = readFile(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1)+string.len(field2a))
assert(file["slf"] == 0)
assert(file["str"] == field3)

closeIdent(ident1)
closeIdent(ident2)
closeIdent(ident3)
waitRead(0.0,-1)
