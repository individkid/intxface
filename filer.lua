dofile("type.lua")

field1 = "Str(hello)"
field2a = "Str(ok)"
field2b = "Str(OK)"
field3 = "Str(again)"
name = "filer.--"

-- MAIN
ident1 = wrapType("Filez","fielerLua","fileC")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writePersist(file,ident1)
file = {}
file["act"] = "AppHub"
file["idx"] = 0
file["loc"] = 0
file["str"] = field1
writePersist(file,ident1)
file["str"] = field2a
writePersist(file,ident1)
file["str"] = field3
writePersist(file,ident1)
file = readPersist(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 1)
assert(file["str"] == field1)
file = readPersist(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 1)
assert(file["str"] == field2a)
file = readPersist(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1)+string.len(field2a))
assert(file["slf"] == 1)
assert(file["str"] == field3)

ident2 = wrapType("Filez","fielerLua","fileC")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writePersist(file,ident2)
file = readPersist(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == field1)
file = readPersist(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 0)
assert(file["str"] == field2a)
file = readPersist(ident2)
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
writePersist(file,ident2)
file = readPersist(ident2)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 1)
assert(file["str"] == field2b)
file = readPersist(ident1)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 0)
assert(file["str"] == field2b)

ident3 = wrapType("Filez","fielerLua","fileC")
file = {}
file["act"] = "NewHub"
file["idx"] = 0
file["str"] = name
writePersist(file,ident3)
file = readPersist(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["slf"] == 0)
assert(file["str"] == field1)
file = readPersist(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1))
assert(file["slf"] == 0)
assert(file["str"] == field2b)
file = readPersist(ident3)
assert(file["act"] == "HubCfg")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(field1)+string.len(field2a))
assert(file["slf"] == 0)
assert(file["str"] == field3)

closeIdent(ident1)
closeIdent(ident2)
closeIdent(ident3)
waitRead(0.0,-1)
