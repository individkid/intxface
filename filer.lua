dofile("type.lua")

prefix = "hello "
field1 = "ok"
field2 = "OK"
suffix = " again\n"
data1 = prefix..field1..suffix
data2 = prefix..field2..string.sub(data1,string.len(prefix)+string.len(field2)+1)
name = "filer.--"

-- MAIN
ident = forkExec("file")
file = {}
file["act"] = "NewThd"
file["idx"] = 0
file["num"] = 1
file["siz"] = {string.len(name)}
file["ptr"] = {name}
writeFile(file,ident)
file = {}
file["act"] = "CfgThd"
file["idx"] = 0
file["num"] = 1
file["siz"] = {string.len(data1)}
file["ptr"] = {data1}
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "ThdCmd")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["num"] == 1)
assert(#file["siz"] == 1)
assert(file["siz"][1] == string.len(data1))
assert(#file["ptr"] == 1)
assert(file["ptr"][1] == data1)
file = {}
file["act"] = "EndPrc"
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "PrcEnd")

ident = forkExec("file")
file = {}
file["act"] = "NewThd"
file["idx"] = 0
file["num"] = 1
file["siz"] = {string.len(name)}
file["ptr"] = {name}
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "ThdCfg")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["num"] == 1)
assert(#file["siz"] == 1)
assert(file["siz"][1] == string.len(data1))
assert(#file["ptr"] == 1)
assert(file["ptr"][1] == data1)
file = {}
file["act"] = "CmdThd"
file["idx"] = 0
file["loc"] = string.len(prefix)
file["num"] = 1
file["siz"] = {string.len(field2)}
file["ptr"] = {field2}
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "ThdCmd")
assert(file["idx"] == 0)
assert(file["loc"] == string.len(prefix))
assert(file["num"] == 1)
assert(#file["siz"] == 1)
assert(file["siz"][1] == string.len(field2))
assert(#file["ptr"] == 1)
assert(file["ptr"][1] == field2)
file = {}
file["act"] = "EndPrc"
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "PrcEnd")

actual = {}
expect = {data2}
for line in io.lines(name) do
	actual[#actual+1] = line.."\n"
end
assert(#actual == #expect)
for k,v in ipairs(actual) do
	assert(expect[k] == v)
end
