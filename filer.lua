--[[
*    filer.lua
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
