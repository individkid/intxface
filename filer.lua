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

-- MAIN
ident = forkExec("file")
file = {}
file["act"] = "NewThd"
file["idx"] = 0
file["num"] = 1
name = "filer.txt"
file["siz"] = {string.len(name)}
file["ptr"] = {name}
writeFile(file,ident)
file["act"] = "CmdThd"
file["idx"] = 0
file["loc"] = 0
file["num"] = 1
data = "hello ok again\n"
file["siz"] = {string.len(data)}
file["ptr"] = {data}
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "ThdCmd")
assert(file["idx"] == 0)
assert(file["loc"] == 0)
assert(file["num"] == 1)
assert(#file["siz"] == 1)
assert(file["siz"][1] == string.len(data))
assert(#file["ptr"] == 1)
assert(file["ptr"][1] == data)
file["act"] = "EndPrc"
writeFile(file,ident)
file = readFile(ident)
assert(file["act"] == "PrcEnd")
