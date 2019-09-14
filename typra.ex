#!/usr/bin/env lua
--[[
*    typra.ex
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

assert(loadfile("type.inc"))()

Enum1 = {
	"Value11",
	"Value12",
	"Value13",
}
Enum2 = {
	"Value21",
	"Value22",
	"Value23",
}
Struct1 = {
	{"next","Struct1",{},1},
	{"field1","float",{},{2}},
	{"field2","double",{},{3}},
	{"field3","int",{},{2,2}},
	{"field4","char",{},0},
	{"field5","int",{},3},
	{"field6","Enum1",{},{}},
	{"field7","Enum2",{},{}},
	{"field8","int",{["field6"]={["Value11"]=true}}},
	{"field9","int",{["field6"]={["Value11"]=true}}},
	{"field10","int",{["field6"]={["Value12"]=true}}},
	{"field11","int",{["field6"]={["Value12"]=true},["field7"]={["Value21"]=true}}},
	{"field12","int",{["field6"]={["Value12"]=true},["field7"]={["Value22"]=true,["Value23"]=true}}},
	{"field13","int",{["field6"]={["Value13"]=true}}},
	{"field14","int",{},{}},
}
Enums = {
	["Enum1"]=true,["Enum2"]=true,
}
Structs = {
	["Struct1"]=true,
}
EnumOrder = {
	"Enum1","Enum2",
}
StructOrder = {
	"Struct1",
}
function enumOf(str)
	if str == "Enum1" then return Enum1 end
	if str == "Enum2" then return Enum2 end
	return {}
end
function structOf(str)
	if str == "Struct1" then return Struct1 end
	return {}
end
Stimulus = {
	{"findString(\"hello ok again and again\",\"hello\")"},
	{"findString(\"hello ok again() and again\",\"again\")"},
	{"findString(\"hello() ok again and again\",\"hello\")"},
	{"Enum1"},
	{"Enum2"},
	{"allOf(Enum1)"},
	{"allBefore(Enum1,\"Value12\")"},
	{"allExcept(Enum1,allBefore)"},
	{"unionSet(allBefore,allExcept)"},
	{"differSet(allOf,allBefore)"},
	{"equalSet(allExcept,differSet)"},
	{"equalSet(allOf,allExcept)"},
	{"allBefore(Enum1,\"Value13\")"},
	{"interSet(differSet,allOf)"},
	{"structTagSpace(Struct1)","structTagSpace(Struct1)"},
	{"interDimSet({[\"field6\"]=allBefore},structTagSpace,structTagSpace)","structTagSpace"},
	{"differDimSet(structTagSpace,interDimSet,structTagSpace)","structTagSpace"},
}
Expected = {
	"1,-5",
	"10,5;22,-5",
	"1,5",
	"Value11,Value12,Value13",
	"Value21,Value22,Value23",
	"Value11,Value12,Value13",
	"Value11,(Value12),(Value13)",
	"(Value11),Value12,Value13",
	"Value11,Value12,Value13",
	"(Value11),Value12,Value13",
	"true",
	"false",
	"Value11,Value12,(Value13)",
	"(Value11),Value12,Value13",
	"field6:Value11,Value12,Value13;field7:Value21,Value22,Value23",
	"field6:Value11,Value12,(Value13);field7:Value21,Value22,Value23",
	"field6:(Value11),(Value12),(Value13);field7:(Value21),(Value22),(Value23)",
}
Monitor = {
	"showFind",
	"showFind",
	"showFind",
	"showList",
	"showList",
	"showSet",
	"showSet",
	"showSet",
	"showSet",
	"showSet",
	"showBool",
	"showBool",
	"showSet",
	"showSet",
	"showDimSet",
	"showDimSet",
	"showDimSet",
}
file = io.open("type.txt","w")
io.output(file)
abbrev = {}
for k,v in ipairs(Stimulus) do
	chunk = "return "..Monitor[k].."("
	for j,u in ipairs(v) do
		expand = u
		for key,val in pairs(abbrev) do
			found = findString(expand,key)
			delta = 0
			for index,value in ipairs(found) do
				pos = value[1]
				len = value[2]
				if len < 0 then
					neg = -len
					sum = pos+delta
					if sum > 1 then before = string.sub(expand,1,sum-1) else before = "" end
					if sum+neg <= string.len(expand) then after = string.sub(expand,sum+neg,-1) else after = "" end
					expand = before..val..after
					delta = val:len()-neg
				end
			end
		end
		if j > 1 then chunk = chunk.."," end
		chunk = chunk..expand
	end
	chunk = chunk..")"
	-- print(chunk)
	func = assert(load(chunk))
	actual = func()
	io.write(v[1].." "..actual.."\n")
	found = {string.find(v[1],"^%a+%(")}
	if found and found[1] and found[2] then
		pos = found[1]
		lim = found[2]
		key = string.sub(v[1],pos,lim-1)
		abbrev[key] = expand
	end
end
io.close(file)
file = io.open("type.txt","r")
io.input(file)
for k,v in ipairs(Expected) do
	line = io.read(); if line ~= Stimulus[k][1].." "..v then print("error: "..line); os.exit() end
end
line = io.read(); if line ~= nil then print("error: "..line); os.exit() end
io.close(file)
print("typra.ex")
