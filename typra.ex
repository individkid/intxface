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

loadfile("type.inc")()

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
file = io.open("type.txt","w")
io.output(file)
io.write("Enum1 "..showList(Enum1).."\n")
io.write("Enum2 "..showList(Enum2).."\n")
io.write("allOf(Enum1) "..showSet(allOf(Enum1)).."\n")
io.write("allBefore(Enum1,\"Value12\") "..showSet(allBefore(Enum1,"Value12")).."\n")
io.write("allExcept(Enum1,allBefore) "..showSet(allExcept(Enum1,allBefore(Enum1,"Value12"))).."\n")
io.write("unionSet(allBefore,allExcept) "..showSet(unionSet(allBefore(Enum1,"Value12"),allExcept(Enum1,allBefore(Enum1,"Value12")))).."\n")
io.write("differSet(allOf,allBefore) "..showSet(differSet(allOf(Enum1),allBefore(Enum1,"Value12"))).."\n")
io.write("equalSet(allExcept,differSet) "..showBool(equalSet(allExcept(Enum1,allBefore(Enum1,"Value12")),differSet(allOf(Enum1),allBefore(Enum1,"Value12")))).."\n")
io.write("equalSet(allOf,AllExcept) "..showBool(equalSet(allOf(Enum1),allExcept(Enum1,allBefore(Enum1,"Value12")))).."\n")
io.write("allBefore(Enum1,\"Value13\") "..showSet(allBefore(Enum1,"Value13")).."\n")
io.write("interSet(differSet,allBefore) "..showSet(interSet(differSet(allOf(Enum1),allBefore(Enum1,"Value12")),allBefore(Enum1,"Value13"))).."\n")
io.close(file)
file = io.open("type.txt","r")
io.input(file)
line = io.read(); if line ~= "Enum1 Value11,Value12,Value13" then print("error:"..line); io.exit() end
line = io.read(); if line ~= "Enum2 Value21,Value22,Value23" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "allOf(Enum1) Value11,Value12,Value13" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "allBefore(Enum1,\"Value12\") Value11,(Value12),(Value13)" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "allExcept(Enum1,allBefore) (Value11),Value12,Value13" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "unionSet(allBefore,allExcept) Value11,Value12,Value13" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "differSet(allOf,allBefore) (Value11),Value12,Value13" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "equalSet(allExcept,differSet) true" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "equalSet(allOf,AllExcept) false" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "allBefore(Enum1,\"Value13\") Value11,Value12,(Value13)" then print("error:"..line); os.exit() end
line = io.read(); if line ~= "interSet(differSet,allBefore) (Value11),Value12,(Value13)" then print("error:"..line); os.exit() end
io.close(file)
print("typera.ex")
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
