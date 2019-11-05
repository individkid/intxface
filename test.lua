--[[
*    test.lua
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

dofile("show.lua")

--HERE Enums
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
--HERE Structs
Struct2 = {
	{"field1","int",{},{}},
	{"field2","int",{},{}},
}
Struct1 = {
	{"next","Struct1",{},0},
	{"field1","float",{},{2}},
	{"field2","double",{},{3}},
	{"field3","int",{},{2,2}},
	{"field4","char*",{},{}},
	{"field5","int",{},3},
	{"field6","Enum1",{},{}},
	{"field7","Enum2",{},{}},
	{"field8","int",{["field6"]={["Value11"]=true}},{}},
	{"field9","int",{["field6"]={["Value11"]=true}},{}},
	{"field10","int",{["field6"]={["Value12"]=true}},{}},
	{"field11","int",{["field6"]={["Value12"]=true},["field7"]={["Value21"]=true}},{}},
	{"field12","int",{["field6"]={["Value12"]=true},["field7"]={["Value22"]=true,["Value23"]=true}},{}},
	{"field13","int",{["field6"]={["Value13"]=true}},{}},
	{"field14","int",{},{}},
	{"field15","int",{},"field14"},
	{"field16","Struct2",{},2},
	{"field17","Struct2",{},{2}},
}
--HERE
Enums,Enumz = listHere("Enums")
Structs,Structz = listHere("Structs")
