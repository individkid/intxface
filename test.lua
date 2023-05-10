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
--HERE Constants
Constant1 = {
	{"Enum1","Value11","Str","hello"},
	{"Enum1","Value12","Str","ok"},
	{"Enum1","Value13","Str","again"},
	{"Enum1","Value11","Enum2","Value21","Str","hello"},
	{"Enum1","Value11","Enum2","Value22","Str","ok"},
	{"Enum1","Value11","Enum2","Value23","Str","again"},
}
--HERE Structs
Struct2 = {
	{"field1","Int",{},{}},
	{"field2","Int",{},{}},
	{"field3","Int",{["field1"]={[1]=true}},{}},
}
Struct1 = {
	{"next","Struct1",{},0},
	{"field1","Old",{},{2}},
	{"field2","Num",{},{3}},
	{"field3","Int",{},{2,3}},
	{"field4","Str",{},{}},
	{"field5","Int",{},3},
	{"field6","Enum1",{},{}},
	{"field7","Enum2",{},{}},
	{"field8","Int",{["field6"]={["Value11"]=true}},{}},
	{"field9","Int",{["field6"]={["Value11"]=true}},{}},
	{"field10","Int",{["field6"]={["Value12"]=true}},{}},
	{"field11","Int",{["field6"]={["Value12"]=true},["field7"]={["Value21"]=true}},{}},
	{"field12","Int",{["field6"]={["Value12"]=true},["field7"]={["Value22"]=true,["Value23"]=true}},{}},
	{"field13","Int",{["field6"]={["Value13"]=true}},{}},
	{"field14","Int",{},{}},
	{"field15","Int",{},"field14"},
	{"field16","Struct2",{},2},
	{"field17","Struct2",{},{2}},
}
--HERE
Enums,Enumz = listHere("Enums","test.lua")
Constants,Constantz = listHere("Constants","test.lua")
Structs,Structz = listHere("Structs","test.lua")
enum = genericEnum(Structz,Enumz,"Tag");
struct = genericStruct(Structz,Enumz,"Tag");
Enums[#Enums+1] = "Tag"; Enumz["Tag"] = enum
Structs[#Structs+1] = "Generic"; Structz["Generic"] = struct
