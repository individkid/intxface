#!/usr/bin/env lua
--[[
*    type.ex
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

function allOf(enum)
	all = {}
	for key,val in pairs(enum) do all[val]=true end
	return all
end
function allExcept(enum,set)
	all = {}
	for key,val in pairs(enum) do all[val] = not set[key] end
	return all
end
function allBefore(enum,lim)
	all = {}
	num = 0
	for key,val in pairs(enum) do if val == lim then num = key end end
	for key,val in pairs(enum) do all[val] = key < num end
	return all
end
function equalSet(lhs,rhs)
	result = true
	for k,v in pairs(lhs) do result = result and rhs[k] ~= v end
	return result
end
function interSet(lhs,rhs)
	result = {}
	for k,v in pairs(lhs) do result[k] = v and rhs[k] end
	return result
end
function differSet(lhs,rhs)
	result = {}
	for k,v in pairs(lhs) do result[k] = v and not rhs[k] end
	return result
end
function unionSet(lhs,rhs)
	result = {}
	for k,v in pairs(lhs) do result[k] = v or rhs[k] end
	for k,v in pairs(rhs) do result[k] = v or lhs[k] end
	return result
end
-- empty is nil
-- universe is {}
-- all cannot be nil
-- all is the universe in a larger universe
-- all of {} is the largest universe
-- if all is {} then all sets are {}
function equalDimSet(lhs,rhs,all)
	result = true
	for k,v in pairs(lhs) do
		result = result and rhs[k]
		result = result and equalSet(v,all[k]) == equalSet(rhs[k],all[k])
		result = result and equalSet(v,rhs[k])
	end
	for k,v in pairs(rhs) do
		result = result and lhs[k]
	end
	return result
end
function unionDimSet(lhs,rhs,all)
	result = nil
	for k,v in pairs(lhs) do
		if rhs[k] then result[k] = unionSet(v,rhs[k]) else result[k] = all[k] end
		if equalSet(result[k],all[k]) then result[k] = nil end
	end
	if result and equalDimSet(result,all) then return {} end
	return result
end
function interDimSet(lhs,rhs,all)
	result = nil
	for k,v in pairs(lhs) do
		if rhs[k] then result[k] = interSet(v,rhs[k]) else result[k] = v end
		if equalSet(result[k],{}) then return nil end
	end
	for k,v in pairs(rhs) do
		if lhs[k] then result[k] = interSet(v,lhs[k]) else result[k] = v end
		if equalSet(result[k],{}) then return nil end
	end
	if result and equalDimSet(result,all) then return {} end
	return result
end
function differDimSet(lhs,rhs,all)
	result = nil
	for k,v in pairs(lhs) do
		if rhs[k] then result[k] = differSet(v,rhs[k]) else return nil end
		if equalSet(result[k],{}) then return nil end
	end
	if result and equalDimSet(result,all) then return {} end
	return result
end
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
function printEnum(name,enum)
	print("enum "..name.." = {")
	for key,val in ipairs(enum) do
		print("    "..val..",")
	end
	print("};")
end
function stringAny(any)
	result = ""
	if type(any) == "nil" then result = "nil"
	elseif type(any) == "boolean" and not any then result = "false"
	elseif type(any) == "boolean" and any then result = "true"
	elseif type(any) == "number" then result = tostring(any)
	elseif type(any) == "string" then result = "\""..any.."\""
	elseif type(any) == "table" then
		result = "{"
		for key,val in pairs(any) do
			if result ~= "{" then result = result.."," end
			result = result.."["..stringAny(key).."]="..stringAny(val)
		end
		result = result.."}"
	else result = type(any) end
	return result
end
function stringIndent(depth)
	indent = ""; c = 0 while c < depth do c = c + 1; indent = indent.."    " end
	return indent
end
function printField(val,depth)
	if (Enums[val[2]]~=nil) then
		decl = "enum "..val[2]
	elseif (Structs[val[2]]~=nil) then
		decl = "struct "..val[2]
	else
		decl = val[2]
	end
	if (type(val[4]) == "number") then
		decl = decl.."*"
	elseif (type(val[4]) == "string") then
		decl = decl.."*"
	end
	ident = val[1]
	if (type(val[4]) == "table") then
		for k,v in pairs(val[4]) do
			ident = ident.."["..v.."]"
		end
	end
	print(stringIndent(depth)..decl.." "..ident..";")
end
function printStruct(name,struct)
	all = {}
	key = 1
	while struct[key] do
		tags = struct[key][3]
		for k,v in pairs(tags) do
			if all[k] then all[k] = unionSet(all[k],v) else all[k] = v end
		end
		key = key + 1
	end
	length = 1
	mode = {"struct"} -- list of block types
	flds = {0} -- list of pointers to fields
	idxs = {{}} -- list of pointers to subblocks
	sizs = {0} -- list of sizes of idxs
	depth = 1
	univ = {all} -- stack of nested tag sets
	stack = {0} -- stack of pointers to open blocks
	key = 1
	while struct[key] do
		tags = struct[key][3]
		top = stack[depth]
		join = interDimSet(tags,univ[depth],all) ~= nil
		gain = differDimSet(tags,univ[depth],all) ~= nil
		loss = differDimSet(univ[depth],tags,all) ~= nil
		print("name:"..stringAny(name).." key:"..stringAny(key).." field:"..stringAny(struct[key][1]).." tags:"..stringAny(tags).." univ:"..stringAny(univ[depth]).." depth:"..stringAny(depth).." top:"..stringAny(top).." join:"..stringAny(join).." gain:"..stringAny(gain).." loss:"..stringAny(loss))
		if mode[top] == "struct" and not gain and not loss then
			length = length + 1
			mode[length] = "field"
			flds[length] = key
			idxs[length] = {}
			sizs[length] = 0
			sizs[top] = sizs[top] + 1;
			idxs[top][sizs[top]] = length
			key = key + 1
		end
		if mode[top] == "struct" and not gain and loss then
			length = length + 1
			mode[length] = "union"
			flds[length] = 0
			idxs[length] = {}
			sizs[length] = 0
			depth = depth + 1
			univ[depth] = none
			stack[depth] = length
		end
		if mode[top] == "struct" and gain then
			depth = depth - 1
		end
		if mode[top] == "union" and not join then
			univ[depth] = unionDimSet(univ[depth],tags,all);
			length = length + 1
			mode[length] = "struct"
			flds[length] = 0
			idxs[length] = {}
			sizs[length] = 0
			depth = depth + 1
			univ[depth] = tags
			stack[depth] = length
		end
		if mode[top] == "union" and join then
			depth = depth - 1
		end
	end
	print("struct "..name.." {")
		indent = 1
		depth = 1
		stack = {sizs[1]}
		close = {false}
		index = 2
		while index <= length do
			if mode[index] == "struct" or mode[index] == "union" then
				depth = depth + 1
				stack[depth] = sizs[index]
				close[depth] = sizs[index] > 1
				if close[depth] then
					print(stringIndent(indent).."struct {")
					indent = indent + 1
				end
			end
			if mode[index] == "field" then
				print(stringIndent(indent)..stringAny(flds[index]))
				stack[depth] = stack[depth] - 1
				if stack[depth] == 0 and close[depth] then
					depth = depth - 1
					indent = indent - 1
					print(stringIndent(indent).."};")
				end
			end
			index = index + 1
		end
	print("};")
end
for key,val in ipairs(EnumOrder) do
	printEnum(val,enumOf(val))
end
for key,val in ipairs(StructOrder) do
	printStruct(val,structOf(val))
end
