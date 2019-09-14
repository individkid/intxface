--[[
*    type.inc
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

function showBool(bool)
	local str = ""
	if bool then str = "true" else str = "false" end
	return str
end
function showList(list)
	if not list then print("list nil") end
	local str = ""
	for key,val in ipairs(list) do
		if str ~= "" then str = str.."," end
		str = str..val
	end
	return str
end
function showSet(set)
	if not set then return "universe" end
	local tab = {}
	for key,val in pairs(set) do
		table.insert(tab,key)
	end
	table.sort(tab)
	local str = ""
	for key,val in ipairs(tab) do
		if str ~= "" then str = str.."," end
		if set[val] then str = str..val else str = str.."("..val..")" end
	end
	return str
end
function allOf(enum)
	local all = {}
	for key,val in pairs(enum) do all[val]=true end
	return all
end
function allExcept(enum,set)
	local all = {}
	for key,val in pairs(enum) do all[val] = not set[val] end
	return all
end
function allBefore(enum,lim)
	local all = {}
	local num = 0
	for key,val in pairs(enum) do if val == lim then num = key end end
	for key,val in pairs(enum) do all[val] = key < num end
	return all
end
function equalSet(lhs,rhs)
	local result = true
	for k,v in pairs(lhs) do result = result and rhs[k] == v end
	for k,v in pairs(rhs) do result = result and lhs[k] == v end
	return result
end
function interSet(lhs,rhs)
	local result = {}
	for k,v in pairs(lhs) do result[k] = v and rhs[k] end
	for k,v in pairs(rhs) do result[k] = v and lhs[k] end
	return result
end
function differSet(lhs,rhs)
	local result = {}
	for k,v in pairs(lhs) do result[k] = v and not rhs[k] end
	return result
end
function unionSet(lhs,rhs)
	local result = {}
	for k,v in pairs(lhs) do result[k] = v or rhs[k] end
	for k,v in pairs(rhs) do result[k] = v or lhs[k] end
	return result
end
-- for Set empty is {}
-- for Set universe is nil
-- for DimSet empty is nil
-- for DimSet universe is {}
-- all cannot be nil
-- all is the universe in a larger universe
-- all of {} is the largest universe
-- if all is {} then all sets are {}
function showDimSet(ds,all)
	if not ds then return "empty" end
	local tab = {}
	for key,val in pairs(all) do
		table.insert(tab,key)
	end
	table.sort(tab)
	local str = ""
	for k,v in ipairs(tab) do
		if str ~= "" then str = str..";" end
		if not ds then str = str..v..":"..showSet(all[v])
		elseif ds[v] then str = str..v..":"..showSet(interSet(all[v],ds[v]))
		else str = str..v..":"..showSet(interSet(all[v],{})) end
	end
	return str
end
function equalDimSet(lhs,rhs,all)
	if lhs == nil and rhs == nil then return true end
	if lhs == nil or rhs == nil then return false end
	local result = true
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
	local result = nil
	for k,v in pairs(lhs) do
		if result == nil then result = {} end
		if rhs[k] then result[k] = unionSet(v,rhs[k]) else result[k] = all[k] end
		if equalSet(result[k],all[k]) then result[k] = nil end
	end
	if result and equalDimSet(result,all,all) then return {} end
	return result
end
function interDimSet(lhs,rhs,all)
	local result = nil
	for k,v in pairs(lhs) do
		if result == nil then result = {} end
		if rhs[k] then result[k] = interSet(v,rhs[k]) else result[k] = v end
		if result[k] and equalSet(result[k],{}) then return nil end
	end
	for k,v in pairs(rhs) do
		if not result then result = {} end
		if lhs[k] then result[k] = interSet(v,lhs[k]) else result[k] = v end
		if result[k] and equalSet(result[k],{}) then return nil end
	end
	if result and equalDimSet(result,all,all) then return {} end
	return result
end
function differDimSet(lhs,rhs,all)
	local result = nil
	for k,v in pairs(lhs) do
		if not result then result = {} end
		if rhs[k] then result[k] = differSet(v,rhs[k]) else return nil end
		if result and result[k] and equalSet(result[k],{}) then return nil end
	end
	if result and equalDimSet(result,all,all) then return {} end
	return result
end
function showFind(found)
	local str = ""
	for k,v in ipairs(found) do
		if str ~= "" then str = str..";" end
		str = str..v[1]..","..v[2]
	end
	return str
end
function findString(str,pat)
	local result = {}
	for i = 1,str:len()-pat:len()+1 do
		local found = true
		for j = 1,pat:len() do
			if str:sub(i+j-1,i+j-1) ~= pat:sub(j,j) then found = false; break end
		end
		if found and i+pat:len() <= str:len() and str:sub(i+pat:len(),i+pat:len()) == "(" then
			table.insert(result,{i,pat:len()})
		end
		if found and i+pat:len() <= str:len() and str:sub(i+pat:len(),i+pat:len()) ~= "(" then
			table.insert(result,{i,-pat:len()})
		end
		if found and i+pat:len() > str:len() then
			table.insert(result,{i,-pat:len()})
		end
	end
	return result
end
function printEnum(name,enum)
	print("enum "..name.." = {")
	for key,val in ipairs(enum) do
		print("    "..val..",")
	end
	print("};")
end
function stringAny(any)
	local result = ""
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
	local indent = ""; c = 0 while c < depth do c = c + 1; indent = indent.."    " end
	return indent
end
function printField(val,depth)
	local decl;
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
	local ident = val[1]
	if (type(val[4]) == "table") then
		for k,v in pairs(val[4]) do
			ident = ident.."["..v.."]"
		end
	end
	print(stringIndent(depth)..decl.." "..ident..";")
end
function structTagSpace(struct)
	local all = {}
	local key = 1
	while struct[key] do
		local tags = struct[key][3]
		for k,v in pairs(tags) do
			if all[k] then all[k] = unionSet(all[k],v) else all[k] = v end
		end
		key = key + 1
	end
	return all
end
function printStruct(name,struct)
	local all = structTagSpace(struct)
	local length = 1
	local mode = {"struct"} -- list of block types
	local flds = {0} -- list of pointers to fields
	local idxs = {{}} -- list of pointers to subblocks
	local sizs = {0} -- list of sizes of idxs
	local depth = 1
	local univ = {all} -- stack of nested tag sets
	local stack = {0} -- stack of pointers to open blocks
	local key = 1
	while struct[key] do
		local tags = struct[key][3]
		local top = stack[depth]
		local join = interDimSet(tags,univ[depth],all) ~= nil
		local gain = differDimSet(tags,univ[depth],all) ~= nil
		local loss = differDimSet(univ[depth],tags,all) ~= nil
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
		local indent = 1
		local depth = 1
		local stack = {sizs[1]}
		local close = {false}
		local index = 2
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
