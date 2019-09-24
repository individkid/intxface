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
function equalSetF(lhs,rhs)
	return (lhs and rhs) or (not lhs and not rhs)
end
function equalSet(lhs,rhs)
	local result = true
	for k,v in pairs(lhs) do result = result and equalSetF(rhs[k],v) end
	for k,v in pairs(rhs) do result = result and equalSetF(lhs[k],v) end
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
-- for Plaid empty is nil
-- for Plaid universe is {}
-- all cannot be nil
-- all is the universe in a larger universe
-- all of {} is the largest universe
-- if all is {} then all sets are {}
function showPlaid(ds,all)
	if not ds then return "empty" end
	local tab = {}
	for key,val in pairs(all) do
		table.insert(tab,key)
	end
	table.sort(tab)
	local str = ""
	for k,v in ipairs(tab) do
		if str ~= "" then str = str..";" end
		if not ds[v] then str = str..v..":"..showSet(all[v])
		else str = str..v..":"..showSet(interSet(all[v],ds[v])) end
	end
	return str
end
function equalPlaid(lhs,rhs,all)
	if lhs == nil and rhs == nil then return true end
	if rhs == nil or lhs == nil then return false end
	local result = true
	for k,v in pairs(all) do
		local lhv = lhs[k]; if not lhv then lhv = all[k] end
		local rhv = rhs[k]; if not rhv then rhv = all[k] end
		result = result and equalSet(lhv,rhv)
	end
	return result
end
function interPlaid(lhs,rhs,all)
	if lhs == nil or rhs == nil then return nil end
	local result = {}
	for k,v in pairs(all) do
		local lhv = lhs[k]; if not lhv then lhv = all[k] end
		local rhv = rhs[k]; if not rhv then rhv = all[k] end
		result[k] = interSet(lhv,rhv)
		if equalSet(result[k],{}) then return nil end
	end
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
function showEnum(name,enum)
	local str = "enum "..name.." {\n"
	for key,val in ipairs(enum) do
		str = str.."    "..val..",\n"
	end
	str = str.."};"
	return str
end
function showAny(any)
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
			result = result.."["..showAny(key).."]="..showAny(val)
		end
		result = result.."}"
	else result = type(any) end
	return result
end
function showIndent(depth)
	local indent = ""; c = 0 while c < depth do c = c + 1; indent = indent.."    " end
	return indent
end
function showField(val)
	local decl
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
	return decl.." "..ident..";"
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
function nestStruct(nest)
	-- return list of pairs of open on unequal and close on next unequal
	local result = {}
	local start = nil
	for k,v in ipairs(nest) do
		if v and not start then start = k end
		if not v and start then result[#result+1] = {start,k}; start = nil end
	end
	if start then result[#result+1] = {start,#nest} end
	return result
end
--  1 2 3 4 5 6 7 8 9 a b
--  a a b b b c d e f g g
--  1 0 1 1 0 0 0 0 0 1 0  (0 unequal, 1 equal)
-- + 0 * 0 0 - 0 0 0 + 0 - (+ open, * close/open, - close struct)
--  (1,2),(3,5),(a,b)      (open struct, close before opens)
function chainStruct(struct)
	-- return list of bool of whether successive fields are equal with first and last false
	local all = structTagSpace(struct)
	local result = {}
	for k,v in ipairs(struct) do
		result[#result+1] = (struct[k+1] ~= nil) and equalPlaid(v[3],struct[k+1][3],all)
	end
	return result
end
--  1 2 3 4 5 6 7 8 9 a b
--  a a b b b c d e f g g  (b c d mutually disjoint)
--  0 0 1 1 1 1 0 0 0 0 0  (0 conjoint, 1 disjoint back to conjoint)
-- 0 0 + 0 0 0 0 - 0 0 0 0 (+ open, - close union)
-- (3,7) (open union before open struct, close before opens)
function splitStructF(field,sofar,all)
	for k,v in ipairs(sofar) do
		if not equalPlaid(interPlaid(v,field,all),nil,all) then
			return false
		end
	end
	return true
end
function splitStruct(struct)
	-- return list of bool of whether successive structs are disjoint with first and last false
	local all = structTagSpace(struct)
	local result = {}
	local sofar = {}
	local skip = 0
	local joint = 0
	for k,v in ipairs(struct) do
		if (#sofar == 0) then
			skip = skip + 1
			sofar = {v[3]}
		elseif equalPlaid(v[3],sofar[#sofar],all) then
			skip = skip + 1
			sofar[#sofar+1] = v[3]
		elseif splitStructF(v[3],sofar,all) then
			skip = skip + 1
			joint = joint + 1
			sofar[#sofar+1] = v[3]
		elseif (joint > 0) then
			while (skip > 0) do result[#result+1] = true; skip = skip - 1 end
			result[#result] = false
			skip = skip + 1
			sofar = {v[3]}
			joint = 0
		else
			while (skip > 0) do result[#result+1] = false; skip = skip - 1 end
			skip = skip + 1
			sofar = {v[3]}
		end
 	end
 	if (joint > 0) then
		while (skip > 0) do result[#result+1] = true; skip = skip - 1 end
 	else
		while (skip > 0) do result[#result+1] = false; skip = skip - 1 end
 	end
	return result
end
function showStruct(name,struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local result = ""
	local link = 1
	local step = 1
	local depth = 1
	for k,v in ipairs(struct) do
		if split[step] and k == split[step][1] then
			result = result..showIndent(depth).."union {\n"; depth = depth + 1
		end
		if chain[link] and k == chain[link][1] then
			result = result..showIndent(depth).."struct {\n"; depth = depth + 1
		end
		result = result..showIndent(depth)..showField(v).."\n"
		if chain[link] and k == chain[link][2] then
			depth = depth - 1; result = result..showIndent(depth).."};\n"; link = link + 1
		end
		if split[step] and k == split[step][2] then
			depth = depth - 1; result = result..showIndent(depth).."};\n"; step = step + 1
		end
	end
	return "struct "..name.." {\n"..result.."};"
end
