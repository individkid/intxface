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
function listSort(map)
	local tab = {}
	for key,val in pairs(map) do
		table.insert(tab,key)
	end
	table.sort(tab)
	return tab
end
function showSet(set)
	if not set then return "universe" end
	local str = ""
	for key,val in ipairs(listSort(set)) do
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
	local str = ""
	for k,v in ipairs(listSort(all)) do
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
function showAny(any)
	local result = ""
	if type(any) == "nil" then result = "nil"
	elseif type(any) == "boolean" and not any then result = "false"
	elseif type(any) == "boolean" and any then result = "true"
	elseif type(any) == "number" then result = tostring(any)
	elseif type(any) == "string" then result = "\""..any.."\""
	elseif type(any) == "table" then
		result = "{"
		for k,key in ipairs(listSort(any)) do
			if result ~= "{" then result = result.."," end
			result = result.."["..showAny(key).."]="..showAny(any[key])
		end
		result = result.."}"
	else result = type(any) end
	return result
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
function showIndent(depth)
	local indent = ""; c = 0 while c < depth do c = c + 1; indent = indent.."    " end
	return indent
end
function showEnumC(name,enum)
	local str = "enum "..name.." {\n"
	for key,val in ipairs(enum) do
		str = str..showIndent(1)..val..",\n"
	end
	str = str.."};"
	return str
end
function showEnumHs(name,enum)
	local str = "data "..name.." = "..name.."\n"
	for key,val in ipairs(enum) do
		str = str..showIndent(1)..val
		if (key < #enum) then
			str = str.." |\n"
		else
			str = str.."\n"
		end
	end
	return str.."--"
end
function showStructCH(val)
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
	return decl
end
function showStructCG(val)
	local ident = val[1]
	if (type(val[4]) == "table") then
		for k,v in pairs(val[4]) do
			ident = ident.."["..v.."]"
		end
	end
	return ident
end
function showStructCF(val)
	local decl = showStructCH(val)
	local ident = showStructCG(val)
	return decl.." "..ident..";"
end
function showStructC(name,struct)
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
		result = result..showIndent(depth)..showStructCF(v).."\n"
		if chain[link] and k == chain[link][2] then
			depth = depth - 1; result = result..showIndent(depth).."};\n"; link = link + 1
		end
		if split[step] and k == split[step][2] then
			depth = depth - 1; result = result..showIndent(depth).."};\n"; step = step + 1
		end
	end
	return "struct "..name.." {\n"..result.."};"
end
function showStructHsF(val)
	local result = ""
	local pre = ""
	local post = ""
	if ((type(val[4]) == "table") and (#val[4] > 0)) or
		(type(val[4]) == "number") or (type(val[4]) == "string") then
		pre = "["
		post = "]"
	end
	if (val[2] == "int") then
		result = result..showIndent(1)..pre.."Int"..post
	elseif (val[2] == "double") then
		result = result..showIndent(1)..pre.."Double"..post
	elseif (val[2] == "float") then
		result = result..showIndent(1)..pre.."Float"..post
	elseif (val[2] == "char*") then
		result = result..showIndent(1)..pre.."String"..post
	else
		result = result..showIndent(1).."--[[ "..pre..val[2]..post.." --]]"
	end
	return result
end
function showStructHsG(struct,chain,split,func)
	local result = ""
	local link = 1
	local step = 1
	local chains = 0
	local splits = 0
	for k,v in ipairs(struct) do
		local links = 0; link,links,chains = showStructHsH(chain,k,link,links,chains)
		local steps = 0; step,steps,splits = showStructHsH(split,k,step,steps,splits)
		local joints = chains; if (links == -1) then joints = joints + 1 end
		local cracks = splits; if (steps == -1) then cracks = cracks + 1 end
		result = result..func(k,links,steps,joints,cracks)
	end
	return result
end
function showStructHsH(ary,idx,sub,dif,sum)
	if ary[sub] and (idx == ary[sub][1]) then
		sum = sum + 1
		dif = dif + 1
	elseif ary[sub] and (idx == ary[sub][2]) then
		sub = sub + 1
		sum = sum - 1
		dif = dif - 1
	end
	return sub,dif,sum
end
function showStructHs(name,struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local result = ""
	local structs = 1
	result = result..showStructHsG(struct,chain,split,function(index,links,steps,chains,splits)
		result = ""
		if (links == 1) then
			result = result.."data "..name.."A"..structs.." = "..name.."A"..structs.."\n"
			structs = structs + 1
		end
		if (chains == 1) then
			result = result..showStructHsF(struct[index]).." -- "..struct[index][1].."\n"
		end
		return result
	end)
	local unions = 1
	structs = 1
	result = result..showStructHsG(struct,chain,split,function(index,links,steps,chains,splits)
		result = ""
		if (steps == 1) then
			result = result.."data "..name.."B"..unions.." = "..name.."B"..unions.."\n"
			unions = unions + 1
		end
		if (splits == 1) then
			if (links == 1) then
				result = result..showIndent(1)..name.."A"..structs.."\n"
			end
			if (chains == 0) then
				result = result..showStructHsF(struct[index]).." -- "..struct[index][1].."\n"
			end
		end
		if (links == 1) then
			structs = structs + 1
		end
		return result
	end)
	result = result.."data "..name.." = "..name.."\n"
	unions = 1
	structs = 1
	result = result..showStructHsG(struct,chain,split,function(index,links,steps,chains,splits)
		result = ""
		if (links == 1) and (steps == 0) then
			result = result..showIndent(1)..name.."A"..structs.."\n"
		end
		if (links == 1) then
			structs = structs + 1
		end
		if (steps == 1) then
			result = result..showIndent(1)..name.."B"..unions.."\n"
			unions = unions + 1
		end
		if (chains == 0) and (splits == 0) then
			result = result..showStructHsF(struct[index]).." -- "..struct[index][1].."\n"
		end
		return result
	end)
	result = result.."--"
	return result
end
function showStreamCF(name)
	if (name == "int") then
		return "allocInt"
	elseif (name == "double") then
		return "allocDouble"
	elseif (name == "float") then
		return "allocFloat"
	elseif (name == "char*") then
		return "allocStr"
	end
	return "alloc"..name
end
function showStreamC(name,struct,show,pre,post)
	local result = ""
	result = result..pre..name.."(struct "..name.." *ptr"..post..")"
	if prototype then return result..";" end
	result = result.."\n{\n"
	local all = structTagSpace(struct)
	for k,v in ipairs(struct) do
		local depth = 1
		local plaid = v[3]
		local fields = listSort(plaid)
		for index,field in ipairs(fields) do
			if index > 1 then
				result = result..") and\n"..showIndent(depth+1).."("
			else
				result = result..showIndent(depth).."if (("
			end
			local vals = listSort(plaid[field])
			for key,val in ipairs(vals) do
				if key > 1 then
					result = result..") or ("
				else
					result = result.."("
				end
				result = result.."ptr->"..field.."=="..val
				if key == #vals then
					result = result..")"
				end
			end
			if index == #fields then
				result = result.."))\n"
				depth = depth + 1
			end
		end
		local sub = nil
		local lim = nil
		local ebr = ""
		if (type(v[4]) == "table") then
			sub = ""
			for key,val in ipairs(v[4]) do
				result = result..showIndent(depth)
				result = result.."for (int i"..key.." = 0; i"..key.." < "..val.."; i"..key.."++)\n"
				sub = sub.."[i"..key.."]"
				depth = depth + 1
			end
			result = result..showIndent(depth)
		elseif (type(v[4]) == "number") and (v[4] > 0) then
			sub = ""
			lim = v[4]
		elseif (type(v[4]) == "string") then
			sub = ""
			lim = "ptr->"..v[4]
		else
			result = result..showIndent(depth)
		end
		if lim then
			if (show == showReadCF) or (show == showRandCF) then
				result = result..showIndent(depth)
				result = result..showStreamCF(v[2]).."(&ptr->"..v[1]..","..lim..");\n"
			end
			result = result..showIndent(depth)
			result = result.."for (int i = 0; i < "..lim.."; i++)\n"
			sub = "[i]"
			depth = depth + 1
			result = result..showIndent(depth)
		end
		if (sub == nil) then
			result = result..ebr.."// "..showAny(v).."\n"
		else
			result = result..show(v,ebr,sub)
		end
	end
	if (show == showCompCF) then
		result = result..showIndent(1).."return true;\n"
	end
	result = result.."}"
	return result
end
function showReadCF(v,ebr,sub)
	result = ""
	if (v[2] == "int") then
		result = result.."ptr->"..v[1]..sub.." = readInt(idx);"..ebr.."\n"
	elseif (v[2] == "double") then
		result = result.."ptr->"..v[1]..sub.." = readNum(idx);"..ebr.."\n"
	elseif (v[2] == "float") then
		result = result.."{double temp = readNum(idx); ptr->"..v[1]..sub.." = temp;}"..ebr.."\n"
	elseif (v[2] == "char*") then
		result = result.."{char *temp = readStr(idx); "
		result = result.."allocStr(&ptr->"..v[1]..sub..",temp);}"..ebr.."\n"
	elseif (Enums[v[2]]~=nil) then
		result = result.."{int temp = readInt(idx); ptr->"..v[1]..sub.." = temp;}"..ebr.."\n"
	elseif (Structs[v[2]]~=nil) then
		result = result.."read"..v[2].."(&ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	else
		result = result..ebr.."; // "..showAny(v).."\n"
	end
	return result
end
function showWriteCF(v,ebr,sub)
	result = ""
	if (v[2] == "int") then
		result = result.."writeInt(ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (v[2] == "double") then
		result = result.."writeNum(ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (v[2] == "float") then
		result = result.."{double temp = ptr->"..v[1]..sub.."; writeNum(temp,idx);}"..ebr.."\n"
	elseif (v[2] == "char*") then
		result = result.."writeStr(ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (Enums[v[2]]~=nil) then
		result = result.."{int temp = ptr->"..v[1]..sub.."; writeInt(temp,idx);}"..ebr.."\n"
	elseif (Structs[v[2]]~=nil) then
		result = result.."write"..v[2].."(&ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	else
		result = result..ebr.."; // "..showAny(v).."\n"
	end
	return result
end
function showReadC(name,struct)
	return showStreamC(name,struct,showReadCF,"void read",", int idx")
end
function showWriteC(name,struct)
	return showStreamC(name,struct,showWriteCF,"void write",", int idx")
end
function showAllocC(name,typ)
	result = ""
	if (name == "Str") then
		result = result.."void allocStr(char **ptr, const char *str)"
		if prototype then return result..";" end
		result = result.."\n{\n"
		result = result..showIndent(1).."*ptr = realloc(*ptr,strlen(str)+1);\n"
		result = result..showIndent(1).."strcpy(*ptr,str);\n"
		result = result.."}"
	elseif (Enums[name] ~= nil) then
		result = result.."void alloc"..name.."(enum "..name.." **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof(enum "..name.."));\n"
		result = result.."}"
	elseif (Structs[name] ~= nil) then
		result = result.."void alloc"..name.."(struct "..name.." **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof(struct "..name.."));\n"
		result = result..showIndent(1).."for (int i = 0; i < siz; i++) {"
		for k,v in ipairs(typ) do
			if ((type(v[4]) == "number") and (v[4] > 0)) or (type(v[4]) == "string") then
				result = result.."\n"..showIndent(2).."(*ptr)[i]."..v[1].." = 0;"
			end
		end
		result = result.."}\n".."}"
	else
		result = result.."void alloc"..name.."("..typ.." **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof("..typ.."));\n"
		result = result.."}"
	end
	return result
end
randInt = 0
randNum = 0.1
function showRandCF(v,ebr,sub)
	local result = ""
	if (v[2] == "int") then
		result = result.."ptr->"..v[1]..sub.." = "..randInt..";"..ebr.."\n"
		randInt = randInt + 1;
	elseif (v[2] == "double") then
		result = result.."ptr->"..v[1]..sub.." = "..randNum..";"..ebr.."\n"
		randNum = randNum + 1.0
	elseif (v[2] == "float") then
		result = result.."ptr->"..v[1]..sub.." = "..randNum..";"..ebr.."\n"
		randNum = randNum + 1.0
	elseif (v[2] == "char*") then
		result = result.."{char *temp = \"hello ok again\"; "
		result = result.."allocStr(&ptr->"..v[1]..sub..",temp);}"..ebr.."\n"
	elseif (Enums[v[2]]~=nil) then
		result = result.."ptr->"..v[1]..sub.." = "..randInt..";"..ebr.."\n"
		randInt = randInt + 1;
	elseif (Structs[v[2]]~=nil) then
		result = result.."rand"..v[2].."(&ptr->"..v[1]..sub..");"..ebr.."\n"
	else
		result = result..ebr.."; // "..showAny(v).."\n"
	end
	return result
end
function showCompCF(v,ebr,sub)
	local result = ""
	if (v[2] == "int") or (v[2] == "double") or (v[2] == "float") or (Enums[v[2]]~=nil) then
		result = result.."if (ptr->"..v[1]..sub.." != cmp->"..v[1]..sub..") return false;"..ebr.."\n"
		randInt = randInt + 1;
	elseif (v[2] == "char*") then
		result = result.."if (strcmp(ptr->"..v[1]..sub..",cmp->"..v[1]..sub..") != 0) return false;"..ebr.."\n"
	elseif (Structs[v[2]]~=nil) then
		result = result.."if (!comp"..v[2].."(&ptr->"..v[1]..sub..", &cmp->"..v[1]..sub..")) return false;"..ebr.."\n"
	else
		result = result..ebr.."; // "..showAny(v).."\n"
	end
	return result
end
function showRandC(name,struct)
	return showStreamC(name,struct,showRandCF,"void rand","")
end
function showCompC(name,struct)
	return showStreamC(name,struct,showCompCF,"int comp",", struct "..name.." *cmp")
end
function listHere(name)
	local result = {}
	local found = false
	for line in io.lines(arg[0]) do
		local set = false
		local clear = false
		if (string.find(line,"^--HERE "..name)) then set = true
		elseif(string.find(line,"^--HERE")) then clear = true end
		if clear then found = false end
		if found then
			local match
			_, _, match = string.find(line,"([A-Za-z0-9]+) = {")
			if (match) then result[match] = _G[match] end
		end
		if set then found = true end
	end
	return result
end
function showCall(map,func)
	local result = ""
	for k,v in ipairs(listSort(map)) do
		if result ~= "" then result = result.."\n" end
		result = result..func(v,map[v])
	end
	return result
end
function showFuncC()
	local result = ""
	result = result..showAllocC("Int","int").."\n"
	result = result..showAllocC("Double","double").."\n"
	result = result..showAllocC("Float","float").."\n"
	result = result..showAllocC("Str","char*").."\n"
	result = result..showCall(Structs,showAllocC).."\n"
	result = result..showCall(Structs,showReadC).."\n"
	result = result..showCall(Structs,showWriteC).."\n"
	result = result..showCall(Structs,showRandC).."\n"
	result = result..showCall(Structs,showCompC)
	return result
end
function showTypeH()
	local result = ""
	result = result..showCall(Enums,showEnumC).."\n"
	result = result..showCall(Structs,showStructC).."\n"
	prototype = true
	result = result..showFuncC()
	return result
end
function showTypeC()
	local result = ""
	result = result.."#include <stdlib.h>\n"
	result = result.."#include <string.h>\n"
	result = result.."#include \"face.h\"\n"
	prototype = false
	result = result..showFuncC()
	return result
end
function showTypeHs()
	local result = ""
	return result
end
function showTypeLua()
	local result = ""
	return result
end
function showTyperC()
	local result = ""
	result = result.."#include \"face.h\"\n"
	result = result.."#include \"typer.h\"\n"
	result = result..showTypeC().."\n"
	result = result.."int main(int argc, char **argv)\n"..
	"{\n"..
	"	if (argc == 4) {\n"..
	"	pipeInit(argv[1],argv[2]);\n"..
	"	return 0;}\n"..
	"	forkExec(\"a.out\");\n"..
	"	forkExec(\"b.out\");\n"..
	"	forkExec(\"typer.lua\");\n"..
	"	sleepSec(1);\n"
	result = result..
	"	// alloc structs\n"..
	"	// init structs\n"..
	"	// write structs\n"..
	"	// read structs\n"..
	"	// compa structs\n"
	result = result..
	"	printf(\"typer.c\\n\");\n"..
	"	return 0;\n"..
	"}"
	return result
end
function showTyperHs()
	local result = ""
	result = result..showTypeHs()
	return result
end
function showTyperLua()
	local result = ""
	result = result..showTypeLua()
	return result
end
