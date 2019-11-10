--[[
*    show.lua
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
	if str == "" then return "all" end
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
function stringStructF(ary,idx,sub,dif,sum)
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
function stringStruct(struct,chain,split,func)
	local result = ""
	local link = 1
	local step = 1
	local chains = 0
	local splits = 0
	for k,v in ipairs(struct) do
		local links = 0; link,links,chains = stringStructF(chain,k,link,links,chains)
		local steps = 0; step,steps,splits = stringStructF(split,k,step,steps,splits)
		local joints = chains; if (links == -1) then joints = joints + 1 end
		local cracks = splits; if (steps == -1) then cracks = cracks + 1 end
		result = result..func(k,links,steps,joints,cracks)
	end
	return result
end
function treeStruct(limit,chain,split,index,link,step)
	if (index >= limit) then return {} end
	local begi = limit; if chain[link] then begi = chain[link][1] end
	local endi = limit; if chain[link] then endi = chain[link][2] end
	local mini = limit; if split[step] then mini = split[step][1] end
	local maxi = limit; if split[step] then maxi = split[step][2] end
	if (index < begi) and (index < mini) then
		local head = {index,index,"field",{}}
		local tail = treeStruct(limit,chain,split,index+1,link,step)
		table.insert(tail,1,head)
		return tail
	end
	if (index == mini) then
		local head = {mini,maxi,"union",treeStruct(maxi+1,chain,split,mini,link,step+1)}
		while (chain[link][1] < split[step][2]) do link = link + 1 end
		local tail = treeStruct(limit,chain,split,maxi+1,link,step+1)
		table.insert(tail,1,head)
		return tail
	end
	if (index == begi) then
		local head = {begi,endi,"struct",treeStruct(endi+1,chain,split,begi,link+1,step)}
		local tail = treeStruct(limit,chain,split,endi+1,link+1,step)
		table.insert(tail,1,head)
		return tail
	end
	return {}
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
	str = str..showIndent(1)..name.."s\n"
	str = str.."};"
	return str
end
function showStructCH(val)
	local decl
	if (Enumz[val[2]]~=nil) then
		decl = "enum "..val[2]
	elseif (Structz[val[2]]~=nil) then
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
	local all = structTagSpace(struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local result = ""
	local link = 1
	local step = 1
	local depth = 1
	local single = 1
	for k,v in ipairs(struct) do
		if split[step] and k == split[step][1] then
			result = result..showIndent(depth).."union {\n"
			depth = depth + 1
		end
		if chain[link] and k == chain[link][1] then
			result = result..showIndent(depth).."struct { // "
			result = result..showPlaid(v[3],all).."\n"
			single = 0; depth = depth + 1
		end
		if single == 1 then
			result = result..showIndent(depth)..showStructCF(v).." // "
			result = result..showPlaid(v[3],all).."\n"
		else
			result = result..showIndent(depth)..showStructCF(v).."\n"
		end
		if chain[link] and k == chain[link][2] then
			single = 1; depth = depth - 1; link = link + 1
			result = result..showIndent(depth).."};\n"
		end
		if split[step] and k == split[step][2] then
			depth = depth - 1; step = step + 1
			result = result..showIndent(depth).."};\n"
		end
	end
	return "struct "..name.." {\n"..result.."};"
end
function showCondC(struct,dset)
	local cond = ""
	local terms = 0
	for i,dim in ipairs(listSort(dset)) do
		local set = dset[dim]
		local term = ""
		local factors = 0
		for idx,fld in ipairs(struct) do
			if (fld[1] == dim) then
				for j,val in ipairs(listSort(set)) do
					local factor = "ptr->"..struct[idx][1].." == "..val
					if (factors > 0) then term = term.." || " end
					term = term.."("..factor..")"
					factors = factors + 1
				end
			end
		end
		if (terms > 0) then cond = cond.." && " end
		if (factors > 1) then
			cond = cond.."("..term..")"
		else
			cond = cond..term
		end
		terms = terms + 1
	end
	if (terms == 0) then
		cond = "1"
	elseif (terms > 1) then
		cond = "("..cond..")"
	end
	return cond
end
function showStreamCF(name,field,lim)
	local result = "alloc"
	local cast = ""
	if (name == "int") then
		result = result.."Int"
	elseif (name == "long long") then
		result = result.."New"
	elseif (name == "double") then
		result = result.."Num"
	elseif (name == "float") then
		result = result.."Old"
	elseif (name == "char*") then
		result = result.."Ptr"
		cast = "(void**)"
	else
		result = result..name
	end
	result = result.."("..cast.."&ptr->"..field..","..lim..");\n"
	return result
end
function showStreamC(name,struct,show,pre,post)
	local result = ""
	result = result..pre..name.."(struct "..name.." *ptr"..post..")"
	if prototype then return result..";" end
	result = result.."\n{\n"
	if (show == showSizeCF) then
		result = result..showIndent(1).."int result = 0;\n"
	end
	local all = structTagSpace(struct)
	for k,v in ipairs(struct) do
		local depth = 1
		local plaid = v[3]
		local fields = listSort(plaid)
		local cond = showCondC(struct,plaid)
		local sub = nil
		local lim = nil
		local ebr = ""
		if (cond ~= "1") then
			result = result..showIndent(depth)
			result = result.."if "..cond.."\n"
			depth = depth + 1
		end
		if (type(v[4]) == "table") then
			sub = ""
			for key,val in ipairs(v[4]) do
				result = result..showIndent(depth)
				result = result.."for (int i"..key.." = 0; i"..key.." < "..val.."; i"..key.."++)\n"
				sub = sub.."[i"..key.."]"
				depth = depth + 1
			end
			result = result..showIndent(depth)
		elseif (type(v[4]) == "number") then
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
				result = result..showStreamCF(v[2],v[1],lim)
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
		result = result..showIndent(1).."return 1;\n"
	end
	if (show == showSizeCF) then
		result = result..showIndent(1).."return result;\n"
	end
	result = result.."}"
	return result
end
function showReadCF(v,ebr,sub)
	result = ""
	if (v[2] == "int") then
		result = result.."ptr->"..v[1]..sub.." = readInt(idx);"..ebr.."\n"
	elseif (v[2] == "long long") then
		result = result.."ptr->"..v[1]..sub.." = readNew(idx);"..ebr.."\n"
	elseif (v[2] == "double") then
		result = result.."ptr->"..v[1]..sub.." = readNum(idx);"..ebr.."\n"
	elseif (v[2] == "float") then
		result = result.."ptr->"..v[1]..sub.." = readOld(idx);"..ebr.."\n"
	elseif (v[2] == "char*") then
		result = result.."readStr(callStr,&ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (Enumz[v[2]]~=nil) then
		result = result.."{int temp = readInt(idx); ptr->"..v[1]..sub.." = temp;}"..ebr.."\n"
	elseif (Structz[v[2]]~=nil) then
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
	elseif (v[2] == "long long") then
		result = result.."writeNew(ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (v[2] == "double") then
		result = result.."writeNum(ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (v[2] == "float") then
		result = result.."writeOld(ptr->"..v[1]..sub..",idx);"..ebr.."\n"
	elseif (v[2] == "char*") then
		result = result.."writeStr(ptr->"..v[1]..sub..",1,idx);"..ebr.."\n"
	elseif (Enumz[v[2]]~=nil) then
		result = result.."{int temp = ptr->"..v[1]..sub.."; writeInt(temp,idx);}"..ebr.."\n"
	elseif (Structz[v[2]]~=nil) then
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
	local qualify = showIndent(1).."if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}\n"
	qualify = qualify..showIndent(1).."if (siz == 0) return;\n"
	if (name == "Str") then
		result = result.."void allocStr(char **ptr, const char *str)"
		if prototype then
		result = result..";\n"
		else
		result = result.."\n{\n"
		result = result..showIndent(1).."if (*ptr && str == 0) {free(*ptr); *ptr = 0;}\n"
		result = result..showIndent(1).."if (str == 0) return;\n"
		result = result..showIndent(1).."*ptr = realloc(*ptr,strlen(str)+1);\n"
		result = result..showIndent(1).."strcpy(*ptr,str);\n"
		result = result.."}\n"
		end
		result = result.."void callStr(char* str, int trm, void*arg)"
		if prototype then return result..";" end
		result = result.."\n{\n"
		result = result..showIndent(1).."char **ptr = arg;\n"
		result = result..showIndent(1).."allocStr(ptr,str);\n"
		result = result.."}"
	elseif (name == "Ptr") then
		result = result.."void allocPtr(void **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"..qualify
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof(void*));\n"
		result = result.."}"
	elseif (Enumz[name] ~= nil) then
		result = result.."void alloc"..name.."(enum "..name.." **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"..qualify
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof(enum "..name.."));\n"
		result = result.."}"
	elseif (Structz[name] ~= nil) then
		result = result.."void alloc"..name.."(struct "..name.." **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"..qualify
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof(struct "..name.."));\n"
		result = result..showIndent(1).."for (int i = 0; i < siz; i++) {"
		for k,v in ipairs(typ) do
			if (type(v[4]) == "number") or (type(v[4]) == "string") or (v[2] == "char*") then
				result = result.."\n"..showIndent(2).."(*ptr)[i]."..v[1].." = 0;"
			end
		end
		result = result.."}\n".."}"
	else
		result = result.."void alloc"..name.."("..typ.." **ptr, int siz)"
		if prototype then return result..";" end
		result = result.."\n{\n"..qualify
		result = result..showIndent(1).."*ptr = realloc(*ptr,siz*sizeof("..typ.."));\n"
		result = result.."}"
	end
	return result
end
randInt = 0
randNew = 10
randNum = 0.1
randOld = 0.2
function showRandCF(v,ebr,sub)
	local result = ""
	if (v[2] == "int") then
		result = result.."ptr->"..v[1]..sub.." = "..randInt..";"..ebr.."\n"
		randInt = randInt + 1;
	elseif (v[2] == "long long") then
		result = result.."ptr->"..v[1]..sub.." = "..randNew..";"..ebr.."\n"
		randInt = randInt + 1;
	elseif (v[2] == "double") then
		result = result.."ptr->"..v[1]..sub.." = "..randNum..";"..ebr.."\n"
		randNum = randNum + 1.0
	elseif (v[2] == "float") then
		result = result.."ptr->"..v[1]..sub.." = "..randOld..";"..ebr.."\n"
		randNum = randNum + 1.0
	elseif (v[2] == "char*") then
		result = result.."{const char *temp = \"hello ok again\"; "
		result = result.."allocStr(&ptr->"..v[1]..sub..",temp);}"..ebr.."\n"
	elseif (Enumz[v[2]]~=nil) then
		result = result.."ptr->"..v[1]..sub.." = "..randInt.."%"..v[2].."s;"..ebr.."\n"
		randInt = randInt + 1;
	elseif (Structz[v[2]]~=nil) then
		result = result.."rand"..v[2].."(&ptr->"..v[1]..sub..");"..ebr.."\n"
	else
		result = result..ebr.."; // "..showAny(v).."\n"
	end
	return result
end
function showCompCF(v,ebr,sub)
	local result = ""
	if (v[2] == "int") or (v[2] == "long long") or (v[2] == "double") or (v[2] == "float") or (Enumz[v[2]]~=nil) then
		result = result.."if (ptr->"..v[1]..sub.." != cmp->"..v[1]..sub..") return 0;"..ebr.."\n"
		randInt = randInt + 1;
	elseif (v[2] == "char*") then
		result = result.."if (strcmp(ptr->"..v[1]..sub..",cmp->"..v[1]..sub..") != 0) return 0;"..ebr.."\n"
	elseif (Structz[v[2]]~=nil) then
		result = result.."if (!comp"..v[2].."(&ptr->"..v[1]..sub..", &cmp->"..v[1]..sub..")) return 0;"..ebr.."\n"
	else
		result = result..ebr.."; // "..showAny(v).."\n"
	end
	return result
end
function showSizeCF(v,ebr,sub)
	local result = ""
	if (v[2] == "int") or (v[2] == "long long") or (v[2] == "double") or (v[2] == "float") then
		result = result.."result = result + sizeof("..v[2]..");"..ebr.."\n"
	elseif (Enumz[v[2]]~=nil) then
		result = result.."result = result + sizeof(int);"..ebr.."\n"
	elseif (v[2] == "char*") then
		result = result.."result = result + strlen(ptr->"..v[1]..sub..") + 1;"..ebr.."\n"
	elseif (Structz[v[2]]~=nil) then
		result = result.."result = result + size"..v[2].."(&ptr->"..v[1]..sub..");"..ebr.."\n"
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
function showSizeC(name,struct)
	return showStreamC(name,struct,showSizeCF,"int size","")
end
function showEnumHs(name,enum)
	local result = "data "..name.." =\n"
	for key,val in ipairs(enum) do
		result = result..showIndent(1)..val.." |\n"
	end
	result = result..showIndent(1)..name.."s deriving (Eq)\n"
	return result.."--"
end
function showCodeHs(name,enum)
	local result = ""
	result = result.."read"..name.."F :: Int -> IO "..name.."\n"
	for key,val in ipairs(enum) do
		result = result.."read"..name.."F "..(key-1).." = return "..val.."\n"
	end
	result = result.."read"..name.."F _ = return "..name.."s\n"
	result = result.."read"..name.." :: Int -> IO "..name.."\n"
	result = result.."read"..name.." idx = (readInt idx) >>= read"..name.."F\n"
	result = result.."write"..name.."F :: "..name.." -> Int\n"
	for key,val in ipairs(enum) do
		result = result.."write"..name.."F "..val.." = "..(key-1).."\n"
	end
	result = result.."write"..name.."F _ = "..#enum.."\n"
	result = result.."write"..name.." :: "..name.." -> Int -> IO ()\n"
	result = result.."write"..name.." a idx = writeInt (write"..name.."F a) idx\n"
	result = result.."--"
	return result
end
function showStructHsF(val)
	local result = ""
	local pre = ""
	local post = ""
	if ((type(val[4]) == "table") and (#val[4] > 0)) then
		local count = 1
		while (count <= #val[4]) do
			pre = pre.."["
			post = post.."]"
			count = count + 1
		end
	end
	if (type(val[4]) == "number") or (type(val[4]) == "string") then
		pre = "["
		post = "]"
	end
	if (val[2] == "int") then
		result = result..pre.."Int"..post
	elseif (val[2] == "long long") then
		result = result..pre.."Integer"..post
	elseif (val[2] == "double") then
		result = result..pre.."Double"..post
	elseif (val[2] == "float") then
		result = result..pre.."Float"..post
	elseif (val[2] == "char*") then
		result = result..pre.."String"..post
	else
		result = result..pre..val[2]..post
	end
	return result
end
function showStructHs(name,struct)
	local all = structTagSpace(struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local result = ""
	local structs = 1
	result = result..stringStruct(struct,chain,split,function(index,links,steps,chains,splits)
		local result = ""
		if (links == 1 and splits == 0) then
			result = result.."data "..name.."A"..chain[structs][1].."X"..chain[structs][2]
			result = result.." = "..name.."A"..chain[structs][1].."X"..chain[structs][2]
			result = result.." -- "..showPlaid(struct[index][3],all).."\n"
		end
		if (chains == 1 and splits == 0) then
			result = result..showIndent(1)..showStructHsF(struct[index]).." -- "..struct[index][1].."\n"
		end
		if (links == -1 and splits == 0) then
			result = result..showIndent(1).."deriving (Eq)\n"
		end
		if (links == -1) then
			structs = structs + 1
		end
		return result
	end)
	local unions = 1
	structs = 1
	result = result..stringStruct(struct,chain,split,function(index,links,steps,chains,splits)
		local result = ""
		if (steps == 1) then
			result = result.."data "..name.."A"..split[unions][1].."X"..split[unions][2].." =\n"
		end
		if (splits == 1) then
			if (links == 1) then
				result = result..showIndent(1)..name.."A"..split[unions][1].."X"..split[unions][2]
				result = result.."B"..chain[structs][1].."X"..chain[structs][2]
				result = result.." -- "..showPlaid(struct[index][3],all).."\n"
			end
			if (chains == 1) and (links ~= -1) then
				result = result..showIndent(2)..showStructHsF(struct[index]).." -- "..struct[index][1].."\n"
			end
			if (chains == 1) and (links == -1) then
				result = result..showIndent(2)..showStructHsF(struct[index]).." | -- "..struct[index][1].."\n"
			end
			if (chains == 0) then
				result = result..showIndent(1)..name.."A"..split[unions][1].."X"..split[unions][2]
				result = result.."B"..index
				result = result.." "..showStructHsF(struct[index]).." | -- "..struct[index][1]
				result = result.." -- "..showPlaid(struct[index][3],all).."\n"
			end
		end
		if (steps == -1) then
			result = result..showIndent(1)..name.."A"..split[unions][1].."X"..split[unions][2]
			result = result.."Bs deriving (Eq)\n"
			unions = unions + 1
		end
		if (links == -1) then
			structs = structs + 1
		end
		return result
	end)
	result = result.."data "..name.." = "..name.."\n"
	unions = 1
	structs = 1
	result = result..stringStruct(struct,chain,split,function(index,links,steps,chains,splits)
		local result = ""
		if (links == 1) and (steps == 0) then
			result = result..showIndent(1)..name.."A"..chain[structs][1].."X"..chain[structs][2].."\n"
		end
		if (links == 1) then
			structs = structs + 1
		end
		if (steps == 1) then
			result = result..showIndent(1)..name.."A"..split[unions][1].."X"..split[unions][2].."\n"
			unions = unions + 1
		end
		if (chains == 0) and (splits == 0) then
			result = result..showStructHsF(struct[index]).." -- "..struct[index][1].."\n"
		end
		return result
	end)
	result = result..showIndent(1).."deriving (Eq)\n"
	result = result.."--"
	return result
end
function showAccessHsF(index,field,name,tree)
	local result = ""
	local branch = 1
	while (tree[branch]) do
		local leaf = " a"..tree[branch][1]
		local node = " a"..tree[branch][1].."x"..tree[branch][2]
		if (tree[branch][3] == "field") and (tree[branch][1] == index) then
			result = result.." "..field
		end
		if (tree[branch][3] == "field") and (tree[branch][1] ~= index) then
			result = result..leaf
		end
		if (tree[branch][3] == "struct") and (tree[branch][1] <= index) and (tree[branch][2] >= index) then
			result = result.." ("..name.."A"..tree[branch][1].."X"..tree[branch][2]
			result = result..showAccessHsF(index,field,name,tree[branch][4])..")"
		end
		if (tree[branch][3] == "struct") and not ((tree[branch][1] <= index) and (tree[branch][2] >= index)) then
			result = result..node
		end
		if (tree[branch][3] == "union") and (tree[branch][1] <= index) and (tree[branch][2] >= index) then
			local extend = "A"..tree[branch][1].."X"..tree[branch][2]
			result = result..showAccessHsG(index,field,name,tree[branch][4],extend)
		end
		if (tree[branch][3] == "union") and not ((tree[branch][1] <= index) and (tree[branch][2] >= index)) then
			result = result..node
		end
		branch = branch + 1
	end
	return result
end
function showAccessHsG(index,field,name,tree,extend)
	local result = ""
	local branch = 1
	while (tree[branch]) do
		local clip = "B"..tree[branch][1]
		local trim = "B"..tree[branch][1].."X"..tree[branch][2]
		if (tree[branch][3] == "field") and (tree[branch][1] == index) then
			result = result.." ("..name..extend..clip.." "..field..")"
		end
		if (tree[branch][3] == "struct") and (tree[branch][1] <= index) and (tree[branch][2] >= index) then
			result = result.." ("..name..extend
			result = result..trim..showAccessHsF(index,field,name,tree[branch][4],nil)..")"
		end
		branch = branch + 1
	end
	return result
end
function showAccessHs(name,struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local tree = treeStruct(#struct+1,chain,split,1,1,1)
	local result = ""
	for k,v in ipairs(struct) do
		result = result.."get"..name.."C"..v[1].." :: "
		result = result..name.." -> "..showStructHsF(v).."\n"
		result = result.."get"..name.."C"..v[1].." ("..name
		result = result..showAccessHsF(k,"a",name,tree)
		result = result..") = a\n"
	end
	result = result.."--\n"
	for k,v in ipairs(struct) do
		result = result.."set"..name.."C"..v[1].." :: "
		result = result..name.." -> "..showStructHsF(v).." -> "..name.."\n"
		result = result.."set"..name.."C"..v[1].." ("..name
		result = result..showAccessHsF(k,"_",name,tree)
		result = result..") a = ("..name
		result = result..showAccessHsF(k,"a",name,tree)
		result = result..")\n"
	end
	result = result.."--"
	return result
end
function showHelpHs()
	result = ""
	result = result.."listHelp :: Int -> IO a -> IO [a]\n"
	result = result.."listHelp 0 b = return []\n"
	result = result.."listHelp a b = do\n"
	result = result..showIndent(1).."d <- b\n"
	result = result..showIndent(1).."e <- listHelp (a-1) b\n"
	result = result..showIndent(1).."return (d:e)\n"
	result = result.."assertHelp :: Int -> (a -> IO ()) -> [a] -> IO ()\n"
	result = result.."assertHelp a _ []\n"
	result = result..showIndent(1).."| a == 0 = return ()\n"
	result = result..showIndent(1).."| otherwise = undefined\n"
	result = result.."assertHelp a f (b:c)\n"
	result = result..showIndent(1).."| a > 0 = (f b) >> (assertHelp (a-1) f c)\n"
	result = result..showIndent(1).."| otherwise = undefined\n"
	result = result.."condHelp :: Bool -> a -> IO a -> IO a\n"
	result = result.."condHelp True _ a = a\n"
	result = result.."condHelp False a _ = return a\n"
	result = result.."firstHelp :: Eq a => a -> [a] -> IO a\n"
	result = result.."firstHelp a [] = return a\n"
	result = result.."firstHelp a (b:c)\n"
	result = result..showIndent(1).."| a == b = firstHelp a c\n"
	result = result..showIndent(1).."| otherwise = return b\n"
	result = result.."--"
	return result
end
function showCondHs(struct,dset)
	local cond = ""
	local terms = 0
	for i,dim in ipairs(listSort(dset)) do
		local set = dset[dim]
		local term = ""
		local factors = 0
		for idx,fld in ipairs(struct) do
			if (fld[1] == dim) then
				for j,val in ipairs(listSort(set)) do
					local factor = "a"..idx.." == "..val
					if (factors > 0) then term = term.." || " end
					term = term.."("..factor..")"
					factors = factors + 1
				end
			end
		end
		if (terms > 0) then cond = cond.." && " end
		if (factors > 1) then
			cond = cond.."("..term..")"
		else
			cond = cond..term
		end
		terms = terms + 1
	end
	if (terms == 0) then
		cond = "true"
	elseif (terms > 1) then
		cond = "("..cond..")"
	end
	return cond
end
function showReadHsF(name,struct,node,depth)
	local result = ""
	if (node[3] == "field") then
		result = result..showReadHsG(struct,node[1],depth)
	end
	if (node[3] == "struct") then
		result = result..showReadHsH(name,struct,node,depth)
	end
	if (node[3] == "union") then
		result = result..showReadHsI(name,struct,node,depth)
	end
	return result
end
function showReadHsG(struct,index,depth)
	local result = ""
	local field = struct[index]
	result = result..showIndent(depth).."a"..index.." <- "
	local count = 0
	if (type(field[4]) == "table") then
		while (count < #field[4]) do
			count = count + 1
			result = result.."listHelp "..field[4][count].." ("
		end
	end
	if (type(field[4]) == "string") then
		local found = "0"
		for k,v in ipairs(struct) do
			if (v[1] == field[4]) then found = "a"..k end
		end
		result = result.."listHelp "..found.." ("
		count = count + 1
	end
	if (type(field[4]) == "number") then
		result = result.."listHelp "..field[4].." ("
		count = count + 1
	end
	if (field[2] == "int") then result = result.."readInt"
	elseif (field[2] == "long long") then result = result.."readNew"
	elseif (field[2] == "double") then result = result.."readNum"
	elseif (field[2] == "float") then result = result.."readOld"
	elseif (field[2] == "char*") then
		result = result.."fmap fst (readStr"
		count = count + 1
	else result = result.."read"..field[2] end
	result = result.." idx"
	while (count > 0) do
		result = result..")"
		count = count - 1
	end
	result = result.."\n"
	return result
end
function showReadHsH(name,struct,node,depth)
	local result = ""
	local args = ""
	local tree = node[4]
	local branch = 1
	while (tree[branch]) do
		local temp = tree[branch]
		local arg = "a"..temp[1]
		if (temp[3] ~= "field") then arg = arg.."x"..temp[2] end
		args = args.." "..arg
		result = result..showReadHsF(name,struct,temp,depth)
		branch = branch + 1
	end
	if (node[3] == "top") then
		result = result..showIndent(depth).."return ("..name..args..")\n"
	else
		result = result..showIndent(depth).."a"..node[1].."x"..node[2]
		result = result.." <- return ("..name.."A"..node[1].."X"..node[2]..args..")\n"
	end
	return result
end
function showReadHsI(name,struct,node,depth)
	local result = ""
	local none = name.."A"..node[1].."X"..node[2].."Bs"
	local args = ""
	local tree = node[4]
	local branch = 1
	while (tree[branch]) do
		local temp = tree[branch]
		local func = name.."A"..node[1].."X"..node[2].."B"..temp[1]
		if (temp[3] ~= "field") then func = func.."X"..temp[2] end
		local arg = "a"..temp[1]
		if (temp[3] ~= "field") then arg = arg.."x"..temp[2] end
		if (args ~= "") then args = args.."," end
		args = args..arg
		result = result..showIndent(depth)..arg.." <- condHelp "
		result = result..showCondHs(struct,struct[temp[1]][3])
		result = result.." "..none.." (do\n"
		result = result..showReadHsJ(func,struct,temp,depth+1)
		branch = branch + 1
	end
	result = result..showIndent(depth).."a"..node[1].."x"..node[2]
	result = result.." <- firstHelp "..none.." ["..args.."]\n"
	return result
end
function showReadHsJ(func,struct,node,depth)
	local result = ""
	local tree = node[4]
	if (node[3] == "field") then tree = {node} end
	local args = ""
	local branch = 1
	while (tree[branch]) do
		local temp = tree[branch]
		local arg = "a"..temp[1]
		if (args ~= "") then args = args.." " end
		args = args..arg
		result = result..showReadHsG(struct,temp[1],depth)
		branch = branch + 1
	end
	result = result..showIndent(depth).."return ("..func.." "..args.."))\n"
	return result
end
function showReadHs(name,struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local tree = treeStruct(#struct+1,chain,split,1,1,1)
	local result = ""
	result = result.."read"..name.." :: Int -> IO "..name.."\n"
	result = result.."read"..name.." idx = do\n"
	result = result..showReadHsH(name,struct,{1,#struct,"top",tree},1)
	result = result.."--"
	return result
end
function showWriteHsF(name,struct,tree)
	local result = ""
	local branch = 1
	while (tree[branch]) do
		local node = tree[branch]
		if (node[3] == "field") then
			result = result.." a"..node[1]
		end
		if (node[3] == "struct") then
			result = result.." ("..name.."A"..node[1].."X"..node[2]
			result = result..showWriteHsF(name,struct,node[4])
			result = result..")"
		end
		if (node[3] == "union") then
			result = result.." a"..node[1].."x"..node[2]
		end
		branch = branch + 1
	end
	return result
end
function showWriteHsG(name,struct,tree,depth)
	local result = ""
	local branch = 1
	while (tree[branch]) do
		local node = tree[branch]
		if (node[3] == "field") then
			result = result..showIndent(depth)
			result = result..showWriteHsH(struct,node[1])
		end
		if (node[3] == "struct") then
			result = result..showWriteHsG(name,struct,node[4],depth)
		end
		if (node[3] == "union") then
			result = result..showWriteHsI(name,struct,node,depth)
		end
		branch = branch + 1
		if (tree[branch]) then
			result = result.."\n"
		end
	end
	return result
end
function showWriteHsH(struct,index)
	local result = ""
	local field = struct[index]
	local count = 0
	if (type(field[4]) == "table") and (#field[4] ~= 0) then
		while (count < #field[4]) do
			count = count + 1
			result = result.."assertHelp "..field[4][count].." ("
		end
	end
	if (type(field[4]) == "string") then
		local found = "0"
		for k,v in ipairs(struct) do
			if (v[1] == field[4]) then found = "a"..k end
		end
		result = result.."assertHelp "..found.." ("
		count = count + 1
	end
	if (type(field[4]) == "number") then
		result = result.."assertHelp "..field[4].." ("
		count = count + 1
	end
	local anon = false
	if (count > 0) then
		anon = true
		result = result.."\\x -> "
	end
	if (field[2] == "int") then result = result.."writeInt"
	elseif (field[2] == "long long") then result = result.."writeNew"
	elseif (field[2] == "double") then result = result.."writeNum"
	elseif (field[2] == "float") then result = result.."writeOld"
	elseif (field[2] == "char*") then result = result.."writeStr"
	else result = result.."write"..field[2] end
	if anon and (field[2] == "float") then
		result = result.." x"
	elseif anon and (field[2] ~= "float") then
		result = result.." x"
	elseif not anon and (field[2] == "float") then
		result = result.." a"..index
	elseif not anon and (field[2] ~= "float") then
		result = result.." a"..index
	end
	if (field[2] == "char*") then result = result.." True" end
	result = result.." idx"
	while (count > 0) do
		result = result..")"
		count = count - 1
	end
	if anon then
		result = result.." a"..index
	end
	return result
end
function showWriteHsI(name,struct,given,depth)
	local result = ""
	local tree = given[4]
	local branch = 1
	while (tree[branch]) do
		local node = tree[branch]
		result = result..showIndent(depth).."condHelp "
		result = result..showCondHs(struct,struct[node[1]][3])
		result = result.." () ((\\("..name.."A"..given[1].."X"..given[2]
		result = result.."B"..node[1]
		if (node[3] == "field") then
			result = result..showWriteHsF(name,struct,{node})
			result = result..") -> "
			result = result..showWriteHsH(struct,node[1])
		else
			result = result.."X"..node[2]
			result = result..showWriteHsF(name,struct,node[4])
			result = result..") -> do\n"
			result = result..showWriteHsG(name,struct,node[4],depth+1)
		end
		result = result..") a"..given[1].."x"..given[2]..")"
		branch = branch + 1
		if (tree[branch]) then
			result = result.."\n"
		end
	end
	return result
end
function showWriteHs(name,struct)
	local chain = nestStruct(chainStruct(struct))
	local split = nestStruct(splitStruct(struct))
	local tree = treeStruct(#struct+1,chain,split,1,1,1)
	local result = ""
	result = result.."write"..name.." :: "..name.." -> Int -> IO ()\n"
	result = result.."write"..name.." ("..name
	result = result..showWriteHsF(name,struct,tree)
	result = result..") idx = do\n"
	result = result..showWriteHsG(name,struct,tree,1).."\n"
	result = result.."--"
	return result
end
function showCodeLua(name,enum)
	local result = ""
	result = result.."function read"..name.."(idx)\n"
	result = result..showIndent(1).."val = readInt(idx)\n"
	for key,val in ipairs(enum) do
		if (key == 1) then
			result = result..showIndent(1).."if (val == 0) then return \""..val.."\"\n"
		else
			result = result..showIndent(1).."elseif (val == "..(key-1)..") then return \""..val.."\"\n"
		end
	end
	result = result..showIndent(1).."else return nil end\n"
	result = result.."end\n"
	result = result.."function write"..name.."(val,idx)\n"
	for key,val in ipairs(enum) do
		if (key == 1) then
			result = result..showIndent(1).."if (val == \""..val.."\") then writeInt(0,idx)\n"
		else
			result = result..showIndent(1).."elseif (val == \""..val.."\") then writeInt("..(key-1)..",idx)\n"
		end
	end
	result = result..showIndent(1).."else writeInt("..(#enum+1)..",idx) end\n"
	result = result.."end\n"
	result = result.."--"
	return result
end
function showCondLua(struct,dset)
	local cond = ""
	local terms = 0
	for i,dim in ipairs(listSort(dset)) do
		local set = dset[dim]
		local term = ""
		local factors = 0
		for idx,fld in ipairs(struct) do
			if (fld[1] == dim) then
				for j,val in ipairs(listSort(set)) do
					local factor = "tab[\""..struct[idx][1].."\"] == \""..val.."\""
					if (factors > 0) then term = term.." or " end
					term = term.."("..factor..")"
					factors = factors + 1
				end
			end
		end
		if (terms > 0) then cond = cond.." and " end
		if (factors > 1) then
			cond = cond.."("..term..")"
		else
			cond = cond..term
		end
		terms = terms + 1
	end
	if (terms == 0) then
		cond = "true"
	elseif (terms > 1) then
		cond = "("..cond..")"
	end
	return cond
end
function showReadLua(name,struct)
	local result = ""
	result = result.."function read"..name.."(idx)\n"
	result = result..showIndent(1).."local tab = {}\n"
	for index,field in ipairs(struct) do
		local count = 0
		local conds = 0
		local cond = showCondLua(struct,field[3])
		if (cond ~= "true") then
			count = count + 1
			conds = conds + 1
			result = result..showIndent(count).."if "..cond.." then\n"
		end
		local sub = ""
		if (type(field[4]) == "table") then
			local squares = 1
			local square = 1
			local limit = count + #field[4]
			while (count < limit) do
				count = count + 1
				result = result..showIndent(count).."tab[\""..field[1].."\"]"
				local index = square
				while (index < squares) do
					result = result.."[i"..index.."]"
					index = index + 1
				end
				squares = squares + 1
				result = result.." = {}\n"
				result = result..showIndent(count).."local i"..count.." = 1\n"
				result = result..showIndent(count).."while (i"..count.." <= "..field[4][count]..") do\n"
				sub = sub.."[i"..count.."]"
			end
		end
		if (type(field[4]) == "string") then
			local found = "0"
			for k,v in ipairs(struct) do
				if (v[1] == field[4]) then found = "a"..k end
			end
			count = count + 1
			result = result..showIndent(count).."tab[\""..field[1].."\"] = {}\n"
			result = result..showIndent(count).."local i"..count.." = 1\n"
			result = result..showIndent(count).."while (i"..count.." <= tab[\""..field[4].."\"]) do\n"
			sub = sub.."[i"..count.."]"
		end
		if (type(field[4]) == "number") then
			count = count + 1
			result = result..showIndent(count).."tab[\""..field[1].."\"] = {}\n"
			result = result..showIndent(count).."local i"..count.." = 1\n"
			result = result..showIndent(count).."while (i"..count.." <= "..field[4]..") do\n"
			sub = sub.."[i"..count.."]"
		end
		result = result..showIndent(count+1).."tab[\""..field[1].."\"]"..sub.." = "
		if (field[2] == "int") then result = result.."readInt(idx)"
		elseif (field[2] == "long long") then result = result.."readNew(idx)"
		elseif (field[2] == "double") then result = result.."readNum(idx)"
		elseif (field[2] == "float") then result = result.."readOld(idx)"
		elseif (field[2] == "char*") then result = result.."readStr(idx)"
		else result = result.."read"..field[2].."(idx)" end
		result = result.."\n"
		while (count > 0) do
			if (conds == 0) then
				result = result..showIndent(count+1).."i"..count.." = i"..count.." + 1\n"
			else
				conds = conds - 1
			end
			result = result..showIndent(count).."end\n"
			count = count - 1
		end
	end
	result = result..showIndent(1).."return tab\n"
	result = result.."end\n"
	result = result.."--"
	return result
end
function showWriteLua(name,struct)
	local result = ""
	result = result.."function write"..name.."(tab,idx)\n"
	for index,field in ipairs(struct) do
		local count = 0
		local conds = 0
		local cond = showCondLua(struct,field[3])
		if (cond ~= "true") then
			count = count + 1
			conds = conds + 1
			result = result..showIndent(count).."if "..cond.." then\n"
		end
		local sub = ""
		if (type(field[4]) == "table") then
			while (count < #field[4]) do
				count = count + 1
				result = result..showIndent(count).."local i"..count.." = 1\n"
				result = result..showIndent(count).."while (i"..count.." <= "..field[4][count]..") do\n"
				sub = sub.."[i"..count.."]"
			end
		end
		if (type(field[4]) == "string") then
			local found = "0"
			for k,v in ipairs(struct) do
				if (v[1] == field[4]) then found = "a"..k end
			end
			count = count + 1
			result = result..showIndent(count).."local i"..count.." = 1\n"
			result = result..showIndent(count).."while (i"..count.." <= tab[\""..field[4].."\"]) do\n"
			sub = sub.."[i"..count.."]"
		end
		if (type(field[4]) == "number") then
			count = count + 1
			result = result..showIndent(count).."local i"..count.." = 1\n"
			result = result..showIndent(count).."while (i"..count.." <= "..field[4]..") do\n"
			sub = sub.."[i"..count.."]"
		end
		local value = "tab[\""..field[1].."\"]"..sub
		result = result..showIndent(count+1)
		if (field[2] == "int") then result = result.."writeInt("..value..",idx)"
		elseif (field[2] == "long long") then result = result.."writeNew("..value..",idx)"
		elseif (field[2] == "double") then result = result.."writeNum("..value..",idx)"
		elseif (field[2] == "float") then result = result.."writeOld("..value..",idx)"
		elseif (field[2] == "char*") then result = result.."writeStr("..value..",1,idx)"
		else result = result.."write"..field[2].."("..value..",idx)" end
		result = result.."\n"
		while (count > 0) do
			if (conds == 0) then
				result = result..showIndent(count+1).."i"..count.." = i"..count.." + 1\n"
			else
				conds = conds - 1
			end
			result = result..showIndent(count).."end\n"
			count = count - 1
		end
	end
	result = result.."end\n"
	result = result.."--"
	return result
end
function listHere(name,file)
	local list = {}
	local map = {}
	local found = false
	for line in io.lines(file) do
		local set = false
		local clear = false
		if (string.find(line,"^--HERE "..name)) then set = true
		elseif(string.find(line,"^--HERE")) then clear = true end
		if clear then found = false end
		if found then
			local match
			_, _, match = string.find(line,"([A-Za-z0-9]+) = {")
			if (match) then
				list[#list+1] = match
				map[match] = _G[match]
			end
		end
		if set then found = true end
	end
	return list,map
end
function showCall(list,map,func)
	local result = ""
	for k,v in ipairs(list) do
		if result ~= "" then result = result.."\n" end
		result = result..func(v,map[v])
	end
	return result
end
function showShareC()
	local result = ""
	result = result..showAllocC("Int","int").."\n"
	result = result..showAllocC("New","long long").."\n"
	result = result..showAllocC("Num","double").."\n"
	result = result..showAllocC("Old","float").."\n"
	result = result..showAllocC("Str","char*").."\n"
	result = result..showAllocC("Ptr","char*").."\n"
	return result
end
function showFuncC()
	local result = ""
	-- result = desult..showSpoofC().."\n"
	result = result..showCall(Structs,Structz,showAllocC).."\n"
	result = result..showCall(Structs,Structz,showReadC).."\n"
	result = result..showCall(Structs,Structz,showWriteC).."\n"
	result = result..showCall(Structs,Structz,showRandC).."\n"
	result = result..showCall(Structs,Structz,showCompC).."\n"
	result = result..showCall(Structs,Structz,showSizeC)
	return result
end
function showWrapH()
	local result = ""
	prototype = true
	result = result..showShareC()
	return result
end
function showWrapC()
	local result = ""
	prototype = false
	result = result..showShareC()
	return result
end
function showCallH()
	local result = ""
	result = result..showCall(Enums,Enumz,showEnumC).."\n"
	result = result..showCall(Structs,Structz,showStructC).."\n"
	prototype = true
	result = result..showFuncC()
	return result
end
function showCallC()
	local result = ""
	prototype = false
	result = result..showFuncC()
	return result
end
function showCallHs()
	local result = ""
	result = result..showCall(Enums,Enumz,showEnumHs).."\n"
	result = result..showCall(Enums,Enumz,showCodeHs).."\n"
	result = result..showCall(Structs,Structz,showStructHs).."\n"
	result = result..showCall(Structs,Structz,showAccessHs).."\n"
	result = result..showHelpHs().."\n"
	result = result..showCall(Structs,Structz,showReadHs).."\n"
	result = result..showCall(Structs,Structz,showWriteHs).."\n"
	return result
end
function showCallLua()
	local result = ""
	result = result..showCall(Enums,Enumz,showCodeLua).."\n"
	result = result..showCall(Structs,Structz,showReadLua).."\n"
	result = result..showCall(Structs,Structz,showWriteLua).."\n"
	return result
end