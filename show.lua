require "luax"
require "coroutine"

function showIndent(depth)
	local indent = ""; c = 0 while c < depth do c = c + 1; indent = indent.."    " end
	return indent
end
function listSort(map)
	local tab = {}
	for key,val in pairs(map) do
		table.insert(tab,key)
	end
	table.sort(tab)
	return tab
end
function listFlatten(list)
	local ret = {}
	if (type(list) == "table") then
		for k,v in ipairs(list) do
			local rec = listFlatten(v)
			for ky,vl in ipairs(rec) do
				ret[#ret+1] = vl
			end
		end
	else
		ret[#ret+1] = list
	end
	return ret
end
function overLap(val,field)
	-- assume there are no disjoint dimensions
	local share = true
	for ky,vl in pairs(field[3]) do
		if val[3][ky] then
			-- val and field share dimension ky
			share = false
			for k,v in pairs(field[3][ky]) do
				-- k is a region of field[3][ky]
				if val[3][ky][k] then
					-- k is a region of val[3][ky]
					share = true
				end
			end
			if not share then
				-- this dimension is disjoint
				break
			end
			-- subsequent dimensions might be disjoint
		end
	end
	return share
end
function overAny(list,field)
	for key,val in ipairs(list) do
		-- val is a field from the list
		if overLap(val,field) then
			-- val has no disjoint dimension
			return true
		end
	end
	-- every one has a disjoint dimension
	return false
end
function equalLap(val,field)
	for ky,vl in pairs(field[3]) do
		if not val[3][ky] then
			return false
		end
		for k,v in pairs(field[3][ky]) do
			if not val[3][ky][k] then
				return false
			end
		end
	end
	for ky,vl in pairs(val[3]) do
		if not field[3][ky] then
			return false
		end
		for k,y in pairs(val[3][ky]) do
			if not field[3][ky][k] then
				return false
			end
		end
	end
	return true
end
function equalAll(list,field)
	for key,val in ipairs(list) do
		if not equalLap(val,field) then
			return false
		end
	end
	return true
end
function addTo(list,field)
	list[#list+1] = field
end
function sortIter(map)
	local tab = {}
	for key,val in pairs(map) do
		table.insert(tab,key)
	end
	table.sort(tab)
	local idx = 0
	return function()
		idx = idx + 1
		return tab[idx],map[tab[idx]]
	end
end
function sizeIter(map)
	local count = 0
	for key,val in pairs(map) do
		count = count + 1
	end
	return count
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
function showStructCK(field)
	local result = ""
	local separate = " "
	for key,val in sortIter(field[3]) do
		result = result..separate..key..":"
		separate = ";"
		local sep = ""
		for ky,vl in sortIter(val) do
			result = result..sep..ky
			sep = ","
		end
	end
	return result
end
function showStructCJ(field)
	local result = ""
	if (type(field[4]) == "table") then
		for k,v in pairs(field[4]) do
			result = result.."["..v.."]"
		end
	end
	return result
end
function showStructCI(field)
	local result
	if (Enumz[field[2]]~=nil) then
		result = "enum "..field[2]
	elseif (Structz[field[2]]~=nil) then
		result = "struct "..field[2]
	elseif (field[2] == "Chr") then
		result = "char"
	elseif (field[2] == "Int") then
		result = "int"
	elseif (field[2] == "Int32") then
		result = "int32_t"
	elseif (field[2] == "New") then
		result = "long long"
	elseif (field[2] == "Num") then
		result = "double"
	elseif (field[2] == "Old") then
		result = "float"
	elseif (field[2] == "Str") then
		result = "char*"
	elseif (field[2] == "Dat") then
		result = "void*"
	else
		result = field[2]
	end
	if (type(field[4]) == "number") then
		result = result.."*"
	elseif (type(field[4]) == "string") then
		result = result.."*"
	end
	return result
end
function showStructCH(pre,field,post)
	coroutine.yield(pre..showStructCI(field).." "..field[1]..showStructCJ(field)..";"..post)
end
function showStructCG(pre,mid,second)
	local post = ""
	if #second > 0 then 
		post = showStructCK(second[1])
	end
	if #second > 1 then
		coroutine.yield(pre..mid.."struct { //"..post)
		for id,fd in ipairs(second) do
			showStructCH(pre.."    ",fd,"")
		end
		coroutine.yield(pre..mid.."};")
	else
		for id,fd in ipairs(second) do
			showStructCH(pre.."",fd," //"..post)
		end
	end
end     
function showStructCF(struct)
	local first = {}
	local second = {}
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("// 1")
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("// 2")
			showStructCG("    ","// ",second)
			second = {}
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("// 3")
			coroutine.yield("    union {")
			showStructCG("        ","",second)
			second = {}
			addTo(first,fld)
			addTo(second,fld)
		elseif not firster then
			-- coroutine.yield("// 4")
			showStructCG("        ","",second)
			second = {}
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("// 5")
			showStructCG("        ","",second)
			coroutine.yield("    };")
			first = {}
			second = {}
			addTo(second,fld)
		end
	end
	if sizeIter(second[1][3]) == 0 then
		-- coroutine.yield("// 6")
		showStructCG("    ","// ",second)
	elseif #first == 0 then
		-- coroutine.yield("// 7")
		showStructCG("    ","",second)
	else
		-- coroutine.yield("// 8")
		showStructCG("        ","",second)
		coroutine.yield("    };")
	end
end
function showStructC(name,struct)
	local result = ""
	local wrap = coroutine.wrap(showStructCF)
	local line = wrap(struct)
	while line do
		result = result..line.."\n"
		line = wrap()
	end
	return "struct "..name.." {\n"..result.."};"
end
function showReadCJ(pre,field,sub,arg,post)
	if (Enumz[field[2]]~=nil) then
		coroutine.yield(pre.."{int temp = readInt(idx); ".."ptr->"..field[1]..sub.." = temp;}"..post)
	elseif (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."read"..field[2].."(&ptr->"..field[1]..sub..",idx);"..post)
	elseif (field[2] == "Str") then
		coroutine.yield(pre.."readStr(&ptr->"..field[1]..sub..",idx);"..post)
	elseif (field[2] == "Dat") then
		coroutine.yield(pre.."readDat(&ptr->"..field[1]..sub..",idx);"..post)
	else
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = read"..field[2].."(idx);"..post)
	end	
end
function showReadCI(pre,field,post)
	local count = 0
	if (type(field[4]) == "table") then
		count = sizeIter(field[4])
	end
	if (type(field[4]) == "table") and (count == 0) then
		showReadCJ(pre,field,"","0",post)
	elseif (type(field[4]) == "table") then
		local indent = ""
		local sub = ""
		local arg = ""..#field[4]
		for key,val in ipairs(field[4]) do
			sub = sub.."[i"..key.."]"
			arg = arg..",i"..key
			coroutine.yield(pre..indent.."for (int i"..key.." = 0; i"..key.." < "..val.."; i"..key.."++)")
			indent = indent.."    "
		end
		showReadCJ(pre..indent,field,sub,arg,post)
	elseif (type(field[4]) == "number") then
		coroutine.yield(pre.."alloc"..field[2].."(&ptr->"..field[1]..","..field[4]..");")
		coroutine.yield(pre.."for (int i = 0; i < "..field[4].."; i++)")
		showReadCJ(pre.."    ",field,"[i]","1,i",post)
	elseif (type(field[4]) == "string") then
		coroutine.yield(pre.."alloc"..field[2].."(&ptr->"..field[1]..",ptr->"..field[4]..");")
		coroutine.yield(pre.."for (int i = 0; i < ptr->"..field[4].."; i++)")
		showReadCJ(pre.."    ",field,"[i]","1,i",post)
	end
end
function showReadCH(field)
	local condit = ""
	local seper = ""
	local count = sizeIter(field[3])
	for key,val in sortIter(field[3]) do
		local cond = ""
		local sep = ""
		local cnt = sizeIter(val)
		for ky,vl in sortIter(val) do
			if (cnt > 1) or (count > 1) then
				cond = cond..sep.."(ptr->"..key.." == "..ky..")"
			else
				cond = cond..sep.."ptr->"..key.." == "..ky
			end
			sep = " || "
		end
		if (cnt > 1) then
			condit = condit..seper.."("..cond..")"
		else
			condit = condit..seper..cond
		end
		seper = " && "
	end
	if (count > 0) then
		coroutine.yield("    if ("..condit..") {")
		showReadCI("        ",field,"}")
	else
		showReadCI("    ",field,"")
	end
end
function showReadCG(second)
	for id,fd in ipairs(second) do
		showReadCH(fd)
	end
end
function showReadCF(struct)
	local first = {}
	local second = {}
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("// 1")
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("// 2")
			showReadCG(second)
			second = {}
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("// 3")
			showReadCG(second)
			second = {}
			addTo(first,fld)
			addTo(second,fld)
		elseif not firster then
			-- coroutine.yield("// 4")
			showReadCG(second)
			second = {}
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("// 5")
			showReadCG(second)
			first = {}
			second = {}
			addTo(second,fld)
		end
	end
	showReadCG(second)
end
function showReadCE(name,struct)
	local result = ""
	local wrap = coroutine.wrap(showReadCF)
	local line = wrap(struct)
	while line do
		result = result..line.."\n"
		line = wrap()
	end
	return result
end
function showReadC(name,struct)
	local result = ""
	result = result.."void read"..name.."(struct "..name.." *ptr, int idx)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	result = result.."    free"..name.."(ptr);\n"
	result = result..showReadCE(name,struct)
	return result.."}"
end
function showWriteCJ(pre,field,sub,arg,post)
	if (Enumz[field[2]]~=nil) then
		coroutine.yield(pre.."{int temp = ptr->"..field[1]..sub.."; writeInt(temp,idx);}"..post)
	elseif (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."write"..field[2].."(&ptr->"..field[1]..sub..",idx);"..post)
	elseif (field[2] == "Str") then
		coroutine.yield(pre.."writeStr(ptr->"..field[1]..sub..",idx);"..post)
	elseif (field[2] == "Dat") then
		coroutine.yield(pre.."writeDat(ptr->"..field[1]..sub..",idx);"..post)
	else
		coroutine.yield(pre.."write"..field[2].."(ptr->"..field[1]..sub..",idx);"..post)
	end	
end
function showWriteCI(pre,field,post)
	local count = 0
	if (type(field[4]) == "table") then
		count = sizeIter(field[4])
	end
	if (type(field[4]) == "table") and (count == 0) then
		showWriteCJ(pre,field,"","0",post)
	elseif (type(field[4]) == "table") then
		local indent = ""
		local sub = ""
		local arg = ""..#field[4]
		for key,val in ipairs(field[4]) do
			sub = sub.."[i"..key.."]"
			arg = arg..",i"..key
			coroutine.yield(pre..indent.."for (int i"..key.." = 0; i"..key.." < "..val.."; i"..key.."++)")
			indent = indent.."    "
		end
		showWriteCJ(pre..indent,field,sub,arg,post)
	elseif (type(field[4]) == "number") then
		coroutine.yield(pre.."for (int i = 0; i < "..field[4].."; i++)")
		showWriteCJ(pre.."    ",field,"[i]","1,i",post)
	elseif (type(field[4]) == "string") then
		coroutine.yield(pre.."for (int i = 0; i < ptr->"..field[4].."; i++)")
		showWriteCJ(pre.."    ",field,"[i]","1,i",post)
	end
end
function showWriteC(name,struct)
	local result = ""
	local temp = showReadCI
	result = result.."void write"..name.."(struct "..name.." *ptr, int idx)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	showReadCI = showWriteCI
	result = result..showReadCE(name,struct)
	showReadCI = temp
	return result.."}"
end
function showFreeCJ(pre,field,sub,arg,post)
	if (Enumz[field[2]]~=nil) then
		coroutine.yield(pre..";"..post)
	elseif (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."free"..field[2].."(&ptr->"..field[1]..sub..");"..post)
	elseif (field[2] == "Str") then
		coroutine.yield(pre.."assignStr(&ptr->"..field[1]..sub..",0);"..post)
	elseif (field[2] == "Dat") then
		coroutine.yield(pre.."assignDat(&ptr->"..field[1]..sub..",0);"..post)
	else
		coroutine.yield(pre..";"..post)
	end	
end
function showFreeCI(pre,field,post)
	local recurse = (Structz[field[2]]~=nil) or (field[2] == "Str") or (field[2] == "Dat")
	local count = 0
	if (type(field[4]) == "table") then
		count = sizeIter(field[4])
	end
	if (type(field[4]) == "table") then
		local indent = ""
		local sub = ""
		local arg = ""..#field[4]
		if not (count == 0) then
			for key,val in ipairs(field[4]) do
				sub = sub.."[i"..key.."]"
				arg = arg..",i"..key
				coroutine.yield(pre..indent.."for (int i"..key.." = 0; i"..key.." < "..val.."; i"..key.."++)")
				indent = indent.."    "
			end
		end
		showFreeCJ(pre..indent,field,sub,arg,post)
	elseif (type(field[4]) == "number") then
		if recurse then
			coroutine.yield(pre.."if (ptr->"..field[1]..")")
			coroutine.yield(pre.."    for (int i = 0; i < "..field[4].."; i++)")
			showFreeCJ(pre.."        ",field,"[i]","1,i","")
		end
		coroutine.yield(pre.."alloc"..field[2].."(&ptr->"..field[1]..",0);"..post)
	elseif (type(field[4]) == "string") then
		if recurse then
			coroutine.yield(pre.."if (ptr->"..field[1]..")")
			coroutine.yield(pre.."    for (int i = 0; i < ptr->"..field[4].."; i++)")
			showFreeCJ(pre.."        ",field,"[i]","1,i","")
		end
		coroutine.yield(pre.."alloc"..field[2].."(&ptr->"..field[1]..",0);"..post)
	end
end
function showFreeCH(field)
	local recurse = (Structz[field[2]]~=nil) or (field[2] == "Str") or (field[2] == "Dat")
	local alloc = (type(field[4]) == "number") or (type(field[4]) == "string")
	local condit = ""
	local seper = ""
	local count = sizeIter(field[3])
	for key,val in sortIter(field[3]) do
		local cond = ""
		local sep = ""
		local cnt = sizeIter(val)
		for ky,vl in sortIter(val) do
			if (cnt > 1) or (count > 1) then
				cond = cond..sep.."(ptr->"..key.." == "..ky..")"
			else
				cond = cond..sep.."ptr->"..key.." == "..ky
			end
			sep = " || "
		end
		if (cnt > 1) then
			condit = condit..seper.."("..cond..")"
		else
			condit = condit..seper..cond
		end
		seper = " && "
	end
	if recurse or alloc then
		if (count > 0) then
			coroutine.yield("    if ("..condit..") {")
			showFreeCI("        ",field,"}")
		else
			showFreeCI("    ",field,"")
		end
	end
end
function showFreeC(name,struct)
	local result = ""
	local temp = showReadCH
	result = result.."void free"..name.."(struct "..name.." *ptr)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	result = result.."    if (ptr == 0) return;\n"
	showReadCH = showFreeCH
	result = result..showReadCE(name,struct)
	showReadCH = temp
	return result.."}"
end
function showAllocC(name,typ)
	local result = ""
	if (Enumz[name] ~= nil) then
		result = result.."void alloc"..name.."(enum "..name.." **ptr, int siz)"
		if prototype then return result..";" else result = result.."\n{\n" end
		result = result..showIndent(1).."if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}\n"
		result = result..showIndent(1).."if (siz == 0) return;\n"
		result = result..showIndent(1).."*ptr = malloc(siz*sizeof(enum "..name.."));\n"
		result = result..showIndent(1).."if (*ptr == 0) ERROR();\n"
		result = result..showIndent(1).."for (int i = 0; i < siz; i++) (*ptr)[i] = 0;\n"
		result = result.."}"
	elseif (Structz[name] ~= nil) then
		result = result.."void alloc"..name.."(struct "..name.." **ptr, int siz)"
		if prototype then return result..";" else result = result.."\n{\n" end
		result = result..showIndent(1).."if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}\n"
		result = result..showIndent(1).."if (siz == 0) return;\n"
		result = result..showIndent(1).."*ptr = malloc(siz*sizeof(struct "..name.."));\n"
		result = result..showIndent(1).."if (*ptr == 0) ERROR();\n"
		result = result..showIndent(1).."struct "..name.." init = {0};\n"
		result = result..showIndent(1).."for (int i = 0; i < siz; i++)\n"
		result = result..showIndent(2).."memcpy(&(*ptr)[i],&init,sizeof(init));\n"
		result = result.."}"
	end
	return result
end
randChr = 1
randInt = 10
randNew = 100
randInt32 = 1000
randNum = 0.1
randOld = 0.2
function showRandCJ(pre,field,sub,arg,post)
	if (Enumz[field[2]]~=nil) then
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = "..randInt.."%"..field[2].."s;"..post)
			-- "{int temp = readInt(idx); ".."ptr->"..field[1]..sub.." = temp;}"..post)
		randInt = randInt + 1;
	elseif (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."rand"..field[2].."(&ptr->"..field[1]..sub..");"..post)
	elseif (field[2] == "Int") then
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = "..randInt..";"..post)
		randInt = randInt + 1;
	elseif (field[2] == "Int32") then
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = "..randInt32..";"..post)
		randInt32 = randInt32 + 1;
	elseif (field[2] == "New") then
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = "..randNew..";"..post)
		randNew = randNew + 1;
	elseif (field[2] == "Num") then
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = "..randNum..";"..post)
		randNum = randNum + 1.0
	elseif (field[2] == "Old") then
		coroutine.yield(pre.."ptr->"..field[1]..sub.." = "..randOld..";"..post)
		randOld = randOld + 1.0
	elseif (field[2] == "Str") then
		coroutine.yield(pre.."{const char *temp = \"hello ok again\"; assignStr(&ptr->"..field[1]..sub..",temp);}"..post)
	elseif (field[2] == "Dat") then
		coroutine.yield(pre.."{const char *temp = \"\\x0e\\x00\\x00\\x00hello ok again\"; assignDat(&ptr->"..field[1]..sub..",temp);}"..post)
	end	
end
function showRandC(name,struct)
	local result = ""
	local temp = showReadCJ
	result = result.."void rand"..name.."(struct "..name.." *ptr)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	result = result.."    free"..name.."(ptr);\n"
	showReadCJ = showRandCJ
	result = result..showReadCE(name,struct)
	showReadCJ = temp
	return result.."}"
end
function showCompCJ(pre,field,sub,arg,post)
	if (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."if (!comp"..field[2].."(&ptr->"..field[1]..sub..", &cmp->"..field[1]..sub..")) return 0;"..post)
	elseif (field[2] == "Chr") or (field[2] == "Int") or (field[2] == "Int32") or (field[2] == "New") or (field[2] == "Num") or (field[2] == "Old") or (Enumz[field[2]]~=nil) then
		coroutine.yield(pre.."if (ptr->"..field[1]..sub.." != cmp->"..field[1]..sub..") return 0;"..post)
	elseif (field[2] == "Str") then
		coroutine.yield(pre.."if (strcmp(ptr->"..field[1]..sub..",cmp->"..field[1]..sub..") != 0) return 0;"..post)
	elseif (field[2] == "Dat") then
		coroutine.yield(pre.."if (memcmp(ptr->"..field[1]..sub..",cmp->"..field[1]..sub..",*(int*)ptr->"..field[1]..sub..") != 0) return 0;"..post)
	end
end
function showCompC(name,struct)
	local result = ""
	local temp = table.pack(showReadCI,showWriteCJ)
	result = result.."int comp"..name.."(struct "..name.." *ptr, struct "..name.." *cmp)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	showReadCI,showWriteCJ = showWriteCI,showCompCJ
	result = result..showReadCE(name,struct)
	showReadCI,showWriteCJ = table.unpack(temp)
	result = result.."    return 1;\n"
	return result.."}"
end
function showSizeCJ(pre,field,sub,arg,post)
	if (Enumz[field[2]]~=nil) then
		coroutine.yield(pre.."result = result + sizeof(int);"..post)
	elseif (field[2] == "Str") then
		coroutine.yield(pre.."result = result + strlen(ptr->"..field[1]..sub..") + 1;"..post)
	elseif (field[2] == "Dat") then
		coroutine.yield(pre.."result = result + *(int*)(ptr->"..field[1]..sub..") + sizeof(int);"..post)
	elseif (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."result = result + size"..field[2].."(&ptr->"..field[1]..sub..");"..post)
	elseif (field[2] == "Chr") then
		coroutine.yield(pre.."result = result + sizeof(char);"..post)
	elseif (field[2] == "Int") then
		coroutine.yield(pre.."result = result + sizeof(int);"..post)
	elseif (field[2] == "Int32") then
		coroutine.yield(pre.."result = result + sizeof(int32_t);"..post)
	elseif (field[2] == "New") then
		coroutine.yield(pre.."result = result + sizeof(long long);"..post)
	elseif (field[2] == "Num") then
		coroutine.yield(pre.."result = result + sizeof(double);"..post)
	elseif (field[2] == "Old") then
		coroutine.yield(pre.."result = result + sizeof(float);"..post)
	end
end
function showSizeC(name,struct)
	local result = ""
	local temp = table.pack(showReadCI,showWriteCJ)
	result = result.."int size"..name.."(struct "..name.." *ptr)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	result = result.."    int result = 0;\n"
	showReadCI,showWriteCJ = showWriteCI,showSizeCJ
	result = result..showReadCE(name,struct)
	showReadCI,showWriteCJ = table.unpack(temp)
	result = result.."    return result;\n"
	return result.."}"
end
function showShowEC(name,enum)
	local result = ""
	result = result.."void show"..name.."(enum "..name.." val, char **str, int *len)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."switch (val) {\n"
	for i,v in ipairs(enum) do
		result = result..showIndent(2).."case("..v.."): showEnum(\""..name.."\",\""..v.."\",str,len); break;\n"
	end
	result = result..showIndent(2).."case("..name.."s): showEnum(\""..name.."\",\""..name.."s\",str,len); break;\n"
	result = result..showIndent(1).."}\n"
	return result.."}"
end
function showShowCJ(pre,field,sub,arg,post)
	if (Structz[field[2]]~=nil) then
		coroutine.yield(pre.."{showField(\""..field[1].."\",str,len,"..arg.."); show"..field[2].."(&ptr->"..field[1]..sub..",str,len);}"..post)
	else
		coroutine.yield(pre.."{showField(\""..field[1].."\",str,len,"..arg.."); show"..field[2].."(ptr->"..field[1]..sub..",str,len);}"..post)
	end
end
function showShowSC(name,struct)
	local result = ""
	local temp = table.pack(showReadCI,showWriteCJ)
	result = result.."void show"..name.."(struct "..name.." *ptr, char **str, int *len)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	result = result.."    showOpen(\""..name.."\",str,len);\n"
	showReadCI,showWriteCJ = showWriteCI,showShowCJ
	result = result..showReadCE(name,struct)
	showReadCI,showWriteCJ = table.unpack(temp)
	result = result.."    showClose(str,len);\n"
	return result.."}"
end
global_name = "Oops"
function showHideCJ(pre,field,sub,arg,post)
	coroutine.yield(pre.."if (!hideField(\""..field[1].."\",str,len,"..arg..") || !hide"..field[2].."(&ptr->"..field[1]..sub..",str,len)) {free"..global_name.."(ptr); return 0;}"..post)
end
function showHideSC(name,struct)
	local result = ""
	local temp = showReadCJ
	result = result.."int hide"..name.."(struct "..name.." *ptr, const char *str, int *len)"
	if prototype then return result..";" end
	result = result.."\n{\n"
	result = result.."    free"..name.."(ptr);\n"
	result = result.."    if (!hideOpen(\""..name.."\",str,len)) {free"..name.."(ptr); return 0;}\n"
	global_name = name
	showReadCJ = showHideCJ
	result = result..showReadCE(name,struct)
	showReadCJ = temp
	global_name = "Oops"
	result = result.."    if (!hideClose(str,len)) {free"..name.."(ptr); return 0;}\n"
	result = result.."    return 1;\n"
	return result.."}"
end
function showHideEC(name,enum)
	local result = ""
	result = result.."int hide"..name.."(enum "..name.." *val, const char *str, int *len)"
	if prototype then result = result..";\n" else result = result.."\n{\n"
	for i,v in ipairs(enum) do
		result = result..showIndent(1).."if (hideEnum(\""..name.."\",\""..v.."\",str,len)) {*val = "..v.."; return 1;}\n"
	end
	result = result..showIndent(1).."if (hideEnum(\""..name.."\",\""..name.."s\",str,len)) {*val = "..name.."s; return 1;}\n"
	result = result..showIndent(1).."return 0;\n"
	result = result.."}" end
	return result
end
function showConstantCF(name,sub,list)
	local result = name
	local last = nil
	if k == #list then
		return list[#list]
	end
	for k,v in ipairs(list) do
		if last then
			if k < sub and sub < #list then
				result = result.."_"..string.gsub(v,"[ ():]","_")
			elseif k == sub and sub == #list and last == "Str" then
				result = "\""..v.."\""
			elseif k == sub and sub == #list and last == "Int" then
				result = "(int)"..v
			elseif k == sub and sub == #list then
				result = v
			elseif k == sub then
				result = result.."__"..last
			elseif k == (sub+2) then
				result = result.."__"..last
			else
				result = result.."_"..last
			end
			last = nil
		else
			last = v
		end
	end
	return result
end
function showConstantCG(sub,list)
	local result = {}
	local last = nil
	for k,v in ipairs(list) do
		if last then
			if k >= sub and last == "Str" then
				result[#result+1] = "char *"
			elseif k >= sub and last == "Int" then
				result[#result+1] = "int"
			elseif k >= sub then
				result[#result+1] = "enum "..last
			end
			last = nil
		else
			last = v
		end
	end
	return result
end
function showConstantC(name,constant)
	local result = ""
	local mapping = {}
	local arguments = {}
	local values = {}
	for key,val in ipairs(constant) do
		local goon = true
		nestInit(#val)
		for k,v in ipairs(val) do nestElem(k-1,v) end
		nestScan()
		while goon do
			local line = {}
			goon = not (nestPass() == 0)
			for k,v in ipairs(val) do line[#line+1] = nestRepl(k-1) end
			values[#values+1] = line
		end
	end
	nestInit(0)
	for key,val in ipairs(values) do
		local last = nil
		for k,v in ipairs(val) do
			if last and (k < #val) then -- last is type, v is constant
				local func = showConstantCF(name,k,val) -- func is containing function
				local gunc = showConstantCF(name,k+2,val) -- gunc is return value
				local args = showConstantCG(k,val) -- args are argument types
				-- io.stderr:write("func "..func.." v "..v.." gunc "..gunc.." k "..k.."/"..#val.."\n")
				if not arguments[func] then arguments[func] = args end
				if not mapping[func] then mapping[func] = {} end
				mapping[func][v] = gunc
				last = nil
			else -- v is type
				last = v
			end
		end
	end
	for key,val in pairs(arguments) do if #val > 1 then
		local declare = key.." ("
		-- result = result.."func "..key.."(_ arg: "
		for k,v in ipairs(val) do
			-- io.stderr:write("k "..k.."/"..#val.." v "..v.."\n")
			if k == #val then
				declare = v.." "..declare
				-- io.stderr:write("declare "..declare.."\n")
				if prototype then
					result = result..declare..";\n"
				else
					result = result..declare.."\n"
					result = result.."{\n"
					result = result.."    switch (arg) {\n"
				end
			elseif k == 1 then
				declare = declare..v.." arg)"
			else
				declare = "(*"..declare..")("..v..")"
			end
		end
		if not prototype then
			for k,v in pairs(mapping[key]) do
				result = result.."        case ("..k.."): return "..v..";\n"
			end
			result = result.."        default : return 0;\n"
			result = result.."    }\n"
			result = result.."}\n"
		end
	end end
	result = result.."//"
	return result
end
function showTypeCF(type)
	local result = ""
	if (not (Structz[type] == nil)) then
		result = result.."struct "..type
	elseif (not (Enumz[type] == nil)) then
		result = result.."enum "..type
	elseif (type == "Chr") then
		result = result.."char"
	elseif (type == "Int") then
		result = result.."int"
	elseif (type == "Int32") then
		result = result.."int32_t"
	elseif (type == "New") then
		result = result.."long long"
	elseif (type == "Num") then
		result = result.."double"
	elseif (type == "Old") then
		result = result.."float"
	elseif (type == "Str") then
		result = result.."char*"
	elseif (type == "Dat") then
		result = result.."void*"
	end
	return result
end
function showRtypeC(list)
	local result = ""
	result = result.."void readType(char **str, int *len, int typ, int idx)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case("..(k-1).."): {\n"
		if (not (Structz[v] == nil)) then
			result = result..showIndent(2)..showTypeCF(v).." tmp = {0};\n"
			result = result..showIndent(2).."read"..v.."(&tmp,idx);\n"
			result = result..showIndent(2).."show"..v.."(&tmp,str,len);\n"
		elseif (not (Enumz[v] == nil)) then
			result = result..showIndent(2)..showTypeCF(v).." tmp = readInt(idx);\n"
			result = result..showIndent(2).."show"..v.."(tmp,str,len);\n"
		elseif (v == "Dat") or (v == "Str") then
			result = result..showIndent(2)..showTypeCF(v).." tmp = 0;\n"
			result = result..showIndent(2).."read"..v.."(&tmp,idx);\n"
			result = result..showIndent(2).."show"..v.."(tmp,str,len);\n"
			result = result..showIndent(2).."free(tmp);\n"
		else
			result = result..showIndent(2)..showTypeCF(v).." tmp = read"..v.."(idx);\n"
			result = result..showIndent(2).."show"..v.."(tmp,str,len);\n"
		end
		result = result..showIndent(2).."break;}\n"
	end
	result = result..showIndent(1).."}\n"
	result = result.."}\n"
	return result
end
function showWtypeC(list)
	local result = ""
	result = result.."void writeType(const char *str, int *len, int typ, int idx)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case("..(k-1).."): {\n"
		if (not (Structz[v] == nil)) then
			result = result..showIndent(2)..showTypeCF(v).." tmp = {0};\n"
			result = result..showIndent(2).."if (!hide"..v.."(&tmp,str,len)) callNote(idx);\n"
			result = result..showIndent(2).."write"..v.."(&tmp,idx);\n"
		elseif (not (Enumz[v] == nil)) then
			result = result..showIndent(2)..showTypeCF(v).." tmp = 0;\n"
			result = result..showIndent(2).."if (!hide"..v.."(&tmp,str,len)) callNote(idx);\n"
			result = result..showIndent(2).."writeInt(tmp,idx);\n"
		elseif (v == "Dat") or (v == "Str") then
			result = result..showIndent(2)..showTypeCF(v).." tmp = 0;\n"
			result = result..showIndent(2).."if (!hide"..v.."(&tmp,str,len)) callNote(idx);\n"
			result = result..showIndent(2).."write"..v.."(tmp,idx);\n"
		else
			result = result..showIndent(2)..showTypeCF(v).." tmp = 0;\n"
			result = result..showIndent(2).."if (!hide"..v.."(&tmp,str,len)) callNote(idx);\n"
			result = result..showIndent(2).."write"..v.."(tmp,idx);\n"
		end
		result = result..showIndent(2).."break;}\n"
	end
	result = result..showIndent(1).."}\n"
	result = result.."}\n"
	return result
end
function showLtypeC(list)
	local result = ""
	result = result.."void loopType(int typ, int one, int oth)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."int len = 0;\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case("..(k-1).."): {\n"
		if (not (Structz[v] == nil)) then
			result = result..showIndent(2)..showTypeCF(v).." tmp = {0};\n"
			result = result..showIndent(2).."read"..v.."(&tmp,one);\n"
			result = result..showIndent(2).."write"..v.."(&tmp,oth);\n"
		elseif (not (Enumz[v] == nil)) then
			result = result..showIndent(2)..showTypeCF(v).." tmp = readInt(one);\n"
			result = result..showIndent(2).."writeInt(tmp,oth);\n"
		elseif (v == "Dat") or (v == "Str") then
			result = result..showIndent(2)..showTypeCF(v).." tmp = 0;\n"
			result = result..showIndent(2).."read"..v.."(&tmp,one);\n"
			result = result..showIndent(2).."write"..v.."(tmp,oth);\n"
		else
			result = result..showIndent(2)..showTypeCF(v).." tmp = read"..v.."(one);\n"
			result = result..showIndent(2).."write"..v.."(tmp,oth);\n"
		end
		result = result..showIndent(2).."break;}\n"
	end
	result = result..showIndent(1).."}\n"
	result = result.."}\n"
	return result
end
function showIdentC(list)
	local result = ""
	result = result.."int identType(const char *str)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."int len = 0;\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."if (hideIdent(\""..v.."\",str,&len)) return "..(k-1)..";\n"
	end
	result = result..showIndent(1).."return -1;\n"
	result = result.."}\n"
	return result
end
function showRfieldC(list,map)
	local result = ""
	result = result.."void readField(int typ, int fld, int idx, int ifd, int xfd, int ofd)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case("..(k-1).."): {\n"
		result = result..showIndent(2).."struct "..v.." tmp = {0};\n"
		result = result..showIndent(2).."read"..v.."(&tmp,ifd);\n"
		result = result..showIndent(2).."switch (fld) {\n"
		for ky,vl in ipairs(map[v]) do
			result = result..showIndent(2).."case("..(ky-1).."): {\n"
			lval = "tmp."..vl[1]
			if ((type(vl[4]) == "table") and (#vl[4] > 1)) then
				lval = "((int*)("..lval.."))"
			end
			if (not (type(vl[4]) == "table") or (#vl[4] > 0)) then
				lval = lval.."[idx]"
			end
			if (not (Structz[vl[2]] == nil)) then
				result = result..showIndent(3).."read"..vl[2].."(&"..lval
			elseif (not (Enumz[vl[2]] == nil)) then
				result = result..showIndent(3)..lval
			elseif (vl[2] == "Str") then
				result = result..showIndent(3).."readStr(&"..lval
			elseif (vl[2] == "Dat") then
				result = result..showIndent(3).."readDat(&"..lval
			else
				result = result..showIndent(3)..lval
			end
			if (not (Structz[vl[2]] == nil)) then
				result = result..",xfd);\n"
			elseif (not (Enumz[vl[2]] == nil)) then
				result = result.." = readInt(xfd);\n"
			elseif (vl[2] == "Str") then
				result = result..",xfd);\n"
			elseif (vl[2] == "Dat") then
				result = result..",xfd);\n"
			else
				result = result.." = read"..vl[2].."(xfd);\n"
			end
			result = result..showIndent(3).."write"..v.."(&tmp,ofd);\n"
			result = result..showIndent(3).."break;}\n"
		end
		result = result..showIndent(2).."}\n"
		result = result..showIndent(2).."break;}\n"
	end
	result = result..showIndent(1).."}\n"
	result = result.."}\n"
	return result
end
function showWfieldC(list,map)
	local result = ""
	result = result.."void writeField(int typ, int fld, int idx, int ifd, int ofd)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case("..(k-1).."): {\n"
		result = result..showIndent(2).."struct "..v.." tmp = {0};\n"
		result = result..showIndent(2).."read"..v.."(&tmp,ifd);\n"
		result = result..showIndent(2).."switch (fld) {\n"
		for ky,vl in ipairs(map[v]) do
			result = result..showIndent(2).."case("..(ky-1).."): {\n"
			lval = "tmp."..vl[1]
			if ((type(vl[4]) == "table") and (#vl[4] > 1)) then
				lval = "((int*)("..lval.."))"
			end
			if (not (type(vl[4]) == "table") or (#vl[4] > 0)) then
				lval = lval.."[idx]"
			end
			if (not (Structz[vl[2]] == nil)) then
				result = result..showIndent(3).."write"..vl[2].."(&"..lval
			elseif (not (Enumz[vl[2]] == nil)) then
				result = result..showIndent(3).."writeInt("..lval
			else
				result = result..showIndent(3).."write"..vl[2].."("..lval
			end
			result = result..",ofd);\n"
			result = result..showIndent(3).."break;}\n"
		end
		result = result..showIndent(2).."}\n"
		result = result..showIndent(2).."break;}\n"
	end
	result = result..showIndent(1).."}\n"
	result = result.."}\n"
	return result
end
function showIfieldC(list,map)
	local result = ""
	result = result.."int identField(int typ, const char *str)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."int len = 0;\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case ("..(k-1).."): // "..v.."\n"
		for ky,vl in ipairs(map[v]) do
			result = result..showIndent(2).."if (hideIdent(\""..vl[1].."\",str,&len)) return "..(ky-1)..";\n"
		end
		result = result..showIndent(1).."break;\n"
	end
	result = result..showIndent(1).."}\n"
	result = result..showIndent(1).."return -1;\n"
	result = result.."}\n"
	return result
end
function showTfieldC(list,map)
	local result = ""
	result = result.."int identSubtype(int typ, int pos)"
	if prototype then return result..";\n" end
	result = result.."\n{\n"
	result = result..showIndent(1).."int len = 0;\n"
	result = result..showIndent(1).."switch (typ) {\n"
	for k,v in ipairs(list) do
		result = result..showIndent(1).."case ("..(k-1).."): // "..v.."\n"
		result = result..showIndent(2).."switch (pos) {\n"
		for ky,vl in ipairs(map[v]) do
			result = result..showIndent(2).."case ("..(ky-1).."): return identType(\""..vl[2].."\");\n"
		end
		result = result..showIndent(2).."default: return -1;}\n"
		result = result..showIndent(2).."break;\n"
	end
	result = result..showIndent(1).."default: return -1;}\n"
	result = result..showIndent(1).."return -1;\n"
	result = result.."}\n"
	return result
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
	if showhide then
		result = result.."hide"..name.." :: RefStr -> IO (Maybe "..name..")\n"
		result = result.."hide"..name.." idx = do\n"
		result = result..showIndent(1).."a0 <- return Nothing\n"
		for key,val in ipairs(enum) do
			result = result..showIndent(1).."a"..key.." <- enumHelp (hideEnum \""..name.."\" \""..val.."\" idx) "..val.." (return a"..(key-1)..")\n"
		end
		result = result..showIndent(1).."return a"..#enum.."\n"
		result = result.."show"..name.." :: "..name.." -> IORef String -> IO ()\n"
		for key,val in ipairs(enum) do
			result = result.."show"..name.." "..val.." = showEnum \""..name.."\" \""..val.."\"\n"
		end
		result = result .. "show"..name.." "..name.."s = showEnum \""..name.."\" \""..name.."s\"\n"
	else
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
	end
	result = result.."--"
	return result
end
function showStructHsK(field)
	local result = ""
	local separate = " "
	for key,val in sortIter(field[3]) do
		result = result..separate..key..":"
		separate = ";"
		local sep = ""
		for ky,vl in sortIter(val) do
			result = result..sep..ky
			sep = ","
		end
	end
	return result
end
function showStructHsJ(name,pre,post)
	local result = ""
	if (name == "Chr") then
		result = result..pre.."Char"..post
	elseif (name == "Int") then
		result = result..pre.."Int"..post
	elseif (name == "Int32") then
		result = result..pre.."Int32"..post
	elseif (name == "New") then
		result = result..pre.."Integer"..post
	elseif (name == "Num") then
		result = result..pre.."Double"..post
	elseif (name == "Old") then
		result = result..pre.."Float"..post
	elseif (name == "Str") then
		result = result..pre.."String"..post
	elseif (name == "Dat") then
		result = result..pre.."Dat"..post
	else
		result = result..pre..name..post
	end
	return result
end
function showStructHsI(val)
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
	result = result..showStructHsJ(val[2],pre,post)
	return result
end
function showStructHsH(pre,field,mid,post)
	coroutine.yield(pre..showStructHsI(field)..mid.." -- "..field[1]..post)
end
function showStructHsG(pre,mid,second)
	local last = #second
	local post = ""
	if #second > 0 then 
		post = showStructCK(second[1])
	end
	if #second > 1 then
		local space = "    "
		if (mid == "") then
			coroutine.yield("data "..pre.." = "..pre.." --"..post)
		else
			coroutine.yield("    "..pre.." --"..post)
			space = space.."    "
		end
		for id,fd in ipairs(second) do
			if (id == last) then
				showStructHsH(space,fd,mid,"")
			else
				showStructHsH(space,fd,"","")
			end
		end
	else
		if (mid == "") then
			coroutine.yield("data "..pre.." =")
		end
		for id,fd in ipairs(second) do
			showStructHsH("    "..pre.." ",fd,mid," --"..post)
		end
	end
end     
function showStructHsF(name,struct,extra)
	local first = {}
	local second = {}
	local index = 1
	local outer = 1
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("-- 0")
			addTo(second,fld)
		elseif #first == 0 and seconder and sizeIter(second[1][3]) > 0 then
			-- coroutine.yield("-- 1")
			coroutine.yield("data "..name.."A"..index.." =")
			extra[#extra+1] = name.."A"..index
			outer = index
			showStructHsG(name.."A"..outer.."B"..index," |",second)
			coroutine.yield("    "..name.."A"..outer.."Bs deriving (Eq)")
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("-- 2")
			showStructHsG(name.."A"..index,"",second)
			extra[#extra+1] = name.."A"..index
			coroutine.yield("    deriving (Eq)")
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("-- 3")
			coroutine.yield("data "..name.."A"..index.." =")
			extra[#extra+1] = name.."A"..index
			outer = index
			showStructHsG(name.."A"..outer.."B"..index," |",second)
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		elseif #first > 0 and not firster then
			-- coroutine.yield("-- 4")
			showStructHsG(name.."A"..outer.."B"..index," |",second)
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("-- 5")
			showStructHsG(name.."A"..outer.."B"..index," |",second)
			coroutine.yield("    "..name.."A"..outer.."Bs deriving (Eq)")
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		end
	end
	if #first == 0 and sizeIter(second[1][3]) > 0 then
		-- coroutine.yield("-- 6a")
		coroutine.yield("data "..name.."A"..index.." =")
		extra[#extra+1] = name.."A"..index
		outer = index
		showStructHsG(name.."A"..outer.."B"..index," |",second)
		coroutine.yield("    "..name.."A"..outer.."Bs deriving (Eq)")
	elseif #first == 0 then
		-- coroutine.yield("-- 6b")
		showStructHsG(name.."A"..index,"",second)
		extra[#extra+1] = name.."A"..index
		coroutine.yield("    deriving (Eq)")
	else
		-- coroutine.yield("-- 6c")
		showStructHsG(name.."A"..outer.."B"..index," |",second)
		coroutine.yield("    "..name.."A"..outer.."Bs deriving (Eq)")
	end
end
function showStructHs(name,struct)
	local result = ""
	local extra = {}
	local wrap = coroutine.wrap(showStructHsF)
	local line = wrap(name,struct,extra)
	while line do
		result = result..line.."\n"
		line = wrap()
	end
	result = result.."data "..name.." = "..name.."\n"
	for idx,val in ipairs(extra) do
		result = result.."    "..val.."\n"
	end
	result = result.."    deriving (Eq)\n"
	return result.."--"
end
function howTreeHsG(base,index,type,second)
	for id,fd in ipairs(second) do
		coroutine.yield(base+id-1,index,type)
	end	
end
function howTreeHsF(name,struct)
	local first = {}
	local second = {}
	local index = 1
	local outer = 1
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("-- 0")
			addTo(second,fld)
		elseif #first == 0 and seconder and sizeIter(second[1][3]) > 0 then
			-- coroutine.yield("-- 1")
			outer = index
			howTreeHsG(index,outer,1,second)
			howTreeHsG(index,index,0,second)
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("-- 2")
			howTreeHsG(index,index,0,second)
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("-- 3")
			outer = index
			howTreeHsG(index,outer,1,second)
			howTreeHsG(index,index,0,second)
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		elseif not firster then
			-- coroutine.yield("-- 4")
			howTreeHsG(index,outer,1,second)
			howTreeHsG(index,index,0,second)
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("-- 5")
			howTreeHsG(index,outer,1,second)
			howTreeHsG(index,index,0,second)
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		end
	end
	if #first == 0 and sizeIter(second[1][3]) > 0 then
		-- coroutine.yield("-- 6a")
		outer = index
		howTreeHsG(index,outer,1,second)
		howTreeHsG(index,index,0,second)
	elseif #first == 0 then
		-- coroutine.yield("-- 6b")
		howTreeHsG(index,index,0,second)
	else
		-- coroutine.yield("-- 6c")
		howTreeHsG(index,outer,1,second)
		howTreeHsG(index,index,0,second)
	end
end
function howEqualHs(struct)
	local result = {}
	local wrap = coroutine.wrap(howTreeHsF)
	local key,val,typ = wrap(name,struct,extra)
	while key do
		if (typ == 0) then
			result[key] = val
		end
		key,val,typ = wrap()
	end
	return result
end
function howDisjointHs(struct)
	local result = {}
	local wrap = coroutine.wrap(howTreeHsF)
	local key,val,typ = wrap(name,struct,extra)
	while key do
		if (typ == 1) then
			result[key] = val
		end
		key,val,typ = wrap()
	end
	return result
end
function showAccessHsF(index,idx,max,list)
	local ed = (list[idx] ~= nil)
	local er = (list[idx] ~= nil) and (list[idx] == idx)
	local ee = (list[idx] ~= nil) and ((idx == max) or (list[idx] ~= nil and list[idx] ~= list[idx+1]))
	local ea = (list[idx] ~= nil) and (list[index] == list[idx])
	return ed,er,ee,ea
end
function sb(val)
	if val then return "1" else return "0" end
end
function showAccessHsG(name,struct,var,index,equal,disjoint)
	local result = ""
	local comment = ""
	for idx,fld in ipairs(struct) do
		local eed,eer,eee,eea = showAccessHsF(index,idx,#struct,equal)
		local ded,der,dee,dea = showAccessHsF(index,idx,#struct,disjoint)
		if (eer and not ded and eea) then
			-- comment = comment.." "..idx.."->1"
			result = result.." ".."("..name.."A"..idx
		end
		if (eer and ded and eea) then
			-- comment = comment.." "..idx.."->2"
			result = result.." ".."("..name.."A"..disjoint[idx].."B"..idx
		end
		if (eea and (index ~= idx)) then
			-- comment = comment.." "..idx.."->3"
			result = result.." ".."a"..idx
		end
		if (index == idx) then
			-- comment = comment.." "..idx.."->4"
			result = result.." "..var
		end
		if (eer and not ded and not eea) then
			-- comment = comment.." "..idx.."->5"
			result = result.." ".."a"..idx
		end
		if (der and not dea) then
			-- comment = comment.." "..idx.."->6"
			result = result.." ".."a"..idx
		end
		if (eee and eea) then
			-- comment = comment.." "..idx.."->7"
			result = result..")"
		end
	end
	return result,comment
end
function showAccessHs(name,struct)
	local equal = howEqualHs(struct)
	local disjoint = howDisjointHs(struct)
	local result = ""
	local comment = ""
	for k,v in ipairs(struct) do
		result = result.."get"..name.."C"..v[1].." :: "
		result = result..name.." -> "..showStructHsI(v).."\n"
		result = result.."get"..name.."C"..v[1].." ("..name
		tempr,tempc = showAccessHsG(name,struct,"a",k,equal,disjoint)
		result = result..tempr
		comment = comment..tempc
		if comment ~= "" then comment = " --1"..comment end
		result = result..") = a"..comment.."\n"
		comment = ""
	end
	for k,v in ipairs(struct) do
		result = result.."set"..name.."C"..v[1].." :: "
		result = result..name.." -> "..showStructHsI(v).." -> "..name.."\n"
		result = result.."set"..name.."C"..v[1].." ("..name
		tempr,tempc = showAccessHsG(name,struct,"_",k,equal,disjoint)
		result = result..tempr
		comment = comment..tempc
		if comment ~= "" then comment = " --2"..comment end
		result = result..") a = ("..name
		tempr,tempc = showAccessHsG(name,struct,"a",k,equal,disjoint)
		result = result..tempr
		comment = comment..tempc
		if comment ~= "" then comment = " --3"..comment end
		result = result..")"..comment.."\n"
		comment = ""
	end
	result = result.."--"
	return result
end
function showHelpHs()
	local result = ""
	if showhide then
		result = result.."fieldHelp :: IO (Maybe a) -> IO (Maybe b) -> IO (Maybe b)\n"
		result = result.."fieldHelp a b = do\n"
		result = result..showIndent(1).."ax <- a\n"
		result = result..showIndent(1).."bx <- b\n"
		result = result..showIndent(1).."return (ax >> bx)\n"
		result = result.."hideHelp :: Maybe Int -> IO (Maybe a) -> IO (Maybe [a])\n"
		result = result.."hideHelp Nothing _ = return Nothing\n"
		result = result.."hideHelp (Just 0) b = return (Just [])\n"
		result = result.."hideHelp (Just a) b = do\n"
		result = result..showIndent(1).."d <- b\n"
		result = result..showIndent(1).."e <- hideHelp (Just (a-1)) b\n"
		result = result..showIndent(1).."return (Just (:) <*> d <*> e)\n"
		result = result.."enumHelp :: IO (Maybe Bool) -> a -> IO (Maybe a) -> IO (Maybe a)\n"
		result = result.."enumHelp a b c = do\n"
		result = result..showIndent(1).."x <- a\n"
		result = result..showIndent(1).."y <- c\n"
		result = result..showIndent(1).."case (x,y) of\n"
		result = result..showIndent(2).."(Nothing,_) -> c\n"
		result = result..showIndent(2).."(_,Nothing) -> return (Just b)\n"
		result = result..showIndent(2).."(_,Just _) -> c\n"
		result = result.."showHelp :: Int -> Int -> ([Int] -> a -> IO ()) -> [Int] -> [a] -> IO ()\n"
		result = result.."showHelp a b _ _ []\n"
		result = result..showIndent(1).."| a == b = return ()\n"
		result = result..showIndent(1).."| otherwise = undefined\n"
		result = result.."showHelp a b f c (d:e)\n"
		result = result..showIndent(1).."| a > b = (f (b:c) d) >> (showHelp a (b+1) f c e)\n"
		result = result..showIndent(1).."| otherwise = undefined\n"
	else
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
	end
	result = result.."--"
	return result
end
function showCondHsF(val)
	if showhide == "hide" then
		return "(Just "..val..")"
	else
		return val
	end
end
function showCondHs(struct,dset)
	local cond = ""
	local terms = 0
	for i,dim in ipairs(listSort(dset)) do
		local set = dset[dim]
		local term = ""
		local factors = 0
		for idx,fld in ipairs(struct) do
			-- coroutine.yield("-- "..idx.." : "..fld[1].." == "..dim)
			if (fld[1] == dim) then
				for j,val in ipairs(listSort(set)) do
					local wrap = showCondHsF(val)
					local factor = "a"..idx.." == "..wrap
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
		cond = "True"
	elseif (terms > 1) then
		cond = "("..cond..")"
	end
	return cond
end
function showReadHsK(fld,lst)
	if showhide then
		result = "hideField \""..fld.."\" ["
		for k,v in ipairs(lst) do
			if k > 1 then
				result = result..","
			end
			result = result..v
		end
		return result.."] idx `fieldHelp` hide"

	else
		return "read"
	end
end
function showReadHsJ(pre,val,post)
	if showhide then
		return "hideHelp "..pre..val..post
	else
		return "listHelp "..val
	end
end
function showReadHsI(name,args,before)
	local result = ""
	result = result.."return ("
	if showhide then
		for k,v in ipairs(before) do
			result = result..v.." >> "
		end
		result = result.."Just "..name
		for k,v in ipairs(args) do
			result = result.." <*> "..v
		end
	else
		result = result..name
		for k,v in ipairs(args) do
			result = result.." "..v
		end
	end
	result = result..")"
	return result
end
function showReadHsH(pre,index,struct,field)
	local result = ""
	local count = 0
	local list = {}
	if (type(field[4]) == "table") then
		while (count < #field[4]) do
			count = count + 1
			result = result..showReadHsJ("(Just ",field[4][count],")").." ("
			list[#list+1] = "(Just "..field[4][count]..")"
		end
	end
	if (type(field[4]) == "string") then
		local found = "0"
		for k,v in ipairs(struct) do
			if (v[1] == field[4]) then found = "a"..k end
		end
		result = result..showReadHsJ("",found,"").." ("
		count = count + 1
		list[#list+1] = found
	end
	if (type(field[4]) == "number") then
		result = result..showReadHsJ("(Just ",field[4],")").." ("
		count = count + 1
		list[#list+1] = "(Just "..field[4]..")"
	end
	result = result..showReadHsK(field[1],list)..field[2].." idx"
	while (count > 0) do
		result = result..")"
		count = count - 1
	end
	coroutine.yield("    "..pre.."a"..index.." <- "..result)
end
function showReadHsG(pre,index,struct,second)
	for id,fd in ipairs(second) do
		showReadHsH(pre,index+id-1,struct,fd)
	end
end
function showReadHsF(name,struct,extra)
	local first = {}
	local second = {}
	local list = {}
	local index = 1
	local outer = 1
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("-- 0")
			addTo(second,fld)
		elseif #first == 0 and seconder and sizeIter(second[1][3]) > 0 then
			-- coroutine.yield("-- 1")
			extra[#extra+1] = "b"..index.."x"
			outer = index
			coroutine.yield("    b"..index.."x <- condHelp "..showCondHs(struct,struct[index][3]).." "..showCondHsF(name.."A"..outer.."Bs").." (do")
			showReadHsG("    ",index,struct,second)
			local args = {}; for id=index,(idx-1) do args[#args+1] = "a"..id end
			coroutine.yield("        "..showReadHsI(name.."A"..outer.."B"..index,args,{})..")")
			list = {}
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("-- 2")
			showReadHsG("",index,struct,second)
			local args = {}; for id=index,(idx-1) do args[#args+1] = "a"..id end
			coroutine.yield("    a"..index.."x <- "..showReadHsI(name.."A"..index,args,{}))
			extra[#extra+1] = "a"..index.."x"
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("-- 3")
			extra[#extra+1] = "b"..index.."x"
			outer = index
			coroutine.yield("    b"..index.."x <- condHelp "..showCondHs(struct,struct[index][3]).." "..showCondHsF(name.."A"..outer.."Bs").." (do")
			list[#list+1] = "b"..index.."x"
			showReadHsG("    ",index,struct,second)
			local args = {}; for id=index,(idx-1) do args[#args+1] = "a"..id end
			coroutine.yield("        "..showReadHsI(name.."A"..outer.."B"..index,args,{})..")")
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		elseif not firster then
			-- coroutine.yield("-- 4")
			coroutine.yield("    b"..index.."x <- condHelp "..showCondHs(struct,struct[index][3]).." "..showCondHsF(name.."A"..outer.."Bs").." (do")
			list[#list+1] = "b"..index.."x"
			showReadHsG("    ",index,struct,second)
			local args = {}; for id=index,(idx-1) do args[#args+1] = "a"..id end
			coroutine.yield("        "..showReadHsI(name.."A"..outer.."B"..index,args,{})..")")
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("-- 5")
			coroutine.yield("    b"..index.."x <- condHelp "..showCondHs(struct,struct[index][3]).." "..showCondHsF(name.."A"..outer.."Bs").." (do")
			list[#list+1] = "b"..index.."x"
			showReadHsG("    ",index,struct,second)
			local args = {}; for id=index,(idx-1) do args[#args+1] = "a"..id end
			coroutine.yield("        "..showReadHsI(name.."A"..outer.."B"..index,args,{})..")")
			local sep,str = "",""; for id,vl in ipairs(list) do str = str..sep..vl; sep = "," end
			coroutine.yield("    a"..outer.."x <- firstHelp "..showCondHsF(name.."A"..outer.."Bs").." ["..str.."]")
			list = {}
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		end
	end
	if #first == 0 and sizeIter(second[1][3]) > 0 then
		-- coroutine.yield("-- 6a")
		extra[#extra+1] = "b"..index.."x"
		outer = index
		coroutine.yield("    b"..index.."x <- condHelp "..showCondHs(struct,struct[index][3]).." "..showCondHsF(name.."A"..outer.."Bs").." (do")
		list[#list+1] = "b"..index.."x"
		showReadHsG("    ",index,struct,second)
		local args = {}; for id=index,#struct do args[#args+1] = "a"..id end
		coroutine.yield("        "..showReadHsI(name.."A"..outer.."B"..index,args,{})..")")
		local sep,str = "",""; for id,vl in ipairs(list) do str = str..sep..vl; sep = "," end
		coroutine.yield("    a"..outer.."x <- firstHelp "..showCondHsF(name.."A"..outer.."Bs").." ["..str.."]")
	elseif #first == 0 then
		-- coroutine.yield("-- 6b")
		showReadHsG("",index,struct,second)
		local args = {}; for id=index,#struct do args[#args+1] = "a"..id end
		coroutine.yield("    a"..index.."x <- "..showReadHsI(name.."A"..index,args,{}))
		extra[#extra+1] = "a"..index.."x"
	else
		-- coroutine.yield("-- 6c")
		coroutine.yield("    b"..index.."x <- condHelp "..showCondHs(struct,struct[index][3]).." "..showCondHsF(name.."A"..outer.."Bs").." (do")
		list[#list+1] = "b"..index.."x"
		showReadHsG("    ",index,struct,second)
		local args = {}; for id=index,#struct do args[#args+1] = "a"..id end
		coroutine.yield("        "..showReadHsI(name.."A"..outer.."B"..index,args,{})..")")
		local sep,str = "",""; for id,vl in ipairs(list) do str = str..sep..vl; sep = "," end
		coroutine.yield("    a"..outer.."x <- firstHelp "..showCondHsF(name.."A"..outer.."Bs").." ["..str.."]")
	end
end
function showReadHsE(name,struct,extra)
	local result = ""
	local wrap = coroutine.wrap(showReadHsF)
	local line = wrap(name,struct,extra)
	while line do
		result = result..line.."\n"
		line = wrap()
	end
	return result
end
function showReadHs(name,struct)
	local result = ""
	local extra = {}
	if showhide then
		result = result.."hide"..name.." :: RefStr -> IO (Maybe "..name..")\n"
		result = result.."hide"..name.." idx = do\n"
		result = result.."    x <- hideOpen \""..name.."\" idx\n"
	else
		result = result.."read"..name.." :: Int -> IO "..name.."\n"
		result = result.."read"..name.." idx = do\n"
	end
	result = result..showReadHsE(name,struct,extra)
	if showhide then
		result = result.."    y <- hideClose idx\n"
	end
	result = result.."    "..showReadHsI(name,extra,{"x","y"})
	return result
end
function showWriteHsK(pre,post,index,second)
	for id,fd in ipairs(second) do
		if (id == 1 and id == #second) then
			coroutine.yield(pre.."a"..(index+id-1)..post)
		elseif (id == 1) then
			coroutine.yield(pre.."a"..(index+id-1))
		elseif (id == #second) then
			coroutine.yield("a"..(index+id-1)..post)
		else
			coroutine.yield("a"..(index+id-1))
		end
	end
end
function showWriteHsJ(name,struct)
	local first = {}
	local second = {}
	local index = 1
	local outer = 1
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("-- 0")
			addTo(second,fld)
		elseif #first == 0 and seconder and sizeIter(second[1][3]) > 0 then
			-- coroutine.yield("-- 1")
			coroutine.yield("a"..index)
			outer = index
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("-- 2")
			showWriteHsK("("..name.."A"..index.." ",")",index,second)
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("-- 3")
			coroutine.yield("a"..index)
			outer = index
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		elseif not firster then
			-- coroutine.yield("-- 4")
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("-- 5")
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		end
	end
	if #first == 0 and sizeIter(second[1][3]) > 0 then
		-- coroutine.yield("-- 6a")
		coroutine.yield("a"..index)
	elseif #first == 0 then
		-- coroutine.yield("-- 6b")
		showWriteHsK("("..name.."A"..index.." ",")",index,second)
	else
		-- coroutine.yield("-- 6c")
	end
end
function showWriteHsI(name,struct)
	local result = ""
	local wrap = coroutine.wrap(showWriteHsJ)
	local word = wrap(name,struct)
	while word do
		result = result.." "..word
		word = wrap()
	end
	return result
end
function showWriteHsH(index,struct,field)
	local result = ""
	local count = 0
	local post = ""
	local pvar = "a"..index
	local qvar = pvar
	local sub = "[]"
	if (type(field[4]) == "table") and (#field[4] > 0) then
		while (count < #field[4]) do
			count = count + 1
			if showhide then
				result = result.."showHelp "..field[4][count].." 0 ("
			else
				result = result.."assertHelp "..field[4][count].." ("
			end
		end
		if showhide then
			result = result.."\\x y -> "
			post = post.." []"
			sub = "x"
			qvar = "y"
		else
			result = result.."\\x -> "
			qvar = "x"
		end
		post = post.." "..pvar
	end
	if (type(field[4]) == "string") then
		local found = "0"
		for k,v in ipairs(struct) do
			if (v[1] == field[4]) then found = "a"..k end
		end
		if showhide then
			result = result.."showHelp "..found.." 0 (\\x y -> "
			post = post.." []"
			sub = "x"
			qvar = "y"
		else
			result = result.."assertHelp "..found.." (\\x -> "
			qvar = "x"
		end
		count = count + 1
		post = post.." "..pvar
	end
	if (type(field[4]) == "number") then
		if showhide then
			result = result.."showHelp "..field[4].." 0 (\\x y -> "
			post = post.." []"
			sub = "x"
			qvar = "y"
		else
			result = result.."assertHelp "..field[4].." (\\x -> "
			qvar = "x"
		end
		count = count + 1
		post = post.." "..pvar
	end
	if showhide then
		result = result.."(showField \""..field[1].."\" "..sub.." idx) >> show"..field[2].." "..qvar.." idx"
	else
		result = result.."write"..field[2].." "..qvar.." idx"
	end
	while (count > 0) do
		result = result..")"
		count = count - 1
	end
	return result..post
end
function showWriteHsG(pre,index,outer,limit,name,struct,second)
	if (pre == "") then
		for id,fd in ipairs(second) do
			coroutine.yield("    "..showWriteHsH(index+id-1,struct,fd))
		end
	elseif (#second == 1) then
		local str = ""; for id=index,(limit-1) do str = str.." a"..id end
		for id,fd in ipairs(second) do
			coroutine.yield("    condHelp "..showCondHs(struct,struct[index][3]).." () ((\\("..name.."A"..outer.."B"..index..str..") -> "..showWriteHsH(index+id-1,struct,fd)..") a"..outer..")")
		end
	else
		local str = ""; for id=index,(limit-1) do str = str.." a"..id end
		coroutine.yield("    condHelp "..showCondHs(struct,struct[index][3]).." () ((\\("..name.."A"..outer.."B"..index..str..") -> do")
		for id,fd in ipairs(second) do
			if (id == #second) then
				coroutine.yield("    "..pre..showWriteHsH(index+id-1,struct,fd)..") a"..outer..")")
			else
				coroutine.yield("    "..pre..showWriteHsH(index+id-1,struct,fd))
			end
		end
	end
end
function showWriteHsF(name,struct)
	local first = {}
	local second = {}
	local index = 1
	local outer = 1
	for idx,fld in ipairs(struct) do
		local firster = overAny(first,fld)
		local seconder = overAny(second,fld)
		local secondee = equalAll(second,fld)
		if #second == 0 or secondee then
			-- coroutine.yield("-- 0")
			addTo(second,fld)
		elseif #first == 0 and seconder and sizeIter(second[1][3]) > 0 then
			-- coroutine.yield("-- 1")
			outer = index
			showWriteHsG("    ",index,outer,idx,name,struct,second)
			list = {}
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 and seconder then
			-- coroutine.yield("-- 2")
			showWriteHsG("",index,outer,idx,name,struct,second)
			second = {}
			index = idx
			addTo(second,fld)
		elseif #first == 0 then
			-- coroutine.yield("-- 3")
			outer = index
			showWriteHsG("    ",index,outer,idx,name,struct,second)
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		elseif not firster then
			-- coroutine.yield("-- 4")
			showWriteHsG("    ",index,outer,idx,name,struct,second)
			second = {}
			index = idx
			addTo(first,fld)
			addTo(second,fld)
		else
			-- coroutine.yield("-- 5")
			showWriteHsG("    ",index,outer,idx,name,struct,second)
			list = {}
			first = {}
			second = {}
			index = idx
			addTo(second,fld)
		end
	end
	if #first == 0 and sizeIter(second[1][3]) > 0 then
		-- coroutine.yield("-- 6a")
		outer = index
		showWriteHsG("    ",index,outer,#struct+1,name,struct,second)
	elseif #first == 0 then
		-- coroutine.yield("-- 6b")
		showWriteHsG("",index,outer,#struct+1,name,struct,second)
	else
		-- coroutine.yield("-- 6c")
		showWriteHsG("    ",index,outer,#struct+1,name,struct,second)
	end
end
function showWriteHsE(name,struct)
	local result = ""
	local wrap = coroutine.wrap(showWriteHsF)
	local line = wrap(name,struct)
	while line do
		result = result..line.."\n"
		line = wrap()
	end
	return result
end
function showWriteHs(name,struct)
	local result = ""
	local expand = showWriteHsI(name,struct)
	if showhide then
		result = result.."show"..name.." :: "..name.." -> IORef String -> IO ()\n"
		result = result.."show"..name.." ("..name..expand..") idx = do\n"
		result = result.."    showOpen \""..name.."\" idx\n"
	else
		result = result.."write"..name.." :: "..name.." -> Int -> IO ()\n"
		result = result.."write"..name.." ("..name..expand..") idx = do\n"
	end
	result = result..showWriteHsE(name,struct)
	if showhide then
		result = result.."    showClose idx\n"
	end
	result = result.."--"
	return result
end
function showCastLua(name,list)
	local result = ""
	result = result.."function cast"..name.."(val)\n"
	for key,val in ipairs(list) do
		if (key == 1) then
			result = result..showIndent(1).."if (val == \""..val.."\") then return "..(key-1).."\n"
		else
			result = result..showIndent(1).."elseif (val == \""..val.."\") then return "..(key-1).."\n"
		end
	end
	result = result..showIndent(1).."else return nil end\n"
	result = result.."end"
	return result
end
function showIdentLua(list)
	local result = ""
	result = result.."function castStruct(val)\n"
	for key,val in ipairs(list) do
		if (key == 1) then
			result = result..showIndent(1).."if (val == \""..val.."\") then return "..(key-1).."\n"
		else
			result = result..showIndent(1).."elseif (val == \""..val.."\") then return "..(key-1).."\n"
		end
	end
	result = result..showIndent(1).."else return nil end\n"
	result = result.."end"
	return result
end
function showFieldLua(name,list)
	local result = ""
	result = result.."function cast"..name.."(val)\n"
	for key,val in ipairs(list) do
		if (key == 1) then
			result = result..showIndent(1).."if (val == \""..val[1].."\") then return "..(key-1).."\n"
		else
			result = result..showIndent(1).."elseif (val == \""..val[1].."\") then return "..(key-1).."\n"
		end
	end
	result = result..showIndent(1).."else return nil end\n"
	result = result.."end"
	return result
end
function showCodeLua(name,enum)
	local result = ""
	if showhide then
		result = result.."function hide"..name.."(str)\n"
		for key,val in ipairs(enum) do
			result = result..showIndent(1).."val,str = hideEnum(\""..name.."\",\""..val.."\",str); if not (val == nil) then return \""..val.."\",str end\n"
		end
		result = result..showIndent(1).."return nil,str\n"
		result = result.."end\n"
		result = result.."function show"..name.."(val,str)\n"
		for key,val in ipairs(enum) do
			result = result..showIndent(1).."if (val == \""..val.."\") then str = showEnum(\""..name.."\",\""..val.."\",str) end\n"
		end
		result = result..showIndent(1).."return str\n"
		result = result.."end\n"
		result = result.."--"
		return result
	end
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
	result = result..showIndent(1).."else writeInt("..(#enum)..",idx) end\n"
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
		cond = "True"
	elseif (terms > 1) then
		cond = "("..cond..")"
	end
	return cond
end
nest = 0
function showReadLua(name,struct)
	nest = 0
	local result = ""
	if showhide then
		result = result.."function".." hide"..name.."(str)\n"
		result = result..showIndent(1).."vld,str = hideOpen(str,\""..name.."\"); if (vld == nil) then return nil,str end\n"
	else
		result = result.."function".." read"..name.."(idx)\n"
	end
	result = result..showIndent(1).."local tab = {}\n"
	for index,field in ipairs(struct) do
		local count = 0
		local conds = 0
		local cond = showCondLua(struct,field[3])
		if (cond ~= "True") then
			count = count + 1
			conds = conds + 1
			result = result..showIndent(count).."if "..cond.." ".."then".."\n"
		end
		local list = ""
		local sub = ""
		local sup = ""
		local super = false
		if (type(field[4]) == "table") then
			local squares = 1
			local square = 1
			local limit = count + #field[4]
			local dimen = 0
			while (count < limit) do
				count = count + 1
				dimen = dimen + 1
				result = result..showIndent(count).."tab[\""..field[1].."\"]"
				local index = square
				while (index < squares) do
					result = result.."[i"..index.."]"
					index = index + 1
				end
				result = result.." = {}".."\n"
				squares = squares + 1
				result = result..showIndent(count).."local".." i"..count.." = 1\n"
				result = result..showIndent(count).."while (i"..count.." <= "..field[4][dimen]..") do\n"
				sub = sub.."[i"..count.."]"
				list = list..",".."i"..count
				if (count < limit) then sup = sup.."[i"..count.."]" end
				super = true
			end
		end
		if (type(field[4]) == "string") then
			local found = "0"
			for k,v in ipairs(struct) do
				if (v[1] == field[4]) then found = "a"..k end
			end
			count = count + 1
			result = result..showIndent(count).."tab[\""..field[1].."\"] = {}".."\n"
			result = result..showIndent(count).."local".." i"..count.." = 1\n"
			result = result..showIndent(count).."while (i"..count.." <= tab[\""..field[4].."\"]) do\n"
			sub = sub.."[i"..count.."]"
			list = list..",".."i"..count
			super = true
		end
		if (type(field[4]) == "number") then
			count = count + 1
			result = result..showIndent(count).."tab".."[\""..field[1].."\"] = {}\n"
			result = result..showIndent(count).."local".." i"..count.." = ".."1\n"
			result = result..showIndent(count).."while (i"..count.." <= "..field[4]..") do\n"
			sub = sub.."[i"..count.."]"
			list = list..",".."i"..count
			super = true
		end
		if showhide then
			result = result..showIndent(count+1).."vld,str = hideField(str,\""..field[1].."\""..list.."); if (vld == nil) then return nil,str end; tab".."[\""..field[1].."\"]"..sub..", str = hide"..field[2].."(str); if (tab".."[\""..field[1].."\"]"..sub.." == nil) then return nil,str end\n"
		else
			result = result..showIndent(count+1).."tab".."[\""..field[1].."\"]"..sub.." = ".."read"..field[2].."(idx)\n"
		end
		if (cond ~= "True") then nest = 1 end
		while (count > 0) do
			if (count > conds) then
				result = result..showIndent(count+1).."i"..count.." = i"..count.." + 1\n"
			end
			result = result..showIndent(count).."end\n"
			count = count - 1
		end
	end
	if showhide then
		result = result..showIndent(1).."vld,str = hideClose(str); if (vld == nil) then return nil,str end\n"
		result = result..showIndent(1).."return tab,str\n"
	else
		result = result..showIndent(1).."return tab\n"
	end
	result = result.."end\n"
	result = result.."--"
	return result
end
function showWriteLua(name,struct)
	nest = 0
	local result = ""
	if showhide then
		result = result.."function show"..name.."(tab,str)\n"
		result = result..showIndent(1).."str = showOpen(\""..name.."\")\n"
	else
		result = result.."function write"..name.."(tab,idx)\n"
	end
	for index,field in ipairs(struct) do
		local count = 0
		local conds = 0
		local cond = showCondLua(struct,field[3])
		if (cond ~= "True") then
			count = count + 1
			conds = conds + 1
			result = result..showIndent(count).."if "..cond.." ".."then\n"
		end
		local sub = ""
		local arg = ""
		if (type(field[4]) == "table") then
			while (count < #field[4]) do
				count = count + 1
				result = result..showIndent(count).."local".." i"..count.." = ".."1\n"
				result = result..showIndent(count).."while (i"..count.." <= "..field[4][count]..") do\n"
				sub = sub.."[i"..count.."]"
				arg = arg..",i"..count
			end
		end
		if (type(field[4]) == "string") then
			local found = "0"
			for k,v in ipairs(struct) do
				if (v[1] == field[4]) then found = "a"..k end
			end
			count = count + 1
			result = result..showIndent(count).."local".." i"..count.." = 1\n"
			result = result..showIndent(count).."while (i"..count.." <= ".."tab[\""..field[4].."\"]) do\n"
			sub = sub.."[i"..count.."]"
			arg = arg..",i"..count
		end
		if (type(field[4]) == "number") then
			count = count + 1
			result = result..showIndent(count).."local".." i"..count.." = ".."1".."\n"
			result = result..showIndent(count).."while (i"..count.." <= "..field[4]..") do\n"
			sub = sub.."[i"..count.."]"
			arg = arg..",i"..count
		end
		local value = "tab[\""..field[1].."\"]"..sub
		result = result..showIndent(count+1)
		if showhide then
			result = result.."str = str..showField(\""..field[1].."\""..arg..")..show"..field[2].."("..value..")\n"
		else
			result = result.."write"..field[2].."("..value..",idx)\n"
		end
		if (cond ~= "True") then nest = 1 end
		while (count > 0) do
			if (count > conds) then
				result = result..showIndent(count+1).."i"..count.." = i"..count.." + 1\n"
			end
			result = result..showIndent(count).."end\n"
			count = count - 1
		end
	end
	if showhide then
		result = result..showIndent(1).."str = str..showClose()\n"
		result = result..showIndent(1).."return str\n"
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
		elseif (string.find(line,"^--HERE")) then clear = true end
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
function genericEnum(structz,name)
	local enum = {}
	for k,v in pairs(structz) do enum[#enum+1] = k..name end
	return enum
end
function genericStruct(structz,name)
	local struct = {}
	struct[#struct+1] = {"tag",name,{},{}}
	for k,v in pairs(structz) do struct[#struct+1] = {string.lower(k),k,{["tag"]={[k..name]=true}},{}} end
	return struct
end
function showCall(list,map,func)
	local result = ""
	for k,v in ipairs(list) do
		if result ~= "" then result = result.."\n" end
		result = result..func(v,map[v])
	end
	return result
end
function showFuncC()
	local result = ""
	result = result..showCall(Enums,Enumz,showAllocC).."\n"
	result = result..showCall(Enums,Enumz,showShowEC).."\n"
	result = result..showCall(Enums,Enumz,showHideEC).."\n"
	for k,v in ipairs(Enums) do luaxSide(showCastLua(v,Enumz[v])) end
	result = result..showCall(Constants,Constantz,showConstantC).."\n"
	result = result..showCall(Structs,Structz,showFreeC).."\n"
	result = result..showCall(Structs,Structz,showAllocC).."\n"
	result = result..showCall(Structs,Structz,showReadC).."\n"
	result = result..showCall(Structs,Structz,showWriteC).."\n"
	result = result..showCall(Structs,Structz,showRandC).."\n"
	result = result..showCall(Structs,Structz,showCompC).."\n"
	result = result..showCall(Structs,Structz,showSizeC).."\n"
	result = result..showCall(Structs,Structz,showShowSC).."\n"
	result = result..showCall(Structs,Structz,showHideSC).."\n"
	result = result..showRtypeC(listFlatten({{"Chr","Int","Int32","New","Num","Old","Str","Dat"},Enums,Structs})).."\n"
	result = result..showWtypeC(listFlatten({{"Chr","Int","Int32","New","Num","Old","Str","Dat"},Enums,Structs})).."\n"
	result = result..showLtypeC(listFlatten({{"Chr","Int","Int32","New","Num","Old","Str","Dat"},Enums,Structs})).."\n"
	result = result..showIdentC(listFlatten({{"Chr","Int","Int32","New","Num","Old","Str","Dat"},Enums,Structs})).."\n"
	result = result..showRfieldC(Structs,Structz).."\n"
	result = result..showWfieldC(Structs,Structz).."\n"
	result = result..showIfieldC(Structs,Structz).."\n"
	result = result..showTfieldC(Structs,Structz)
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
	showhide = false
	result = result..showCall(Enums,Enumz,showCodeHs).."\n"
	showhide = true
	result = result..showCall(Enums,Enumz,showCodeHs).."\n"
	result = result..showCall(Structs,Structz,showStructHs).."\n"
	result = result..showCall(Structs,Structz,showAccessHs).."\n"
	showhide = false
	result = result..showHelpHs().."\n"
	showhide = true
	result = result..showHelpHs().."\n"
	showhide = false
	result = result..showCall(Structs,Structz,showReadHs).."\n"
	result = result..showCall(Structs,Structz,showWriteHs).."\n"
	showhide = "hide"
	result = result..showCall(Structs,Structz,showReadHs).."\n"
	showhide = "show"
	result = result..showCall(Structs,Structz,showWriteHs).."\n"
	return result
end
function showCallLua()
	local result = ""
	showhide = false
	result = result..showCall(Enums,Enumz,showCodeLua).."\n"
	showhide = true
	result = result..showCall(Enums,Enumz,showCodeLua).."\n"
	result = result..showCall(Enums,Enumz,showCastLua).."\n"
	result = result..showIdentLua(Structs).."\n"
	result = result..showCall(Structs,Structz,showFieldLua).."\n"
	showhide = false
	result = result..showCall(Structs,Structz,showReadLua).."\n"
	result = result..showCall(Structs,Structz,showWriteLua).."\n"
	showhide = "hide"
	result = result..showCall(Structs,Structz,showReadLua).."\n"
	showhide = "show"
	result = result..showCall(Structs,Structz,showWriteLua)
	return result
end
