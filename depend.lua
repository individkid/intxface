--[[
*	depend.lua
*	Copyright (C) 2019  Paul Coelho
*
*	This program is free software: you can redistribute it and/or modify
*	it under the terms of the GNU General Public License as published by
*	the Free Software Foundation, either version 3 of the License, or
*	(at your option) any later version.
*
*	This program is distributed in the hope that it will be useful,
*	but WITHOUT ANY WARRANTY; without even the implied warranty of
*	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*	GNU General Public License for more details.
*
*	You should have received a copy of the GNU General Public License
*	along with this program.  If not, see <http://www.gnu.org/licenses/>.
--]]

--[[
Theory:
Files are buildable or not.

Adding dependies to a makefile does not add build rules.
So, if a file is buildable in the presence of some set of files,
then it is buildable by a makefile without dependencies.

If a makefile has wildcards, the wildcards must also be after the colons.
So, if a file can be built, its name is known from makefile and listing.

Thus, we can build all buildable files,
by cumulatively attempting to build all that can be named.

A file's dependencies are deducible if it and its dependencies exist.
--]]
function debug(map)
	for k,v in pairs(map) do
		io.stderr:write(k..":")
		for key,val in pairs(v) do
			io.stderr:write(" "..key)
		end
		io.stderr:write("\n")
	end
end
function boolStr(val)
	if val then
		return "true"
	end
	return "false"
end
function depend(dep)
	pat = string.match(dep,"(.*)C%.o"); if pat then return pat.."C" end
	pat = string.match(dep,"(.*)%.hs"); if pat then return pat.."Hs" end
	pat = string.match(dep,"(.*)%.lua"); if pat then return pat.."Lua" end
	pat = string.match(dep,"(.*)Sw%.o"); if pat then return pat.."Sw" end
	pat = string.match(dep,"(.*)C%.o"); if pat then return pat..".so" end
	pat = string.match(dep,"(.*)G%.o"); if pat then return pat..".so" end
	pat = string.match(dep,"(.*)%.c"); if pat then return pat.."C.o" end
	pat = string.match(dep,"(.*)%.m"); if pat then return pat.."C.o" end
	pat = string.match(dep,"(.*)%.cpp"); if pat then return pat.."C.o" end
	pat = string.match(dep,"(.*)%.sw"); if pat then return pat.."Sw.o" end
	pat = string.match(dep,"(.*)%.g"); if pat then return pat.."G.o" end
	return ""
end
function source(file)
	base,ext = string.match(file,fileExpr)
	if base and (
		ext == ".h" or
		ext == ".c" or
		ext == ".m" or
		ext == ".cpp" or
		ext == ".hs" or
		ext == ".lua" or
		ext == ".gen" or
		ext == ".src" or
		ext == ".sw" or
		ext == ".g")
	then
		return true
	end
	return false
end
function insert(tab,sub,val)
	if not tab[sub] then tab[sub] = {} end
	tab[sub][val] = true
end

-- make until every new target fails.
while true do
-- find current directory contents
fileExpr = "(.*)(%..*)"
files = {}
extants = {}
os.execute("ls -1 > depend.txt")
dirlist = io.open("depend.txt")
for line in dirlist:lines() do
	files[#files+1] = line
	extants[line] = true
end
dirlist:close()
-- find targets from makefile rules
targets = {}
table.sort(files)
makefile = io.open("Makefile", "r")
for line in makefile:lines() do
	tgt,pat = string.match(line,"%%(.*): %%(.*)")
	if tgt then
		expr = "^(.*)("..string.gsub(pat,"%.","%%.")..")$"
		for k,v in ipairs(files) do
			base,ext = string.match(v,expr)
			if base then
				targets[base..tgt] = true
			end
		end
	end
end
makefile:close()
-- attempt to make each target
goback = false
for k,v in pairs(targets) do
	if not extants[k] then
		-- io.stderr:write("make("..k..")\n")
		retval = os.execute("make DEPEND=1 "..k.." 2> depend.err > depend.out")
		retval = retval and os.execute("[ -e "..k.." ]")
		if retval then
			goback = true
		end
	end
end
-- go back to start if any nontrivial make succeeded
if not goback then break end
end
scripts = {}
os.execute("grep -l -- 'int main(int argc' *.c > depend.txt")
os.execute("grep -l -- 'int main(int argc' *.cpp >> depend.txt")
os.execute("grep -l -- 'int main(void)' *.m >> depend.txt")
os.execute("grep -l -- 'main :: IO ()' *.hs >> depend.txt")
os.execute("grep -l -- '-- MAIN' *.lua >> depend.txt")
os.execute("grep -l -- '// MAIN' *.sw >> depend.txt")
os.execute("grep -l -- '// MAIN' *.g >> depend.txt")
greplist = io.open("depend.txt")
for line in greplist:lines() do
	scripts[line] = true
end
greplist:close()

-- find dependencies based on file extension.
cDeclareExpr = "^[^%s].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%("
swDeclareExpr = "^func%s%s*([a-z][a-zA-Z0-9_]*)%("
hsDeclareExpr = "^([^ ]+) +:: "
luaDeclareExpr = "^function%s%s*([a-z][a-zA-Z0-9_]*)%("
cInvokeExpr = "(.*)[^a-zA-Z0-9_.]([a-z][a-zA-Z0-9_]*)%("
hsInvokeExpr = "(.*)%(([a-z][a-zA-Z0-9_]*)"
includeExpr = "^#include "
moduleExpr = "^module +([^ ]*) +where"
importExpr = "^import +([^ ]*)"
foreignExpr = "^foreign import ccall "
dofileExpr = "^dofile%("
requireExpr = "^require +"
graphicsExpr = "makeLibrary%(filepath:"
cOpenExpr = "/*"; cCloseExpr = "*/"
luaOpenExpr = "-[["; luaCloseExpr = "-]]"
hsOpenExpr = "{-"; hsCloseExpr = "-}"
swOpenExpr = "/*"; swCloseExpr = "*/"
cCommentExpr = "//"; luaCommentExpr = "--"
hsCommentExpr = "--"; swCommentExpr = "//"
edges = {}
for k,v in pairs(files) do
	if source(v) and extants[v] then
		base,ext = string.match(v,fileExpr)
		-- io.stderr:write("examine("..v..")\n")
		edges[v] = {}
		if
			(ext == ".lua") or
			(ext == ".gen") or
			(ext == ".src")
		then
			openExpr = luaOpenExpr
			closeExpr = luaCloseExpr
			commentExpr = luaCommentExpr
			declareExpr = luaDeclareExpr
			invokeExpr = cInvokeExpr
		elseif (ext == ".sw") then
			openExpr = swOpenExpr
			closeExpr = swCloseExpr
			commentExpr = swCommentExpr
			declareExpr = swDeclareExpr
			invokeExpr = cInvokeExpr
		elseif (ext == ".hs") then
			openExpr = hsOpenExpr
			closeExpr = hsCloseExpr
			commentExpr = hsCommentExpr
			declareExpr = hsDeclareExpr
			invokeExpr = hsInvokeExpr
		else
			openExpr = cOpenExpr
			closeExpr = cCloseExpr
			commentExpr = cCommentExpr
			declareExpr = cDeclareExpr
			invokeExpr = cInvokeExpr
		end
		file = io.open(v)
		cmnt = false; abrv = false; quot = false
		for line in file:lines() do
			more = line
			name = ""
			len = more:len() - 1
			bgn = 1; ndg = 1
			if (not quot) and (not cmnt) and (not abrv) then
				while (bgn<=len) do
					if (more:sub(bgn,bgn+1)==openExpr) then break end
					if (more:sub(bgn,bgn+1)==commentExpr) then break end
					if (more:sub(bgn,bgn)=="\"") then break end
					bgn = bgn + 1
				end
				if (bgn<=len) then
					if (more:sub(bgn,bgn+1)==openExpr) then cmnt = true end
					if (more:sub(bgn,bgn+1)==commentExpr) then abrv = true end
					if (more:sub(bgn,bgn)=="\"") then quot = true end
				end
			end
			if cmnt then
				ndg = bgn
				while (ndg<=len) do
					if (more:sub(ndg,ndg+1)==closeExpr) then break end
					ndg = ndg + 1
				end
			end
			if abrv then
				ndg = more:len()
			end
			if quot then
				ndg = bgn + 1
				while (ndg<=more:len()) do
					if (more:sub(ndg,ndg)=="\"") then break end
					ndg = ndg + 1
				end
			end
			if cmnt or abrv or quot then
				if cmnt and (more:sub(ndg,ndg+1)==closeExpr) then
					cmnt = false;
				end
				if abrv then
					abrv = false;
				end
				if quot and (more:sub(ndg,ndg)=="\"") then
					name = more:sub(bgn+1,ndg-1)
					quot = false
				end
				more = more:sub(1,bgn-1)..more:sub(ndg+2)
			end
			declareVal = string.match(more,declareExpr)
			includeVal = string.match(more,includeExpr)
			moduleVal = string.match(more,moduleExpr)
			if (ext == ".hs") then hsImportVal = string.match(more,importExpr) end
			if (ext == ".sw") then swImportVal = string.match(more,importExpr) end
			foreighVal = string.match(more,foreignExpr)
			dofileVal = string.match(more,dofileExpr)
			requireVal = string.match(more,requireExpr)
			if (ext == ".sw") then graphicsVal = string.match(more,graphicsExpr) end
			if includeVal then insert(edges,v,name)
			elseif moduleVal then insert(edges,moduleVal,v)
			elseif hsImportVal then insert(edges,v,hsImportVal)
			elseif swImportVal then insert(edges,v,swImportVal..".h")
			elseif foreignVal then insert(edges,v,name)
			elseif dofileVal then insert(edges,v,name)
			elseif requireVal then insert(edges,v,name..".c")
			elseif declareVal then insert(edges,declareVal,v)
			else while (1) do
				more,invokeVal = string.match(more,invokeExpr)
				if not invokeVal then break end
				insert(edges,v,invokeVal)
			end end
			if graphicsVal then
				insert(edges,v,name)
				if not edges[name] then edges[name] = {} end
			end
		end
	end
end
-- debug(edges)

-- recursively add dependencies.
function flatten(str,dst,ext,mid,src,map)
	-- depend and recurse if non-file that does not depend on src
	-- depend and recurse if file that has given extension
	ba,ex = string.match(str,fileExpr)
	if src then for k,v in pairs(src) do
		b,e = string.match(k,fileExpr)
		if
			not dst[k] and (k ~= str) and map[k] and not scripts[k] and
			((b and (e == ext)) or (not b and not map[k][str]))
		then
			if b and (e == ext) then
				dst[k] = true
			end
			if
				((ex ~= ".lua") and (ex ~= ".gen")) or
				(e and (e ~= ".c") and (e ~= ".h"))
			then
				flatten(str,dst,".h",k,map[k],map)
				flatten(str,dst,".c",k,map[k],map)
				flatten(str,dst,".m",k,map[k],map)
				flatten(str,dst,".cpp",k,map[k],map)
			end
			if (ex == ".sw") then flatten(str,dst,".sw",k,map[k],map) end
			if (ex == ".hs") then flatten(str,dst,".hs",k,map[k],map) end
			if (ex == ".lua") then flatten(str,dst,".src",k,map[k],map) end
			if (ex == ".lua") then flatten(str,dst,".lua",k,map[k],map) end
			if (ex == ".gen") then flatten(str,dst,".src",k,map[k],map) end
		end
	end end
end
flats = {}
for k,v in pairs(edges) do if source(k) then
	-- io.stderr:write("flatten("..k..")\n")
	base,ext = string.match(k,fileExpr)
	flats[k] = {}
	flatten(k,flats[k],".h",k,v,edges)
	flatten(k,flats[k],".c",k,v,edges)
	flatten(k,flats[k],".m",k,v,edges)
	flatten(k,flats[k],".cpp",k,v,edges)
	if (ext == ".sw") then flatten(k,flats[k],".sw",k,v,edges) end
	if (ext == ".sw") then flatten(k,flats[k],".so",k,v,edges) end
	if (ext == ".hs") then flatten(k,flats[k],".hs",k,v,edges) end
	if (ext == ".lua") then flatten(k,flats[k],".src",k,v,edges) end
	if (ext == ".lua") then flatten(k,flats[k],".lua",k,v,edges) end
	if (ext == ".gen") then flatten(k,flats[k],".src",k,v,edges) end
end end
shareds = {}
for k,v in pairs(flats) do
	base,ext = string.match(k,fileExpr)
	if not base then
		node = nil
	elseif (ext == ".lua") or (ext == ".gen") then
		-- io.stderr:write("node "..k.."\n")
		node = v
	elseif extants[base..".gen"] and
		((ext == ".h") or
		(ext == ".c") or
		(ext == ".hs") or
		(ext == ".lua") or
		(ext == ".sw"))
	then
		-- io.stderr:write("node "..k..": "..base..".gen\n")
		node = edges[base..".gen"]
	else
		node = nil
	end
	if node then
		for key,val in pairs(node) do
			-- io.stderr:write("check "..key.."\n")
			b,e = string.match(key,fileExpr)
			if (e == ".c") or (e == ".m") or (e == ".cpp") then
				-- io.stderr:write("shared "..k..": "..key.."\n")
				shareds[key] = true
			end
		end
	end
end
-- debug(flats)

-- convert to makefile expectations.
function filter(tab,dst,mid,src,pat,rep)
	ba,ex = string.match(dst,fileExpr)
	pat = string.gsub(pat,"%.","%%.")
	if src then for k,v in pairs(src) do
		str,num = string.gsub(k,pat,rep)
		b,e = string.match(k,fileExpr)
		if (num > 0) and (ba ~= b) then
			insert(tab,dst,str)
		end
	end end
end
targets = {}
for k,v in pairs(flats) do
	-- io.stderr:write("convert("..k..")\n")
	base,ext = string.match(k,fileExpr)
	if base then
		if
			(ext == ".h") or
			(ext == ".c") or
			(ext == ".hs") or
			(ext == ".lua") or
			(ext == ".sw")
		then
			if extants[base..".gen"] then
				filter(targets,k,base..".gen",flats[base..".gen"],".src",".src")
				filter(targets,k,base..".gen",flats[base..".gen"],".c",".so")
				filter(targets,k,base..".gen",flats[base..".gen"],".m",".so")
				filter(targets,k,base..".gen",flats[base..".gen"],".cpp",".so")
			end
		end
		if
			(ext == ".c") or
			(ext == ".m") or
			(ext == ".cpp")
		then
			if scripts[k] then
				filter(targets,base.."C",k,v,".c","C.o")
				filter(targets,base.."C",k,v,".m","C.o")
				filter(targets,base.."C",k,v,".cpp","C.o")
			end
			if shareds[k] then
				filter(targets,base..".so",k,v,".c","C.o")
				filter(targets,base..".so",k,v,".m","C.o")
				filter(targets,base..".so",k,v,".cpp","C.o")
			end
			filter(targets,base.."C.o",k,v,".h",".h")
		end
		if (ext == ".hs") then
			if scripts[k] then
				filter(targets,base.."Hs",k,v,".c","C.o")
				filter(targets,base.."Hs",k,v,".m","C.o")
				filter(targets,base.."Hs",k,v,".cpp","C.o")
				filter(targets,base.."Hs",k,v,".hs",".hs")
			end
		end
		if (ext == ".lua") then
			if scripts[k] then
				filter(targets,base.."Lua",k,v,".lua",".lua")
				filter(targets,base.."Lua",k,v,".src",".src")
				filter(targets,base.."Lua",k,v,".c",".so")
				filter(targets,base.."Lua",k,v,".m",".so")
				filter(targets,base.."Lua",k,v,".cpp",".so")
			end
		end
		if (ext == ".sw") then
			if scripts[k] then
				filter(targets,base.."Sw",k,v,".c","C.o")
				filter(targets,base.."Sw",k,v,".m","C.o")
				filter(targets,base.."Sw",k,v,".cpp","C.o")
				filter(targets,base.."Sw",k,v,".so",".so")
			end
			filter(targets,base.."Sw.o",k,v,".sw",".sw")
			filter(targets,base.."Sw.o",k,v,".h",".h")
		end
	end
end

-- print out targets and dependencies.
orders = {}
for k,v in pairs(targets) do
	orders[#orders+1] = k
end
table.sort(orders)
for k,v in ipairs(orders) do
	str = v..":"
	ords = {}
	for key,val in pairs(targets[v]) do
		ords[#ords+1] = key
	end
	table.sort(ords)
	for key,val in ipairs(ords) do
		str = str.." "..val
	end
	print(str)
end
