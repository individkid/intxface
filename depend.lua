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
function debug1(k,v)
	io.stderr:write(k..":")
	for key,val in pairs(v) do
		io.stderr:write(" "..key)
	end
	io.stderr:write("\n")
end
function debug(map)
	for k,v in pairs(map) do
		debug1(k,v)
	end
end
function insert(tab,sub,val,dbg)
	if not tab[sub] and val and not (val == "") then tab[sub] = {} end
	if val and not (val == "") then tab[sub][val] = true end
end
while true do
-- List .c .hs .sw .cpp .lua .gen .src .metal .g .o files.
files = {}
os.execute("ls -1 > depend.tmp")
dirlist = io.open("depend.tmp")
for line in dirlist:lines() do
	files[line] = true
end
dirlist:close()
-- Read Makefile for targets of files.
targets = {}
makefile = io.open("Makefile", "r")
for line in makefile:lines() do
	tgt,pat = string.match(line,"%%(.*): %%(.*)")
	if tgt then
		expr = "^(.*)("..string.gsub(pat,"%.","%%.")..")$"
		for k,v in pairs(files) do
			base,ext = string.match(k,expr)
			if base then
				targets[base..tgt] = true
			end
		end
	end
end
makefile:close()
-- Call make on each target not in files and note if it was built.
goback = false
for k,v in pairs(targets) do
	if not files[k] then
		retval = os.execute("make DEPEND=1 "..k.." 2> depend.err > depend.out")
		retval = retval and os.execute("[ -e "..k.." ]")
		if retval then
			goback = true
		end
	end
end
-- Repeat until none built.
if not goback then break end end
-- List files with main pattern for extension.
mains = {}
os.execute("grep -l -- 'int main(int argc' *.c > depend.tmp")
os.execute("grep -l -- 'int main(int argc' *.cpp >> depend.tmp")
os.execute("grep -l -- 'int main(void)' *.m >> depend.tmp")
os.execute("grep -l -- 'main :: IO ()' *.hs >> depend.tmp")
os.execute("grep -l -- '-- MAIN' *.lua >> depend.tmp")
os.execute("grep -l -- '// MAIN' *.sw >> depend.tmp")
os.execute("grep -l -- '// MAIN' *.g >> depend.tmp")
greplist = io.open("depend.tmp")
for line in greplist:lines() do
	mains[line] = true
end
greplist:close()
-- List generated files per extension
generates = {}
-- Find dependees dependers includes of the file they are in from multiple regexs per extension.
fileExpr = "(.*)(%..*)"
cDeclareExpr = "^[^%s].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%("
swDeclareExpr = "^func%s%s*([a-z][a-zA-Z0-9_]*)"
luaDeclareExpr = "^function%s%s*([a-z][a-zA-Z0-9_]*)%("
cInvokeExpr = "(.*)[^a-zA-Z0-9_.]([a-z][a-zA-Z0-9_]*)%("
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
invokes = {} -- per file depends on funcs
declares = {} -- per func depends on files
includes = {} -- per file depends on files
for k,v in pairs(files) do
	base,ext = string.match(k,fileExpr)
	if
		base and
		(ext == ".lua" or
		ext == ".gen" or
		ext == ".src") then
		openExpr = luaOpenExpr
		closeExpr = luaCloseExpr
		commentExpr = luaCommentExpr
		declareExpr = luaDeclareExpr
		invokeExpr = cInvokeExpr
	elseif
		base and
		ext == ".sw" then
		openExpr = swOpenExpr
		closeExpr = swCloseExpr
		commentExpr = swCommentExpr
		declareExpr = swDeclareExpr
		invokeExpr = cInvokeExpr
	elseif
		base and
		ext == ".hs" then
		openExpr = hsOpenExpr
		closeExpr = hsCloseExpr
		commentExpr = hsCommentExpr
		declareExpr = moduleExpr
		invokeExpr = importExpr
	elseif
		base and
		(ext == ".h" or
		ext == ".c" or
		ext == ".m" or
		ext == ".cpp") then -- TODO add .g for includes
		openExpr = cOpenExpr
		closeExpr = cCloseExpr
		commentExpr = cCommentExpr
		declareExpr = cDeclareExpr
		invokeExpr = cInvokeExpr
	else
		base = nil
	end
	if base then
		file = io.open(k)
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
			if (ext == ".sw") then importVal = string.match(more,importExpr) else importVal = nil end
			foreignVal = string.match(more,foreignExpr)
			dofileVal = string.match(more,dofileExpr)
			requireVal = string.match(more,requireExpr)
			if (ext == ".sw") then graphicsVal = string.match(more,graphicsExpr) else graphicsVal = nil end
			if includeVal then insert(includes,k,name,"includeVal")
			elseif importVal then insert(includes,k,importVal..".h","importVal")
			elseif dofileVal then insert(includes,k,name,"dofileVal")
			elseif requireVal then insert(includes,k,name..".c","requireVal")
			elseif graphicsVal then insert(includes,k,name,"graphicsVal")
			elseif declareVal then insert(declares,declareVal,k,"declareVal")
			elseif foreignVal then insert(invokes,k,name,"foreignVal")
			else while (1) do
				more,invokeVal = string.match(more,invokeExpr)
				if not invokeVal then break end
				insert(invokes,k,invokeVal,"invokeVal")
			end end
		end
	end
end
-- Eliminate invokes that are declares in same file.
values = {}
for k,v in pairs(invokes) do -- file,set-of-func
	vs = {}
	for ky,vl in pairs(v) do -- func,true
		found = false
		set = declares[ky] -- set-of-file
		if set then
			for key,val in pairs(set) do -- file,true
				if key == k then found = true end
			end
		end
		if not found then vs[ky] = vl end
	end
	if next(vs) then values[k] = vs end
end
invokes = values
-- Convert invokes to includes.
for k,v in pairs(invokes) do -- file,set-of-func
	b,e = string.match(k,fileExpr)
	for ky,vl in pairs(v) do -- func,true
		set = declares[ky] -- set-of-file
		if set then for key,val in pairs(set) do -- file,true
			if includes[k] == nil then includes[k] = {} end
			includes[k][key] = true
			io.stderr:write("debug: "..k.."->"..ky.."->"..key.."\n")
		end end
	end
end
io.stderr:write("HERE includes\n"); debug(includes)
-- Collect dependencies of targets
function filePairs(ext,src,dst)
	local todo = {}
	local vals = {}
	local done = {}
	local i = 0
	local func = function(k,v)
		if type(dst[k]) == "table" and type(v) == "table" then
			for ke,va in pairs(v) do dst[k][ke] = va end
		elseif type(dst[k]) == "table" and not v == nil then
			dst[k][v] = true
		elseif not dst[k] == nil and type(v) == "table" then
			local temp = dst[k]
			dst[k] = v
			dst[k][temp] = true
		else
			dst[k] = v
		end
	end
	local gunc = function(k,v)
		if not done[k] then
			todo[#todo+1] = k
			vals[#vals+1] = v
			done[k] = true
		end
	end
	if type(src) == "table" then
		for k,v in pairs(src) do
			gunc(k,v)
		end
	end
	return function()
		while i < #todo do
			local j = 0
			i = i + 1
			b,e = string.match(todo[i],fileExpr)
			while j < #ext do
				j = j + 1
				if e == ext[j] then
					return b,j,vals[i],func,gunc
				end
			end
		end
		return nil
	end
end
function collect(exr,der,srr,exe,dee,sre,dst)
	for b,i,v,f in filePairs({exr},srr,dst) do
		vs = {}
		for ba,j,va,fu,gu in filePairs(exe,sre[b..exr],vs) do
			for bas,k,val in filePairs(exe,sre[ba..exe[j]]) do
				fu(bas..dee[k],true) -- for vs
				gu(bas..exe[k],true) -- more todo after v
			end
		end
		f(b..der,vs) -- for dst
	end
end
function include(exr,der,srr,exe,dee,sre,dst)
	for b,i,v,f in filePairs({exr},srr,dst) do
		for ba,j,va in filePairs(exe,sre) do
			f(b..der,ba..dee[j])
		end
	end
end
depends = {}
-- %C: *C.o *.so
collect(".c","C",mains,{".c",".m",".cpp"},{"C.o","C.o"},includes,depends)
include(".c","C",mains,{".so"},{".so"},includes,depends)
collect(".m","C",mains,{".c",".m",".cpp"},{"C.o","C.o"},includes,depends)
include(".m","C",mains,{".so"},{".so"},includes,depends)
collect(".cpp","C",mains,{".c",".m",".cpp"},{"C.o","C.o"},includes,depends)
include(".cpp","C",mains,{".so"},{".so"},includes,depends)
-- %Hs: *C.o *.hs
collect(".hs","Hs",mains,{".c"},{"C.o"},includes,depends)
collect(".hs","Hs",mains,{".hs"},{".hs"},includes,depends)
-- %Lua: *.so *.lua *.src
collect(".lua","Lua",mains,{".c"},{".so"},includes,depends)
collect(".lua","Lua",mains,{".lua",".src"},{".lua",".src"},includes,depends)
-- %Sw: *C.o
collect(".sw","Sw",mains,{".c"},{"C.o"},includes,depends)
-- %C.o: *.h
collect(".c","C.o",mains,{".h"},{".h"},includes,depends)
-- %Sw.o: *.sw
collect(".sw","Sw.o",mains,{".sw"},{".sw"},includes,depends)
-- %G.o: *.h
collect(".g","G.o",mains,{".h"},{".h"},includes,depends)
-- %.*: *.src
collect(".gen",".h",generates[".h"],{".src"},{".src"},includes,depends)
collect(".gen",".c",generates[".c"],{".src"},{".src"},includes,depends)
collect(".gen",".m",generates[".m"],{".src"},{".src"},includes,depends)
collect(".gen",".cpp",generates[".cpp"],{".src"},{".src"},includes,depends)
collect(".gen",".hs",generates[".hs"],{".src"},{".src"},includes,depends)
collect(".gen",".lua",generates[".lua"],{".src"},{".src"},includes,depends)
collect(".gen",".sw",generates[".sw"],{".src"},{".src"},includes,depends)
collect(".gen",".g",generates[".g"],{".src"},{".src"},includes,depends)
io.stderr:write("HERE depends\n"); debug(depends)
-- Print sorted dependencies
lines = {}; for k,v in pairs(depends) do lines[#lines+1] = k end; table.sort(lines)
io.stderr:write("HERE ",#lines,"\n")
for k,v in ipairs(lines) do
	words = {}; for ke,va in pairs(depends[v]) do words[#words+1] = ke end; table.sort(words)
	str = v..":"; for ke,va in ipairs(words) do str = str.." "..va end;
	if #words > 0 then print(str) end
end