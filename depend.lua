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
verbose = 0
function precede(work,todo,list,tab,set,key)
	work[#work+1] = tab
	todo[#todo+1] = set
	list[#list+1] = key
end
function finish(work,todo,list)
	work[#work] = nil;
	todo[#todo] = nil;
	list[#list] = nil;
end
function consume(todo,list)
	list[#list] = next(todo[#todo])
	todo[#todo][list[#list]] = nil
end
function follow(work,todo,list,tab)
	work[#work+1] = tab
	todo[#todo+1] = {}
	for k,v in pairs(work[#work]) do todo[#todo][k] = true end
	list[#list+1] = false
	consume(todo,list)
end
function modify(given,control)
	local work = {}
	local todo = {}
	local list = {}
	follow(work,todo,list,given)
	repeat
		-- follow to leaf
		while (#work > 0) and (type(work[#work][list[#list]]) == "table") and
			not (type(next(work[#work][list[#list]])) == "nil") do
			follow(work,todo,list,work[#work][list[#list]])
		end
		control(work,todo,list)
		local twork = {}
		local ttodo = {}
		local tlist = {}
		local last = work[1]
		-- clean off invalid
		for k,v in ipairs(list) do
			if not (work[k] == last) or not (type(work[k]) == "table") or not work[k][v] then break end
			last = work[k][v]
			precede(twork,ttodo,tlist,work[k],todo[k],v)
		end
		-- clean off finished
		while (#twork > 0) and (type(next(ttodo[#ttodo])) == "nil") do
			finish(twork,ttodo,tlist)
		end
		-- consume next
		if (#twork > 0) then
			consume(ttodo,tlist)
		end
		list = tlist
		work = twork
		todo = ttodo
	until (#work == 0)
end
function debug(given)
	local str = ""
	local open = false
	local done = {}
	local function control(tab,set,key)
		local same = 0
		while (#done > #key) do
			done[#done] = nil
		end
		while (same < #done) and (done[same+1] == key[same+1]) do
			same = same + 1
		end
		while (#done > 0) and (same < #done) do
			done[#done] = nil
		end
		while (#done < #key) do
			local temp = key[#done+1]
			local count = 1
			done[#done+1] = temp
			if (#done == #key) and open then
				str = str..", "
			end
			if (#done == #key) and not open then
				open = true
				str = str.." ("
			end
			if (#done < #key) and open then
				open = false
				str = str..")\n"
			end
			while (#done < #key) and (count < #done) do
				count = count + 1
				str = str.." "
			end
			str = str..temp
			if (#done < (#key-1)) then
				str = str..":\n"
			end
		end
	end
	modify(given,control)
	if open then str = str..")" end
	io.stderr:write(str.."\n")
end
function copy(given)
	local retval = {}
	local function control(tab,set,key)
		local temp = retval
		for k,v in ipairs(key) do
			if (type(temp[v]) == "nil") then
				if (type(tab[k][v]) == "table") then
					temp[v] = {}
				else
					temp[v] = tab[k][v]
				end
			end
			temp = temp[v]
		end
	end
	modify(given,control)
	return retval
end
function contour(dst,src,lev)
	local function control(tab,set,key)
		if (#key >= lev) then dst[key[lev]] = true end
	end
	modify(src,control)
end
function inboth(src1,src2)
	local retval = {}
	local function control(tab,set,key)
		local temp = src2
		for k,v in ipairs(key) do
			if not (type(temp) == "table") then return end
			if (type(temp[v]) == "nil") then return end
			temp = temp[v]
		end
		temp = retval
		for k,v in ipairs(key) do
			if (type(temp[v]) == "nil") then
				if (type(tab[k][v]) == "table") then
					temp[v] = {}
				else
					temp[v] = tab[k][v]
				end
			end
			temp = temp[v]
		end
	end
	modify(src1,control)
	return retval
end
function make()
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
		if not goback then break end
	end
end
function glob(mains,files)
	-- List files with main pattern for extension.
	os.execute("grep -l -E -- '^int main\\(int argc' *.c > depend.tmp")
	os.execute("grep -l -E -- '^int main\\(int argc' *.cpp >> depend.tmp")
	os.execute("grep -l -E -- '^int main\\(void\\)' *.m >> depend.tmp")
	os.execute("grep -l -E -- '^main :: IO \\(\\)' *.hs >> depend.tmp")
	os.execute("grep -l -E -- '^-- MAIN' *.lua >> depend.tmp")
	os.execute("grep -l -E -- '^// MAIN' *.sw >> depend.tmp")
	os.execute("grep -l -E -- '^// MAIN' *.g >> depend.tmp")
	local greplist = io.open("depend.tmp")
	for line in greplist:lines() do
		mains[line] = true
	end
	greplist:close()
	-- List all files
	os.execute("ls > depend.tmp")
	greplist = io.open("depend.tmp")
	for line in greplist:lines() do
		files[line] = true
	end
	greplist:close()
end
function parse(invokes,declares,includes)
	local function insert1(set,val,dbg)
		if val and not (val == "") and not set[val] then set[val] = {} end
		if val and not (val == "") then set[val][dbg] = true end
	end
	local function insert(tab,sub,val,dbg)
		if val and not (val == "") and not tab[sub] then tab[sub] = {} end
		insert1(tab[sub],val,dbg)
	end
	-- Find dependees dependers includes of the file they are in from multiple regexs per extension.
	local fileExpr = "(.*)(%..*)"
	local cDeclareExpr = "^[^%s].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%("
	local swDeclareExpr = "^func%s%s*([a-z][a-zA-Z0-9_]*)"
	local luaDeclareExpr = "^function%s%s*([a-z][a-zA-Z0-9_]*)%("
	local cInvokeExpr = "(.*)[^a-zA-Z0-9_.]([a-z][a-zA-Z0-9_]*)%("
	local includeExpr = "^#include "
	local moduleExpr = "^module +([^ ]*) +where"
	local importExpr = "^import +([^ ]*)"
	local foreignExpr = "^foreign import ccall "
	local dofileExpr = "^dofile%("
	local requireExpr = "^require +"
	local graphicsExpr = "makeLibrary%(filepath:"
	local cOpenExpr = "/*"; local cCloseExpr = "*/"
	local luaOpenExpr = "-[["; local luaCloseExpr = "-]]"
	local hsOpenExpr = "{-"; local hsCloseExpr = "-}"
	local swOpenExpr = "/*"; local swCloseExpr = "*/"
	local cCommentExpr = "//"; local luaCommentExpr = "--"
	local hsCommentExpr = "--"; local swCommentExpr = "//"
	for k,v in pairs(files) do
		local openExpr,commentExpr,declareExpr,invokeExpr
		local base,ext = string.match(k,fileExpr)
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
			local file = io.open(k);
			local cmnt = false; local abrv = false; local quot = false
			for line in file:lines() do
				local more = line
				local name = ""
				local len = more:len() - 1
				local bgn = 1; local ndg = 1
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
				local declareVal,includeVal,importVal,foreignVal,dofileVal,requireVal,graphicsVal
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
end
function depender(file)
	local fileExpr = "(.*)(%..*)"
	local base,ext = string.match(file,fileExpr)
	if mains[file] then
		return base.."C"
	end
	return nil
end
function dependee(file)
	local fileExpr = "(.*)(%..*)"
	local base,ext = string.match(file,fileExpr)
	if ext ==  ".c" then
		return base.."C.o"
	end
	return nil
end
mains = {} -- set of main files
files = {} -- set of all files
invokes = {} -- per file depends on funcs
declares = {} -- per func depends on files
includes = {} -- per file depends on files
depends = {}
reasons = {}
depends = {}
dependers = {}
dependees = {}
make()
glob(mains,files)
parse(invokes,declares,includes)
io.stderr:write("HERE invokes\n"); debug(invokes)
io.stderr:write("HERE declares\n"); debug(declares)
contour(depends,mains,1)
contour(depends,invokes,1)
contour(depends,declares,2)
contour(depends,includes,1)
contour(depends,includes,2)
io.stderr:write("HERE contour\n"); debug(depends)
io.stderr:write("HERE files\n"); debug(files)
reasons = inboth(depends,files)
io.stderr:write("HERE inboth\n"); debug(reasons)
--[[
verbose = 100; connect(reasons,invokes,declares); verbose = 0
io.stderr:write("HERE connect\n"); debug(reasons)
direct(reasons,includes)
collect(reasons,includes)
finish(depends,reasons,depender,dependee)
for k,v in pairs(depends) do
	local sorted = {}
	dependers[#dependers+1] = k
	for ky,vl in pairs(v) do
		sorted[#sorted+1] = ky
	end
	table.sort(sorted)
	dependees[k] = sorted
end
table.sort(dependers)
for k,v in ipairs(dependers) do
	local str = v..":"
	for ky,vl in ipairs(dependees[v]) do
		str = str.." "..vl
	end
	print(str)
end
--]]
