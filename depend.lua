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
function sizeof(tab)
	local count = 0
	for k,v in pairs(tab) do
		count = count + 1
	end
	return count
end
function empty(tab)
	for k,v in pairs(tab) do
		return false
	end
	return true
end
function isin(tab,key,val)
	local temp = tab
	for k,v in ipairs(key) do
		if not (type(temp) == "table") then return false end
		if (type(temp[v]) == "nil") then return false end
		temp = temp[v]
	end
	if not (temp == val) then return false end
	return true
end
function ismatch(key,exp)
	for k,v in ipairs(key) do
		if (type(exp[k]) == "string") then
			if not string.match(v,exp[k]) then
				return false
			end
		end
	end
	return true
end
function andwhy(work,der,dee,why)
	if not (type(work[der]) == "table") then work[der] = {} end
	if not (type(work[der][dee]) == "table") then work[der][dee] = {} end
	work[der][dee][why] = true
end
function depend(tab,key,val)
	if not (type(tab[key]) == "table") then tab[key] = {} end
	tab[key][val] = true
end
function insert(work,tab,key)
	if not (type(work) == "table") then return end
	local temp = work
	for k,v in ipairs(key) do
		if not (type(temp[v]) == "table") then
			if (type(tab[k][v]) == "table") or (type(tab[k][v]) == "nil") then
				temp[v] = {}
			else
				temp[v] = tab[k][v]
			end
		end
		temp = temp[v]
	end
end
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
	list[#list+1] = next(todo[#todo])
	todo[#todo][list[#list]] = nil
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
		insert(retval,tab,key)
	end
	modify(given,control)
	return retval
end
function inboth(src1,src2)
	local retval = {}
	local function control(tab,set,key)
		if not isin(src2,key,tab[#tab][key[#key]]) then return end
		insert(retval,tab,key)
	end
	modify(src1,control)
	return retval
end
function ineither(src1,src2)
	local retval = {}
	local function control(tab,set,key)
		insert(retval,tab,key)
	end
	modify(src1,control)
	modify(src2,control)
	return retval
end
function infirst(src1,src2)
	local retval = {}
	local function control(tab,set,key)
		if isin(src2,key,tab[#tab][key[#key]]) then return end
		insert(retval,tab,key)
	end
	modify(src1,control)
	return retval
end
function filter(given,exp)
	local retval = {}
	local function control(tab,set,key)
		if not ismatch(key,exp) then return end
		insert(retval,tab,key)
	end
	modify(given,control)
	return retval
end
function distill(given,exp)
	local retval = {}
	local function control(tab,set,key)
		if ismatch(key,exp) then return end
		insert(retval,tab,key)
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
function connect(given,derfun,fundee)
	local function control(tab,set,key)
		if (#key < 1) then return end
		local work,der = tab[1],key[1]
		if (type(derfun[der]) == "table") then
			for k,v in pairs(derfun[der]) do
				local why = k
				if (type(fundee[why]) == "table") then
					for ky,vl in pairs(fundee[why]) do
						local dee = ky
						andwhy(work,der,dee,why)
					end
				end
			end
		end
	end
	modify(given,control)
end
function direct(given,derdee)
	local function control(tab,set,key)
		if (#key < 1) then return end
		local work,der = tab[1],key[1]
		if (type(derdee[der]) == "table") then
			for k,v in pairs(derdee[der]) do
				local dee = k
				if (type(v) == "table") then
					for ky,vl in pairs(v) do
						local why = ky
						andwhy(work,der,dee,why)
					end
				end
			end
		end
	end
	modify(given,control)
end
function collect(given,derdee)
	local function control(tab,set,key)
		if (#key < 2) then return end
		local der = key[1]
		local work,todo,why = tab[2],set[2],key[2]
		if (type(derdee[why]) == "table") then
			for k,v in pairs(derdee[why]) do
				local dee = k
				if not (type(work[dee]) == "table") then
					work[dee] = {}
					todo[dee] = true
				end
				work[dee][why] = true
			end
		end
	end
	modify(given,control)
end
function convert(given,mains,files)
	local retval = {}
	local function control(tab,set,key)
		if (#key < 3) then return end
		local der,dee,why = key[1],key[2],tab[3]
		local fileExpr = "(.*)(%..*)"
		local baser,exter = string.match(der,fileExpr)
		local basee,extee = string.match(dee,fileExpr)
		if (exter == ".c") and (extee == ".c") and mains[der] and not (der == dee) then depend(retval,baser.."C",basee.."C.o") end
		if (exter == ".cpp") and (extee == ".c") and mains[der] then depend(retval,baser.."C",basee.."C.o") end
		if (exter == ".hs") and (extee == ".hs") and mains[der] and not (der == dee) then depend(retval,baser.."Hs",dee) end
		if (exter == ".hs") and (extee == ".c") and mains[der] then depend(retval,baser.."Hs",basee.."C.o") end
		if (exter == ".lua") and (extee == ".lua") and mains[der] and not (der == dee) then depend(retval,baser.."Lua",dee) end
		if (exter == ".lua") and (extee == ".src") and mains[der] then depend(retval,baser.."Lua",dee) end
		if (exter == ".lua") and (extee == ".c") and mains[der] then depend(retval,baser.."Lua",basee..".so") end
	end
	modify(given,control)
	return retval
end
function make()
	while true do
		-- List .c .hs .sw .cpp .lua .gen .src .metal .g .o files.
		local files = {}
		os.execute("ls -1 > depend.tmp")
		local dirlist = io.open("depend.tmp")
		for line in dirlist:lines() do
			files[line] = true
		end
		dirlist:close()
		-- Read Makefile for targets of files.
		local targets = {}
		local makefile = io.open("Makefile", "r")
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
		local goback = false
		for k,v in pairs(targets) do
			if not files[k] then
				local retval = os.execute("make DEPEND=1 "..k.." 2> depend.err > depend.out")
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
function parse(files,invokes,declares,includes)
	local function insert1(set,val,dbg)
		if val and not (val == "") and not set[val] then set[val] = {} end
		if val and not (val == "") then set[val][dbg] = true end
	end
	local function insert2(tab,sub,val,dbg)
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
				local declareVal,includeVal,importVal,foreignVal,dofileVal,requireVal,graphicsVal,invokeVal
				declareVal = string.match(more,declareExpr)
				includeVal = string.match(more,includeExpr)
				if (ext == ".sw") then importVal = string.match(more,importExpr) else importVal = nil end
				foreignVal = string.match(more,foreignExpr)
				dofileVal = string.match(more,dofileExpr)
				requireVal = string.match(more,requireExpr)
				if (ext == ".sw") then graphicsVal = string.match(more,graphicsExpr) else graphicsVal = nil end
				if (ext == ".hs") then invokeVal = string.match(more,invokeExpr) else invokeVal = nil end
				if includeVal then insert2(includes,k,name,"includeVal")
				elseif importVal then insert2(includes,k,importVal..".h","importVal")
				elseif dofileVal then insert2(includes,k,name,"dofileVal")
				elseif requireVal then insert2(includes,k,name..".c","requireVal")
				elseif graphicsVal then insert2(includes,k,name,"graphicsVal")
				elseif declareVal then insert2(declares,declareVal,k,"declareVal")
				elseif foreignVal then insert2(invokes,k,name,"foreignVal")
				elseif invokeVal then insert2(invokes,k,invokeVal,"invokeVal")
				else while (1) do
					more,invokeVal = string.match(more,invokeExpr)
					if not invokeVal then break end
					insert2(invokes,k,invokeVal,"invokeVal")
				end end
			end
		end
	end
end
mains = {} -- set of main files
files = {} -- set of all files
invokes = {} -- per file depends on funcs
declares = {} -- per func depends on files
includes = {} -- per file depends on files
depends = {}
reasons = {}
converts = {}
dependers = {}
dependees = {}
make()
glob(mains,files)
parse(files,invokes,declares,includes)
invokes = distill(invokes,{".*%.lua"})
invokes = distill(invokes,{".*%.gen"})
invokes = distill(invokes,{".*%.src"})
declares = distill(declares,{nil,".*%.lua"})
declares = distill(declares,{nil,".*%.gen"})
declares = distill(declares,{nil,".*%.src"})
contour(depends,mains,1)
contour(depends,invokes,1)
contour(depends,declares,2)
contour(depends,includes,1)
contour(depends,includes,2)
depends = inboth(depends,files)
connect(depends,invokes,declares)
direct(depends,includes)
directs = copy(depends)
collect(depends,directs)
converts = convert(depends,mains)
for k,v in pairs(converts) do
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
