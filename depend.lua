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
function modify(given,control)
	local list = {true} -- path of keys to leaf of given
	local work = {given} -- references to values of keys
	local todo = {{}} -- keys subsequent to each key
	local retval = given
	if type(given) == "table" then
		for k,v in pairs(given) do
			todo[#todo][k] = true
		end
	else
		todo[#todo][given] = true
	end
	while todo[#todo] do
		local append,replace
		list[#list] = next(todo[#todo])
		for k,v in ipairs(list) do
			if type(work[k]) == "table" then
				if not work[k][v] then
					io.stderr:write(" OOPS")
					for ky,vl in pairs(work[k]) do io.stderr:write(" "..tostring(ky)) end
					io.stderr:write(" "..tostring(v).."\n")
					return
				end
			else
				if not (work[k] == v) then io.stderr:write(" OoPs "..tostring(v).."\n") return end
			end
		end
		if verbose > 0 then io.stderr:write("#list:"..tostring(#list)) end
		if verbose > 0 and #list > 1 then io.stderr:write(" -1:"..tostring(list[#list-1])) end
		if verbose > 0 and #list > 0 then io.stderr:write(" -0:"..tostring(list[#list-0])) end
		append,replace = control(list)
		if not append then append = {} end
		if not (type(append) == "table") then append = {append} end
		if not replace then replace = {} end
		if not (type(replace) == "table") then replace = {replace} end
		for k,v in ipairs(append) do
			local value,isfunc,istab,isval,isbool
			if type(replace[k]) == "function" then isfunc = true else isfunc = false end
			if type(work[#work]) == "table" then istab = true else istab = false end
			if isfunc and istab then
				value,isval = replace[k](work[#work][v],true)
				if verbose > 0 then io.stderr:write(";true") end
			elseif isfunc then
				value,isval = replace[k](work[#work],false)
				if verbose > 0 then io.stderr:write(";false") end
			else
				value = replace[k]
				if value then isval = true else isval = false end
			end
			if type(isval) == "boolean" then isbool = true else isbool = false end
			if isbool and isval then
				if verbose > 0 then io.stderr:write(" produce") end
				todo[#todo][v] = true
			elseif isbool then
				if verbose > 0 then io.stderr:write(" consume") end
				todo[#todo][v] = nil
			else
				if verbose > 0 then io.stderr:write(" ignore") end
			end
			if istab then
				work[#work][v] = value
			elseif value and isfunc then
				work[#work] = value
			elseif value then
				work[#work] = {}
				work[#work][v] = value
			else
				work[#work] = v
			end
			if not (istab and (type(work[#work]) == "table")) and #work > 1 then
				work[#work-1][list[#list-1]] = work[#work]
			end
		end
		if not (type(work[#work]) == "table") then
			if verbose > 0 then io.stderr:write(" consume") end
			todo[#todo][list[#list]] = nil
		end
		while todo[#todo] and not next(todo[#todo]) do
			if verbose > 0 then io.stderr:write(" shed") end
			if #work == 1 then
				retval = work[#work]
			end
			list[#list] = nil
			work[#work] = nil
			todo[#todo] = nil
			if todo[#todo] then
				todo[#todo][list[#list]] = nil
			end
		end
		if todo[#todo] and todo[#todo][list[#list]] and (type(work[#work]) == "table") and work[#work][list[#list]] then
			if verbose > 0 then io.stderr:write(" nest") end
			local value = work[#work][list[#list]]
			list[#list+1] = true
			work[#work+1] = value
			todo[#todo+1] = {}
			if type(work[#work]) == "table" then
				for k,v in pairs(work[#work]) do
					todo[#todo][k] = true
				end
			else
				todo[#todo][work[#work]] = true
			end
		end
		if verbose > 0 then io.stderr:write("\n"); verbose = verbose - 1 end
	end
	return retval
end
function merge(key,dee,new,chk)
	local k = key
	local d = dee
	local n = new
	local c = chk
	return function (work,istab)
		local val,ref,vis
		if istab and type(work) == "table" then
			if verbose > 0 then io.stderr:write(" table:") end
			val = work
			ref = val
			vis = n
		elseif istab then
			if verbose > 0 then io.stderr:write(" istab:") end
			val = {}
			ref = val
			vis = n
		else
			if verbose > 0 then io.stderr:write(" merge:") end
			val = {}
			val[d] = {}
			ref = val[d]
			vis = n
		end
		if vis and c then io.stderr:write("oops\n") end
		if verbose > 0 then io.stderr:write(tostring(d)..";"..tostring(k)) end
		ref[k] = true
		return val,vis
	end
end
function debug(given)
	local str = ""
	local first = true
	local open = false
	local pend = {}
	local done = 0
	local function control(list)
		if #list < #pend then
			-- io.stderr:write("#list "..tostring(#list).." #pend "..tostring(#pend).." done "..tostring(done).." open "..tostring(open))
			if (done+1) < (#pend-1) and open then
				open = false
				str = str..")"
			end
			if (done+1) < (#pend-2) and first then
				first = false
			elseif (done+1) < (#pend-2) then
				str = str.."\n"
			end
			while (done+1) < (#pend-0) do
				local count = 1
				done = done+1
				-- done < #pend
				-- #pend is true
				-- #pend-1 is set value (not indented, not colonized, parenthesized)
				-- #pend-2 is key of set (not indented, colonized, not parenthesized)
				-- #pend-3 is key of key (indented, colonized, not parenthesized)
				while done < (#pend-2) and count < done do
					str = str.." "
					count = count + 1
				end
				str = str.." "
				if done > (#pend-2) and not open then
					open = true
					str = str.."("
				end
				-- io.stderr:write(" "..pend[done])
				str = str..pend[done]
				if done < (#pend-2) then
					str = str..":"
				end
			end
			-- io.stderr:write("\n")
		end
		pend = {}
		for k,v in ipairs(list) do
			pend[k] = v
		end
		if done > (#pend-1) then
			done = #pend - 1
		end
	end
	modify(given,control)
	control({})
	if open then str = str..")" end
	io.stderr:write(str.."\n")
end
function copy(given)
	local function control(list)
		local append = {}
		local replace = {}
		local temp = given
		local match = true
		for k,v in ipairs(list) do
			if type(temp) == "table" and temp[v] then
				temp = temp[v]
			else
				match = false
			end
		end
		if type(temp) == "table" and not match then
			append[#append+1] = list[#list]
			for k,v in pairs(temp) do
				append[#append+1] = k
				replace[#append] = true
			end
		end
		return append,replace
	end
	return modify(true,control)
end
function inboth(given,files,level)
	local function control(list)
		local append = {}
		local replace = {}
		if #list == level and not files[list[#list]] then
			append[#append+1] = list[#list]
		end
		return append,replace
	end
	return modify(given,control)
end
function contour(given,mapping,level)
	function control(list)
		local append = {}
		local replace = {}
		if #list == level then
			given[list[#list]] = true
		end
		return append,replace
	end
	modify(mapping,control)
	return given
end
function connect(given,invokes,declares)
	done = {}
	function control(list)
		local append = {}
		local replace = {}
		if verbose > 0 then io.stderr:write(" connect:"..tostring(#list)) end
		if #list == 2 and invokes[list[1]] and not done[list[1]] then
			if verbose > 0 then io.stderr:write(";"..tostring(list[#list-1])) end
			done[list[1]] = true
			for k,v in pairs(invokes[list[1]]) do
				if declares[k] then
					for ky,vl in pairs(declares[k]) do
						if verbose > 0 then io.stderr:write(" control:"..tostring(ky)..";"..tostring(k)) end
						append[#append+1] = ky
						replace[#append] = merge(k,ky,false)
					end
				end
			end
		end
		return append,replace
	end
	modify(given,control)
end
function direct(given,includes)
	local done = {}
	local function control(list)
		local append = {}
		local replace = {}
		-- for k,v in pairs(list) do io.stderr:write(" "..tostring(v)) end; io.stderr:write("\n")
		if #list == 2 and includes[list[1]] and not done[list[1]] then
			done[list[1]] = true -- no need to add to depender more than once
			for k,v in pairs(includes[list[1]]) do -- k is dependee of list[1]; v is set of reasons
				for ky,vl in pairs(v) do -- ky is reason; vl is true
					append[#append+1] = k -- add dependee to depender
					replace[#append] = merge(ky,k)
				end
			end
		end
		return append,replace
	end
	return modify(given,control)
end
function collect(given,includes)
	local done = {}
	local function control(list)
		local append = {}
		local replace = {}
		-- for k,v in pairs(list) do io.stderr:write(" "..tostring(v)) end; io.stderr:write("\n")
		if #list == 2 and includes[list[1]] then
			if not done[list[1]] then done[list[1]] = {} end
			for k,v in pairs(includes[list[1]]) do -- k is dependee of list[1]
				done[list[1]][k] = true
				for ky,vl in pairs(includes[k]) do -- ky is indirect dependee of list[1]
					append[#append+1] = ky -- add indirect dependee
					replace[#append] = merge(k,ky,true,done[list[1]][ky])
				end
			end
		end
		return append,replace
	end
	return modify(given,control)
end
function finish(given,reasons,depender,dependee)
	local function control(list)
		local append = {}
		local replace = {}
		if #list == 2 then
			if not given[depender(list[1])] then
				given[depender(list[1])] = {}
			end
			given[depender(list[1])][dependee(list[2])] = true
		end
		return append,replace
	end
	return modify(reasons,control)
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
reasons = {}
depends = {}
dependers = {}
dependees = {}
make()
glob(mains,files)
parse(invokes,declares,includes)
contour(reasons,mains,1)
contour(reasons,invokes,1)
contour(reasons,declares,2)
contour(reasons,includes,1)
contour(reasons,includes,2)
io.stderr:write("HERE contour\n"); debug(reasons)
inboth(reasons,files,1)
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
