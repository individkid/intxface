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
		local append,replace,consume
		list[#list] = next(todo[#todo])
		append,replace,consume = control(list)
		if not append then append = {} end
		if not (type(append) == "table") then append = {append} end
		if not replace then replace = {} end
		if not (type(replace) == "table") then replace = {replace} end
		if not consume then consume = {} end
		if not (type(consume) == "table") then consume = {consume} end
		for k,v in ipairs(append) do
			local value,isfunc,istab,isval,isbool
			if type(replace[k]) == "function" then isfunc = true else isfunc = false end
			if type(work[#work]) == "table" then istab = true else istab = false end
			if isfunc and istab then
				value,isval = replace[k](work[#work][v],true)
			elseif isfunc then
				value,isval = replace[k](work[#work],false)
			else
				value = replace[k]
				if value then isval = true else isval = false end
			end
			if type(isval) == "boolean" then isbool = true else isbool = false end
			-- io.stderr:write(" #work "..#work.." #todo "..#todo.." isbool "..tostring(isbool).." isval "..tostring(isval).." istab "..tostring(istab).." v "..tostring(v).." value "..tostring(value).."\n")
			if isbool and isval then
				todo[#todo][v] = true
			elseif isbool then
				todo[#todo][v] = nil
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
			if not istab and #work > 1 then
				work[#work-1][list[#list-1]] = work[#work]
			end
		end
		for k,v in ipairs(consume) do
			todo[#todo][v] = nil
		end
		if not (type(work[#work]) == "table") then
			todo[#todo][list[#list]] = nil
		end
		while todo[#todo] and not next(todo[#todo]) do
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
	end
	return retval
end
function debug(given)
	local str = ""
	local first = true
	local open = false
	local pend = {}
	local done = 0
	local function control(list)
		-- io.stderr:write(tostring(first).." "..tostring(open).." "..tostring(done).." "..tostring(#list).." "..tostring(#pend).."\n")
		-- for k,v in pairs(list) do io.stderr:write(" "..tostring(v)) end; io.stderr:write("\n")
		-- for k,v in pairs(pend) do io.stderr:write(" "..tostring(v)) end; io.stderr:write("\n")
		if #list < #pend then
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
				str = str..pend[done]
				if done < (#pend-2) then
					str = str..":"
				end
			end
		end
		pend = {}
		for k,v in ipairs(list) do
			pend[k] = v
		end
		if done > (#pend-1) then
			done = #pend - 1
		end
		-- io.stderr:write(str.."\n")
		-- for k,v in pairs(pend) do io.stderr:write(" "..tostring(v)) end; io.stderr:write("\n")
		-- for k,v in pairs(list) do io.stderr:write(" "..tostring(v)) end; io.stderr:write("\n")
		-- io.stderr:write(tostring(first).." "..tostring(open).." "..tostring(done).." "..tostring(#list).." "..tostring(#pend).."\n")
		-- io.stderr:write("---\n")
	end
	modify(given,control)
	str = str..")\n"
	io.stderr:write(str)
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
end
function collect(given,include,from,to)
	local function control(list)
		local append = {}
		local replace = {}
		local consume = {}
		if #list == 2 and from(list[2]) then
			-- list[1] is depender
			-- list[2] is dependee
			-- list[3] would be intermediary between depender and dependee
			-- list[4] would be true
			consume[#consume+1] = list[2]
			for k,v in pairs(include[list[2]]) do
				-- k is dependee of dependee-cum-intermediary
				local deb = list[2]
				local function merge(work)
					-- work is set of intermediaries for key
					local value,isval
					if work then isval = nil else isval = true end
					-- if work is nil then key is a new dependee.
					-- isval of true indicates unvisited.
					-- if work is already a dependee then
					-- it may or may not have been visited.
					-- isval of nil leaves whether visited alone.
					if isval then value = {} else value = work end
					-- a new dependee has no intermediaries yet.
					value[deb] = true
					-- add dependee-cum-intermediary to dependee
					return value,isval
				end
				if to(key) then
					append[#append+1] = k
					replace[#replace+1] = merge
				end
			end
		end
		return append,replace,consume
	end
	return modify(given,control)
end
function insert1(set,val,dbg)
	if val and not (val == "") and not set[val] then set[val] = {} end
	if val and not (val == "") then set[val][dbg] = true end
end
function insert(tab,sub,val,dbg)
	if val and not (val == "") and not tab[sub] then tab[sub] = {} end
	insert1(tab[sub],val,dbg)
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
mains = {} -- set of main files
files = {} -- set of all files
invokes = {} -- per file depends on funcs
declares = {} -- per func depends on files
includes = {} -- per file depends on files
depends = {}
make()
glob(mains,files)
parse(invokes,declares,includes)
contour(depends,mains,1)
contour(depends,invokes,1)
contour(depends,declares,2)
contour(depends,includes,1)
contour(depends,includes,2)
inboth(depends,files,1)

example = copy(includes)
count1 = 0; for k,v in pairs(includes) do count1 = count1 + 1 end
count2 = 0; for k,v in pairs(example) do count2 = count2 + 1 end
io.stderr:write("#includes "..tostring(count1).." #example "..tostring(count2).."\n")
example = copy(depends)
count1 = 0; for k,v in pairs(depends) do count1 = count1 + 1 end
count2 = 0; for k,v in pairs(example) do count2 = count2 + 1 end
io.stderr:write("#depends "..tostring(count1).." #example "..tostring(count2).."\n")
