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
Identifier in c is alpha prefix, alphanumeric, open-paren suffix.
Note this does not capture extern variable dependencies in c.
A include in c is dependee.
A identifier in indented line in c is synonym for dependee.
A identifier on unindented line in c is synonym for depender.
A import of hs in hs is synonym for dependee.
A foreign of c in hs is synonym for dependee.
A module in hs is synonym for depender.
A dofile in lua/gen is dependee.
A require of c in lua/gen is synonym for dependee.
A io.open in gen is depender.
A closure of nonindented colon line in Makefile is depender and dependee(s).

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

while true do
-- find current directory contents
files = {}
extants = {}
os.execute("ls -1 > depend.txt")
dirlist = io.open("depend.txt")
for line in dirlist:lines() do
	files[#files+1] = line
	extants[line] = true
end
dirlist:close()
scripts = {}
os.execute("grep -l -- '-- MAIN' *.lua *.gen > depend.txt")
greplist = io.open("depend.txt")
for line in greplist:lines() do
	scripts[line] = true
end
greplist:close()
-- find nodes from makefile rules applied to current directory contents
edges = {}
makefile = io.open("Makefile", "r")
for line in makefile:lines() do
	pats = {}
	trgt,pats[1],more = string.match(line,"%%(.*): %%([^ ]*)(.*)")
	if (trgt ~= nil) then
		while (more ~= "") do
			pats[#pats+1],more = string.match(more," %%([^ ]*)(.*)")
		end
		args = files; table.sort(args)
		table.sort(pats)
		saved = nil
		deps = {}
		strs = ""
		count = 1
		for k,v in ipairs(args) do
			expr = "^(.*)("..string.gsub(pats[count],"%.","%%.")..")$"
			base,ext = string.match(v,expr)
			if saved and base ~= saved then
				saved = nil
				deps = {}
				strs = ""
				count = 1
			end
			if (base and ext) then
				saved = base
				deps[base..ext] = true
				strs = strs.." "..base..ext
				count = count + 1
			end
			if not pats[count] then
				-- print(saved..trgt..":"..strs)
				edges[saved..trgt] = {} -- deps
				saved = nil
				deps = {}
				strs = ""
				count = 1
			end
		end
	end
end
makefile:close()
-- attempt to make each node
goback = false
for k,v in pairs(edges) do
	if (not extants[k]) then
		retval = os.execute("make DEPEND=1 "..k.." 2> depend.err > depend.out")
		retval = retval and os.execute("[ -e "..k.." ]")
		if retval then
			goback = true
			-- print(k)
			-- makeerr = io.open("depend.err")
			-- for line in makeerr:lines() do
			-- print("err "..line)
			-- end
			-- makeerr:close()
			-- makeout = io.open("depend.out")
			-- for line in makeout:lines() do
			-- print("out "..line)
			-- end
			-- makeout:close()
		end
	end
end
-- go back to start if any nontrivial make succeeded
if not goback then break end
end

-- find edges from .c files in current directory
exper = "^[^%s].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%(.*$"
expee = "(.*)[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%("
for k,v in ipairs(files) do
	if (string.match(v,"^.*%.c$")) then
		-- print(v..":")
		file = io.open(v)
		cmnt = false; abrv = false; quot = false
		for line in file:lines() do
			more = line
			name = ""
			len = more:len() - 1
			bgn = 1; ndg = 1
			if (not quot) and (not cmnt) and (not abrv) then
				while (bgn<=len) do
					if (more:sub(bgn,bgn+1)=="/*") then break end
					if (more:sub(bgn,bgn+1)=="//") then break end
					if (more:sub(bgn,bgn)=="\"") then break end
					bgn = bgn + 1
				end
				if (bgn<=len) then
					if (more:sub(bgn,bgn+1)=="/*") then cmnt = true end
					if (more:sub(bgn,bgn+1)=="//") then abrv = true end
					if (more:sub(bgn,bgn)=="\"") then quot = true end
				end
			end
			if cmnt then
				ndg = bgn
				while (ndg<=len) do
					if (more:sub(ndg,ndg+1)=="*/") then break end
					ndg = ndg + 1
				end
				if (ndg==len) then ndg=ndg+1 end
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
				if cmnt and (more:sub(ndg-1,ndg)=="*/") then
					cmnt = false;
				end
				if abrv then
					abrv = false;
				end
				if quot and (more:sub(ndg,ndg)=="\"") then
					name = more:sub(bgn+1,ndg-1)
					quot = false
				end
				more = more:sub(1,bgn-1)..more:sub(ndg+1)
			end
			depender = string.match(more,exper)
			if (depender) then
				-- print(depender..": "..v)
				if not edges[depender] then edges[depender] = {} end
				edges[depender][v] = true
			elseif (more == "#include ") then
				-- print(v..": "..name)
				if not edges[v] then edges[v] = {} end
				edges[v][name] = true
			else while (1) do
				more,dependee = string.match(more,expee)
				if not dependee then break end
				-- print(v..": "..dependee)
				if not edges[v] then edges[v] = {} end
				edges[v][dependee] = true
			end end
		end
		file:close()
	end
end
-- find edges from .hs files in current directory
exper = "^module +([^ ]*) +where"
expee = "^import +([^ ]*)"
expex = "^foreign import ccall \"([^\"]*)\""
for k,v in ipairs(files) do
	if (string.match(v,"^.*%.hs$")) then
		file = io.open(v)
		for line in file:lines() do
			depender = string.match(line,exper)
			import = string.match(line,expee)
			foreign = string.match(line,expex)
			if depender then
				-- print(depender..": "..v)
				if not edges[depender] then edges[depender] = {} end
				edges[depender][v] = true
			end
			if import then
				-- print(v..": "..import)
				if not edges[v] then edges[v] = {} end
				edges[v][import] = true
			end
			if foreign then
				-- print(v..": "..foreign)
				if not edges[v] then edges[v] = {} end
				edges[v][foreign] = true
			end
		end
		file:close()
	end
end
-- find edges from .lua and .gen files in current directory
needed = {}
depended = {}
exper = "forkExec%(\"([^\"]*)\"%)"
expee = "^dofile%(\"([^\"]*)\"%)"
expex = "^require +\"([^\"]*)\""
for k,v in ipairs(files) do
	if (string.match(v,"^.*%.lua$") or string.match(v,"^.*%.gen$")) then
		file = io.open(v)
		for line in file:lines() do
			depend = string.match(line,exper)
			import = string.match(line,expee)
			foreign = string.match(line,expex)
			if depend then
				-- print("depend: "..v..": "..depend)
				if not edges[v] then edges[v] = {} end
				edges[v][depend] = true
				needed[depend] = true
			end
			if import then
				-- print(v..": "..import)
				if not edges[v] then edges[v] = {} end
				edges[v][import] = true
			end
			if foreign then
				-- print(v..": "..foreign..".c")
				if not edges[v] then edges[v] = {} end
				edges[v][foreign..".c"] = true
				depended[foreign..".c"] = true
			end
		end
		file:close()
	end
end

-- eliminate duplicate graph edges and nonfile nodes
-- print("HERE")
entries = {}
for k,v in pairs(edges) do
	for key,val in pairs(v) do
		base,ext = string.match(key,"(.*)(%..*)")
		if (k == "main") and (ext == ".c") then
			-- print(key..": main")
			entries[key] = true
		end
		if (k == "Main") and (ext == ".hs") then
			-- print(key..": Main")
			entries[key] = true
		end
	end
end
for k,v in pairs(scripts) do
	entries[k] = true
end
update = {}
for k,v in pairs(edges) do
	deps = {}
	count = 0
	if extants[k] or needed[k] then
		-- deps[k] = true;
		-- count = count + 1
		for key,val in pairs(v) do
			if extants[key] or needed[key] then
				if (key ~= k) then
					if not deps[key] then
						count = count + 1
					end
					-- print(k..": "..key)
					deps[key] = true
				end
			elseif edges[key] then
				for ky,vl in pairs(edges[key]) do
					if (ky ~= k) then
						if (not deps[ky]) then
							count = count + 1
						end
						-- print(k..": "..key..": "..ky)
						deps[ky] = true
					end
				end
			end
		end
	end
	if (count > 0) then
		update[k] = deps
	end
end
edges = update
-- flatten *C *Hs *Lua *Gen dependencies
update = {}
for k,v in pairs(edges) do
	base,ext = string.match(k,"(.*)(%..*)")
	deps = v
	if base and entries[k] then
		count = 1
		while (count > 0) do
			count = 0
			for key,val in pairs(v) do
				b,e = string.match(key,"(.*)(%..*)")
				if edges[key] and (not needed[key]) and
					((ext ~= ".lua") or (e ~= ".c")) and
					((ext ~= ".gen") or (e ~= ".c")) then
					for ky,vl in pairs(edges[key]) do
						if (not deps[ky]) then
							count = count + 1;
							deps[ky] = vl
							-- print(k..": "..key..": "..ky)
						end
					end
				end
			end
		end
	end
	update[k] = deps
end
edges = update
--[[
print("HERE")
for k,v in pairs(edges) do
	print(k)
	for key,val in pairs(v) do
		print(" "..key)
	end
end
--]]

-- create depend.mk file from graph
-- print("HERE")
update = {}
for k,v in pairs(edges) do
	base,ext = string.match(k,"(.*)(%..*)")
	if base and (ext == ".c") and entries[k] then
		deps = {}
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if b and (e == ".c") then
				deps[#deps+1] = b.."C.o"
			end
		end
		table.sort(deps)
		update[base.."C"] = deps
	end
	if base and (ext == ".c") and depended[k] then
		deps = {}
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if b and (e == ".c") then
				deps[#deps+1] = b.."C.o"
			end
		end
		if (#deps > 0) then
			table.sort(deps)
			update[base..".so"] = deps
		end
	end
	if base and (ext == ".c") then
		deps = {}
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if (e == ".h") then
				deps[#deps+1] = key
			end
		end
		table.sort(deps)
		update[base.."C.o"] = deps
	end
	if base and (ext == ".hs") and entries[k] then
		deps = {}
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if b and (e == ".c") then
				deps[#deps+1] = b.."C.o"
			end
			if b and (e == ".hs") then
				deps[#deps+1] = key
			end
		end
		table.sort(deps)
		update[base.."Hs"] = deps
	end
	if base and (ext == ".lua") and entries[k] then
		deps = {}
		deps[#deps+1] = k
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if b and (e == ".c") then
				deps[#deps+1] = b..".so"
			end
			if b and (e == ".lua") then
				deps[#deps+1] = key
			end
			if b and (e == ".gen") then
				deps[#deps+1] = key
			end
			if needed[key] then
				deps[#deps+1] = key
			end
		end
		table.sort(deps)
		update[base.."Lua"] = deps
	end
	if base and (ext == ".gen") and entries[k] then
		deps = {}
		deps[#deps+1] = k
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if b and (e == ".c") then
				deps[#deps+1] = b..".so"
			end
			if b and (e == ".lua") then
				deps[#deps+1] = key
			end
			if b and (e == ".gen") then
				deps[#deps+1] = key
			end
		end
		table.sort(deps)
		update[base.."Gen"] = deps
	end
	if needed[k] and
		(not string.match(k,"^.*C$")) and
		(not string.match(k,"^.*Hs$")) and
		(not string.match(k,"^.*Lua$")) then
		deps = {}
		for key,val in pairs(v) do
			b,e = string.match(key,"(.*)(%..*)")
			if b and (e == ".c") then
				deps[#deps+1] = b.."C"
			end
		end
		table.sort(deps)
		update[k] = deps
	end
end
edges = update
order = {}
for k,v in pairs(edges) do
	order[#order+1] = k
end
table.sort(order)
for k,v in ipairs(order) do
	str = v..":"
	for key,val in ipairs(edges[v]) do
		str = str.." "..val
	end
	print(str)
end

-- for each node test by make clean node
