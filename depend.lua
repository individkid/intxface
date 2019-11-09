--[[
*    depend.lua
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
-- remove depend.mk file
os.execute("rm -f depend.mk")
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
			expr = "(.*)("..string.gsub(pats[count],"%.","%%.")..")$"
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
		retval = os.execute("make "..k.." 2> depend.err > depend.out")
		retval = retval and os.execute("[ -e "..k.." ]")
		if retval then
			if os.execute("[ -e "..k.." ]") then print("exist") else print("nexist") end
			goback = true
			print(k)
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
				-- print(" "..depender)
				if not edges[depender] then edges[depender] = {} end
				edges[depender][file] = true
			elseif (more == "#include ") then
				-- print(" "..name)
				if not edges[file] then edges[file] = {} end
				edges[file][name] = true
			else while (1) do
				more,dependee = string.match(more,expee)
				if not dependee then break end
				-- print(" "..dependee)
				if not edges[file] then edges[file] = {} end
				edges[file][dependee] = true
			end end
		end
		file:close()
	end
end
-- find edges from .hs files in current directory
-- find edges from .lua files in current directory
-- find edges from .gen files in current directory
-- eliminate duplicate graph edges and nonfile nodes
-- insert C.so and/or C.o dependee/depender nodes
-- create depend.mk file from graph
-- for each node test by make clean node