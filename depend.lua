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
A require of c in lua/gen is synonym for dependee of .so dependee.
A io.open in gen is depender.
A closure of nonindented colon line in Makefile is depender and dependee(s).
Make all nonexistent non-synonym dependers that have all dependees.
Repeat until none made.
Replace synonym nodes by the depender they are in.
Eliminate duplicate graph edges and singleton nodes.
Spit out the dependence graph.
Test by make clean target for each node.
--]]

-- remove .depend file
-- find current directory contents
files = {}
os.execute("ls -1 > depend.txt")
dirlist = io.open("depend.txt")
for line in dirlist:lines() do
	files[#files+1] = line
end
dirlist:close()
-- find edges from .c files in current directory
-- find edges from .hs files in current directory
-- find edges from .lua files in current directory
-- find edges from .gen files in current directory
-- find edges from makefile rules applied to current directory contents
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
		deps = ""
		count = 1
		for k,v in ipairs(args) do
			expr = "(.*)("..string.gsub(pats[count],"%.","%%.")..")$"
			base,ext = string.match(v,expr)
			if saved and base ~= saved then
				saved = nil
				deps = ""
				count = 1
			end
			if (base and ext) then
				saved = base
				deps = deps.." "..base..ext
				count = count + 1
			end
			if not pats[count] then
				print(saved..trgt..":"..deps)
				saved = nil
				deps = ""
				count = 1
			end
		end
	end
end
makefile:close()
-- find which leaves are in current directory
-- find which nodes have only leaves in current directory
-- for each satisfied nonsingleton file node, make node
-- go back to start if some make was not up to date
-- eliminate duplicate graph edges, singleton nodes, and nonfile nodes
-- create .depend file from graph
-- for each node, make clean node
