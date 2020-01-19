--[[
*    opengl.lua
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

inc, ext = string.match(arg[1],"(.*)%.(.*)")
state = {}
stack = {{inc,ext,io.open(arg[1])}}
function read(stack)
	return function()
		local line = nil
		while not line do
			if not stack[#stack][3] then return nil end
			line = stack[#stack][3]:read("*l")
			if line then return line end
			stack[#stack] = nil
			if #stack == 0 then return nil end
		end
	end
end
for line in read(stack) do
	local pat, rep = string.match(line,"^#define%s+([^%s]+)%s+(.*)")
	local inc, ext = string.match(line,"^#include \"(.*)%.(.*)\"")
	if pat and rep then
		state[pat] = rep
	elseif inc and ext then
		local found = false
		for k,v in ipairs(stack) do
			if v[1] == inc and v[2] == ext then found = true end
		end
		if not found then
			stack[#stack+1] = {inc,ext,io.open(inc.."."..ext)}
		end
	elseif stack[#stack][2] == "sl" then
		local newline = line
		for k,v in pairs(state) do
			expr = "(.*)"..k.."(.*)"
			before, after = string.match(newline,expr)
			while before and after do
				newline = before..v..after
				before, after = string.match(newline,expr)
			end
		end
		print(newline)
	end
end
