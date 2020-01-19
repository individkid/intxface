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

state = {}
stack = {{"","sl",io.stdin}}
function read(stack)
	return function()
		local line = nil
		while not line do
			line = stack[#stack][3]:read("*l")
			if line then return line end
			stack[#stack] = nil
			if #stack == 0 then return nil end
		end
	end
end
for line in read(stack) do
	pat, rep = string.match(line,"^#define%s+([^%s]+)%s+(.*)")
	inc, ext = string.match(line,"^#include \"(.*)%.(.*)\"")
	if pat and rep then
		state[pat] = rep
	elseif inc and ext then
		found = false
		for k,v in ipairs(stack) do
			if v[1] == inc and v[2] == ext then found = true end
		end
		if not found then
			stack[#stack+1] = {inc,ext,io.open(inc.."."..ext)}
		end
	elseif stack[#stack][2] == "sl" then
		newline = line
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
