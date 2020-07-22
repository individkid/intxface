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

if not arg[1] then return end
inc, ext = string.match(arg[1],"(.*)%.(.*)")
if not inc or not ext then return end
state = {}
defin = {}
stack = {{inc,ext,io.open(arg[1])}}
if not stack[1][3] then return end
depth = {}
function readFunc(stack)
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
index = 2
while arg[index] do
	state[arg[index]] = true
	index = index + 1
end
for line in readFunc(stack) do
	local pat, rep = string.match(line,"^#define%s+([^%s]+)%s+(.*)")
	if not pat then pat = string.match(line,"^#define%s+([^%s]+)") end
	local inc, ext = string.match(line,"^#include%s+\"(.*)%.(.*)\"")
	local ifdef = string.match(line,"^#ifdef%s+(.*)")
	local ifndef = string.match(line,"^#ifndef%s+(.*)")
	local endif = string.match(line,"^#endif")
	local found = false
	for k,v in pairs(depth) do
		if not v then
			found = true
		else
		end
	end
	if ifdef then
		if state[ifdef] or defin[ifdef] then
			depth[#depth+1] = true
		else
			depth[#depth+1] = false
		end
	elseif ifndef then
		if not state[ifndef] and not defin[ifndef] then
			depth[#depth+1] = true
		else
			depth[#depth+1] = false
		end
	elseif endif then
		depth[#depth] = nil
	elseif pat and rep and not found then
		state[pat] = rep
	elseif pat and not found then
		defin[pat] = true
	elseif inc and ext and not found then
		local found = false
		for k,v in ipairs(stack) do
			if v[1] == inc and v[2] == ext then found = true end
		end
		if not found then
			stack[#stack+1] = {inc,ext,io.open(inc.."."..ext)}
		end
	elseif stack[#stack][2] == "sl" and not found then
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
