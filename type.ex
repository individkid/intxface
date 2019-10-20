#!/usr/bin/env lua
--[[
*    type.ex
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

dofile("type.inc")

--HERE Enums
--HERE Structs
--HERE
Enums,Enumz = listHere("Enums")
Structs,Structz = listHere("Structs")
file = io.open("type.h", "w")
file:write(showTypeH().."\n")
file:close()
file = io.open("type.c", "w")
file:write(showTypeC().."\n")
file:close()
file = io.open("Type.hs", "w")
file:write(showTypeHs().."\n")
file:close()
file = io.open("type.lua", "w")
file:write(showTypeLua().."\n")
file:close()
