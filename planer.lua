dofile("type.lua")
require "sugy"

tests = {}
for i,v in ipairs(arg) do
	tag,res = hideArgument(v,0)
	if tag then tests[#tests+1] = tag end
end
found = 0
pass = 0
for i,v in ipairs(tests) do
	v["idx"] = rdwrInit(v["inp"],v["out"])
	if (v["oth"] ~= v["typ"]) then
		found = i
	end
	if (v["oth"] == v["typ"]) then
		pass = i
	end
end
function readPrint(test)
	center = readCenter(tests[found]["idx"])
	print(test["typ"].." mem:"..center["mem"].." siz:"..center["siz"])
	-- for i,v in ipairs(center["cfg"]) do print(test["typ"].." cfg["..(i-1).."]:"..v) end
	-- for i,v in ipairs(center["val"]) do print(test["typ"].." val["..(i-1).."]:"..v) end
	-- print(showCenter(center))
end
function hideSugar(expr)
	show = sugarRepl("Center(mem:Transferzsiz:1idx:0slf:0exe[0]:Machine(xfr:Voidexp[0]:$("..expr..")))")
	center,len = hideCenter(show,0)
	if center == nil then print("oops "..len.." "..show) end
	return center
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Filez" then
	center = hideSugar("Wos RegisterWake #1 << #"..castMask("TestMsk").." Op")
	writeCenter(center,tests[found]["idx"])
	readPrint(tests[found])
	center = hideSugar("RegisterExit := #1")
	writeCenter(center,tests[found]["idx"])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
	return
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Planez" then
	-- behave as vulkanCpp would
	readPrint(tests[found])
	writeCenter(center,tests[found]["idx"])
	readPrint(tests[found])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
	return
end

for i,v in ipairs(tests) do
	writeProgram(v["typ"],v["idx"])
	writeProgram(v["oth"],v["idx"])
end
for i,v in ipairs(tests) do if v["typ"] ~= v["oth"] then
	slf = readProgram(v["idx"])
	oth = readProgram(v["idx"])
	print(v["typ"]..":"..slf..":"..oth)
end end
