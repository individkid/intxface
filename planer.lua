dofile("type.lua")

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
	for i,v in ipairs(center["cfg"]) do print(test["typ"].." cfg["..(i-1).."]:"..v) end
	for i,v in ipairs(center["val"]) do print(test["typ"].." val["..(i-1).."]:"..v) end
	-- print(showCenter(center))
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Filez" then
	-- write TestMsk to ScratchDescrs and 0 to RegisterEval
	center = hideCenter("Center(mem:Configurezsiz:2idx:0slf:0cfg[0]:ScratchDescrscfg[1]:RegisterEvalval[0]:"..castMask("TestMsk").."val[1]:0)")
	if center == nil then print("oops") end
	writeCenter(center,tests[found]["idx"])
	readPrint(tests[found])
	-- -- write 1 to RegisterExit
	center = hideCenter("Center(mem:Configurezsiz:1idx:0slf:0cfg[0]:RegisterExitval[0]:1)")
	if center == nil then print("oops") end
	writeCenter(center,tests[found]["idx"])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
	-- print("TODO4")
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
