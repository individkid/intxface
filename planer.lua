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
	if (v["oth"] == "Planez") then found = i end
	if (v["oth"] == v["typ"]) then pass = i end
end
if #tests == 2 and found > 0 and tests[found]["oth"] == "Planez" then
	print("TODO "..tests[found]["typ"])
	-- writeCenter(hideCenter("Configurez(siz:0idx:0slf:0)"),tests[found]["idx"])
	-- print("TODO1") -- write TestMsk to ScratchDescrs and 0 to RegisterEval
	-- print(showCenter(readCenter(tests[found]["idx"])))
	-- print("TODO2") -- write 1 to RegisterEval
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
	-- print("TODO3")
	return
end
if #tests == 2 and found > 0 and tests[found]["oth"] == "Filez" then
	print("TODO "..tests[found]["typ"])
	-- behave as vulkanCpp would
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
