dofile("type.lua")

tests = {}
for i,v in ipairs(arg) do
	tag,res = hideArgument(v,0)
	if tag then tests[#tests+1] = tag end
end
for i,v in ipairs(tests) do
	v["idx"] = rdwrInit(v["inp"],v["out"])
	writeProgram(v["typ"],v["idx"])
	writeProgram(v["oth"],v["idx"])
end
for i,v in ipairs(tests) do if v["typ"] ~= v["oth"] then
	slf = readProgram(v["idx"])
	oth = readProgram(v["idx"])
	print(v["typ"]..":"..slf..":"..oth)
end end
