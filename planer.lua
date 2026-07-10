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
function hideSugar(cntr)
	show = sugarRepl(cntr)
	center,len = hideCenter(show,0)
	if center == nil then print("oops "..len.." "..show) end
	return center
end
function machSugar(mach)
	return hideSugar("Center(mem:Transferzsiz:1idx:0slf:0exe[0]:"..mach..")")
end
function exprSugar(expr)
	return machSugar("Machine(xfr:Voidexp[0]:$("..expr.."))")
end
function atomSugar(list,idx)
	show = "Center(mem:Rebootzsiz:"..#list.."idx:0slf:0"
	for i,v in ipairs(list) do
	if v["mem"] == "Transferz" then
	show = show.."sub["..(i-1).."]:-1" else
	show = show.."sub["..(i-1).."]:"..castMemory(hide["mem"]) end end
	show = show..")"
	center = hideSugar(show)
	writeCenter(center,idx)
	for i,v in ipairs(list) do
	writeCenter(v,idx) end
end
function listSugar(src)
	mach0 = machSugar("Machine(xfr:Dopysig:2arg[0]:$(#"..castMemory("Kernelz")..")arg[1]:$(#"..castMemory("Memorys").."))")
	mach1 = machSugar("Machine(xfr:Voidexp[0]:$(CenterSrc := #"..castProgram(src).."))")
	mach2 = machSugar("Machine(xfr:Tsagesiz:1sav[0]:CenterSrcidx[0]:$(#"..castMemory("Memorys").."))")
	mach3 = machSugar("Machine(xfr:Qopysig:1arg[0]:$(#"..castMemory("Memorys").."))")
	return {mach0,mach1,mach2,mach3}
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Filez" then
	list = listSugar(tests[found]["typ"])
	atomSugar(list,tests[found]["idx"])
	readPrint(tests[found])
	list = listSugar(tests[found]["oth"])
	atomSugar(list,tests[found]["idx"])
	center = exprSugar("RegisterExit := #1")
	writeCenter(center,tests[found]["idx"])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
	return
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Planez" then
	-- behave as vulkanCpp would
	readPrint(tests[found])
	center = exprSugar("Wos RegisterWake #1 << #"..castMask("TestMsk").." Op")
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
