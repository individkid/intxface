dofile("type.lua")
require "sugy"

tests = {}
more = nil
for i,v in ipairs(arg) do
	tag,res = hideArgument(v,0)
	if tag then tests[#tests+1] = tag
	else more = v end
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
function centSugar(cent)
	show = sugarRepl(cent)
	center,len = hideCenter(show,0)
	if center == nil then io.stderr:write("oops "..len.." "..show.."\n") end
	return center
end
function machSugar(mach)
	return centSugar("Center(mem:Transferzsiz:1idx:0slf:0exe[0]:"..mach..")")
end
function exprSugar(expr)
	return machSugar("Machine(xfr:Voidexp[0]:"..expr..")")
end
function atomSugar(list,idx)
	cent = "Center(mem:Rebootzsiz:"..#list.."idx:0slf:0"
	for i,v in ipairs(list) do
	if v["mem"] == "Transferz" then
	cent = cent.."sub["..(i-1).."]:-1" else
	cent = cent.."sub["..(i-1).."]:"..castMemory(v["mem"]) end end
	cent = cent..")"
	center = centSugar(cent)
	writeCenter(center,idx)
	for i,v in ipairs(list) do
	writeCenter(v,idx) end
end
function listSugar(src)
	mach0 = machSugar("Machine(xfr:Dopysig:2arg[0]:$(#"..castMemory("Kernelz")..")arg[1]:$(#"..castMemory("Memorys").."))")
	mach1 = exprSugar("$(CenterSrc := #"..castProgram(src)..")")
	mach2 = machSugar("Machine(xfr:Tsagesiz:1sav[0]:CenterSrcidx[0]:$(#"..castMemory("Memorys").."))")
	mach3 = machSugar("Machine(xfr:Qopysig:1arg[0]:$(#"..castMemory("Memorys").."))")
	return {mach0,mach1,mach2,mach3}
end
function pipeTest()
	list = listSugar(tests[found]["typ"])
	atomSugar(list,tests[found]["idx"])
	readPrint(tests[found])
end
function doneTest()
	list = listSugar(tests[found]["oth"])
	atomSugar(list,tests[found]["idx"])
	center = exprSugar("$(RegisterExit := #1)")
	writeCenter(center,tests[found]["idx"])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
end
function initTest()
	list = {}
	list[#list+1] = machSugar("Machine(xfr:Voidexp[0]:$(CenterSiz := #1))")
	list[#list+1] = machSugar("Machine(xfr:Tsagesiz:1sav[0]:CenterSizidx[0]:$(@getcfg))")
	list[#list+1] = machSugar("Machine(xfr:Evalres[0]:$(@getcfg)fnc[0]:Express(opr:FldOpfld[0]:$(@_)fld[1]:$(?ScratchFrames)fld[2]:$(#0)fid:Str(cfg)))")
	list[#list+1] = machSugar("Machine(xfr:Ropysig:1arg[0]:$(@getcfg))")
	atomSugar(list,tests[found]["idx"])
	center = readCenter(tests[found]["idx"])
	frames = center["cfg"][1]
	print("frames "..frames)
	list = {}
	list[#list+1] = centSugar("Center(mem:Drawzsiz:1idx:0slf:0drw[0]:Draw(con:Const(tag:ResrcConres:SwapRes)ptr:Dat()siz:0))")
	list[#list+1] = machSugar("Machine(xfr:Bopysig:2arg[0]:$(#"..castMemory("Drawz")..")arg[1]:$(#0))")
	list[#list+1] = machSugar("Machine(xfr:Wopysig:1arg[0]:$(#"..castMemory("Drawz").."))")
	for i = 0, (castMicro("Micros")-1) do
	list[#list+1] = centSugar("Center(mem:Drawzsiz:1idx:0slf:0drw[0]:Draw(con:Const(tag:ResrcConres:PipeRes)ptr:Dat()siz:2arg[0]:"..i.."arg[1]:"..i.."))")
	list[#list+1] = machSugar("Machine(xfr:Bopysig:2arg[0]:$(#"..castMemory("Drawz")..")arg[1]:$(#0))")
	list[#list+1] = machSugar("Machine(xfr:Wopysig:1arg[0]:$(#"..castMemory("Drawz").."))")
	end
	atomSugar(list,tests[found]["idx"])
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Filez" then
	pipeTest()
	if more then
	initTest()
	end
	doneTest()
	return
end
if #tests == 2 and found > 0 and tests[pass]["typ"] == "Planez" then
	-- behave as vulkanCpp would
	readPrint(tests[found])
	center = exprSugar("$(Wos RegisterWake #1 << #"..castMask("TestMsk").." Op)")
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
