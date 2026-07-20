dofile("type.lua")
require "sugy"
require "fmty"
require "luax" -- for face.c

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
function readConfig(res,cfg)
	list = {}
	list[#list+1] = machSugar("Machine(xfr:Voidexp[0]:$(CenterSiz := #"..#cfg.."))")
	list[#list+1] = machSugar("Machine(xfr:Tsagesiz:1sav[0]:CenterSizidx[0]:$(@getcfg))")
	for i,v in ipairs(cfg) do
	list[#list+1] = machSugar("Machine(xfr:Evalres[0]:$(@getcfg)fnc[0]:Express(opr:FldOpfld[0]:$(@_)fld[1]:$(?"..v..")fld[2]:$(#"..(i-1)..")fid:Str(cfg)))")
	end
	list[#list+1] = machSugar("Machine(xfr:Ropysig:1arg[0]:$(@getcfg))")
	atomSugar(list,tests[found]["idx"])
	center = readCenter(tests[found]["idx"])
	for i,v in ipairs(center["cfg"]) do res[i] = v end
end
function listResrc(lst,res,arg)
	cent = "Center(mem:Drawzsiz:1idx:0slf:0drw[0]:Draw(con:Const(tag:ResrcConres:"..res..")ptr:Dat()"
	cent = cent.."siz:"..#arg
	for i,v in ipairs(arg) do
	cent = cent.."arg["..(i-1).."]:"..v
	end
	cent = cent.."))"
	lst[#lst+1] = centSugar(cent)
	lst[#lst+1] = machSugar("Machine(xfr:Bopysig:2arg[0]:$(#"..castMemory("Drawz")..")arg[1]:$(#0))")
	lst[#lst+1] = machSugar("Machine(xfr:Wopysig:1arg[0]:$(#"..castMemory("Drawz").."))")
end
function listMemory(lst,mem,arg)
	cent = "Center(mem:"..mem.."siz:1idx:0slf:0"..arg..")"
	lst[#lst+1] = centSugar(cent)
	lst[#lst+1] = machSugar("Machine(xfr:Bopysig:2arg[0]:$(#"..castMemory(mem)..")arg[1]:$(#0))")
	lst[#lst+1] = machSugar("Machine(xfr:Wopysig:1arg[0]:$(#"..castMemory(mem).."))")
end
function initTest()
	list = {}; listResrc(list,"SwapRes",{})
	atomSugar(list,tests[found]["idx"])
	config = {} readConfig(config,{"ScratchFrames","UniformWid","UniformHei"})
	frames = config[1] width = config[2] height = config[3]
	print("frames:"..frames.." width:"..width.." height:"..height)
	list = {}; for i = 0, (castMicro("Micros")-1) do listResrc(list,"PipeRes",{i,i}--[[IDerIns Micro]]) end
	for i = 0, frames-1 do listResrc(list,"ChainRes",{}) end
	listMemory(list,"Uniformz","uni[0]:Uniform(all:0one:1idx:0use:0tri:0num:0vtx:0mat:0bas:0pro:1wid:"..width.."hei:"..height..")")
	dat,wid,hei,cha = fmtxStbi("texture.jpg")
	-- listMemory(list,"Imagez","img[0]:Image(dat:"..showDat(dat,"").."wid:"..wid.."hei:"..hei.."cha:"..cha..")")
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
