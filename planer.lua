dofile("type.lua")
require "sugy"
require "fmty"

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
index = 0
function atomSugar(list,idx,str)
	-- io.stderr:write(str..":"..index..":"..#list.."\n")
	cent = "Center(mem:Rebootzsiz:"..#list.."idx:"..index.."slf:0"
	index = index + 1
	for i,v in ipairs(list) do
	if v["mem"] == "Transferz" then
	cent = cent.."sub["..(i-1).."]:-1"
	elseif v["mem"] == "Drawz" then
	cent = cent.."sub["..(i-1).."]:"..(castMemory("Memorys")+1)
	elseif v["mem"] == "Getcfgz" then
	cent = cent.."sub["..(i-1).."]:"..(castMemory("Memorys")+1)
	else cent = cent.."sub["..(i-1).."]:"..castMemory(v["mem"]) end end
	cent = cent..")"
	center = centSugar(cent)
	writeCenter(center,idx)
	for i,v in ipairs(list) do
	writeCenter(v,idx) end
	for i in pairs(list) do list[i] = nil end
end
function listSugar(src)
	mach1 = exprSugar("$(CenterSrc := #"..castProgram(src)..")")
	mach2 = machSugar("Machine(xfr:Tsagesiz:1sav[0]:CenterSrcidx[0]:$(@getcfg))")
	mach3 = machSugar("Machine(xfr:Qopysig:1arg[0]:$(@getcfg))")
	return {mach1,mach2,mach3}
end
function pipeTest()
	list = listSugar(tests[found]["typ"])
	atomSugar(list,tests[found]["idx"],"Pipe")
	readPrint(tests[found])
end
function doneTest()
	list = listSugar(tests[found]["oth"])
	atomSugar(list,tests[found]["idx"],"Done")
	center = exprSugar("$(RegisterExit := #1)")
	writeCenter(center,tests[found]["idx"])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
end
function flushTest()
	list = listSugar(tests[found]["oth"])
	atomSugar(list,tests[found]["idx"],"Done")
	list[#list+1] = exprSugar("$(ScratchDescrs := @pass)")
	readConfig(list,config,{"ScratchDescrs"})
	print("pass:"..config[1])
	-- above read forces all prior Rebootz to complete before following Exit
	center = exprSugar("$(RegisterExit := #1)")
	writeCenter(center,tests[found]["idx"])
	writeProgram(tests[pass]["typ"],tests[pass]["idx"])
end
function readConfig(list,res,cfg)
	cent = "Center(mem:Getcfgzsiz:0idx:0slf:0)"
	list[#list+1] = centSugar(cent) -- this prevents Pull blocking
	list[#list+1] = machSugar("Machine(xfr:Voidexp[0]:$(CenterSiz := #"..#cfg.."))")
	list[#list+1] = machSugar("Machine(xfr:Tsagesiz:1sav[0]:CenterSizidx[0]:$(#"..(castMemory("Memorys")+1).."))")
	for i,v in ipairs(cfg) do
	list[#list+1] = machSugar("Machine(xfr:Evalres[0]:$(#"..(castMemory("Memorys")+1)..")fnc[0]:Express(opr:FldOpfld[0]:$(@_)fld[1]:$(?"..v..")fld[2]:$(#"..(i-1)..")fid:Str(cfg)))")
	end
	list[#list+1] = machSugar("Machine(xfr:Qopysig:1arg[0]:$(#"..(castMemory("Memorys")+1).."))")
	str = "Read"
	for i,v in ipairs(cfg) do str = str..":"..v end
	atomSugar(list,tests[found]["idx"],str)
	center = readCenter(tests[found]["idx"])
	for i,v in ipairs(center["cfg"]) do res[i] = v end
end
function writeConfig(list,val,cfg)
	if #list > 0 then atomSugar(list,tests[found]["idx"]) end
	for i,v in ipairs(cfg) do
	list[#list+1] = machSugar("Machine(xfr:Voidexp[0]:$("..v.." := #"..val[i].."))")
	end
	atomSugar(list,tests[found]["idx"],"Write")
end
-- TODO for listDraw, use rsp of RetRsp
function listResrc(lst,res,arg)
	cent = "Center(mem:Drawzsiz:1idx:0slf:0drw[0]:Draw(con:Const(tag:ResrcConres:"..res..")ptr:Dat()"
	cent = cent.."siz:"..#arg
	for i,v in ipairs(arg) do cent = cent.."arg["..(i-1).."]:"..v end
	cent = cent.."))"
	lst[#lst+1] = centSugar(cent)
	-- TODO Void and Tsage to set rsp to RptRsp
	lst[#lst+1] = machSugar("Machine(xfr:Bopysig:2arg[0]:$(#"..(castMemory("Memorys")+1)..")arg[1]:$(#0))")
end
function listMemory(lst,mem,fld,arg)
	cent = "Center(mem:"..mem.."siz:"..#arg.."idx:0slf:0"
	for i,v in ipairs(arg) do cent = cent..fld.."["..(i-1).."]:"..v end
	cent = cent..")"
	lst[#lst+1] = centSugar(cent)
	-- TODO Void and Tsage to set rsp to RptRsp
	lst[#lst+1] = machSugar("Machine(xfr:Bopysig:2arg[0]:$(#"..castMemory(mem)..")arg[1]:$(#0))")
end
function listSpoof(lst,mem,fld,arg)
	cent = "Center(mem:"..mem.."siz:"..#arg.."idx:0slf:-1"
	for i,v in ipairs(arg) do cent = cent..fld.."["..(i-1).."]:"..v end
	cent = cent..")"
	lst[#lst+1] = centSugar(cent)
	lst[#lst+1] = machSugar("Machine(xfr:Qopysig:1arg[0]:$(#"..castMemory(mem).."))")
	atomSugar(list,tests[found]["idx"],"Spoof")
end
function writeCent(lst,mem,idx,slf,fld,arg)
	if #lst > 0 then atomSugar(lst,tests[found]["idx"],"Cent") end
	cent = "Center(mem:"..mem.."siz:"..#arg.."idx:"..idx.."slf:"..slf
	for i,v in ipairs(arg) do cent = cent..fld.."["..(i-1).."]:"..v end
	cent = cent..")"
	writeCenter(centSugar(cent),tests[found]["idx"]);
end
function initTest()
	list = {}; listResrc(list,"SwapRes",{})
	atomSugar(list,tests[found]["idx"],"Swap")
	-- TODO following write to Memorys+1 can be overwritten by Done from above Bopy of Drawz from Memorys+1
	-- TODO I guess wait for Done by waiting for CenterPtr of Memorys+1 to be nonzero in listResrc
	config = {} readConfig(list,config,{"ScratchFrames","UniformWid","UniformHei"})
	frames = config[1] width = config[2] height = config[3]
	print("frames:"..frames.." width:"..width.." height:"..height)
	-- for i = 0, (castMicro("Micros")-1) do listResrc(list,"PipeRes",{i,i}--[[IDerIns Micro]]) end
	--[[for i = 0, frames-1 do listResrc(list,"ChainRes",{}) end
	listMemory(list,"Uniformz","uni",{"Uniform(all:0one:1idx:0use:0tri:0num:0vtx:0mat:0bas:0pro:1wid:"..width.."hei:"..height..")"})
	dat,wid,hei,cha = fmtxStbi("texture.jpg")
	listMemory(list,"Imagez","img",{"Image(dat:"..showDat(dat,"").."wid:"..wid.."hei:"..hei.."cha:"..cha..")"})
	listMemory(list,"Storagez","sto",{"Int32(456)"})
	ident = "Matrix("
	for i = 0, 15 do ident = ident.."mat["..i.."]:Old("
	if (i//4) == (i-((i//4)*4)) then ident = ident.."1.0)"
	else ident = ident.."0.0)" end end
	ident = ident..")"
	mat = {} for i = 0, 4 do mat[i+1] = ident end
	for i = 0, frames-1 do listMemory(list,"Matrixz","mat",mat) end
	--
	ver={}
	ver[1]="Vertex(vec[0]:-0.5vec[1]:-0.5vec[2]:0.4vec[3]:1.0ord[0]:1.0ord[1]:0.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[2]="Vertex(vec[0]: 0.5vec[1]:-0.5vec[2]:0.4vec[3]:1.0ord[0]:0.0ord[1]:0.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[3]="Vertex(vec[0]: 0.5vec[1]: 0.5vec[2]:0.4vec[3]:1.0ord[0]:0.0ord[1]:1.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[4]="Vertex(vec[0]:-0.5vec[1]: 0.5vec[2]:0.4vec[3]:1.0ord[0]:1.0ord[1]:1.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[5]="Vertex(vec[0]:-0.5vec[1]:-0.5vec[2]:0.5vec[3]:1.0ord[0]:1.0ord[1]:0.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[6]="Vertex(vec[0]: 0.5vec[1]:-0.5vec[2]:0.5vec[3]:1.0ord[0]:0.0ord[1]:0.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[7]="Vertex(vec[0]: 0.5vec[1]: 0.5vec[2]:0.5vec[3]:1.0ord[0]:0.0ord[1]:1.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	ver[8]="Vertex(vec[0]:-0.5vec[1]: 0.5vec[2]:0.5vec[3]:1.0ord[0]:1.0ord[1]:1.0ord[2]:0.0ord[3]:0.0ref[0]:0ref[1]:1ref[2]:0ref[3]:0)"
	listMemory(list,"Bringupz","ver",ver) -- FetchPhs 0
	--
	-- writeConfig(list,{(1<<castVerbose("PipeVrb"))},{"RegisterVerb"})
	-- readConfig(list,config,{"RegisterVerb"})
	--
	idt={}
	idt[1]="Int32(3)";idt[2]="Int32(3)";idt[3]="Int32(3)";idt[4]="Int32(3)"
	idt[5]="Int32(4)";idt[6]="Int32(4)";idt[7]="Int32(4)";idt[8]="Int32(4)"
	-- writeCent(list,"Identz",0,0,"idt",idt) -- FetchPhs 1
	listSpoof(list,"Identz","idt",idt) -- FetchPhs 1
	-- listMemory(list,"Identz","idt",idt) -- FetchPhs 1
	--
	-- writeConfig(list,{0},{"RegisterVerb"})
	-- readConfig(list,config,{"RegisterVerb"})
	--
	ind={}
	ind[1]="Int32(0)";ind[2]="Int32(1)";ind[3]="Int32(2)";ind[4]="Int32(2)";ind[5]="Int32(3)";ind[6]="Int32(0)";
	ind[7]="Int32(4)";ind[8]="Int32(5)";ind[9]="Int32(6)";ind[10]="Int32(6)";ind[11]="Int32(7)";ind[12]="Int32(4)";
	listSpoof(list,"Indexz","ind",ind) -- IndexPhs 0
	--
	config[1] = 0 while(config[1] ~= 1) do
	list[#list+1] = machSugar("Machine(xfr:Stagesiz:1sav[0]:CenterPtridx[0]:$(@index))")
	readConfig(list,config,{"CenterPtr"}) end--]]
	atomSugar(list,tests[found]["idx"],"Test")
end

function runTest()
	-- TODO draw and manipulate with Demo
end

if #tests == 2 and found > 0 and tests[pass]["typ"] == "Filez" then
	pipeTest()
	if more then
	initTest()
	runTest()
	flushTest()
	else
	doneTest()
	end
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
