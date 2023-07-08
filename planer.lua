dofile("type.lua")
ident = wrapType("Planez","planerLua","metalSw")
os.execute("sleep 1")
center = {["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=0,["cfg"]={"PierceSize"},["val"]={1}}
writeCenter(center,ident)
stimulus = hideCenter("Center(mem:Memory(Piercez)siz:Int(1)idx:Int(0)slf:Int(0)pie[0]:Pierce("..
	"fix[0]:Old(0.000000)fix[1]:Old(0.100000)fix[2]:Old(0.200000)fix[3]:Old(0.300000)"..
	"nml[0]:Old(1.000000)nml[1]:Old(1.100000)nml[2]:Old(1.200000)nml[3]:Old(1.300000)"..
	"vld:Int32(1)idx:Int32(0)pad[0]:Int32(0)pad[1]:Int32(0)))")
writeCenter(stimulus,ident)
center = {["mem"]="Piercez",["siz"]=0,["idx"]=0,["slf"]=2}
writeCenter(center,ident)
result = readCenter(ident)
if showCenter(stimulus,"") ~= showCenter(result,"") then io.stderr:write("planerLua: mismatch\n"); os.exit(-1) end
os.execute("sleep 1")
center = {["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=3,["cfg"]={"RegisterOpen"},["val"]={0}}
writeCenter(center,ident)
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: metalSw\n"); os.exit(-1) end
