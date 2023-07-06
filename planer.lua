dofile("type.lua")
ident = wrapType("Planez","planerLua","metalSw")
os.execute("sleep 1")
center = {["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=0,["cfg"]={"PierceSize"},["val"]={1}}
writeCenter(center,ident)
center = {["mem"]="Piercez",["siz"]=1,["idx"]=0,["slf"]=1,["pie"]={{["fix"]={0.0,0.1,0.2,0.3},["nml"]={1.0,1.1,1.2,1.3},["vld"]=1,["idx"]=0,["pad"]={0,0}}}}
writeCenter(center,ident)
-- center = {["mem"]="Piercez",["siz"]=0,["idx"]=0,["slf"]=2}
-- writeCenter(center,ident)
-- center = readCenter(ident)
-- io.stdout:write("mem %s\n",center["mem"])
os.execute("sleep 1")
center = {["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=3,["cfg"]={"RegisterOpen"},["val"]={0}}
writeCenter(center,ident)
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: metalSw\n"); os.exit(-1) end
