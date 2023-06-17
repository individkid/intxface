dofile("type.lua")
center = {["req"]="SetReq",["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=0,["cfg"]={"RegisterOpen"},["val"]={0}}
ident = wrapType("Planez","planerLua","metalSw")
os.execute("sleep 1")
writeCenter(center,ident)
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: metalSw\n"); os.exit(-1) end
