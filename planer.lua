dofile("type.lua")
center = {["cmd"]="SetCmd",["mem"]="Configurez",["siz"]=0,["idx"]=0,["slf"]=0}
ident = wrapType("Planez","planerLua","planeSw")
os.execute("sleep 1")
writeCenter(center,ident)
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: planeSw\n"); os.exit(-1) end
