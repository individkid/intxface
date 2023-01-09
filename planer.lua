dofile("type.lua")
center = {["cmd"]="SetCmd",["mem"]="Configurez",["siz"]=0,["idx"]=0,["slf"]=0}
ident = forkExec("planeSw")
if ident == -1 then io.stderr:write("planerLua: cannot execute file: planeSw\n"); os.exit(-1) end
os.execute("sleep 1")
writeCenter(center,ident)
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: planeSw\n"); os.exit(-1) end
