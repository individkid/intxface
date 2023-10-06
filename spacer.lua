dofile("type.lua")
initialize = hideChange("Change(cfg:Emerg(Boundaries)idx:Int(0)siz:Int(0))")
ident = wrapType("Spacez","spacerLua","spaceHs")
writeChange(initialize,ident)
if waitExit() < 0 then io.stderr:write("spacerLua: bad exit status: spaceHs\n"); os.exit(-1) end
