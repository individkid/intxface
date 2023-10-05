dofile("type.lua")
initialize = hideSidedness("Sidedness(cfg:Emergent(Boundaries)siz:Int(0))")
ident = wrapType("Spacez","spacerLua","spaceHs")
writeSidedness(initialize,ident)
if waitExit() < 0 then io.stderr:write("spacerLua: bad exit status: spaceHs\n"); os.exit(-1) end
