dofile("type.lua")
initialize = hideChange("Change(cfg:Emerg(Numerics)idx:Int(0)siz:Int(0))")
finalize = hideChange("Change(cfg:Emerg(Emergs)idx:Int(0)siz:Int(0))")
ident = wrapType("Spacez","spacerLua","spaceHs")
writeChange(initialize,ident)
writeChange(finalize,ident)
if waitExit() < 0 then io.stderr:write("spacerLua: bad exit status: spaceHs\n"); os.exit(-1) end
