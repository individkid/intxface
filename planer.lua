dofile("type.lua")
ident = forkExec("planeSw")
if ident == -1 then io.stderr:write("planerLua: cannot execute file: planeSw\n"); os.exit(-1) end
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: planeSw\n"); os.exit(-1) end
