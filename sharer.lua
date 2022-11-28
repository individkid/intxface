dofile("type.lua")
ident = forkExec("share")
if ident == -1 then io.stderr:write("sharerLua: cannot execute file: share\n"); os.exit(-1) end
sub = waitRead(0.0,-1)
if not (sub == ident) then io.stderr:write("unexpected ident\n"); os.exit(-1) end
readEof(ident)
