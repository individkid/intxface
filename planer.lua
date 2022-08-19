dofile("type.lua")
ident = forkExec("plane")
if ident == -1 then io.stderr:write("planerLua: cannot execute file: plane\n"); os.exit(-1) end
sub = waitAny()
if not (sub == ident) then io.stderr:write("unexpected ident\n"); os.exit(-1) end
readEof(ident)