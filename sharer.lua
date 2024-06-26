dofile("type.lua")
file = io.popen("uname", 'r')
uname = file:read()
file:close()
if #arg > 0 then
	argument = initWrap(arg[1]); idx = argument["idx"]
	term = hidePersist("Persist(act:Goal(Goals))")
	stim = hidePersist("Persist(act:Goal(NewHub)idx:Int(-1)str:Str(hello))"); expected = showPersist(stim,"");
	writePersist(stim,idx); actual = showPersist(readPersist(idx),"")
	if expected ~= actual then io.stderr:write("sharerLua: expected ~= actual\n"); os.exit(-1) end
	writePersist(term,idx); os.exit(0)
end
if uname == "Darwin" then
	cmd = "gtimeout 5s ./shareC"
else
	cmd = "timeout 5s ./shareC"
end
cmd = cmd..
	" 'Execute(typ:Str(Persist)url:Str(sharerLua)arg[0]:Argument(typ:Program(Programs)inp:Int(-1)out:Int(-1)idx:Int(-1)))'"..
	" 'Fanout(siz:Int(2)dst[0]:Str(fanout)dst[1]:Str()typ:Str(Persist)str:Str(fanout))'"
if not os.execute(cmd) then io.stderr:write("sharerLua: cannot execute file: shareC\n"); os.exit(-1) end
