if #arg > 0 then
	-- write and read some type, write again to terminate shareC, and exit successfully
	io.stderr:write("sharerLua: cannot filter Persist\n"); os.exit(-1)
end
cmd = "gtimeout 5s ./shareC"..
	" 'Execute(typ:Str(Persist)url:Str(sharerLua)arg[0]:Argument(typ:Process(Processs)inp:Int(-1)out:Int(-1)idx:Int(-1)))'"..
	" 'Fanout(siz:Int(1)dst[0]:Str()typ:Str(Persist)str:Str(fanout))'"
if not os.execute(cmd) then io.stderr:write("sharerLua: cannot execute file: shareC\n"); os.exit(-1) end
