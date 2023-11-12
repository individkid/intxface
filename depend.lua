
indent = 0
prefix = ""
verbose = false
function indentwrite(line)
	local count = 0
	if prefix == "" or verbose then
		while indent > count do io.stderr:write("- "); count = count + 1 end
		io.stderr:write(prefix..line)
	end
	prefix = ""
end

function writeout(filename,depends)
	local keys = {}
	local file = io.open(filename,"w")
	for k,_ in pairs(depends) do keys[#keys+1] = k end
	table.sort(keys)
	for _,k in ipairs(keys) do
		local vals = {}
		for v,_ in pairs(depends[k]) do vals[#vals+1] = v end
		table.sort(vals)
		file:write(k..":")
		for _,v in ipairs(vals) do file:write(" "..v) end
		file:write("\n")
	end
	file:close()
end

function callmatch(values,line,pat)
	indent = indent + 1; indentwrite("callmatch: "..line.."\n",indent)
	values[2] = string.match(line,pat)
	indent = indent - 1; return (values[2] ~= nil)
end

function makecopy(values,target,suf)
	local match = values[2]..suf
	local depends = values[1]
	local file = io.open("subdir."..match.."/"..match,"r")
	indent = indent + 1; indentwrite("makecopy: "..match.." "..target.."\n",indent)
	if file == nil and trymake(match) == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	for line in io.lines("subdir."..match.."/depend.mk") do
		local depender,tail = string.match(line,"^ *([%w.]*) *: *(.*)$")
		while tail ~= nil do
			local head,next = string.match(tail,"^([%w.]*) *(.*)$")
			tail = next
			if head == nil or head == "" then break end
			if depends[depender] == nil then depends[depender] = {} end
			depends[depender][head] = true
		end
	end
	if depends[target] == nil then depends[target] = {} end
	depends[target][match] = true
	os.execute("cp subdir."..match.."/"..match.." subdir."..target.."/")
	indent = indent - 1; return true
end

function copysource(values,target,ext)
	local match = values[2]
	local depends = values[1]
	local file = io.open(match.."."..ext,"r")
	indent = indent + 1; indentwrite("copysource: "..match.." "..target.." "..ext.."\n",indent)
	if file == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	if depends[target] == nil then depends[target] = {} end
	depends[target][match.."."..ext] = true
	os.execute("cp "..match.."."..ext.." subdir."..target.."/")
	indent = indent - 1; return true
end

function findsource(values,pre,post,ext,suf)
	local match = values[2]
	local found = nil
	local file = io.popen("grep -s -l -E '"..pre..match..post.."' *."..ext,"r")
	indent = indent + 1; indentwrite("findsource: "..match.." "..ext.." "..suf.."\n",indent)
	while true do
		local line = file:read()
		if line == nil then break end
		if found ~= nil then indent = indent - 1; return false end
		found = string.match(line,"^([%w]*)."..ext.."$")
	end
	io.close(file)
	if found == nil then indent = indent - 1; return false end
	values[2] = found..suf
	indent = indent - 1; return true
end

function trymatch(values,target)
	indent = indent + 1; indentwrite("trymatch: "..target.."\n",indent)
	for line in io.lines("stderr."..target) do
		prefix = "1a "; if callmatch(values,line,"^/bin/sh: ./([%w]*C): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "1b "; if callmatch(values,line,"^/bin/sh: ./([%w]*Lua): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "2a "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2b "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C.o'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2c "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Cpp.o'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "2d "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Lua'.  Stop.$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "2e "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Hs'.  Stop.$") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "2f "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).so'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		prefix = "2g "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and makecopy(values,target,".c") then indent = indent - 1; return true end
		prefix = "2h "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2i "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).gen', needed by `[%w]*.dep'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2j "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).h'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2k "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Lua'.  Stop.$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		prefix = "2l "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).lua'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2m "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Hs'.  Stop.$") and makecopy(values,target,".hs") then indent = indent - 1; return true end
		prefix = "2n "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).hs'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2o "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c', needed by `[%w]*C.o'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2p "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).h', needed by `[%w]*C.o'.  Stop.$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "2q "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).cpp', needed by `[%w]*Cpp.o'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "2r "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).hs', needed by `[%w]*Hs'.  Stop.$") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "3a "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "3b "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and makecopy(values,target,".h") then indent = indent - 1; return true end
		prefix = "4a "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(","c","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "4b "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(","cpp","Cpp.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "5 "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"^module "," where$","hs","") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "6 "; if callmatch(values,line,"^[%w]*: cannot execute file: ([%w]*)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "7a "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "7b "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		prefix = "8 "; if callmatch(values,line,"^lua: [%w./]*:[%d]*: module '([%w]*)' not found:$") and makecopy(values,target,".so") then indent = indent - 1; return true end
	end
	-- os.execute("rm -rf subdir.*")
	indentwrite("trymatch: no match: stderr."..target..":\n",indent)
	for line in io.lines("stderr."..target) do io.stderr:write(line.."\n") end
	os.exit(-1)
end

function trymake(target)
	indent = indent + 1; indentwrite("trymake: "..target.."\n",indent)
	os.execute("rm -rf subdir."..target.." stderr."..target.." stdout."..target)
	os.execute("mkdir subdir."..target)
	os.execute("cp Makefile subdir."..target)
	local depends = {} -- map from depender to set of dependee
	local values = {depends,nil,nil,nil} -- depend.mk, first match, second match, saved match
	while true do
		writeout("subdir."..target.."/depend.mk",depends)
		os.execute("make -C subdir."..target.." "..target.." 2> stderr."..target.." > stdout."..target)
		os.execute("if [ ! -e subdir."..target.."/"..target.." ] ; then echo \"trymake: failed to make "..target.."\" >> stderr."..target.." ; fi")
		local done = true
		for line in io.lines("stderr."..target) do done = false end
		if done then
			os.execute("rm -rf stderr."..target.." stdout."..target)
			indent = indent - 1; return depends
		end
		if not trymatch(values,target) then break end
	end
	indent = indent - 1; return nil
end

depends = {}
for _,target in ipairs({"facer.log","typra.log","typer.log","filer.log","planra.log","planer.log","spacra.log","spacer.log","hole","line","metal","space","pipe","page","share"}) do
	for der,dee in pairs(trymake(target)) do
		if depends[der] == nil then depends[der] = {} end
		for k,_ in pairs(dee) do depends[der][k] = true end
	end
end
writeout("depend.mk",depends)
