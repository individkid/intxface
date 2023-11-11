
indent = 0
prefix = ""
verbose = false
function indentwrite(line)
	local count = 0
	if prefix == "" or verbose then
		while indent > count do io.stderr:write(" "); count = count + 1 end
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

function copymake(values,target)
	local match = values[2]
	local depends = values[1]
	local file = io.open("subdir."..match.."/"..match,"r")
	indent = indent + 1; indentwrite("copymake: "..match.." "..target.."\n",indent)
	if file == nil and trymake(match) == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	for line in io.lines("subdir."..match.."/depend.mk") do
		local depender,tail = string.match(line,"^ *([%w]*) *: *(.*)$")
		while tail ~= nil do
			local head,next = string.match(tail,"^([%w]*) *(.*)$")
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

function findsource(values,ext,suf)
	local match = values[2]
	local found = nil
	local file = io.popen("grep -l -E '^[^[:space:]][^[:space:]]* *"..match.."\\(' *."..ext,"r")
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
		--  matches can append to depends, copy from other subdirs, copy from source, recurse to another target
		prefix = "1 "; if callmatch(values,line,"^/bin/sh: ./([%w]*C): No such file or directory$") and copymake(values,target) then indent = indent - 1; return true end
		prefix = "2 "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "3 "; if callmatch(values,line,"^[%w]*.c:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "4 "; if callmatch(values,line,"^./[%w]*.h:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "5 "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"c","C.o") and copymake(values,target) then indent = indent - 1; return true end
		prefix = "6 "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C.o'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "7 "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"cpp","Cpp.o") and copymake(values,target) then indent = indent - 1; return true end
		prefix = "8 "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Cpp.o'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "9 "; if callmatch(values,line,"^[%w]*.cpp:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
	end
	os.execute("rm -rf subdir.*")
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

depends = trymake("all")
if depends then writeout("depend.mk",depends) end
