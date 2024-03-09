
file = io.popen("uname", 'r')
uname = file:read()
file:close()
dbgline = {}
dbgent = 0

function indentwrite(line,indent)
	local count = 0
	while indent > count do io.stderr:write("- "); count = count + 1 end
	io.stderr:write(line)
end

function writeout(dirname,depends)
	local file = io.open(dirname.."/depend.mk","w")
	local keys = {}
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

function writextra(dirname,extras)
	for key,val in pairs(extras) do
 		local file = io.open(dirname.."/"..key..".mk","w")
		local keys = {}
		for k,_ in pairs(val) do keys[#keys+1] = k end
		table.sort(keys)
		for _,k in ipairs(keys) do file:write(" "..k) end
		file:write("\n")
		file:close()
	end
end

function getdepend(target)
	local depender = nil
	for line in io.lines("stdout."..target) do
		local maybe = nil
		if uname == "Linux" then
			maybe = string.match(line,"^ *Must remake target '([%w.]*)'.$")
		else
			maybe = string.match(line,"^ *Must remake target `([%w.]*)'.$")
		end
		if maybe ~= nil then depender = maybe end
	end
	return depender
end

function copydepend(match,depends)
	for line in io.lines("subdir."..match.."/depend.mk") do
		local depender,tail = string.match(line,"^ *([%w.]*) *: *(.*)$")
		if depends[depender] == nil then depends[depender] = {} end
		while tail ~= nil do
			local head,next = string.match(tail,"^([%w.]*) *(.*)$")
			tail = next
			if head == nil or head == "" then break end
			if depender ~= nil and depender ~= head then depends[depender][head] = true end
		end
	end
end

function adddepend(match,depends,depender)
	if depender ~= nil and depender ~= match then
		if depends[depender] == nil then depends[depender] = {} end
		depends[depender][match] = true
	end
end

function addextra(base,extras,func)
	if extras[base] == nil then extras[base] = {} end
	if func ~= nil then extras[base][func] = true end
end

function callmatch(values,line,pat)
	local first,second = string.match(line,pat)
	if second then values[3] = second; values[4] = first; values[5] = second
	else values[3] = first; values[4] = first; values[5] = first end
	if (values[3] == nil) then return false end
	dbgline[dbgent] = dbgline[dbgent].." callmatch:"..values[4]..":"..values[5]
	return true
end

function checksource(values,ext)
	local match = values[3]
	local file = io.open(match..ext,"r")
	if file == nil then return false end
	io.close(file)
	dbgline[dbgent] = dbgline[dbgent].." checksource:"..match
	return true
end

function findsource(values,pre,post,ext)
	local match = values[3]
	local found = nil
	local file = io.popen("grep -s -l -E '"..pre..match..post.."' *"..ext,"r")
	while true do
		local line = file:read()
		if line == nil then break end
		if found ~= nil then return false end
		found = string.match(line,"^([%w]*)"..ext.."$")
	end
	io.close(file)
	if found == nil then return false end
	values[3] = found
	dbgline[dbgent] = dbgline[dbgent].." findsource:"..found
	return true
end

function makecopy(values,target,suf)
	local match = values[3]..suf -- file.c
	local depends = values[1]
	local extras = values[2]
	local file = io.open("subdir."..match.."/"..match,"r")
	local depender = getdepend(target) -- fileC.o
	if file == nil and not trymake({{},extras},match) then return false end
	if file ~= nil then io.close(file) end
	copydepend(match,depends)
	adddepend(match,depends,depender)
	if (match == target) then return false end
	os.execute("cp subdir."..match.."/"..match.." subdir."..target.."/")
	dbgline[dbgent] = dbgline[dbgent].." makecopy:"..target..":"..depender..":"..match
	return true
end

function remakecopy(values,target,suf)
	local match = values[3]..suf -- wrapCpp.o
	local depends = values[1]
	local extras = values[2]
	local depender = values[4] -- luax.so
	local deps = {}
	local exts = {}
	local maybe = "nil"
	if depender then maybe = depender end
	copydepend(depender,deps)
	adddepend(match,deps,depender)
	if not trymake({deps,extras},depender) then return false end
	copydepend(depender,depends)
	os.execute("cp subdir."..depender.."/"..depender.." subdir."..target.."/")
	dbgline[dbgent] = dbgline[dbgent].." remakecopy:"..target..":"..depender..":"..match
	return true
end

function copysource(values,target,ext)
	local match = values[3]..ext
	local depends = values[1]
	local file = io.open(match,"r")
	local depender = getdepend(target)
	if file == nil then return false end
	io.close(file)
	adddepend(match,depends,depender)
	os.execute("cp "..match.." subdir."..target.."/")
	dbgline[dbgent] = dbgline[dbgent].." copysource:"..target..":"..depender..":"..match
	return true
end

function copyxtra(values,target,ext)
	local match = values[3]..ext -- type.gen
	local base = values[3]
	local extras = values[2]
	local file = io.open(match,"r")
	if file == nil then return false end
	io.close(file)
	addextra(base,extras,nil)
	dbgline[dbgent] = dbgline[dbgent].." copyxtra:"..match
	return true
end

function recopyxtra(values,target,ext)
	local match = values[3]..ext -- type.gen
	local base = target..values[3]
	local extras = values[2]
	local func = values[4]..values[5]
	addextra(base,extras,func)
	values[3] = base
	dbgline[dbgent] = dbgline[dbgent].." recopyxtra:"..base..":"..func
	return true
end

function trydone()
	dbgent = dbgent - 1
	return true
end
function trymatch(values,target)
	dbgent = dbgent + 1
	for line in io.lines("stderr."..target) do if uname == "Linux" then
		dbgline[dbgent] = "1a"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)'.  Stop.$") and checksource(values,".c") and makecopy(values,target,"C") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1b"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)C'.  Stop.$") and makecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1c"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)C.o'.  Stop.$") and copysource(values,target,".c") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1d"; if callmatch(values,line,"^make: *** No rule to make target '(%w*).so'.  Stop.$") and makecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1e"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)Cpp.o'.  Stop.$") and copysource(values,target,".cpp") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1f"; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Lua'.  Stop.$") and copysource(values,target,".lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1g"; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).lua'.  Stop.$") and copysource(values,target,".lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1h"; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Hs'.  Stop.$") and copysource(values,target,".hs") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "1i"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)C.o'.  Stop.$") and copyxtra(values,target,".gen") and makecopy(values,target,".c") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "2a"; if callmatch(values,line,"^.*: fatal error: (%w*).h: No such file or directory$") and copysource(values,target,".h") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "2b"; if callmatch(values,line,"^.*: fatal error: (%w*).h: No such file or directory$") and copyxtra(values,target,".gen") and makecopy(values,target,".h") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "3a"; if callmatch(values,line,"^lua: cannot open (%w*).gen: No such file or directory$") and copysource(values,target,".gen") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "3b"; if callmatch(values,line,"^lua: cannot open (%w*).lua: No such file or directory$") and copysource(values,target,".lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "3b"; if callmatch(values,line,"^lua: cannot open (%w*).lua: No such file or directory$") and copyxtra(values,target,".gen") and makecopy(values,target,".lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "4a"; if callmatch(values,line,"^lua: [%w./]*:[%d]*: module '(%w*)' not found:$") and makecopy(values,target,".so") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "5a"; if callmatch(values,line,"^.*./([%w.]*): undefined symbol: (%w*)$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(.*\\)$",".cpp") and remakecopy(values,target,"Cpp.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "5b"; if callmatch(values,line,"^.*./([%w.]*): undefined symbol: (%w*)$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(.*\\)$",".c") and remakecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6a"; if callmatch(values,line,"^make: *** No rule to make target '(%w*).c', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".c") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6b"; if callmatch(values,line,"^make: *** No rule to make target '(%w*).h', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".h") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6c"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)Cpp.o', needed by '[%w.]*'.  Stop.$") and makecopy(values,target,"Cpp.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6d"; if callmatch(values,line,"^make: *** No rule to make target '(%w*).cpp', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".cpp") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6e"; if callmatch(values,line,"^make: *** No rule to make target '(%w*)C.o', needed by '[%w.]*'.  Stop.$") and makecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6f"; if callmatch(values,line,"^make: *** No rule to make target '(%w*).lua', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "6g"; if callmatch(values,line,"^make: *** No rule to make target '(%w*).gen', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".gen") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "7a"; if callmatch(values,line,"^.*: undefined reference to `(%w*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$",".c") and makecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "7d"; if callmatch(values,line,"^.*: undefined reference to `(%w*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$",".cpp") and makecopy(values,target,"Cpp.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "7b"; if callmatch(values,line,"^.*: undefined reference to `([a-z]*)([A-Z]%w*)'$") and findsource(values,"^"," = {",".gen") and recopyxtra(values,target,".gen") and makecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "7c"; if callmatch(values,line,"^.*: undefined reference to '(%w*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$",".c") and makecopy(values,target,"C.o") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "8a"; if callmatch(values,line,"^.*: %W*(%w*)C: not found$") and makecopy(values,target,"C") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "8b"; if callmatch(values,line,"^.*: %W*(%w*)Lua: not found$") and makecopy(values,target,"Lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "8c"; if callmatch(values,line,"^.*: %W*(%w*)Hs: not found$") and makecopy(values,target,"Hs") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "9a"; if callmatch(values,line,"^.*: cannot execute file: (%w*)Lua$") and makecopy(values,target,"Lua") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "9b"; if callmatch(values,line,"^.*: cannot execute file: (%w*)Hs$") and makecopy(values,target,"Hs") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "9c"; if callmatch(values,line,"^.*: cannot execute file: (%w*)C$") and makecopy(values,target,"C") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "10a"; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"^module "," where$",".hs") and copysource(values,target,".hs") then dbgent = dbgent - 1; return true end
		dbgline[dbgent] = "10b"; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"module "," where",".gen") and makecopy(values,target,".hs") then dbgent = dbgent - 1; return true end
	elseif uname == "Darwin" then
	end end
	for line in io.lines("stderr."..target) do io.stderr:write(line.."\n") end
	os.exit(-1)
end

function trymake(values,target)
	local depends = values[1]
	local extras = values[2]
	indentwrite("trymake:"..target.."\n",dbgent)
	os.execute("rm -f subdir."..target.."/"..target.." stderr."..target.." stdout."..target)
	os.execute("mkdir -p subdir."..target)
	os.execute("cp Makefile module.modulemap subdir."..target)
	while true do
		writeout("subdir."..target,depends)
		writextra("subdir."..target,extras)
		os.execute("make -d -C subdir."..target.." "..target.." 2> stderr."..target.." >> stdout."..target)
		local file = io.open("subdir."..target.."/"..target,"r");
		-- file should not be nil if extras is set up before trymake of generated file
		if (file == nil) then os.execute("echo error:"..target..": make passed but build failed >> stderr."..target)
		else io.close(file) end
		local done = true
		for line in io.lines("stderr."..target) do done = false end
		if done then
			os.execute("rm -rf stderr."..target.." stdout."..target)
			indentwrite("subdir."..target.."\n",dbgent)
			return true
		end
		if not trymatch(values,target) then break end
		indentwrite("subdir."..target..":"..dbgline[dbgent+1].."\n",dbgent+1); dbgline[dbgent+1] = nil
	end
	return false
end

targets = {} -- list of top level targets
depends = {} -- map from depender to set of dependees
extras = {} -- map from .gen name to map from generated file to set of functions
for line in io.lines("Makefile") do
	local all = string.match(line,"^all *: *(.*)$")
	if all ~= nil then
		for word in all:gmatch("%S+") do
			targets[#targets+1] = word
		end
		break
	end

end
for key,val in ipairs(targets) do
	local values = {{},{}}
	if not trymake(values,val) then io.stderr("trymake failed\n"); os.exit(-1) end
	for k,v in pairs(values[1]) do
		if (depends[k] == nil) then depends[k] = {} end
		for ky,vl in pairs(v) do depends[k][ky] = true end
	end
	for k,v in pairs(values[2]) do
		if (extras[k] == nil) then extras[k] = {} end
		for ky,vl in pairs(v) do extras[k][ky] = true end
	end
end
writeout(".",depends)
writextra(".",extras)
io.stdout:write("all done\n")
