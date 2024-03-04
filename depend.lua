
indent = 0
saved = {}
file = io.popen("uname", 'r')
uname = file:read()
file:close()

function indentwrite(line)
	local count = 0
	while indent > count do io.stderr:write("- "); count = count + 1 end
	io.stderr:write(line)
end

function writeout(filename,depends)
	local keys = {}
	local file = io.open(filename,"w")
	if (depends ~= nil) then for k,_ in pairs(depends) do keys[#keys+1] = k end end
	table.sort(keys)
	for _,k in ipairs(keys) do
		local vals = {}
		local line = ""
		for v,_ in pairs(depends[k]) do vals[#vals+1] = v end
		table.sort(vals)
		file:write(k..":"); line = k..":"
		for _,v in ipairs(vals) do file:write(" "..v); line = line.." "..v end
		file:write("\n"); line = line.."\n"
		-- indentwrite("writeout: "..line)
	end
	file:close()
end

function getdepender(target)
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

function getdependee(target)
	local depender = nil
	for line in io.lines("stderr."..target) do
		local maybe = string.match(line,"./([%w]*.so): undefined symbol: [%w]*$")
		if maybe ~= nil then return maybe end
	end
	return nil
end

function callmatch(values,line,pat)
	local first,second = string.match(line,pat)
	if second then values[3] = second; values[4] = first..second
	else values[3] = first; values[4] = first end
	return (values[3] ~= nil)
end

function copydepender(match,depends)
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
function adddepender(match,depends,depender)
	if depender ~= nil and depender ~= match then
		if depends[depender] == nil then depends[depender] = {} end
		depends[depender][match] = true
	end
end

function makecopy(values,target,suf)
	local match = values[3]..suf -- face.c
	local depends = values[1]
	local extras = values[2]
	local file = io.open("subdir."..match.."/"..match,"r")
	local depender = getdepender(target) -- faceC.o
	indent = indent + 1; indentwrite("makecopy: "..saved[indent-1].."\n",indent)
	if file == nil and not trymake({{},extras},match) then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	copydepender(match,depends)
	adddepender(match,depends,depender)
	indentwrite("makecopy: cp subdir."..match.."/"..match.." subdir."..target.."/\n")
	os.execute("cp subdir."..match.."/"..match.." subdir."..target.."/")
	indent = indent - 1; return true
end

function remakecopy(values,target,suf)
	local match = values[3]..suf -- wrapCpp.o
	local depends = values[1]
	local extras = values[2]
	local depender = getdependee(target) -- luax.so
	local deps = {}
	local exts = {}
	indent = indent + 1; indentwrite("remakecopy: "..saved[indent-1].."\n",indent)
	copydepender(depender,deps)
	adddepender(match,deps,depender)
	if not trymake({deps,extras},depender) then indent = indent - 1; return false end
	copydepender(depender,depends)
	indentwrite("remakecopy: cp subdir."..depender.."/"..depender.." subdir."..target.."/\n")
	os.execute("cp subdir."..depender.."/"..depender.." subdir."..target.."/")
	indent = indent - 1; return true
end

function copysource(values,target,ext)
	local match = values[3]
	local depends = values[1]
	local file = io.open(match..ext,"r")
	local depender = getdepender(target)
	indent = indent + 1; indentwrite("copysource: "..match.." "..target.." "..ext.."\n",indent)
	if file == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	if depender ~= nil and depender ~= match..ext then
		if depends[depender] == nil then depends[depender] = {} end
		depends[depender][match..ext] = true
		-- if depender == "facerC" and match..ext == "face.hs" then io.stderr:write("here3 "..saved[indent].."\n"); os.exit(-1) end
	end
	os.execute("cp "..match..ext.." subdir."..target.."/")
	indent = indent - 1; return true
end

function copymake(values,target)
	local match = values[3]
	local depends = values[1]
	local extras = values[2]
	local file = io.open(match..".gen","r")
	local depender = getdepender(target)
	indent = indent + 1; indentwrite("copymake: "..saved[indent-1].."\n",indent)
	if file == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	if depender ~= nil and depender ~= match..".gen" then
		if depends[depender] == nil then depends[depender] = {} end
		depends[depender][match..".gen"] = true
		if extras[match] == nil then extras[match] = {} end
		if extras[match][depender] == nil then extras[match][depender] = {} end
	end
	os.execute("cp "..match..".gen subdir."..target.."/")
	indent = indent - 1; return true
end

function copyextra(values,target,suf,dep)
	local base = values[3]
	local match = values[3]..suf -- type.c
	local depender = values[3]..dep -- typeC.o
	local extras = values[2]
	local func = values[4]
	indent = indent + 1; indentwrite("copyextra: "..saved[indent-1].."\n",indent)
	if extras[base] == nil then extras[base] = {} end
	if extras[base][match] == nil then extras[base][match] = {} end
	extras[base][match][func] = true
	indentwrite("copyextra: rm -rf subdir."..target.."/"..depender.."\n")
	os.execute("rm -rf subdir."..target.."/"..depender)
	indentwrite("copyextra: rm -rf subdir."..depender.." stderr."..depender.." stdout."..depender.."\n")
	os.execute("rm -rf subdir."..depender.." stderr."..depender.." stdout."..depender)
	indentwrite("copyextra: rm -rf subdir."..match.." stderr."..match.." stdout."..match.."\n")
	os.execute("rm -rf subdir."..match.." stderr."..match.." stdout."..match)
	indent = indent - 1; return true
end

function findsource(values,pre,post,ext,suf)
	local match = values[3]
	local found = nil
	local file = io.popen("grep -s -l -E '"..pre..match..post.."' *."..ext,"r")
	indent = indent + 1; indentwrite("findsource: "..saved[indent-1].."\n",indent)
	indentwrite("findsource: grep -s -l -E '"..pre..match..post.."' *."..ext.."\n",indent)
	while true do
		local line = file:read()
		if line == nil then break end
		if found ~= nil then indent = indent - 1; return false end
		found = string.match(line,"^([%w]*)."..ext.."$")
	end
	io.close(file)
	if found == nil then indent = indent - 1; return false end
	values[3] = found..suf
	indent = indent - 1; return true
end

function checksource(values,ext)
	local match = values[3]
	local file = io.open(match.."."..ext,"r")
	if file == nil then return false end
	io.close(file)
	return true
end

function trydone()
	local sav = indent
	if saved[indent] then sav = saved[indent] end
	indentwrite("trymatch: "..sav.."\n",indent); indent = indent - 1; return true
end
function trymatch(values,target)
	indent = indent + 1; indentwrite("trymatch: "..target.."\n",indent)
	for line in io.lines("stderr."..target) do if uname == "Linux" then
		saved[indent] = "1a "; if callmatch(values,line,"^/bin/sh: [%d]*: ./([%w]*C): not found$") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "1b "; if callmatch(values,line,"^/bin/sh: [%d]*: ./([%w]*Lua): not found$") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "1c "; if callmatch(values,line,"^/bin/sh: [%d]*: ./([%w]*Hs): not found$") and makecopy(values,target,"") then return trydone() end
		-- saved[indent] = "2a "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C'.  Stop.$") and copysource(values,target,".c") then return trydone() end
		saved[indent] = "2b "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C.o'.  Stop.$") and makecopy(values,target,".c") then return trydone() end
		saved[indent] = "2c "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).c'.  Stop.$") and copysource(values,target,".c") then return trydone() end
		saved[indent] = "2d "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Lua'.  Stop.$") and copysource(values,target,".lua") then return trydone() end
		saved[indent] = "2e "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Hs'.  Stop.$") and copysource(values,target,".hs") then return trydone() end
		saved[indent] = "2f "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).so'.  Stop.$") and makecopy(values,target,"C.o") then return trydone() end
		saved[indent] = "2g "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C'.  Stop.$") and makecopy(values,target,"C.o") then return trydone() end
		saved[indent] = "2h "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).c'.  Stop.$") and copymake(values,target) then return trydone() end
		saved[indent] = "2i "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).h'.  Stop.$") and copymake(values,target) then return trydone() end
		saved[indent] = "2j "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Lua'.  Stop.$") and makecopy(values,target,".lua") then return trydone() end
		saved[indent] = "2k "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).lua'.  Stop.$") and copymake(values,target) then return trydone() end
		saved[indent] = "2l "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Hs'.  Stop.$") and makecopy(values,target,".hs") then return trydone() end
		saved[indent] = "2m "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).hs'.  Stop.$") and copymake(values,target) then return trydone() end
		saved[indent] = "2t "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp.o'.  Stop.$") and copysource(values,target,"Cpp.arg") then return trydone() end
		saved[indent] = "2n "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp.o'.  Stop.$") and makecopy(values,target,".cpp") then return trydone() end
		saved[indent] = "2o "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).cpp'.  Stop.$") and copysource(values,target,".cpp") then return trydone() end
		saved[indent] = "2p "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp'.  Stop.$") and makecopy(values,target,"Cpp.o") then return trydone() end
		saved[indent] = "2q "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)'.  Stop.$") and checksource(values,"c") and makecopy(values,target,"C") then return trydone() end
		saved[indent] = "2r "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)'.  Stop.$") and checksource(values,"cpp") and makecopy(values,target,"Cpp") then return trydone() end
		saved[indent] = "2s "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)'.  Stop.$") and checksource(values,"hs") and makecopy(values,target,"Hs") then return trydone() end
		saved[indent] = "2t "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).vsv'.  Stop.$") and copysource(values,target,".vg") then return trydone() end
		saved[indent] = "2u "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).fsv'.  Stop.$") and copysource(values,target,".fg") then return trydone() end
		saved[indent] = "3a "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp.o', needed by '[%w.]*'.  Stop.$") and makecopy(values,target,".cpp") then return trydone() end
		saved[indent] = "3b "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).c', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".c") then return trydone() end
		saved[indent] = "3c "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).h', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".h") then return trydone() end
		saved[indent] = "3d "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).cpp', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".cpp") then return trydone() end
		saved[indent] = "3e "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C.o', needed by '[%w.]*'.  Stop.$") and makecopy(values,target,".c") then return trydone() end
		saved[indent] = "3f "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).gen', needed by '[%w.]*'.  Stop.$") and copymake(values,target) then return trydone() end
		saved[indent] = "3g "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).lua', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".lua") then return trydone() end
		saved[indent] = "3h "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).hs', needed by '[%w.]*'.  Stop.$") and copysource(values,target,".hs") then return trydone() end
		saved[indent] = "4a "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: ([%w]*).h: No such file or directory$") and copysource(values,target,".h") then return trydone() end
		saved[indent] = "4b "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: ([%w]*).h: No such file or directory$") and makecopy(values,target,".h") then return trydone() end
		saved[indent] = "4c "; if callmatch(values,line,"^.*: fatal error: ([%w]*).cpp: No such file or directory$") and copysource(values,target,".cpp") then return trydone() end
		saved[indent] = "5a "; if callmatch(values,line,"^.*: undefined reference to `([%w]*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","c","C.o") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "5b "; if callmatch(values,line,"^.*: undefined reference to '([%w]*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","c","C.o") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "5c "; if callmatch(values,line,"^.*: undefined reference to `([a-z]*)([A-Z][%w]*)'$") and findsource(values,"^"," = {","gen","") and copyextra(values,target,".c","C.o") and makecopy(values,target,"C.o") then return trydone() end
		saved[indent] = "5e "; if callmatch(values,line,"^.*: undefined reference to `([a-z]*[A-Z][%w]*)'$") and findsource(values,"function ","\\(","gen","") and copyextra(values,target,".c","C.o") and makecopy(values,target,"C.o") then return trydone() end
		saved[indent] = "5d "; if callmatch(values,line,"^.*: undefined reference to `([%w]*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","cpp","Cpp.o") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "6a "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Lua)$") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "6b "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "6c "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*C)$") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "6d "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "7a "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"^module "," where$","hs","") and copysource(values,target,".hs") then return trydone() end
		saved[indent] = "7b "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"module "," where","gen",".hs") and makecopy(values,target,"") then return trydone() end
		saved[indent] = "8a "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and copysource(values,target,".lua") then return trydone() end
		saved[indent] = "8b "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and makecopy(values,target,".lua") then return trydone() end
		saved[indent] = "9a "; if callmatch(values,line,"^lua: [%w./]*:[%d]*: module '([%w]*)' not found:$") and makecopy(values,target,".so") then return trydone() end
		saved[indent] = "10a "; if callmatch(values,line,"[%w./]*%.so: undefined symbol: ([%w]*)$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(.*\\)$","cpp","Cpp.o") and remakecopy(values,target,"") then return trydone() end
		saved[indent] = "10b "; if callmatch(values,line,"[%w./]*%.so: undefined symbol: ([%w]*)$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(.*\\)$","c","C.o") and remakecopy(values,target,"") then return trydone() end
	elseif uname == "Darwin" then
		saved[indent] = "1a "; if callmatch(values,line,"^/bin/sh: ./([%w]*C): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "1b "; if callmatch(values,line,"^/bin/sh: ./([%w]*Lua): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "1b "; if callmatch(values,line,"^/bin/sh: ./([%w]*Hs): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		-- saved[indent] = "2a "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and copysource(values,target,".c") then indent = indent - 1; return true end
		saved[indent] = "2b "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C.o'.  Stop.$") and makecopy(values,target,".c") then indent = indent - 1; return true end
		saved[indent] = "2c "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c'.  Stop.$") and copysource(values,target,".c") then indent = indent - 1; return true end
		saved[indent] = "2d "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Lua'.  Stop.$") and copysource(values,target,".lua") then indent = indent - 1; return true end
		saved[indent] = "2e "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Hs'.  Stop.$") and copysource(values,target,".hs") then indent = indent - 1; return true end
		saved[indent] = "2f "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).so'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		saved[indent] = "2g "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		saved[indent] = "2h "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c'.  Stop.$") and copymake(values,target) then indent = indent - 1; return true end
		saved[indent] = "2i "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).h'.  Stop.$") and copymake(values,target) then indent = indent - 1; return true end
		saved[indent] = "2j "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Lua'.  Stop.$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		saved[indent] = "2k "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).lua'.  Stop.$") and copymake(values,target) then indent = indent - 1; return true end
		saved[indent] = "2l "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Hs'.  Stop.$") and makecopy(values,target,".hs") then indent = indent - 1; return true end
		saved[indent] = "2m "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).hs'.  Stop.$") and copymake(values,target) then indent = indent - 1; return true end
		saved[indent] = "2n "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Cpp.o'.  Stop.$") and makecopy(values,target,".cpp") then indent = indent - 1; return true end
		saved[indent] = "2o "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).cpp'.  Stop.$") and copysource(values,target,".cpp") then indent = indent - 1; return true end
		saved[indent] = "2p "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Cpp'.  Stop.$") and makecopy(values,target,"Cpp.o") then indent = indent - 1; return true end
		saved[indent] = "2q "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"c") and makecopy(values,target,"C") then indent = indent - 1; return true end
		saved[indent] = "2r "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"cpp") and makecopy(values,target,"Cpp") then indent = indent - 1; return true end
		saved[indent] = "2s "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"hs") and makecopy(values,target,"Hs") then indent = indent - 1; return true end
		saved[indent] = "2t "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"sw") and makecopy(values,target,"Sw") then indent = indent - 1; return true end
		saved[indent] = "2u "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Sw'.  Stop.$") and makecopy(values,target,"Sw.o") then indent = indent - 1; return true end
		saved[indent] = "2v "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Sw.o'.  Stop.$") and makecopy(values,target,".sw") then indent = indent - 1; return true end
		saved[indent] = "2w "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).sw'.  Stop.$") and copysource(values,target,".sw") then indent = indent - 1; return true end
		saved[indent] = "2x "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).metallib'.  Stop.$") and copysource(values,target,".g") then indent = indent - 1; return true end
		saved[indent] = "3b "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c', needed by `[%w.]*'.  Stop.$") and copysource(values,target,".c") then indent = indent - 1; return true end
		saved[indent] = "3c "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).h', needed by `[%w.]*'.  Stop.$") and copysource(values,target,".h") then indent = indent - 1; return true end
		saved[indent] = "3d "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).cpp', needed by `[%w.]*'.  Stop.$") and copysource(values,target,".cpp") then indent = indent - 1; return true end
		saved[indent] = "3f "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).gen', needed by `[%w.]*'.  Stop.$") and copymake(values,target) then indent = indent - 1; return true end
		saved[indent] = "3g "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).lua', needed by `[%w.]*'.  Stop.$") and copysource(values,target,".lua") then indent = indent - 1; return true end
		saved[indent] = "3h "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).hs', needed by `[%w.]*'.  Stop.$") and copysource(values,target,".hs") then indent = indent - 1; return true end
		saved[indent] = "3i "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).sw', needed by `[%w.]*'.  Stop.$") and copysource(values,target,".sw") then indent = indent - 1; return true end
		saved[indent] = "4a "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and copysource(values,target,".h") then indent = indent - 1; return true end
		saved[indent] = "4b "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and makecopy(values,target,".h") then indent = indent - 1; return true end
		saved[indent] = "5a "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","c","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "5c "; if callmatch(values,line,"^  _[a-z]*([A-Z][%w]*), referenced from:$") and findsource(values,"^"," = {","gen","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "5d "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","cpp","Cpp.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "6a "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Lua)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "6b "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "6c "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*C)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "6d "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "6e "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*Sw)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "7a "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"^module "," where$","hs","") and copysource(values,target,".hs") then indent = indent - 1; return true end
		saved[indent] = "7b "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"module "," where","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "8a "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and copysource(values,target,".lua") then indent = indent - 1; return true end
		saved[indent] = "8b "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		saved[indent] = "9a "; if callmatch(values,line,"^lua: [%w./]*:[%d]*: module '([%w]*)' not found:$") and makecopy(values,target,".so") then indent = indent - 1; return true end
		saved[indent] = "10a "; if callmatch(values,line,"module.modulemap:[%d]*:[%d]*: error: header '([%w]*).h' not found$") and copysource(values,target,".h") then indent = indent - 1; return true end
		saved[indent] = "10b "; if callmatch(values,line,"module.modulemap:[%d]*:[%d]*: error: header '([%w]*).h' not found$") and makecopy(values,target,".h") then indent = indent - 1; return true end
		saved[indent] = "11a "; if callmatch(values,line,"[%w./]*:[%d]*:[%d]*: error: '([%w]*).h' file not found$") and copysource(values,target,".h") then indent = indent - 1; return true end
		saved[indent] = "12a "; if callmatch(values,line,"cannot load library: ([%w]*).metallib$") and makecopy(values,target,".metallib") then indent = indent - 1; return true end
		saved[indent] = "13a "; if callmatch(values,line,"^    Variable not in scope: [a-z]*([A-Z][%w]*)") and findsource(values,"^"," = {$","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
		saved[indent] = "14a "; if callmatch(values,line,"^    Not in scope: type constructor or class ‘([%w]*)’$") and findsource(values,"^"," = {$","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
	end end
	-- os.execute("rm -rf subdir.*")
	local message = "trymatch depender: "
	local append = getdepender(target)
	if append ~= nil then message = message..append else message = message.."nil" end
	message = message.." target: "
	if target ~= nil then message = message..target else message = message.."nil" end
	message = message.." unmatched:\n"
	indentwrite(message,indent)
	for line in io.lines("stderr."..target) do io.stderr:write(line.."\n") end
	os.exit(-1)
end

function trymake(values,target)
	local depends = values[1]
	local extras = values[2]
	indent = indent + 1; indentwrite("trymake: "..target.."\n",indent)
	os.execute("rm -rf subdir."..target.." stderr."..target.." stdout."..target)
	os.execute("mkdir subdir."..target)
	os.execute("cp Makefile module.modulemap subdir."..target)
	while true do
		writeout("subdir."..target.."/depend.mk",depends)
		for key,val in pairs(extras) do writeout("subdir."..target.."/"..key..".mk",val) end
		indentwrite("trymake: make -d -C subdir."..target.." "..target.." 2> stderr."..target.." >> stdout."..target.."\n",indent)
		os.execute("make -d -C subdir."..target.." "..target.." 2> stderr."..target.." >> stdout."..target)
		local file = io.open("subdir."..target.."/"..target,"r");
		-- file should not be nil if extras is set up before trymake of generated file
		if (file == nil) then os.execute("echo error:"..target..": make passed but build failed >> stderr."..target)
		else io.close(file) end
		local done = true
		for line in io.lines("stderr."..target) do done = false end
		if done then
			os.execute("rm -rf stderr."..target.." stdout."..target)
			indent = indent - 1; return true
		end
		if not trymatch(values,target) then break end
	end
	indent = indent - 1; return false
end

targets = {} -- list of top level targets
depends = {} -- map from depender to set of dependees
extras = {} -- map from .gen name to map from generated file to set of functions
saved[0] = "top"
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
		for ky,vl in pairs(v) do
			if (extras[k][ky] == nil) then extras[k][ky] = {} end
			for ke,va in pairs(vl) do extras[k][ky][ke] = true end
		end
	end
end
writeout("depend.mk",depends)
for key,val in pairs(extras) do writeout(key..".mk",val) end
-- os.execute("rm -rf subdir.* stderr.* stdout.*")
io.stdout:write("all done\n")
