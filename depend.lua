
indent = 0
prefix = ""
verbose = false
saved = ""
file = io.popen("uname", 'r')
uname = file:read()
file:close()

function indentwrite(line)
	local count = 0
	if prefix ~= "" then
		saved = prefix
	end
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

function getdepender(target)
	local depender = nil
	indent = indent + 1; indentwrite("getdepender: "..target.."\n",indent)
	for line in io.lines("stdout."..target) do
		local maybe = nil
		if uname == "Linux" then
			maybe = string.match(line,"^ *Must remake target '([%w.]*)'.$")
		else
			maybe = string.match(line,"^ *Must remake target `([%w.]*)'.$")
		end
		if maybe ~= nil then depender = maybe end
	end
	indent = indent - 1; return depender
end

function getdependee(target)
	local depender = nil
	indent = indent + 1; indentwrite("getdependee: "..target.."\n",indent)
	for line in io.lines("stderr."..target) do
		local maybe = string.match(line,"./([%w]*.so): undefined symbol: [%w]*$")
		if maybe ~= nil then indent = indent - 1; return maybe end
	end
	indent = indent - 1; return nil
end

function callmatch(values,line,pat)
	indent = indent + 1; indentwrite("callmatch: "..line.."\n",indent)
	values[2] = string.match(line,pat)
	indent = indent - 1; return (values[2] ~= nil)
end

function copydepender(match,depends)
	for line in io.lines("subdir."..match.."/depend.mk") do
		local depender,tail = string.match(line,"^ *([%w.]*) *: *(.*)$")
		while tail ~= nil do
			local head,next = string.match(tail,"^([%w.]*) *(.*)$")
			tail = next
			if head == nil or head == "" then break end
			if depender ~= nil and depender ~= head then
				if depends[depender] == nil then depends[depender] = {} end
				depends[depender][head] = true
				-- if depender == "facerC" and head == "face.hs" then io.stderr:write("here1 "..saved.."\n") end
			end
		end
	end
end
function adddepender(match,depends,depender)
	if depender ~= nil and depender ~= match then
		if depends[depender] == nil then depends[depender] = {} end
		depends[depender][match] = true
		-- if depender == "facerC" and match == "face.hs" then io.stderr:write("here2 "..saved.."\n") end
	end
end

function makecopy(values,target,suf)
	local match = values[2]..suf -- face.c
	local depends = values[1]
	local file = io.open("subdir."..match.."/"..match,"r")
	local depender = getdepender(target) -- faceC.o
	indent = indent + 1; indentwrite("makecopy: subdir."..match.."/"..match.." to subdir."..target.."\n",indent)
	if file == nil and trymake(match,{}) == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	copydepender(match,depends)
	adddepender(match,depends,depender)
	os.execute("cp subdir."..match.."/"..match.." subdir."..target.."/")
	indent = indent - 1; return true
end

function remakecopy(values,target,suf)
	local match = values[2]..suf -- wrapCpp.o
	local depends = values[1]
	local depender = getdependee(target) -- luax.so
	local deps = {}
	indent = indent + 1; indentwrite("remakecopy: "..match.." "..target.." "..depender.."\n",indent)
	copydepender(depender,deps)
	adddepender(match,deps,depender)
	if trymake(depender,deps) == nil then indent = indent - 1; return false end
	copydepender(depender,depends)
	os.execute("cp subdir."..depender.."/"..depender.." subdir."..target.."/")
	indent = indent - 1; return true
end

function copysource(values,target,ext)
	local match = values[2]
	local depends = values[1]
	local file = io.open(match.."."..ext,"r")
	local depender = getdepender(target)
	indent = indent + 1; indentwrite("copysource: "..match.." "..target.." "..ext.."\n",indent)
	if file == nil then indent = indent - 1; return false end
	if file ~= nil then io.close(file) end
	if depender ~= nil and depender ~= match.."."..ext then
		if depends[depender] == nil then depends[depender] = {} end
		depends[depender][match.."."..ext] = true
		-- if depender == "facerC" and match.."."..ext == "face.hs" then io.stderr:write("here3 "..saved.."\n"); os.exit(-1) end
	end
	os.execute("cp "..match.."."..ext.." subdir."..target.."/")
	indent = indent - 1; return true
end

function findsource(values,pre,post,ext,suf)
	local match = values[2]
	local found = nil
	local file = io.popen("grep -s -l -E '"..pre..match..post.."' *."..ext,"r")
	indent = indent + 1; indentwrite("findsource: grep -s -l -E '"..pre..match..post.."' *."..ext.." "..suf.."\n",indent)
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

function checksource(values,ext)
	local match = values[2]
	local file = io.open(match.."."..ext,"r")
	if file == nil then return false end
	io.close(file)
	return true
end

function trymatch(values,target)
	indent = indent + 1; indentwrite("trymatch: "..target.."\n",indent)
	for line in io.lines("stderr."..target) do if uname == "Linux" then
		prefix = "1a"; if callmatch(values,line,"^/bin/sh: [%d]*: ./([%w]*C): not found$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "1b"; if callmatch(values,line,"^/bin/sh: [%d]*: ./([%w]*Lua): not found$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "1c "; if callmatch(values,line,"^/bin/sh: [%d]*: ./([%w]*Hs): not found$") and makecopy(values,target,"") then indent = indent - 1; return true end
		-- prefix = "2a "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2b "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C.o'.  Stop.$") and makecopy(values,target,".c") then indent = indent - 1; return true end
		prefix = "2c "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).c'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2d "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Lua'.  Stop.$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "2e "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Hs'.  Stop.$") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "2f "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).so'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		prefix = "2g "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		prefix = "2h "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).c'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2i "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).h'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2j "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Lua'.  Stop.$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		prefix = "2k "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).lua'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2l "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Hs'.  Stop.$") and makecopy(values,target,".hs") then indent = indent - 1; return true end
		prefix = "2m "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).hs'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2n "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp.o'.  Stop.$") and makecopy(values,target,".cpp") then indent = indent - 1; return true end
		prefix = "2o "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).cpp'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "2p "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp'.  Stop.$") and makecopy(values,target,"Cpp.o") then indent = indent - 1; return true end
		prefix = "2q "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)'.  Stop.$") and checksource(values,"c") and makecopy(values,target,"C") then indent = indent - 1; return true end
		prefix = "2r "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)'.  Stop.$") and checksource(values,"cpp") and makecopy(values,target,"Cpp") then indent = indent - 1; return true end
		prefix = "2s "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)'.  Stop.$") and checksource(values,"hs") and makecopy(values,target,"Hs") then indent = indent - 1; return true end
		prefix = "3a "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)Cpp.o', needed by '[%w.]*'.  Stop.$") and makecopy(values,target,".cpp") then indent = indent - 1; return true end
		prefix = "3b "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).c', needed by '[%w.]*'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "3c "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).h', needed by '[%w.]*'.  Stop.$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "3d "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).cpp', needed by '[%w.]*'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "3e "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*)C.o', needed by '[%w.]*'.  Stop.$") and makecopy(values,target,".c") then indent = indent - 1; return true end
		prefix = "3f "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).gen', needed by '[%w.]*'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "3g "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).lua', needed by '[%w.]*'.  Stop.$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "3h "; if callmatch(values,line,"^make: *** No rule to make target '([%w]*).hs', needed by '[%w.]*'.  Stop.$") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "4a "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: ([%w]*).h: No such file or directory$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "4b "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: ([%w]*).h: No such file or directory$") and makecopy(values,target,".h") then indent = indent - 1; return true end
		prefix = "5a "; if callmatch(values,line,"^.*: undefined reference to `([%w]*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","c","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "5b "; if callmatch(values,line,"^.*: undefined reference to '([%w]*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","c","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "5c "; if callmatch(values,line,"^.*: undefined reference to `[a-z]*([A-Z][%w]*)'$") and findsource(values,"^"," = {","gen","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "5d "; if callmatch(values,line,"^.*: undefined reference to `([%w]*)'$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","cpp","Cpp.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6a "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Lua)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6b "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6c "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*C)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6d "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "7a "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"^module "," where$","hs","") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "7b "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"module "," where","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "8a "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "8b "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		prefix = "9a "; if callmatch(values,line,"^lua: [%w./]*:[%d]*: module '([%w]*)' not found:$") and makecopy(values,target,".so") then indent = indent - 1; return true end
		prefix = "10a "; if callmatch(values,line,"[%w./]*: undefined symbol: ([%w]*)$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(.*\\)$","cpp","Cpp.o") and remakecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "10a "; if callmatch(values,line,"[%w./]*: undefined symbol: ([%w]*)$") and findsource(values,"^[^[:space:]][^[:space:]]* *","\\(.*\\)$","c","C.o") and remakecopy(values,target,"") then indent = indent - 1; return true end
	elseif uname == "Darwin" then
		prefix = "1a "; if callmatch(values,line,"^/bin/sh: ./([%w]*C): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "1b "; if callmatch(values,line,"^/bin/sh: ./([%w]*Lua): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "1b "; if callmatch(values,line,"^/bin/sh: ./([%w]*Hs): No such file or directory$") and makecopy(values,target,"") then indent = indent - 1; return true end
		-- prefix = "2a "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2b "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C.o'.  Stop.$") and makecopy(values,target,".c") then indent = indent - 1; return true end
		prefix = "2c "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "2d "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Lua'.  Stop.$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "2e "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Hs'.  Stop.$") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "2f "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).so'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		prefix = "2g "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)C'.  Stop.$") and makecopy(values,target,"C.o") then indent = indent - 1; return true end
		prefix = "2h "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2i "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).h'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2j "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Lua'.  Stop.$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		prefix = "2k "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).lua'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2l "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Hs'.  Stop.$") and makecopy(values,target,".hs") then indent = indent - 1; return true end
		prefix = "2m "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).hs'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "2n "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Cpp.o'.  Stop.$") and makecopy(values,target,".cpp") then indent = indent - 1; return true end
		prefix = "2o "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).cpp'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "2p "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Cpp'.  Stop.$") and makecopy(values,target,"Cpp.o") then indent = indent - 1; return true end
		prefix = "2q "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"c") and makecopy(values,target,"C") then indent = indent - 1; return true end
		prefix = "2r "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"cpp") and makecopy(values,target,"Cpp") then indent = indent - 1; return true end
		prefix = "2s "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"hs") and makecopy(values,target,"Hs") then indent = indent - 1; return true end
		prefix = "2t "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)'.  Stop.$") and checksource(values,"sw") and makecopy(values,target,"Sw") then indent = indent - 1; return true end
		prefix = "2u "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Sw'.  Stop.$") and makecopy(values,target,"Sw.o") then indent = indent - 1; return true end
		prefix = "2v "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*)Sw.o'.  Stop.$") and makecopy(values,target,".sw") then indent = indent - 1; return true end
		prefix = "2w "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).sw'.  Stop.$") and copysource(values,target,"sw") then indent = indent - 1; return true end
		prefix = "2x "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).metallib'.  Stop.$") and copysource(values,target,"g") then indent = indent - 1; return true end
		prefix = "3b "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).c', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"c") then indent = indent - 1; return true end
		prefix = "3c "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).h', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "3d "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).cpp', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"cpp") then indent = indent - 1; return true end
		prefix = "3f "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).gen', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"gen") then indent = indent - 1; return true end
		prefix = "3g "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).lua', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "3h "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).hs', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "3i "; if callmatch(values,line,"^make: *** No rule to make target `([%w]*).sw', needed by `[%w.]*'.  Stop.$") and copysource(values,target,"sw") then indent = indent - 1; return true end
		prefix = "4a "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "4b "; if callmatch(values,line,"^[%w./]*:[%d]*:[%d]*: fatal error: '([%w]*).h' file not found$") and makecopy(values,target,".h") then indent = indent - 1; return true end
		prefix = "5a "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","c","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "5c "; if callmatch(values,line,"^  _[a-z]*([A-Z][%w]*), referenced from:$") and findsource(values,"^"," = {","gen","C.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "5d "; if callmatch(values,line,"^  _([%w]*), referenced from:$") and findsource(values,"^[^[:space:]][^[:space:]]* *\\*?","\\(.*\\)$","cpp","Cpp.o") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6a "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Lua)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6b "; if callmatch(values,line,"^[%w]*C: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6c "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*C)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6d "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*Hs)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "6e "; if callmatch(values,line,"^[%w]*Lua: cannot execute file: ([%w]*Sw)$") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "7a "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"^module "," where$","hs","") and copysource(values,target,"hs") then indent = indent - 1; return true end
		prefix = "7b "; if callmatch(values,line,"^    Could not find module ‘([%w]*)’$") and findsource(values,"module "," where","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "8a "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and copysource(values,target,"lua") then indent = indent - 1; return true end
		prefix = "8b "; if callmatch(values,line,"^lua: cannot open ([%w]*).lua: No such file or directory$") and makecopy(values,target,".lua") then indent = indent - 1; return true end
		prefix = "9a "; if callmatch(values,line,"^lua: [%w./]*:[%d]*: module '([%w]*)' not found:$") and makecopy(values,target,".so") then indent = indent - 1; return true end
		prefix = "10a "; if callmatch(values,line,"module.modulemap:[%d]*:[%d]*: error: header '([%w]*).h' not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "10b "; if callmatch(values,line,"module.modulemap:[%d]*:[%d]*: error: header '([%w]*).h' not found$") and makecopy(values,target,".h") then indent = indent - 1; return true end
		prefix = "11a "; if callmatch(values,line,"[%w./]*:[%d]*:[%d]*: error: '([%w]*).h' file not found$") and copysource(values,target,"h") then indent = indent - 1; return true end
		prefix = "12a "; if callmatch(values,line,"cannot load library: ([%w]*).metallib$") and makecopy(values,target,".metallib") then indent = indent - 1; return true end
		prefix = "13a "; if callmatch(values,line,"^    Variable not in scope: [a-z]*([A-Z][%w]*)") and findsource(values,"^"," = {$","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
		prefix = "14a "; if callmatch(values,line,"^    Not in scope: type constructor or class ‘([%w]*)’$") and findsource(values,"^"," = {$","gen",".hs") and makecopy(values,target,"") then indent = indent - 1; return true end
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

function trymake(target,depends)
	indent = indent + 1; indentwrite("trymake: "..target.."\n",indent)
	os.execute("rm -rf subdir."..target.." stderr."..target.." stdout."..target)
	os.execute("mkdir subdir."..target)
	os.execute("cp Makefile module.modulemap subdir."..target)
	local values = {depends,nil} -- depend.mk, match
	while true do
		writeout("subdir."..target.."/depend.mk",depends)
		indentwrite("trymake: make: "..target.."\n",indent)
		os.execute("make -d -C subdir."..target.." "..target.." 2> stderr."..target.." >> stdout."..target)
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
targets = {}
for line in io.lines("Makefile") do
	local all = string.match(line,"^"..uname.." *: *(.*)$")
	if all ~= nil then
		for word in all:gmatch("%S+") do
			targets[#targets+1] = word
		end
		break
	end

end
for key,val in ipairs(targets) do
	for der,dee in pairs(trymake(val,{})) do
		if depends[der] == nil then depends[der] = {} end
		for k,_ in pairs(dee) do depends[der][k] = true end
	end
end
writeout("depend.mk",depends)
-- os.execute("rm -rf subdir.* stderr.* stdout.*")
io.stdout:write("all done\n")
