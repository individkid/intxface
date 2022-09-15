function debug()
	local greplist = io.open("depend.err")
	for line in greplist:lines() do
		io.stdout:write(line.."\n")
	end
	greplist:close()
end
function bools(vals)
	local retval = ""
	for k,v in ipairs(vals) do
		if v then retval = retval.."1"
		else retval = retval.."0" end
	end
	return retval
end
todo = {}
copy = {}
done = {}
function debugTodo()
	local outer = true
	io.stdout:write("todo(")
	for k,v in ipairs(todo) do
		local first
		if not outer then io.stdout:write(" ") else outer = false end
		io.stdout:write("'"..v.."'")
		io.stdout:write(" copy(")
		first = true
		for ky,vl in pairs(copy[v]) do
			if not first then io.stdout:write(" ") else first = false end
			io.stdout:write(ky)
		end
		io.stdout:write(") done(")
		first = true
		for ky,vl in pairs(done[v]) do
			if not first then io.stdout:write(" ") else first = false end
			io.stdout:write(ky)
		end
		io.stdout:write(")")
	end
	io.stdout:write(")\n")
end
fileExp = "(.*)(%..*)"
findExp = ".*/(.*)(%..*)"
function matchCall(pat,exp,fnc)
	local one,two = string.match(pat,exp)
	if one then fnc(one,two); return true end
	return false
end
function doneSet(dep,dee)
	if done[dep] and done[dep][dee] then return true end
	return false
end
function copySet(dep,dee)
	if copy[dep] and copy[dep][dee] then return true end
	return false
end
function fileExt(file)
	local base,ext = string.match(file,fileExp)
	if not ext then return "" end
	return ext
end
function fileBase(file)
	local base,ext = string.match(file,fileExp)
	if not base then return file end
	return base
end
function sameFile(dep,dee)
	if dep == dee then return true end
	return false
end
function fileExists(file)
	os.execute("ls "..file.." depend > depend.ls 2>&1")
	local greplist = io.open("depend.ls")
	if file == "" then return false end
	for line in greplist:lines() do
		local found = string.match(line,"No such file or directory")
		if found then return false end
	end
	return true
end
function matchExists(pat,exp,rep,fnc)
	local one,two = string.match(pat,exp)
	if not one then return false end
	os.execute("ls "..rep(one,two).." depend > depend.ls 2>&1")
	local greplist = io.open("depend.ls")
	for line in greplist:lines() do
		local found = string.match(line,"No such file or directory")
		if found then return false end
	end
	fnc(one,two)
	return true
end
function mainExists(file,exp)
	os.execute("grep -l -E '"..exp.."' "..file.." > depend.ls 2>&1")
	local greplist = io.open("depend.ls")
	for line in greplist:lines() do
		if line == file then return true end
	end
	return false
end
function findDepend(pat,ext,exp,suf)
	local retval = ""
	os.execute("cp *.c *.h *.src *.gen depend")
	os.execute("(cd depend; ls *.gen) | cut -f 1 -d '.' > depend.rm 2>/dev/null")
	os.execute("for file in `cat depend.rm`; do make -C depend $file"..ext.." > depend.out 2>&1; done")
	os.execute("find . -name '*"..ext.."' > depend.ls 2>&1")
	local filelist = io.open("depend.ls")
	for file in filelist:lines() do
		local basee,extee = string.match(file,findExp)
		local linelist = io.open(file)
		for line in linelist:lines() do
			local found = string.match(line,exp)
			if found == pat then retval = basee..suf; break end
		end
		linelist:close()
		if not (retval == "") then break end
	end
	filelist:close()
	if retval == "" then io.stdout:write("\n"); io.stderr:write("findDepend "..pat.." "..ext.."\n"); os.exit() end
	return retval
end
function sourceDepend(name)
	return findDepend(name,".c","^[^%s#].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%(",".c")
end
function objectDepend(name)
	return findDepend(name,".c","^[^%s#].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%(","C.o")
end
function moduleDepend(name)
	return findDepend(name,".hs","^module ([a-zA-Z]*) where",".hs")
end
function classDepend(name)
	return findDepend(name,".hs","^data [a-zA-Z]* = ([a-zA-Z]*)$",".hs")
end
function pushError(push)
	io.stdout:write(" pushError "..push)
	todo[#todo+1] = push
	copy[push] = {}
	if not done[push] then done[push] = {} end
end
function ruleFunc(file)
	io.stdout:write(" ruleError "..file)
	pushError(file)
end
function ruleCheck(rule)
	local retval = nil
	if matchCall(rule,"^$",function(base) retval = "all" end) then return retval end
	if matchCall(rule,"^(.*)C$",function(base) retval = base.."C.o" end) then return retval end
	if matchCall(rule,"^(.*)Cpp$",function(base) retval = base.."Cpp.o" end) then return retval end
	if matchCall(rule,"^(.*)Hs$",function(base) retval = base..".hs" end) then return retval end
	if matchCall(rule,"^(.*)A$",function(base) retval = base..".agda" end) then return retval end
	if matchCall(rule,"^(.*)Lua$",function(base) retval = base..".lua" end) then return retval end
	if matchCall(rule,"^(.*)M$",function(base) retval = base.."M.o" end) then return retval end
	if matchCall(rule,"^(.*)Sw$",function(base) retval = base.."Sw.o" end) then return retval end
	if matchCall(rule,"^(.*)G%.so$",function(base) retval = base.."G.o" end) then return retval end
	if matchCall(rule,"^(.*)%.so$",function(base) retval = base.."C.o" end) then return retval end
	if matchCall(rule,"^(.*)C%.o$",function(base) retval = base..".c" end) then return retval end
	if matchCall(rule,"^(.*)Cpp%.o$",function(base) retval = base..".cpp" end) then return retval end
	if matchCall(rule,"^(.*)M%.o$",function(base) retval = base..".m" end) then return retval end
	if matchCall(rule,"^(.*)Sw%.o$",function(base) retval = base..".sw" end) then return retval end
	if matchCall(rule,"^(.*)G%.o$",function(base) retval = base..".metal" end) then return retval end
	if matchCall(rule,"^(.*)%.metal$",function(base) retval = base..".g" end) then return retval end
	if matchCall(rule,"^(.*)%.agda$",function(base) retval = base..".a" end) then return retval end
	if matchExists(rule,"^(.*)%.cpp$",function(base) return base..".cppx" end,function(base) retval = base..".cppx" end) then return retval end
	if matchExists(rule,"^(.*)%.sw$",function(base) return base..".swy" end,function(base) retval = base..".swy" end) then return retval end
	if matchExists(rule,"^(.*)%.g$",function(base) return base..".gy" end,function(base) retval = base..".gy" end) then return retval end
	if matchCall(rule,"^(.*)%.dep$",function(base) retval = base..".gen" end) then return retval end
	if matchCall(rule,"^(.*)%.h$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.c$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.cpp$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.hs$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.lua$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.m$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.sw$",function(base) retval = base..".dep" end) then return retval end
	if matchCall(rule,"^(.*)%.g$",function(base) retval = base..".dep" end) then return retval end
	if mainExists(rule..".c","^int main\\(") then retval = rule.."C"; func(retval); return retval end
	if mainExists(rule..".cpp","^int main\\(") then retval = rule.."Cpp"; func(retval); return retval end
	if mainExists(rule..".hs","^main :: IO \\(") then retval = rule.."Hs"; func(retval); return retval end
	if mainExists(rule..".agda","^int main\\(") then retval = rule.."A"; func(retval); return retval end
	if mainExists(rule..".lua","^-- MAIN") then retval = rule.."Lua"; func(retval); return retval end
	if mainExists(rule..".m","^int main\\(") then retval = rule.."M"; func(retval); return retval end
	if mainExists(rule..".sw","^// MAIN") then retval = rule.."Sw"; func(retval); return retval end
	if mainExists(rule..".cppx","^int main\\(") then retval = rule.."Cpp"; func(retval); return retval end
	if mainExists(rule..".swy","^// MAIN") then retval = rule.."Sw"; func(retval); return retval end
	return retval
end
function chainCheck(rule,check)
	local next = rule
	while next do
		if next == check then return true end
		rule = ruleCheck(next)
		if not rule and done[next] then for k,v in pairs(done[next]) do if chainCheck(k,check) then return true end end end
		next = rule
	end
	return false
end
function doneError(file)
	local top = todo[#todo]
	local set = done[top]
	io.stdout:write(" doneError "..top..": "..file)
	set[file] = true
	pushError(file)
end
function bothError(file)
	local top = todo[#todo]
	local set = done[top]
	local map = copy[top]
	io.stdout:write(" bothError "..top..": "..file)
	set[file] = true
	map[file] = true
end
function copyError(file)
	local top = todo[#todo]
	local map = copy[top]
	io.stdout:write(" copyError "..file)
	map[file] = true
end
function popError()
	local top = todo[#todo]
	local save = copy[top]
	todo[#todo] = nil
	copy[top] = nil
	if #todo > 0 then
		local next = todo[#todo]
		for each in pairs(save) do
			copy[next][each] = true
		end
	end
end
local finite = 0
function checkError(check,rule,id)
	local top = todo[#todo]
	local next = ruleCheck(check)
	local object = {rule,check,next}
	local isnext = not not next
	local verb = {bothError,doneError,copyError,pushError}
	local matrix = {}
	local found = {}
	finite = finite + 1
	io.stdout:write("checkError '"..top.."'"..rule.."'"..check)
	if isnext then io.stdout:write("'"..next) end

	for k,v in ipairs(object) do matrix[k] = {}; for ky,vl in ipairs(verb) do matrix[k][ky] = true end end
	-- eliminate next if next is nil
	if not next then matrix[3] = nil end
	-- eliminate copy if object is copied
	for k,v in ipairs(object) do if copySet(top,v) then matrix[k][1] = nil; matrix[k][3] = nil end end
	-- eliminate copy if object does not exist
	for k,v in ipairs(object) do if not fileExists(v) then matrix[k][1] = nil; matrix[k][3] = nil end end
	-- eliminate done if top exists
	for k,v in ipairs(object) do if fileExists(top) then matrix[k][1] = nil; matrix[k][2] = nil end end
	-- eliminate done if object is next
	for k,v in ipairs(object) do if v == next then matrix[k][1] = nil; matrix[k][2] = nil end end
	-- eliminate done if object is top
	for k,v in ipairs(object) do if v == top then matrix[k][1] = nil; matrix[k][2] = nil end end
	-- eliminate done if top is root
	for k,v in ipairs(object) do if top == "" then matrix[k][1] = nil; matrix[k][2] = nil end end
	-- eliminate done if object is indirect next or done of top
	for k,v in ipairs(object) do if chainCheck(top,v) then matrix[k][1] = nil; matrix[k][2] = nil end end
	-- eliminate push if object is copied
	for k,v in ipairs(object) do if copySet(top,v) then matrix[k][2] = nil; matrix[k][4] = nil end end
	-- eliminate push if object is top
	for k,v in ipairs(object) do if v == top then matrix[k][2] = nil; matrix[k][4] = nil end end
	-- eliminate push if object is all
	for k,v in ipairs(object) do if v == "all" then matrix[k][2] = nil; matrix[k][4] = nil end end
	-- eliminate push if object is done
	for k,v in ipairs(object) do if doneSet(top,v) then matrix[k][2] = nil; matrix[k][4] = nil end end
	-- eliminate push if next exists and is not copied
	for k,v in ipairs(object) do if isnext and fileExists(next) and not copySet(top,next) then matrix[k][2] = nil; matrix[k][4] = nil end end
	for k,v in pairs(matrix) do for ky,vl in pairs(v) do found[#found+1] = {k,ky} end end

	for k,v in ipairs(found) do io.stdout:write(" "..v[1]..","..v[2]) end
	if #found > 0 then verb[found[1][2]](object[found[1][1]]) else io.stdout:write(" "..finite.."\n") end
	if #found > 0 and finite > 48 then io.write(" "..finite); io.read() else io.stdout:write(" "..finite.."\n") end
	if #found > 0 and finite < 300 then return end
	io.stderr:write("checkError '"..top.."'"..rule.."'"..check)
	if isnext then io.stderr:write("'"..next) end
	io.stderr:write("'\n");
	os.exit()
end
function checkRule()
	local retval = nil
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if not found and matchCall(line,"^make: %*%*%* %[([.%w]*)%] Error",function(rule) --[[io.stdout:write(line.."\n");]] retval = rule end) then found = true; break end
		if not found and matchCall(line,"^error: failed to make ([.%w]*)$",function(rule) --[[io.stdout:write(line.."\n");]] retval = rule end) then found = true; break end
		if matchCall(line,"^[%s]*([.%w]*):[0-9]*: in main chunk$",function(rule) --[[io.stdout:write(line.."\n");]] retval = rule end) then found = true; end
		if matchCall(line,"^[%s]*%./([.%w]*):[0-9]*: in main chunk$",function(rule) --[[io.stdout:write(line.."\n");]] retval = rule end) then found = true; end
	end
	greplist:close()
	if found and string.match(retval,"^[%w]*%.gen$") then retval = string.match(retval,"^([%w]*)%.gen$")..".dep" end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("checkRule: "..v.."\n") end os.exit() end
	return retval
end
function checkSetup()
	local dep = {}
	local ext = ""
	os.execute("uname > depend.out 2>&1")
	local greplist = io.open("depend.out")
	for line in greplist:lines() do
		if line == "Linux" then ext = "x" end
		if line == "Darwin" then ext = "y" end
	end
	greplist:close()
	os.execute("rm -f depend.mk"..ext)
	os.execute("touch depend.mk"..ext)
	for k,v in pairs(done) do
		dep[#dep+1] = k
	end
	table.sort(dep)
	for k,v in ipairs(dep) do
		local cmd = "echo "..v..":"
		local dee = {}
		for ky,vl in pairs(done[v]) do
			dee[#dee+1] = ky
		end
		table.sort(dee)
		for ky,vl in ipairs(dee) do
			cmd = cmd.." "..vl
		end
		cmd = cmd.." >> depend.mk"..ext
		if not (#dee == 0) then os.execute(cmd) end
	end
	os.execute("rm -rf depend depend.cp depend.ls depend.rm depend.err depend.out")
	os.execute("mkdir depend")
	os.execute("touch depend.cp depend.ls depend.rm depend.err")
	os.execute("cp Makefile depend.mk"..ext.." module.modulemap depend")
end
function checkCopy()
	local set = copy[todo[#todo]]
	local retval = true
	for k,v in pairs(set) do
		os.execute("cp "..k.." depend > depend.cp 2>&1")
		local greplist = io.open("depend.cp")
		local found = false
		local lines = {}
		for line in greplist:lines() do
			lines[#lines+1] = line
		end
		greplist:close()
		if not (#lines == 0) and found then retval = false; break end
		if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("\ncheckCopy: "..v.."\n") end os.exit() end
	end
	return retval
end
function checkMake()
	local top = todo[#todo]
	local cmd = "make -C depend "..top.." 2> depend.err > depend.out"
	io.stdout:write(cmd.."\n")
	os.execute(cmd)
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	if #todo > 1 then
		cmd = "if [ ! -e depend/"..top.." ] ; then echo \"error: failed to make "..top.."\" >> depend.err ; fi"
		os.execute(cmd.."\n")
	end
	for line in greplist:lines() do
		lines[#lines+1] = line
		if matchCall(line,"^/bin/sh: ./([%w]*): No such file or directory$",function(check) --[[io.stdout:write(line.."\n");]] checkError(check,checkRule(),"a") end) then found = true; break end
		if matchCall(line,"No rule to make target `([.%w]*)'.  Stop.$",function(check) --[[io.stdout:write(line.."\n");]] checkError(check,check,"b") end) then found = true; break end
		if matchCall(line,"^lua: cannot open ([.%w]*):",function(check) --[[io.stdout:write(line.."\n");]] checkError(check,checkRule(),"c") end) then found = true; break end
		if matchCall(line,"([.%w]*):[0-9]*:[0-9]*: fatal error: '([.%w]*)' file not found$",function(rule,check) --[[io.stdout:write(line.."\n");]] checkError(check,rule,"d") end) then found = true; break end
		if matchCall(line,"([.%w]*):[0-9]*:[0-9]*: error: '([.%w]*)' file not found$",function(rule,check) --[[io.stdout:write(line.."\n");]] checkError(check,rule,"d") end) then found = true; break end
		if matchCall(line,"^ *._([.%w]*)., referenced from:$",function(check) --[[io.stdout:write(line.."\n");]] checkError(objectDepend(check),checkRule(),"e") end) then found = true; break end
		if matchCall(line,"^([%w]*): cannot execute file: ([%w]*)$",function(rule,check) --[[io.stdout:write(line.."\n");]] checkError(check,rule,"f") end) then found = true; break end
		if matchCall(line,"error: header '([.%w]*)' not found$",function(check) --[[io.stdout:write(line.."\n");]] checkError(check,checkRule(),"g") end) then found = true; break end
		if matchCall(line,"No rule to make target `([.%w]*)', needed by `([.%w]*)'.  Stop.$",function(check,rule) --[[io.stdout:write(line.."\n");]] checkError(check,rule,"h") end) then found = true; break end
		if (#lines == 1) and matchCall(line,"^error: failed to make ([.%w]*)$",function(check) --[[io.stdout:write(line.."\n");]] checkError(check,checkRule(),"i") end) then found = true; break end
		if matchCall(line,"^[0-9]* *%| import ([%w]*)$",function(check) --[[io.stdout:write(line.."\n");]] checkError(moduleDepend(check),checkRule(),"j") end) then found = true; break end
		if matchCall(line,"^lua: [.%w]*:[0-9]*: module '([%w]*)' not found",function(check) --[[io.stdout:write(line.."\n");]] checkError(check..".so",checkRule(),"k") end) then found = true; break end
		if matchCall(line,"error: no such file or directory: '([.%w]*)'$",function(check) --[[io.stdout:write(line.."\n");]] checkError(check,checkRule(),"l") end) then found = true; break end
		if matchCall(line,"Module ‘([%w]*)’ does not export",function(check) --[[io.stdout:write(line.."\n");]] checkError(classDepend(check),checkRule(),"m") end) then found = true; break end
		if matchCall(line,"^([%w]*): cannot load library: ([.%w]*)$",function(rule,check) --[[io.stdout:write(line.."\n");]] checkError(check,rule,"n") end) then found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and found then return false end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("checkMake: "..v.."\n") end os.exit() end
	return true
end
pushError("")
while #todo > 0 do
	checkSetup()
	-- debugTodo()
	if checkCopy() and checkMake() then popError() end
end
io.stdout:write("all done\n")
