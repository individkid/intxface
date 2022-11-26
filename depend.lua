todo = {}
copy = {}
done = {}
fileExp = "(.*)(%..*)"
findExp = ".*/(.*)(%..*)"
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
function doneExists(dep,dee)
	if done[dep] and done[dep][dee] then return true end
	return false
end
function fileExists(file)
	if not file or (file == "") then return false end
	os.execute("ls "..file.." depend > depend.ls 2>&1")
	local greplist = io.open("depend.ls")
	if file == "" then return false end
	for line in greplist:lines() do
		local found = string.match(line,"No such file or directory")
		if found then return false end
	end
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
	return retval
end
function objectDepend(name)
	local retval = findDepend(name,".c","^[^%s#].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%(","C.o")
	if retval == "" then retval = findDepend(name,".cpp","^extern[%s]*\"C\".*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%(","Cpp.o") end
	if retval == "" then io.stdout:write("\n"); io.stderr:write("objectDepend "..name.."\n"); os.exit() end
	return retval
end
function moduleDepend(name)
	local retval = findDepend(name,".hs","^module ([a-zA-Z]*) where",".hs")
	if retval == "" then io.stdout:write("\n"); io.stderr:write("moduleDepend "..name.."\n"); os.exit() end
	return retval
end
function classDepend(name)
	local retval = findDepend(name,".hs","^data [a-zA-Z]* = ([a-zA-Z]*)$",".hs")
	if retval == "" then io.stdout:write("\n"); io.stderr:write("classDepend "..name.."\n"); os.exit() end
	return retval
end
execMap = {{"C","C.o"},{"Cpp","Cpp.o"},{"Hs",".hs"},{"A",".agda"},{"Lua",".lua"},{"M","M.o"},{"Sw","Sw.o"}}
sharedMap = {{"G%.so","G.o"},{"%.so","C.o"}}
objectMap = {{"C%.o",".c"},{"Cpp%.o",".cpp"},{"M%.o",".m"},{"Sw%.o",".sw"},{"G%.o",".metal"}}
renameMap = {{"%.metal",".g"},{"%.agda",".a"}}
unameMap = {{"%.cpp",".cppx"},{"%.sw",".swy"},{"%.g",".gy"}}
generateMap = {"%.h","%.c","%.cpp","%.hs","%.lua","%.m","%.sw","%.g"}
mainMap = {{".c","^int main\\(","C"},{".cpp","^int main\\(","Cpp"},{".hs","^main :: IO \\(","Hs"},{".agda","^int main\\(","A"},{".lua","^-- MAIN","Lua"},{".m","^int main\\(","M"},{".sw","^// MAIN","Sw"},{".cppx","^int main\\(","Cpp"},{".swy","^// MAIN","Sw"}}
function ruleDepend(rule)
	local left, right, base = {},{},nil
	for k,v in ipairs({execMap,sharedMap,objectMap}) do for ky,vl in ipairs(v) do left[#left+1] = vl[1]; right[#right+1] = vl[2] end end
	for k in ipairs(left) do local base = string.match(rule,"^(.*)"..left[k].."$"); if base then return base..right[k] end end
	for k,v in ipairs(unameMap) do local base = string.match(rule,"^(.*)"..v[1].."$"); if base and fileExists(base..v[2]) then return base..v[2] end end
	base = string.match(rule,"^(.*)".."%.dep".."$") if base then return base..".gen" end
	for k,v in ipairs(generateMap) do local base = string.match(rule,"^(.*)"..v.."$"); if base then return base..".dep" end end
	for k,v in ipairs(mainMap) do if mainExists(rule..v[1],v[2]) then return rule..v[3] end end
	return ""
end
function firstDepend(list)
	for k,v in ipairs(list) do
		local found = false
		for ky,vl in ipairs(generateMap) do if string.match(v,"^.*"..vl.."$") then found = true end end
		if not found then return v end
	end
	return nil
end
function listDepend(top,rule)
	if top == rule then return {rule} end
	local list = {}
	local next = ruleDepend(top)
	if not (next == "") then list[#list+1] = next end
	if done[top] then for k,v in pairs(done[top]) do list[#list+1] = k end end
	for k,v in ipairs(list) do
		local temp = listDepend(v,rule)
		if #temp > 0 then
			local retval = {v}
			for ky,vl in ipairs(temp) do retval[#retval+1] = vl end
			return retval
		end
	end
	return {}
end
function pushError(push)
	io.stdout:write(" pushError "..push)
	if push == "" then io.stdout:write("blank\n"); os.exit() end
	todo[#todo+1] = push
	if not copy[push] then copy[push] = {} end
	if not done[push] then done[push] = {} end
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
	if #todo > 0 then
		local next = todo[#todo]
		io.stdout:write("popError "..next..":")
		for each in pairs(copy[next]) do
			io.stdout:write(" "..each)
		end
		io.stdout:write(";")
		for each in pairs(copy[top]) do
			io.stdout:write(" "..each)
			copy[next][each] = true
		end
		io.stdout:write("\n")
	end
end
finite = 0
limit = 200
function checkError(check,rule,id)
	local top = todo[#todo]
	if (top == "all") and not (rule == "all") then check = rule end
	local next = ruleDepend(check)
	local list = listDepend(top,rule)
	local first = firstDepend(list)
	local found = nil
	finite = finite + 1
	-- os.execute("cat depend.err")
	-- debugTodo()
	io.stdout:write("checkError"..id.."("..top..","..rule..","..check..","..next..") ")
	for k,v in ipairs(todo) do io.stdout:write("'"..v) end
	io.stdout:write(":")
	for k,v in ipairs(list) do io.stdout:write(v.."'") end
	io.stdout:write(" ")

	if not found and (top == "all") then found = "1"; io.stdout:write(found); pushError(check) end

	if not found and (top == check) and fileExists(next) then found = "2a"; io.stdout:write(found); copyError(next) end
	if not found and (top == check) and not fileExists(next) then found = "2b"; io.stdout:write(found); pushError(next) end
	if not found and not (top == rule) and string.match("hd",id) then found = "2c"; io.stdout:write(found); pushError(first) end

	if not found and not doneExists(top,check) and fileExists(check) then found = "3a"; io.stdout:write(found); bothError(check) end
	if not found and doneExists(top,check) and fileExists(check) then found = "3b"; io.stdout:write(found); copyError(check) end
	if not found and not doneExists(top,check) and not fileExists(check) then found = "3c"; io.stdout:write(found); doneError(check) end
	if not found and doneExists(top,check) and not fileExists(check) then found = "3d"; io.stdout:write(found); pushError(check) end

	if not found or finite > 300 then io.stdout:write("\n"); os.exit() end
	io.write(" "..finite)
	if finite >= limit then io.read(); return end
	io.stdout:write("\n")
end
function checkRule()
	local base = nil
	local retval = ""
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		base = string.match(line,"^make: %*%*%* %[([.%w]*)%] Error"); if not found and base then retval = base; found = true end
		base = string.match(line,"^error: failed to make ([.%w]*)$"); if not found and base then retval = base; found = true end
		base = string.match(line,"^[%s]*([.%w]*):[0-9]*: in main chunk$"); if base then retval = base; found = true end
		base = string.match(line,"^[%s]*%./([.%w]*):[0-9]*: in main chunk$"); if base then retval = base; found = true end
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
		local check,rule = nil,nil
		lines[#lines+1] = line
		check = string.match(line,"^/bin/sh: ./([%w]*): No such file or directory$"); if check then checkError(check,checkRule(),"a"); found = true; break end
		check = string.match(line,"No rule to make target `([.%w]*)'.  Stop.$"); if check then checkError(check,check,"b"); found = true; break end
		check = string.match(line,"^lua: cannot open ([.%w]*):"); if check then checkError(check,checkRule(),"c"); found = true; break end
		rule,check = string.match(line,"([.%w]*):[0-9]*:[0-9]*: fatal error: '([.%w]*)' file not found$"); if rule and check then checkError(check,checkRule(),"d"); found = true; break end
		rule,check = string.match(line,"([.%w]*):[0-9]*:[0-9]*: error: '([.%w]*)' file not found$"); if rule and check then checkError(check,rule,"d"); found = true; break end
		check = string.match(line,"^ *._([.%w]*)., referenced from:$"); if check then checkError(objectDepend(check),checkRule(),"e"); found = true; break end
		rule,check = string.match(line,"^([%w]*): cannot execute file: ([%w]*)$"); if rule and check then checkError(check,rule,"f"); found = true; break end
		check = string.match(line,"error: header '([.%w]*)' not found$"); if check then checkError(check,checkRule(),"g"); found = true; break end
		check,rule = string.match(line,"No rule to make target `([.%w]*)', needed by `([.%w]*)'.  Stop.$"); if rule and check then checkError(check,rule,"h"); found = true; break end
		check = string.match(line,"^error: failed to make ([.%w]*)$"); if (#lines == 1) and check then checkError(check,checkRule(),"i"); found = true; break end
		check = string.match(line,"^[0-9]* *%| import ([%w]*)$"); if check then checkError(moduleDepend(check),checkRule(),"j"); found = true; break end
		check = string.match(line,"^lua: [.%w]*:[0-9]*: module '([%w]*)' not found"); if check then checkError(check..".so",checkRule(),"k"); found = true; break end
		check = string.match(line,"error: no such file or directory: '([.%w]*)'$"); if check then checkError(check,checkRule(),"l"); found = true; break end
		check = string.match(line,"Not in scope: type constructor or class ‘([%w]*)’$"); if check then checkError(classDepend(check),checkRule(),"m"); found = true; break end
		rule,check = string.match(line,"^([%w]*): cannot load library: ([.%w]*)$"); if rule and check then checkError(check,rule,"n"); found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and found then return false end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("checkMake: "..v.."\n") end os.exit() end
	return true
end
io.stdout:write("main")
pushError("all")
io.stdout:write("\n")
while #todo > 0 do
	checkSetup()
	debugTodo()
	if checkCopy() and checkMake() then popError() end
end
io.stdout:write("all done\n")
