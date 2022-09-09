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
function matchCall(pat,exp,fnc)
	local one,two = string.match(pat,exp)
	if one then fnc(one,two); return true end
	return false
end
function fileExists(file)
	os.execute("ls "..file.." depend > depend.ls 2>&1")
	local greplist = io.open("depend.ls")
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
	os.execute("ls *.gen | cut -f 1 -d '.' > depend.rm 2>&1")
	os.execute("for file in `cat depend.rm`; do make $file"..ext.." > depend.out 2>&1; done")
	os.execute("ls *"..ext.." > depend.ls 2>&1")
	local filelist = io.open("depend.ls")
	for file in filelist:lines() do
		local basee,extee = string.match(file,fileExp)
		local linelist = io.open(file)
		for line in linelist:lines() do
			local found = string.match(line,exp)
			if found == pat then retval = basee..suf; break end
		end
		linelist:close()
		if not (retval == "") then break end
	end
	filelist:close()
	os.execute("for file in `cat depend.rm`; do rm $file"..ext.." > depend.out 2>&1; done")
	if retval == "" then io.stdout:write("\n"); io.stderr:write("findDepend "..pat.."\n"); os.exit() end
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
	io.stdout:write(" pushError\n")
	todo[#todo+1] = push
	copy[push] = {}
	if not done[push] then done[push] = {} end
end
function ruleError(rule)
	if matchCall(rule,"^(.*)C$",function(base) io.stdout:write(" ruleError "..base.."C.o"); pushError(base.."C.o") end) then return end
	if matchCall(rule,"^(.*)Cpp$",function(base) io.stdout:write(" ruleError "..base.."Cpp.o"); pushError(base.."Cpp.o") end) then return end
	if matchCall(rule,"^(.*)Hs$",function(base) io.stdout:write(" ruleError "..base..".hs"); pushError(base..".hs") end) then return end
	if matchCall(rule,"^(.*)A$",function(base) io.stdout:write(" ruleError "..base..".agda"); pushError(base..".agda") end) then return end
	if matchCall(rule,"^(.*)Lua$",function(base) io.stdout:write(" ruleError "..base..".lua"); pushError(base..".lua") end) then return end
	if matchCall(rule,"^(.*)M$",function(base) io.stdout:write(" ruleError "..base.."M.o"); pushError(base.."M.o") end) then return end
	if matchCall(rule,"^(.*)Sw$",function(base) io.stdout:write(" ruleError "..base.."Sw.o"); pushError(base.."Sw.o") end) then return end
	if matchCall(rule,"^(.*)G.so$",function(base) io.stdout:write(" ruleError "..base.."G.o"); pushError(base.."G.o") end) then return end
	if matchCall(rule,"^(.*).so$",function(base) io.stdout:write(" ruleError "..base.."C.o"); pushError(base.."C.o") end) then return end
	if matchCall(rule,"^(.*)C.o$",function(base) io.stdout:write(" ruleError "..base..".c"); pushError(base..".c") end) then return end
	if matchCall(rule,"^(.*)Cpp.o$",function(base) io.stdout:write(" ruleError "..base..".cpp"); pushError(base..".cpp") end) then return end
	if matchCall(rule,"^(.*)M.o$",function(base) io.stdout:write(" ruleError "..base..".m"); pushError(base..".m") end) then return end
	if matchCall(rule,"^(.*)Sw.o$",function(base) io.stdout:write(" ruleError "..base..".sw"); pushError(base..".sw") end) then return end
	if matchCall(rule,"^(.*)G.o$",function(base) io.stdout:write(" ruleError "..base..".metal"); pushError(base..".metal") end) then return end
	if matchCall(rule,"^(.*).metal$",function(base) io.stdout:write(" ruleError "..base..".g"); pushError(base..".g") end) then return end
	if matchCall(rule,"^(.*).agda$",function(base) io.stdout:write(" ruleError "..base..".a"); pushError(base..".a") end) then return end
	if matchExists(rule,"^(.*).cpp$",function(base) return base..".cppx" end,function(base) io.stdout:write(" ruleError "..base..".cppx"); pushError(base..".cppx") end) then return end
	if matchExists(rule,"^(.*).sw$",function(base) return base..".swy" end,function(base) io.stdout:write(" ruleError "..base..".swy"); pushError(base..".swy") end) then return end
	if matchExists(rule,"^(.*).g$",function(base) return base..".gy" end,function(base) io.stdout:write(" ruleError "..base..".gy"); pushError(base..".gy") end) then return end
	if matchCall(rule,"^(.*).h$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).c$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).cpp$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).hs$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).lua$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).m$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).sw$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).g$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if mainExists(rule..".c","^int main\\(") then io.stdout:write(" ruleError "..rule.."C"); pushError(rule.."C"); return end
	if mainExists(rule..".cpp","^int main\\(") then io.stdout:write(" ruleError "..rule.."Cpp"); pushError(rule.."Cpp"); return end
	if mainExists(rule..".hs","^main :: IO \\(") then io.stdout:write(" ruleError "..rule.."Hs"); pushError(rule.."Hs"); return end
	if mainExists(rule..".agda","^int main\\(") then io.stdout:write(" ruleError "..rule.."A"); pushError(rule.."A"); return end
	if mainExists(rule..".lua","^-- MAIN") then io.stdout:write(" ruleError "..rule.."Lua"); pushError(rule.."Lua"); return end
	if mainExists(rule..".m","^int main\\(") then io.stdout:write(" ruleError "..rule.."M"); pushError(rule.."M"); return end
	if mainExists(rule..".sw","^// MAIN") then io.stdout:write(" ruleError "..rule.."Sw"); pushError(rule.."Sw"); return end
	if mainExists(rule..".cppx","^int main\\(") then io.stdout:write(" ruleError "..rule.."Cpp"); pushError(rule.."Cpp"); return end
	if mainExists(rule..".swy","^// MAIN") then io.stdout:write(" ruleError "..rule.."Sw"); pushError(rule.."Sw"); return end
	io.stdout:write("\n"); io.stderr:write("ruleError "..rule.."\n"); os.exit()
end
function bothError(file)
	io.stdout:write(" bothError\n")
	local top = todo[#todo]
	local set = done[top]
	local map = copy[top]
	set[file] = true
	map[file] = true
end
function copyError(file)
	io.stdout:write(" copyError\n")
	local top = todo[#todo]
	local map = copy[top]
	map[file] = true
end
function doneError(file)
	io.stdout:write(" doneError")
	local top = todo[#todo]
	local set = done[top]
	set[file] = true
	pushError(file)
end
function runError(dep,dee)
	io.stdout:write(" runError")
	done[dep][dee] = true
	pushError(dee)
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
function checkError(check,rule,id)
	local top = todo[#todo]
	local isroot = (top == "") or (top == "all")
	local issame = (check == rule)
	local ischeck = (check == top) or (check == "all")
	local isrule = (rule == top) or (rule == "all")
	local excheck = fileExists(check)
	local exrule = fileExists(rule)
	local docheck = not not done[top][check]
	local dorule = not not done[top][rule]
	local cond = id..bools({isroot,issame,ischeck,isrule,excheck,exrule,docheck,dorule})
	io.stdout:write("checkError '"..top.."'"..rule.."'"..check.."' "..cond)
	if cond == "a10000000" then pushError(rule); return end
	if cond == "l00010000" then pushError(check); return end
	if cond == "l00011100" then pushError(check); return end
	if cond == "h10010000" then pushError(check); return end
	if cond == "h00001100" then pushError(check); return end
	if cond == "l00001101" then pushError(check); return end
	if cond == "l00001100" then pushError(check); return end
	if cond == "f00000001" then doneError(check); return end
	if cond == "n00000001" then doneError(check); return end
	if cond == "a00010000" then doneError(check); return end
	if cond == "d00000000" then doneError(check); return end
	if cond == "d00000100" then doneError(check); return end
	if cond == "e00010000" then doneError(check); return end
	if cond == "e00011000" then doneError(check); return end
	if cond == "j00011000" then doneError(check); return end
	if cond == "k00011000" then doneError(check); return end
	if cond == "m00010000" then doneError(check); return end
	if cond == "g00010000" then doneError(check); return end
	if cond == "k00010000" then doneError(check); return end
	if cond == "b01110000" then ruleError(check); return end
	if cond == "i01110000" then ruleError(check); return end
	if cond == "i01111100" then ruleError(check); return end
	if cond == "b01111100" then copyError(check); return end
	if cond == "h00011010" then copyError(check); return end
	if cond == "h00001000" then copyError(check); return end
	if cond == "h00011110" then copyError(check); return end
	if cond == "c00001100" then bothError(check); return end
	if cond == "c00011000" then bothError(check); return end
	if cond == "d00001000" then bothError(check); return end
	if cond == "d00001100" then bothError(check); return end
	if cond == "d00001101" then bothError(check); return end
	if cond == "d00001001" then bothError(check); return end
	if cond == "g00011000" then bothError(check); return end
	if cond == "k00000001" then runError(rule,check); return end
	if cond == "c00001001" then runError(rule,check); return end
	if cond == "c00000001" then runError(rule,check); return end
	io.stdout:write("\n")
	io.stderr:write("checkError '"..top.."'"..rule.."'"..check.."' "..cond.."\n"); os.exit()
end
function checkRule()
	local retval = nil
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if not found and matchCall(line,"^make: %*%*%* %[([.%w]*)%] Error",function(rule) io.stdout:write(line.."\n"); retval = rule end) then found = true; break end
		if not found and matchCall(line,"^error: failed to make ([.%w]*)$",function(rule) io.stdout:write(line.."\n"); retval = rule end) then found = true; break end
		if matchCall(line,"^[%s]*([.%w]*):[0-9]*: in main chunk$",function(rule) io.stdout:write(line.."\n"); retval = rule end) then found = true; end
		if matchCall(line,"^[%s]*%./([%w]*):[0-9]*: in main chunk$",function(rule) io.stdout:write(line.."\n"); retval = rule end) then found = true; end
		if found and string.match(retval,"^[%w]*%.gen$") then found = false end
	end
	greplist:close()
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
		if matchCall(line,"^/bin/sh: ./([%w]*): No such file or directory$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule(),"a") end) then found = true; break end
		if matchCall(line,"No rule to make target `([.%w]*)'.  Stop.$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule(),"b") end) then found = true; break end
		if matchCall(line,"^lua: cannot open ([.%w]*):",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule(),"c") end) then found = true; break end
		if matchCall(line,"([.%w]*):[0-9]*:[0-9]*: fatal error: '([.%w]*)' file not found$",function(rule,check) io.stdout:write(line.."\n"); checkError(check,rule,"d") end) then found = true; break end
		if matchCall(line,"([.%w]*):[0-9]*:[0-9]*: error: '([.%w]*)' file not found$",function(rule,check) io.stdout:write(line.."\n"); checkError(check,rule,"d") end) then found = true; break end
		if matchCall(line,"^ *._([.%w]*)., referenced from:$",function(check) io.stdout:write(line.."\n"); checkError(objectDepend(check),checkRule(),"e") end) then found = true; break end
		if matchCall(line,"^([%w]*): cannot execute file: ([%w]*)$",function(rule,check) io.stdout:write(line.."\n"); checkError(check,rule,"f") end) then found = true; break end
		if matchCall(line,"error: header '([.%w]*)' not found$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule(),"g") end) then found = true; break end
		if matchCall(line,"No rule to make target `([.%w]*)', needed by `([.%w]*)'.  Stop.$",function(check,rule) io.stdout:write(line.."\n"); checkError(check,rule,"h") end) then found = true; break end
		if (#lines == 1) and matchCall(line,"^error: failed to make ([.%w]*)$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule(),"i") end) then found = true; break end
		if matchCall(line,"^[0-9]* *%| import ([%w]*)$",function(check) io.stdout:write(line.."\n"); checkError(moduleDepend(check),checkRule(),"j") end) then found = true; break end
		if matchCall(line,"^lua: [.%w]*:[0-9]*: module '([%w]*)' not found",function(check) io.stdout:write(line.."\n"); checkError(check..".so",checkRule(),"k") end) then found = true; break end
		if matchCall(line,"error: no such file or directory: '([.%w]*)'$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule(),"l") end) then found = true; break end
		if matchCall(line,"Not in scope: type constructor or class ‘([%w]*)’",function(check) io.stdout:write(line.."\n"); checkError(classDepend(check),checkRule(),"m") end) then found = true; break end
		if matchCall(line,"^([%w]*): cannot load library: ([.%w]*)$",function(rule,check) io.stdout:write(line.."\n"); checkError(check,rule,"n") end) then found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and found then return false end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("checkMake: "..v.."\n") end os.exit() end
	return true
end
pushError("")
while #todo > 0 do
	checkSetup()
	debugTodo()
	if checkCopy() and checkMake() then popError() end
end
io.stdout:write("all done\n")
