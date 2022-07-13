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
autoExt = {".h",".c",".m",".cpp",".hs",".lua",".sw"}
mainExt = {".c",".cpp",".m",".hs",".lua",".sw"}
mainSuf = {"C","Cpp","M","Hs","Lua","Sw"}
copySuf = {"C.o","Cpp.o","M.o",".hs",".lua","Sw.o"}
mainPat = {"^int main\\(int argc",
	"^int main\\(int argc",
	"^int main\\(void\\)",
	"^main :: IO \\(\\)",
	"^-- MAIN",
	"^// MAIN",
	"^// MAIN"}
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
function findDepend(pat,ext,exp,suf)
	local retval = ""
	os.execute("ls *.gen | cut -f 1 -d '.' > depend.out")
	os.execute("for file in `cat depend.out`; do make $file"..ext.." > depend.err 2>&1; done")
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
	os.execute("for file in `cat depend.out`; do rm $file"..ext.." > depend.rm 2>&1; done")
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
function pushError(push)
	io.stdout:write(" pushError\n")
	todo[#todo+1] = push
	copy[push] = {}
	if not done[push] then done[push] = {} end
end
function ruleError(rule)
	if matchCall(rule,"^(.*)C$",function(base) io.stdout:write(" ruleError "..base.."C.o"); pushError(base.."C.o") end) then return end
	if matchCall(rule,"^(.*)M$",function(base) io.stdout:write(" ruleError "..base.."M.o"); pushError(base.."M.o") end) then return end
	if matchCall(rule,"^(.*)Cpp$",function(base) io.stdout:write(" ruleError "..base.."Cpp.o"); pushError(base.."Cpp.o") end) then return end
	if matchCall(rule,"^(.*)Hs$",function(base) io.stdout:write(" ruleError "..base..".hs"); pushError(base..".hs") end) then return end
	if matchCall(rule,"^(.*)A$",function(base) io.stdout:write(" ruleError "..base..".agda"); pushError(base..".agda") end) then return end
	if matchCall(rule,"^(.*)Lua$",function(base) io.stdout:write(" ruleError "..base..".lua"); pushError(base..".lua") end) then return end
	if matchCall(rule,"^(.*)Sw$",function(base) io.stdout:write(" ruleError "..base.."Sw.o"); pushError(base.."Sw.o") end) then return end
	if matchCall(rule,"^(.*).so$",function(base) io.stdout:write(" ruleError "..base.."C.o"); pushError(base.."C.o") end) then return end
	if matchCall(rule,"^(.*)G.so$",function(base) io.stdout:write(" ruleError "..base.."G.o"); pushError(base.."G.o") end) then return end
	if matchCall(rule,"^(.*).so$",function(base) io.stdout:write(" ruleError "..base.."C.o"); pushError(base.."C.o") end) then return end
	if matchCall(rule,"^(.*)G.so$",function(base) io.stdout:write(" ruleError "..base.."G.o"); pushError(base.."G.o") end) then return end
	if matchCall(rule,"^(.*)C.o$",function(base) io.stdout:write(" ruleError "..base..".c"); pushError(base..".c") end) then return end
	if matchCall(rule,"^(.*)M.o$",function(base) io.stdout:write(" ruleError "..base..".m"); pushError(base..".m") end) then return end
	if matchCall(rule,"^(.*)Cpp.o$",function(base) io.stdout:write(" ruleError "..base..".cpp"); pushError(base..".cpp") end) then return end
	if matchCall(rule,"^(.*)Sw.o$",function(base) io.stdout:write(" ruleError "..base..".sw"); pushError(base..".sw") end) then return end
	if matchCall(rule,"^(.*)G.o$",function(base) io.stdout:write(" ruleError "..base..".metal"); pushError(base..".metal") end) then return end
	if matchCall(rule,"^(.*).metal$",function(base) io.stdout:write(" ruleError "..base..".g"); pushError(base..".g") end) then return end
	if matchCall(rule,"^(.*).agda$",function(base) io.stdout:write(" ruleError "..base..".a"); pushError(base..".a") end) then return end
	if matchCall(rule,"^(.*).h$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).c$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).m$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).cpp$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).hs$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).lua$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).sw$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if matchCall(rule,"^(.*).g$",function(base) io.stdout:write(" ruleError "..base..".gen"); pushError(base..".gen") end) then return end
	if fileExists(rule..".c") then io.stdout:write(" ruleError "..rule.."C"); pushError(rule.."C"); return end
	if fileExists(rule..".m") then io.stdout:write(" ruleError "..rule.."M"); pushError(rule.."M"); return end
	if fileExists(rule..".cpp") then io.stdout:write(" ruleError "..rule.."Cpp"); pushError(rule.."Cpp"); return end
	if fileExists(rule..".hs") then io.stdout:write(" ruleError "..rule.."Hs"); pushError(rule.."Hs"); return end
	if fileExists(rule..".agda") then io.stdout:write(" ruleError "..rule.."A"); pushError(rule.."A"); return end
	if fileExists(rule..".lua") then io.stdout:write(" ruleError "..rule.."Lua"); pushError(rule.."Lua"); return end
	if fileExists(rule..".sw") then io.stdout:write(" ruleError "..rule.."Sw"); pushError(rule.."Sw"); return end
	io.stdout:write("\n"); io.stderr:write("ruleError "..rule.."\n"); os.exit()
end
function copyError(file)
	io.stdout:write(" copyError\n")
	local top = todo[#todo]
	local set = copy[top]
	set[file] = true
end
function doneError(file)
	io.stdout:write(" doneError\n")
	local top = todo[#todo]
	local set = done[top]
	set[file] = true
end
function fixError(der,dee)
	io.stdout:write(" fixError")
	local set = done[der]
	set[dee] = true
	pushError(der)
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
function checkError(check,rule)
	io.stdout:write("checkError "..check)
	if rule then io.stdout:write(" "..rule) end
	local top = todo[#todo]
	local isroot = (top == "")
	local ischeck = (check == top)
	local isrule = (rule == top)
	local isnil = (rule == nil) or (rule == "all")
	local exists = fileExists(check)
	local isdone = not not done[top][check]
	local cond = bools({isroot,ischeck,isrule,isnil,exists,isdone})
	io.stdout:write(" "..cond)
	if cond == "100000" then pushError(rule); return end
	if cond == "100100" then pushError(check); return end
	if cond == "001000" then doneError(check); return end
	if cond == "010100" then ruleError(check); return end
	if cond == "010110" then copyError(check); return end
	if cond == "000110" then doneError(check); return end
	if cond == "001011" then copyError(check); return end
	if cond == "000100" then doneError(check); return end
	if cond == "001001" then pushError(check); return end
	if cond == "000010" then pushError(rule); return end
	if cond == "000111" then copyError(check); return end
	if cond == "000101" then pushError(check); return end
	if cond == "000000" then fixError(rule,check); return end
	io.stdout:write("\n")
	io.stderr:write("checkError '"..top.."'"..check)
	if rule then io.stderr:write("'"..rule) end
	io.stderr:write("' "..cond.."\n"); os.exit()
end
function checkRule()
	local retval = nil
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if matchCall(line, "^make: %*%*%* %[([.%w]*)%] Error",function(rule) io.stdout:write(line.."\n"); retval = rule end) then found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("make: "..v.."\n") end os.exit() end
	return retval
end
function checkSetup()
	local dep = {}
	os.execute("rm -f depend.mk")
	os.execute("touch depend.mk")
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
		cmd = cmd.." >> depend.mk"
		if not (#dee == 0) then os.execute(cmd) end
	end
	os.execute("rm -rf depend depend.cp depend.ls depend.rm depend.err depend.out")
	os.execute("mkdir depend")
	os.execute("touch depend.cp depend.ls depend.rm depend.err")
	os.execute("cp Makefile depend.mk module.modulemap depend")
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
		if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("\ncopy: "..v.."\n") end os.exit() end
	end
	return retval
end
function checkMake()
	local top = todo[#todo]
	local cmd = "make -C depend "..top.." 2> depend.err > depend.out"
	io.stdout:write(cmd.."\n")
	os.execute(cmd)
	if #todo > 1 then
		cmd = "if [ ! -e depend/"..top.." ] ; then echo \"error: failed to make "..top.."\" >> depend.err ; fi"
		os.execute(cmd.."\n")
	end
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if matchCall(line,"^/bin/sh: ./([.%w]*): No such file or directory$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule()) end) then found = true; break end
		if matchCall(line,"^/bin/sh: ./([%w]*): No such file or directory$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule()) end) then found = true; break end
		if matchCall(line,"No rule to make target `([.%w]*)'.  Stop.$",function(check) io.stdout:write(line.."\n"); checkError(check) end) then found = true; break end
		if matchCall(line,"No rule to make target `([.%w]*)', needed by `([.%w]*)'.  Stop.$",function(check,rule) io.stdout:write(line.."\n"); checkError(check,rule) end) then found = true; break end
		if matchCall(line,"fatal error: '([.%w]*)' file not found$",function(check) io.stdout:write(line.."\n"); checkError(check) end) then found = true; break end
		if matchCall(line,"^ *._([.%w]*)., referenced from:",function(check) io.stdout:write(line.."\n"); checkError(objectDepend(check)) end) then found = true; break end
		if matchCall(line,"^[0-9]* *%| import ([%w]*)$",function(check) io.stdout:write(line.."\n"); checkError(moduleDepend(check)) end) then found = true; break end
		if matchCall(line,"error: no such file or directory: '([.%w]*)'$",function(check) io.stdout:write(line.."\n"); checkError(check) end) then found = true; break end
		if matchCall(line,"^error: cannot execute file: ([%w]*)$",function(check) io.stdout:write(line.."\n"); checkError(check,checkRule()) end) then found = true; break end
		if matchCall(line,"^lua: cannot open ([.%w]*):",function(check) io.stdout:write(line.."\n"); checkError(check) end) then found = true; break end
		if matchCall(line,"^lua: ([.%w]*):[0-9]*: module '([%w]*)' not found",function(rule,check) io.stdout:write(line.."\n"); checkError(check..".so",rule) end) then found = true; break end
		if matchCall(line,"error: header '([.%w]*)' not found$",function(check) io.stdout:write(line.."\n"); checkError(check) end) then found = true; break end
		if matchCall(line,"ld: file not found: ([.%w]*)$",function(check) io.stdout:write(line.."\n"); checkError(check)  end) then found = true; break end
		if matchCall(line,"^error: failed to make ([.%w]*)$",function(check) io.stdout:write(line.."\n"); checkError(check) end) then found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and found then return false end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("make: "..v.."\n") end os.exit() end
	return true
end
pushError("")
while #todo > 0 do
	checkSetup()
	debugTodo()
	if checkCopy() and checkMake() then popError() end
end
io.stdout:write("all done\n")