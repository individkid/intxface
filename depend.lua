function debug()
	local greplist = io.open("depend.err")
	for line in greplist:lines() do
		io.stderr:write(line.."\n")
	end
	greplist:close()
end
todo = {}
copy = {}
done = {}
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
	local one = string.match(pat,exp)
	if one then fnc(one); return true end
	return false
end
function fileExists(file)
	os.execute("ls "..file.." depend 2> depend.err > depend.out")
	local greplist = io.open("depend.err")
	for line in greplist:lines() do
		local found = string.match(line,"No such file or directory")
		if found then return false end
	end
	return true
end
function findDepend(pat,ext,exp,suf)
	local retval = ""
	os.execute("ls *"..ext.." > depend.tmp")
	local filelist = io.open("depend.tmp")
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
	if retval == "" then io.stderr:write("error: "..pat.."\n"); os.exit() end
	io.stderr:write("find "..pat.." "..retval.."\n")
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
function changeError()
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
	os.execute("rm -rf depend depend.err depend.out")
	os.execute("mkdir depend")
	os.execute("touch depend.err depend.out")
	os.execute("cp Makefile depend.mk module.modulemap depend")
end
function allError(all)
	todo[#todo+1] = all
	copy[all] = {}
	done[all] = {}
end
function ruleError(rule)
	if matchCall(rule,"(.*)C",function(base) allError(base.."C.o") end) then return end
	if matchCall(rule,"(.*)M",function(base) allError(base.."M.o") end) then return end
	if matchCall(rule,"(.*)Cpp",function(base) allError(base.."Cpp.o") end) then return end
	if matchCall(rule,"(.*)Hs",function(base) allError(base..".hs") end) then return end
	if matchCall(rule,"(.*)A",function(base) allError(base..".agda") end) then return end
	if matchCall(rule,"(.*)Lua",function(base) allError(base..".lua") end) then return end
	if matchCall(rule,"(.*)Sw",function(base) allError(base.."Sw.o") end) then return end
	if matchCall(rule,"(.*).so",function(base) allError(base.."C.o") end) then return end
	if matchCall(rule,"(.*)G.so",function(base) allError(base.."G.o") end) then return end
	if matchCall(rule,"(.*).so",function(base) allError(base.."C.o") end) then return end
	if matchCall(rule,"(.*)G.so",function(base) allError(base.."G.o") end) then return end
	if matchCall(rule,"(.*)C.o",function(base) allError(base..".c") end) then return end
	if matchCall(rule,"(.*)M.o",function(base) allError(base..".m") end) then return end
	if matchCall(rule,"(.*)Cpp.o",function(base) allError(base..".cpp") end) then return end
	if matchCall(rule,"(.*)Sw.o",function(base) allError(base..".sw") end) then return end
	if matchCall(rule,"(.*)G.o",function(base) allError(base..".metal") end) then return end
	if matchCall(rule,"(.*).metal",function(base) allError(base..".g") end) then return end
	if matchCall(rule,"(.*).agda",function(base) allError(base..".a") end) then return end
	if matchCall(rule,"(.*).h",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).c",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).m",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).cpp",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).hs",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).lua",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).sw",function(base) allError(base..".gen") end) then return end
	if matchCall(rule,"(.*).g",function(base) allError(base..".gen") end) then return end
end
function checkError(check)
	local top = todo[#todo]
	local set = copy[top]
	if fileExists(check) then
		set[check] = true
	else
		allError(check)
	end
end
function buildError(build)
	local top = todo[#todo]
	local set = done[top]
	set[build] = true
end
function noError()
	local top = todo[#todo]
	todo[#todo] = nil
	copy[top] = nil
	done[top] = nil
	if #todo > 0 then
		os.execute("cp depend/"..top.." .")
		copy[todo[#todo]][top] = true
	end
end
function checkCopy()
	local set = copy[todo[#todo]]
	local retval = true
	io.stderr:write("copy")
	for k,v in pairs(set) do
		io.stderr:write(" "..k)
		os.execute("cp "..k.." depend 2> depend.err > depend.out")
		local greplist = io.open("depend.err")
		local found = false
		local lines = {}
		for line in greplist:lines() do
			lines[#lines+1] = line
		end
		greplist:close()
		if not (#lines == 0) and found then retval = false; break end
		if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("\ncopy: "..v.."\n") end os.exit() end
	end
	io.stderr:write("\n")
	return retval
end
function checkMake()
	local top = todo[#todo]
	io.stderr:write("make "..top.."\n")
	os.execute("make -C depend "..top.." 2> depend.err > depend.out")
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if top == "" and matchCall(line,"No rule to make target `(.*)'[.]",function(all) io.stderr:write("all "..line.."\n"); allError(all) end) then found = true; break end
		if not (top == "") and matchCall(line,"No rule to make target `(.*)'[.]",function(rule) io.stderr:write("rule "..line.."\n"); ruleError(rule) end) then found = true; break end
		if matchCall(line,"No rule to make target `(.*)', needed by",function(check) io.stderr:write("check "..line.."\n"); checkError(check) end) then found = true; break end
		if matchCall(line,"fatal error: '(.*)' file not found",function(build) io.stderr:write("build "..line.."\n"); buildError(build) end) then found = true; break end
		if matchCall(line,"^ *._(.*)., referenced from:",function(build) io.stderr:write("build "..line.."\n"); buildError(objectDepend(build)) end) then found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and found then return false end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("make: "..v.."\n") end os.exit() end
	return true
end
allError("")
while #todo > 0 do
	changeError()
	io.stderr:write("todo "..todo[#todo].."\n")
	if checkCopy() and checkMake() then noError() end
end
