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
function matchCall1(pat,exp,fnc)
	local one = string.match(pat,exp)
	if one then fnc(one); return true end
	return false
end
function matchCall2(pat,exp,fnc)
	local one,two = string.match(pat,exp)
	if one and two then fnc(one,two); return true end
	return false
end
function matchCall3(pat,exp,fnc)
	local one,two,three = string.match(pat,exp)
	if one and two and three then fnc(one,two,three); return true end
	return false
end
function appendTarget(der,dee)
	todo[#todo+1] = der
	copy[der] = {[dee] = true}
	done[der] = {}
end
function appendDepend(der,dee,set)
	if copy[dee] then
		for k,v in pairs(copy[dee]) do
			set[k] = true
		end
	else
		set[dee] = true
	end
	done[der][dee] = true
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
function nameDepend(name)
	return findDepend(name,".c","^[^%s#].*[^a-zA-Z0-9_]([a-z][a-zA-Z0-9_]*)%(","C.o")
end
function moduleDepend(module)
	return findDepend(module,".hs","^module ([a-zA-Z]*) where",".hs")
end
function executeCopy(top,set)
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
			if matchCall1(line,"^cp: (.*)Sw.o: No such file or directory$",function(base) io.stderr:write("\ncopy 1 "..line); appendTarget(base.."Sw.o",base..".sw") end) then found = true; break end
			if matchCall1(line,"^cp: (.*)C.o: No such file or directory$",function(base) io.stderr:write("\ncopy 2 "..line); appendTarget(base.."C.o",base..".c") end) then found = true; break end
			if matchCall1(line,"^cp: (.*)Cpp.o: No such file or directory$",function(base) io.stderr:write("\ncopy 3 "..line); appendTarget(base.."Cpp.o",base..".cpp") end) then found = true; break end
		end
		greplist:close()
		if not (#lines == 0) and found then retval = false; break end
		if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("\ncopy: "..v.."\n") end os.exit() end
	end
	io.stderr:write("\n")
	return retval
end
function executeMake(top,set)
	io.stderr:write("make "..top.."\n")
	os.execute("make -C depend "..top.." 2> depend.err > depend.out")
	local greplist = io.open("depend.err")
	local found = false
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if matchCall1(line,"^lua: cannot open (.*): No such file or directory$",function(dofile) io.stderr:write("make 1 "..line.."\n"); appendDepend(top,dofile,set) end) then found = true; break end
		if matchCall1(line,"error: header '(.*)' not found$",function(header) io.stderr:write("make 2 "..line.."\n"); appendDepend(top,header,set) end) then found = true; break end
		if matchCall1(line,"^ *._(.*)., referenced from:",function(name) io.stderr:write("make 3 "..line.."\n"); appendDepend(top,nameDepend(name),set) end) then found = true; break end
		if matchCall1(line,"fatal error: '(.*)' file not found",function(include) io.stderr:write("make 4 "..line.."\n"); appendDepend(top,include,set) end) then found = true; break end
		if matchCall1(line,"^[0-9]* *| import ([a-zA-z]*)",function(module) io.stderr:write("make 5 "..line.."\n"); appendDepend(top,moduleDepend(module),set) end) then found = true; break end
	end
	greplist:close()
	if not (#lines == 0) and found then return false end
	if not (#lines == 0) and not found then for k,v in ipairs(lines) do io.stderr:write("make: "..v.."\n") end os.exit() end
	return true
end
-- List files with main pattern
for k,v in ipairs(mainExt) do
	os.execute("grep -l -E -- '"..mainPat[k].."' *"..v.." > depend.tmp")
	local greplist = io.open("depend.tmp")
	for line in greplist:lines() do
		local baser,exter = string.match(line,fileExp)
		appendTarget(baser..mainSuf[k],baser..copySuf[k])
	end
	greplist:close()
end
-- List generated files
os.execute("ls *.gen > depend.tmp")
greplist = io.open("depend.tmp")
for line in greplist:lines() do
	local baser,exter = string.match(line,fileExp)
	for k,v in ipairs(autoExt) do
		appendTarget(baser..v,line)
	end
end
-- If make reports an error, append to depend.mk and add to copy
-- If copy reports an error, append to todo and copy
greplist:close()
while #todo > 0 do
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
	if executeCopy(todo[#todo],copy[todo[#todo]]) and executeMake(todo[#todo],copy[todo[#todo]]) then
		os.execute("cp depend/* .")
		todo[#todo] = nil
	end
end