function debug()
	local greplist = io.open("depend.err")
	for line in greplist:lines() do
		io.stderr:write(line.."\n")
	end
	greplist:close()
end
todo = {}
copy = {}
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
end
function appendDepend(der,dee,set)
	if copy[dee] then
		for k,v in pairs(copy[dee]) do
			set[k] = true
		end
	else
		set[dee] = true
	end
	os.execute("echo "..der..": "..dee.." >> depend.mk")
end
function executeCopy(top,set)
	local retval = true
	io.stderr:write("copy")
	for k,v in pairs(set) do
		io.stderr:write(" "..k)
		os.execute("cp "..k.." depend 2> depend.err > depend.out")
		local greplist = io.open("depend.err")
		local done = true
		local lines = {}
		for line in greplist:lines() do
			lines[#lines+1] = line
			if matchCall1(line,"^cp: (.*)Sw.o: No such file or directory$",function(base) io.stderr:write("\ncopy 1 "..line); appendTarget(base.."Sw.o",base..".sw") end) then done = false; break end
		end
		greplist:close()
		if not (#lines == 0) and not done then retval = false; break end
		if not (#lines == 0) and done then for k,v in ipairs(lines) do io.stderr:write("\ncopy: "..v.."\n") end os.exit() end
	end
	io.stderr:write("\n")
	return retval
end
function executeMake(top,set)
	io.stderr:write("make "..top.."\n")
	os.execute("make -C depend "..top.." 2> depend.err > depend.out")
	local greplist = io.open("depend.err")
	local done = true
	local lines = {}
	for line in greplist:lines() do
		lines[#lines+1] = line
		if matchCall1(line,"^lua: cannot open (.*): No such file or directory$",function(dofile) io.stderr:write("make 1 "..line.."\n"); appendDepend(top,dofile,set) end) then done = false; break end
		if matchCall1(line,"error: header '(.*)' not found$",function(header) io.stderr:write("make 2 "..line.."\n"); appendDepend(top,header,set) end) then done = false; break end
	end
	greplist:close()
	if not (#lines == 0) and not done then return false end
	if not (#lines == 0) and done then for k,v in ipairs(lines) do io.stderr:write("make: "..v.."\n") end os.exit() end
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
os.execute("rm -f depend.mk")
os.execute("touch depend.mk")
greplist:close()
while #todo > 0 do
	os.execute("rm -rf depend depend.err depend.out")
	os.execute("mkdir depend")
	os.execute("touch depend.err depend.out")
	os.execute("cp Makefile depend.mk module.modulemap depend")
	if executeCopy(todo[#todo],copy[todo[#todo]]) and executeMake(todo[#todo],copy[todo[#todo]]) then
		os.execute("cp depend/* .")
		todo[#todo] = nil
	end
end