test = false
files = {}
for opt in ipairs(arg) do
	if string.match(opt,"--test") then test = true
	else files[#files+1] = opt end
end
if #files == 0 then
	files[#files+1] = "sculpt.--"
end
