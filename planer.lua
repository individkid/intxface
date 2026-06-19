dofile("type.lua")
local success, exitType, exitCode = os.execute("./vulkanCpp '$(Put Strhello ok againOp EndOp Op)'")
if success == nil then
    os.exit(exitCode)
end
