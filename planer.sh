echo '===' planerC planerLua planerC
./planerC '-Linez' planerC hello '-Planez' planerLua ok '-Spacez' planerC again
echo '===' vulkanCpp
./vulkanCpp 'Machine(xfr:Voidexp[0]:$(RegisterExit := #1))' 'Argument(typ:Planezoth:Planezinp:0out:1idx:0)'
echo '===' planerC planerLua planerLua
./planerC '-Filez' planerLua '-Planez' planerLua
echo '===' planerC planerLua vulkanCpp
./planerC '-Filez' planerLua '-Planez' vulkanCpp '$(PutStrHello Ok AgainOpEndOpOp)'
echo '===' planerC planerLua 0 vulkanCpp
./planerC '-Filez' planerLua '0' '-Planez' vulkanCpp '$(PutStrHello Ok AgainOpEndOpOp)'
