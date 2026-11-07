choices=("Red" "Green" "Blue")
random_string=${choices[$RANDOM % ${#choices[@]}]}
echo "Random choice: $random_string"
./planraC
# push literal to stdout queue
# resize kernel, initializing with identities
# cast kernel to string and push to stdout queue
# put Configure changes (ManipFixed Slide,Ortho,Mouse 2,7,0 4+128+1 133) at transfer location
# then timer causes loopback of transfer location
./vulkanCpp '$(Put Strhello ok againOp EndOp Op)'\
	'$(siz = #1)' '$(mem = Cst @kernel TypImmStrMemorysOpOpOp Op)'\
	'Tend(mop[0]:Extexpr(sup[0]:$(@kernel)asr:Assert(PlaceAsr))mpo[0]:Evalexp(fnc[0]:$(@_ .= ptr#0Non@_.ptr#0Op .= mem#0 @mem)))'\
	'Tend(mop[0]:Extexpr(sup[0]:$(@kernel)asr:Assert(PlaceAsr))mpo[0]:Evalexp(fnc[0]:$(@_ .= ptr#0Non@_.ptr#0Op .= siz#0 @siz)))'\
	'Eval(eop[0]:Supexpr(sup[0]:$(@kernel))epo[0]:Evalexp(fnc[0]:$(Non Put Imm @_ Op EndOp Op ; @_ Op)))'\
	'Transferz(siz:1idx:0slf:-1exe[0]:Machine(xfr:Voidfpo[0]:Evalexp(fnc[0]:$(Non ManipLeft := #-20 ManipBase := #-20 ManipFixed := #133 Op))))'\
	'Copy(qop[0]:Extexpr(sup[0]:$(@scratch0)asr:Assert(PlaceAsr))qop[1]:Extexpr(sup[0]:$(@transfer)asr:Assert(PlaceAsr)))'
