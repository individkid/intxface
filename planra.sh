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
	'Move(msz:2mop[0]:Supexpr(sup[0]:$(@kernel))mop[1]:Supexpr(sup[0]:$(@kernel))mpo[0]:Evalexp(fnc[0]:$(@_ .= ptr#0Non@_.ptr#0Op .= mem#0 @mem Op))mpo[1]:Evalexp(fnc[0]:$(@_ .= ptr#0Non@_.ptr#0Op .= siz#0 @siz)))'\
	'Eval(eop[0]:Supexpr(sup[0]:$(@kernel))epo[0]:Evalexp(fnc[0]:$(Non Put Imm @_ Op EndOp Op ; @_ Op)))'\
	'Transferz(siz:1idx:0slf:-1exe[0]:Machine(xfr:Voidfpo[0]:Evalexp(fnc[0]:$(Non ManipLeft := #-20 ManipBase := #-20 ManipFixed := #133 Op))))'\
	'Copy(cop[0]:Supexpr(sup[0]:$(@memorys))cop[1]:Supexpr(sup[0]:$(@transfer)))'
