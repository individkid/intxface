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
	'$(siz = #1)' '$(mem = Cst @kernel ImmStrMemorysOpOp Op)'\
	'Move(atm:2sub[0]:$(@kernel)sub[1]:$(@kernel)fun[0]:$(@_ .= ptr#0Non@_.ptr#0Op .= mem#0 @mem Op)fun[1]:$(@_ .= ptr#0Non@_.ptr#0Op .= siz#0 @siz))'\
	'Eval(res[0]:$(@kernel)fnc[0]:$(Non Put Imm @_ Op EndOp Op ; @_ Op))'\
	'Transferz(siz:1idx:0slf:-1exe[0]:Machine(xfr:Voidexp[0]:$(Non ManipLeft := #-20 ManipBase := #-20 ManipFixed := #133 Op)))'\
	'Dopy(sig:2arg[0]:$(@memorys)arg[1]:$(@transfer))'
