import face
import share

func swiftInit() -> Int32
{
	return 0
}

// MAIN
	let argc = CommandLine.arguments.count
	let argv = CommandLine.arguments
	planeInit(Int32(argc))
	if (argc == 4) {
	cb.hub = pipeInit(argv[1],argv[2])
	if (cb.hub < 0) {callError()}}
	cb.zub = openPipe()
	if (cb.zub < 0) {callError()}
	cb.tub = openPipe()
	if (cb.tub < 0) {callError()}
	threadInit()
	if (argc == 4) {
	displayInit(argv[3])}
	if (argc != 4) {
	displayInit(argv[0])}
	if (swiftInit() != 0 ||
	metalInit() != 0 ||
	vulkanInit() != 0 ||
	openglInit() != 0 ||
	modelInit() != 0) {
	if (argc == 4) {
	bothJump(cb.err,cb.hub)}
	bothJump(cb.err,cb.zub)
	bothJump(cb.err,cb.tub)
	cb.call()}
	cb.done()
	displayDone()
	writeInt(1,cb.zub)
