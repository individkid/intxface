import face
import share
import AppKit
import Metal

var window:NSWindow!
var device:MTLDevice!
var queue:MTLCommandQueue!
var code:MTLCommandBuffer!
var state:MTLRenderPipelineState!
var pass:MTLRenderPassDescriptor!
var compute:MTLComputePipelineState!

func swiftInit() -> Int32
{
	cb.draw = swiftDraw
	window = cocoa as? NSWindow
	device = MTLCreateSystemDefaultDevice()
	queue = device.makeCommandQueue()
	code = queue.makeCommandBuffer()
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {return 0}
	let vertex:MTLFunction! = library.makeFunction(name:"vertex_render")
	let fragment:MTLFunction! = library.makeFunction(name:"fragment_render")
	let descriptor:MTLRenderPipelineDescriptor! = MTLRenderPipelineDescriptor()
	descriptor.vertexFunction = vertex
	descriptor.fragmentFunction = fragment
	return 0
	/*
	state = try? device.makeRenderPipelineState(descriptor:descriptor)
	pass = MTLRenderPassDescriptor()
	pass.colorAttachments[0].loadAction = .clear
	pass.colorAttachments[0].storeAction = .store
	pass.colorAttachments[0].clearColor = MTLClearColorMake(0.0,1.0,1.0,1.0)
	let function:MTLFunction! = library.makeFunction(name:"pierce_main")
	compute = try? device.makeComputePipelineState(function:function)
	return 0
	*/
}

func swiftDraw()
{
	if (false/*compute*/) {
	let encode:MTLComputeCommandEncoder! = code.makeComputeCommandEncoder()
	encode.setComputePipelineState(compute)
	encode.endEncoding()}
	else {
	let encode:MTLRenderCommandEncoder! = code.makeRenderCommandEncoder(descriptor:pass)
	encode.setRenderPipelineState(state)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:0)
	encode.endEncoding()}
	code.commit()
	// cb.swap()
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
	windowInit()
	if (swiftInit() != 0 ||
	metalInit() != 0) {
	if (argc == 4) {
	bothJump(cb.err,cb.hub)}
	bothJump(cb.err,cb.zub)
	bothJump(cb.err,cb.tub)
	cb.call()}
	cb.done()
	displayDone()
	writeInt(1,cb.zub)
	threadDone()
