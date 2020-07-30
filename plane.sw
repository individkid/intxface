import face
import share
import AppKit
import Metal

var window:NSWindow!
var device:MTLDevice!
var queue:MTLCommandQueue!
var state:MTLRenderPipelineState!
var compute:MTLComputePipelineState!
var debug:MTLComputePipelineState!
var pass:MTLRenderPassDescriptor!

func unwrap<T>(_ x: Any) -> T {
  return x as! T
}

func swiftInit() -> Int32
{
	cb.draw = swiftDraw
	window = cocoa as? NSWindow
	device = MTLCreateSystemDefaultDevice()
	queue = device.makeCommandQueue()
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {return 0}
	let vertex:MTLFunction! = library.makeFunction(name:"vertex_debug")
	compute = try? device.makeComputePipelineState(function:vertex)
	let plane = share.Facet()
	let planes = UnsafeMutablePointer<share.Facet>.allocate(capacity:1); planes.pointee = plane
	let chars = UnsafeMutablePointer<Character>.allocate(capacity:1000)
	let planez = device.makeBuffer(bytes:planes,length:MemoryLayout<share.Facet>.size)
	let charz = device.makeBuffer(bytes:chars,length:1000)
	let code:MTLCommandBuffer! = queue.makeCommandBuffer()
	let encode:MTLComputeCommandEncoder! = code.makeComputeCommandEncoder()
	encode.setComputePipelineState(compute)
	encode.setBuffer(planez,offset:0,index:0)
	encode.setBuffer(charz,offset:0,index:1)
	encode.endEncoding()
	code.commit()
	code.waitUntilCompleted()
	return 0
	/*
	let vertex:MTLFunction! = library.makeFunction(name:"vertex_render")
	let fragment:MTLFunction! = library.makeFunction(name:"fragment_render")
	let descriptor:MTLRenderPipelineDescriptor! = MTLRenderPipelineDescriptor()
	descriptor.vertexFunction = vertex
	descriptor.fragmentFunction = fragment
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
	let client:UnsafeMutablePointer<share.Client> = unwrap(Mirror(reflecting: cb.state).descendant(Int(Memory.User.rawValue))!)!
	let user:UnsafeMutablePointer<share.Mode> = client.pointee.user!
	let shader:share.Shader = user.pointee.shader
	// let shader = cb.state.12!.pointee.user!.pointee.shader
	if (shader == share.Track) {
	let code:MTLCommandBuffer! = queue.makeCommandBuffer()
	let encode:MTLComputeCommandEncoder! = code.makeComputeCommandEncoder()
	encode.setComputePipelineState(compute)
	encode.endEncoding()
	code.commit()}
	else if (shader == share.Display) {
	let code:MTLCommandBuffer! = queue.makeCommandBuffer()
	let encode:MTLRenderCommandEncoder! = code.makeRenderCommandEncoder(descriptor:pass)
	encode.setRenderPipelineState(state)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:0)
	encode.endEncoding()
	code.commit()
	cb.swap()}
}

// MAIN
	let argc = CommandLine.arguments.count
	let argv = CommandLine.arguments
	if (argc == 4) {cb.hub = pipeInit(argv[1],argv[2]); if (cb.hub < 0) {callError()}; bothJump(cb.err,cb.hub)}
	cb.zub = openPipe(); if (cb.zub < 0) {callError()}; bothJump(cb.err,cb.zub)
	cb.tub = openPipe(); if (cb.tub < 0) {callError()}; bothJump(cb.err,cb.tub)
	cb.mub = openPipe(); if (cb.mub < 0) {callError()}; bothJump(cb.err,cb.mub)
	planeInit(Int32(argc))
	threadInit()
	if (argc == 4) {displayInit(argv[3])}
	if (argc != 4) {displayInit(argv[0])}
	windowInit()
	if (swiftInit() != 0 || metalInit() != 0) {cb.call()}
	writeInt(1,cb.zub)
	cb.done()
	displayDone()
	threadDone()
