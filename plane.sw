import face
import share
import AppKit
import Metal

var window:NSWindow!
var device:MTLDevice!
var queue:MTLCommandQueue!
var pass:MTLRenderPassDescriptor!
var state:MTLRenderPipelineState!
var compute:MTLComputePipelineState!

func unwrap<T>(_ x: Any) -> T {
  return x as! T
}

func swiftInit() -> Int32
{
	cb.draw = swiftDraw
	window = cocoa as? NSWindow
	device = MTLCreateSystemDefaultDevice()
	queue = device.makeCommandQueue()
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot load shaders")
		return 0
	}
	var plane0 = share.Facet(); plane0.versor = 7; plane0.tag = 63
	var plane1 = share.Facet(); plane1.versor = 9; plane1.tag = 65
	let planes = UnsafeMutablePointer<share.Facet>.allocate(capacity:2); planes[0] = plane0; planes[1] = plane1
	let planez = device.makeBuffer(bytes:planes,length:MemoryLayout<share.Facet>.size*2)
	let charz = device.makeBuffer(length:1000)
	let code:MTLCommandBuffer! = queue.makeCommandBuffer()
	/*
	let debug:MTLFunction! = library.makeFunction(name:"vertex_debug")
	let ignore:MTLFunction! = library.makeFunction(name:"fragment_render")
	pass = MTLRenderPassDescriptor()
	pass.colorAttachments[0].loadAction = .clear
	pass.colorAttachments[0].storeAction = .store
	pass.colorAttachments[0].clearColor = MTLClearColorMake(0.0,1.0,1.0,1.0)
	let descriptor:MTLRenderPipelineDescriptor! = MTLRenderPipelineDescriptor()
	descriptor.vertexFunction = debug
	descriptor.fragmentFunction = ignore
	descriptor.colorAttachments[0].pixelFormat = MTLPixelFormat.rgba16Float
	state = try? device.makeRenderPipelineState(descriptor:descriptor)
	let encode:MTLRenderCommandEncoder! = code.makeRenderCommandEncoder(descriptor:pass)
	encode.setRenderPipelineState(state)
	encode.setVertexBuffer(planez,offset:0,index:0)
	encode.setVertexBuffer(charz,offset:0,index:1)
	encode.drawPrimitives(type:.point,vertexStart:0,vertexCount:2)
	encode.endEncoding()
	*/
	// /*
	let debug:MTLFunction! = library.makeFunction(name:"kernel_debug")
	compute = try? device.makeComputePipelineState(function:debug)
	let encode:MTLComputeCommandEncoder! = code.makeComputeCommandEncoder()
	encode.setComputePipelineState(compute)
	encode.setBuffer(planez,offset:0,index:0)
	encode.setBuffer(charz,offset:0,index:1)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:2,height:1,depth:1)
	encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	encode.endEncoding()
	// */
	code.commit()
	code.waitUntilCompleted()
	var count = 0
	for expected:Int8 in [
		0,16,32,48,80,0,4,56,96,16,7,63,
		0,16,32,48,80,0,4,56,96,16,9,65] {
		let actual:Int8 = charz!.contents().load(fromByteOffset:count,as:Int8.self)
		if (expected != actual) {
			print("mismatch count(\(count)): expected(\(expected)) != actual(\(actual))")
		} else {
			print("match count(\(count)): expected(\(expected)) == actual(\(actual))")
		}
		count = count + 1
	}
	// cb.swap()
	return 1
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
	// encode.setBuffer(planez,offset:0,index:0)
	// encode.setBuffer(charz,offset:0,index:1)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:0,height:1,depth:1)
	encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	encode.endEncoding()
	code.commit()}
	else if (shader == share.Display) {
	let code:MTLCommandBuffer! = queue.makeCommandBuffer()
	let encode:MTLRenderCommandEncoder! = code.makeRenderCommandEncoder(descriptor:pass)
	encode.setRenderPipelineState(state)
	// encode.setVertexBuffer(planez,offset:0,index:0)
	// encode.setVertexBuffer(charz,offset:0,index:1)
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
