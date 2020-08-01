import face
import share
import AppKit
import Metal
import MetalKit

var window:NSWindow!
var device:MTLDevice!
// var view:MTKView!
var view:NSView!
var queue:MTLCommandQueue!
var pass:MTLRenderPassDescriptor!
var render:MTLRenderPipelineState!
var compute:MTLComputePipelineState!
var layer: CAMetalLayer!

func unwrap<T>(_ x: Any) -> T {
  return x as! T
}

func handler(event:NSEvent) -> NSEvent?
{
	print("key down")
	return nil
}

/*
class AppDelegate: NSObject, NSApplicationDelegate
{
	func applicationDidFinishLaunching(_ notification: Notification)
{
}
}
*/

func swiftInit() -> Int32
{
	NSEvent.addLocalMonitorForEvents(matching:NSEvent.EventTypeMask.keyDown,handler:handler)
	cb.draw = swiftDraw
	let _ = NSApplication.shared
	NSApp.setActivationPolicy(.regular)
	NSApp.activate(ignoringOtherApps: true)
	// let delegate = AppDelegate()
	// NSApp.delegate = delegate
	let window = NSWindow(
	    contentRect: NSMakeRect(0, 0, 640, 480),
	    styleMask: [.titled, .closable, .resizable],
	    backing: .buffered,
	    defer: true
	)
	window.title = "Hello, world!"
	window.makeKeyAndOrderFront(nil)
	view = NSView(frame:window.frame)
	window.contentView = view
	device = MTLCreateSystemDefaultDevice()
layer = CAMetalLayer()          // 1
layer.device = device           // 2
layer.pixelFormat = .bgra8Unorm // 3
layer.framebufferOnly = true    // 4
layer.frame = window.frame      // 5
view.layer = layer              // 6
	queue = device.makeCommandQueue()
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot load shaders")
		return 0
	}
	print("before compute")
	// /*
	let debug:MTLFunction! = library.makeFunction(name:"kernel_debug")
	var plane0 = share.Facet(); plane0.versor = 7; plane0.tag = 63
	var plane1 = share.Facet(); plane1.versor = 9; plane1.tag = 65
	let planes = UnsafeMutablePointer<share.Facet>.allocate(capacity:2); planes[0] = plane0; planes[1] = plane1
	let planez = device.makeBuffer(bytes:planes,length:MemoryLayout<share.Facet>.size*2)
	let charz = device.makeBuffer(length:1000)
	compute = try? device.makeComputePipelineState(function:debug)
	let code:MTLCommandBuffer! = queue.makeCommandBuffer()
	let command:MTLComputeCommandEncoder! = code.makeComputeCommandEncoder()
	command.setComputePipelineState(compute)
	command.setBuffer(planez,offset:0,index:0)
	command.setBuffer(charz,offset:0,index:1)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:2,height:1,depth:1)
	command.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	command.endEncoding()
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
	// */
	print("between compute and render")
	// /*
	let vertex:MTLFunction! = library.makeFunction(name:/*"vertex_simple"*/"basic_vertex")
	let fragment:MTLFunction! = library.makeFunction(name:/*"fragment_render"*/"basic_fragment")

/*
	var point0 = share.Simple(); point0.point = (0.0,1.0,0.0,1.0); point0.color = (1.0,0.0,0.0,1.0)
	var point1 = share.Simple(); point1.point = (-1.0,-1.0,0.0,1.0); point1.color = (0.0,1.0,0.0,1.0)
	var point2 = share.Simple(); point2.point = (1.0,-1.0,0.0,1.0); point2.color = (0.0,0.0,1.0,1.0)
	let points = UnsafeMutablePointer<share.Simple>.allocate(capacity:3)
	points[0] = point0; points[1] = point2; points[2] = point2
	let pointz = device.makeBuffer(bytes:points,length:MemoryLayout<share.Simple>.size*3)
*/

let vertexData: [Float] = [
   0.0,  1.0, 0.0,
  -1.0, -1.0, 0.0,
   1.0, -1.0, 0.0
]
var vertexBuffer: MTLBuffer!
let dataSize = vertexData.count * MemoryLayout.size(ofValue: vertexData[0]) // 1
vertexBuffer = device.makeBuffer(bytes: vertexData, length: dataSize, options: []) // 2

guard let drawable = layer?.nextDrawable() else { return 0 }
pass = MTLRenderPassDescriptor()
pass.colorAttachments[0].texture = drawable.texture
pass.colorAttachments[0].loadAction = .clear
pass.colorAttachments[0].clearColor = MTLClearColor(
  red: 0.0, 
  green: 104.0/255.0, 
  blue: 55.0/255.0, 
  alpha: 1.0)
  	// view = MTKView(frame:window.contentLayoutRect,device:device)
//	let text:MTLTextureDescriptor! = MTLTextureDescriptor.texture2DDescriptor(pixelFormat:MTLPixelFormat.rgba8Unorm,width:1,height:1,mipmapped:false)
//	let texture:MTLTexture! = device.makeTexture(descriptor:text)
//	pass = /*view.currentRenderPassDescriptor*/ MTLRenderPassDescriptor()
/*
	pass.colorAttachments[0].texture = texture
	pass.colorAttachments[0].loadAction = .clear
	pass.colorAttachments[0].clearColor = MTLClearColorMake(0.0,1.0,1.0,1.0)
*/

	let pipe = MTLRenderPipelineDescriptor()
	pipe.vertexFunction = vertex
	pipe.fragmentFunction = fragment
	pipe.colorAttachments[0].pixelFormat = /*view.colorPixelFormat*/ /*texture.pixelFormat*/ .bgra8Unorm
	render = try? device.makeRenderPipelineState(descriptor:pipe)

	let recode:MTLCommandBuffer! = queue.makeCommandBuffer()
	let encode:MTLRenderCommandEncoder! = recode.makeRenderCommandEncoder(descriptor:pass)
	encode.setRenderPipelineState(render)
	encode.setVertexBuffer(/*pointz*/vertexBuffer,offset:0,index:0)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:3)
	encode.endEncoding()
	// let drawable:CAMetalDrawable = view.currentDrawable!
	recode.present(drawable)
	recode.commit()
	// */
	print("after render")
	cb.call = {NSApp.run()} // app.run()
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
	encode.setRenderPipelineState(render)
	// encode.setVertexBuffer(planez,offset:0,index:0)
	// encode.setVertexBuffer(charz,offset:0,index:1)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:0)
	encode.endEncoding()
	code.commit()}
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
	// if (argc == 4) {displayInit(argv[3])}
	// if (argc != 4) {displayInit(argv[0])}
	// windowInit()
	if (swiftInit() != 0) {cb.call()}
	writeInt(1,cb.zub)
	cb.done()
	// displayDone()
	threadDone()
