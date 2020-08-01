import face
import share
import AppKit
import Metal

var window:NSWindow!
var view:NSView!
var device:MTLDevice!
var layer: CAMetalLayer!
var drawable: CAMetalDrawable!
var queue:MTLCommandQueue!
var pass:MTLRenderPassDescriptor!
var render:MTLRenderPipelineState!
var compute:MTLComputePipelineState!

struct Pool<T>
{
	var max:Int = 3
	var val:[T] = []
	var pool = Set<Int>()
	var inuse = Set<Int>()
}
struct Share
{
	var basis:share.Linear
	var subject:share.Affine
	var feature:share.Affine
	var feather:share.Vector
	var arrow:share.Vector
	var size:uint
	var tag:uint
	var manip:uint
	var pad:uint
}
struct Pierce
{
	var valid:Bool
	var pad:(uint,uint,uint)
	var point:share.Vector
	var normal:share.Vector
}

var facet:MTLBuffer!
var vertex:MTLBuffer!
var index:MTLBuffer!
var file:MTLBuffer!
var state:Pool<MTLBuffer>
var pierce:Pool<MTLBuffer>

protocol Value
{
	init()
}
func poolId<T:Value>(pool:inout Pool<T>) -> Int?
{
	if (pool.pool.isEmpty && pool.val.count >= pool.max) {return nil}
	if (pool.pool.isEmpty) {
		pool.pool.insert(pool.val.count)
		pool.val.append(T())}
	let head = pool.pool.startIndex
	let index = pool.pool[head]
	return index
}
func poolVal<T:Value>(pool:inout Pool<T>,index:Int) -> T?
{
	if (!pool.inuse.contains(index)) {return nil}
	return pool.val[index]
}
func poolGet<T:Value>(pool:inout Pool<T>,index:Int)
{
	if (!pool.pool.contains(index)) {callError();return}
	pool.pool.remove(index)
	pool.inuse.insert(index)
}
func poolPut<T:Value>(pool:inout Pool<T>,index:Int)
{
	if (!pool.inuse.contains(index)) {callError();return}
	pool.inuse.remove(index)
	pool.pool.insert(index)
}

func unwrap<T>(_ x: Any) -> T
{
  return x as! T
}

func handler(event:NSEvent) -> NSEvent?
{
	print("key down")
	return nil
}

func swiftInit() -> Int32
{
	let _ = NSApplication.shared
	NSApp.setActivationPolicy(.regular)
	NSApp.activate(ignoringOtherApps: true)

	// cb.pos
	// cb.size
	// cb.full
	cb.draw = swiftDraw
	cb.call = {NSApp.run()}
	cb.wake = {
		NSApp.postEvent(
		NSEvent.otherEvent(
		with:.applicationDefined,
		location:NSZeroPoint,
		modifierFlags:.command,
		timestamp:0.0,
		windowNumber:0,
		context:nil,
		subtype:0,
		data1:0,
		data2:0)!,
		atStart:false)}

	NSEvent.addLocalMonitorForEvents(matching:NSEvent.EventTypeMask.keyDown,handler:handler)

	let rect = NSMakeRect(0, 0, 640, 480)
	let mask:NSWindow.StyleMask = [.titled, .closable, .resizable]
	window = NSWindow(contentRect: rect, styleMask: mask, backing: .buffered, defer: true)
	window.title = "Hello, world!"
	window.makeKeyAndOrderFront(nil)
	view = NSView(frame:window.frame)
	window.contentView = view
	device = MTLCreateSystemDefaultDevice()
	layer = CAMetalLayer()
	layer.device = device
	layer.pixelFormat = .bgra8Unorm
	layer.framebufferOnly = true
	layer.frame = window.frame
	view.layer = layer
	queue = device.makeCommandQueue()

	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot load shaders"); return 0}
	var plane0 = share.Facet(); plane0.versor = 7; plane0.tag = 63
	var plane1 = share.Facet(); plane1.versor = 9; plane1.tag = 65
	let planes = [plane0,plane1]
	let planez = device.makeBuffer(bytes:planes,length:MemoryLayout<share.Facet>.size*2)
	var point0 = share.Facet(); point0.plane = (0.0,1.0,0.0); point0.color.0 = (1.0,1.0,0.0,1.0)
	var point1 = share.Facet(); point1.plane = (-1.0,-1.0,0.0); point1.color.0 = (1.0,1.0,0.0,1.0)
	var point2 = share.Facet(); point2.plane = (1.0,-1.0,0.0); point2.color.0 = (1.0,0.5,0.0,1.0)
	let points = [point0,point1,point2]
	let pointz = device.makeBuffer(bytes:points,length:MemoryLayout<share.Facet>.size*3)
	let charz = device.makeBuffer(length:1000)
	var array0 = share.Index(); array0.point = (0,1,2)
	var array1 = share.Index(); array1.point = (3,4,5)
	let arrays = [array0,array1]
	let arrayz = device.makeBuffer(bytes:arrays,length:MemoryLayout<share.Index>.size*2)

	print("before compute")

	guard let debug = library.makeFunction(name:"kernel_debug") else {
		print("cannot make debug"); return 0;}
	if let temp = try? device.makeComputePipelineState(function:debug) {
		compute = temp} else {print("cannot make compute"); return 0;}

	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return 0}
	guard let command = code.makeComputeCommandEncoder() else {
		print("cannot make command"); return 0}
	command.setComputePipelineState(compute)
	command.setBuffer(planez,offset:0,index:0)
	command.setBuffer(arrayz,offset:0,index:1)
	command.setBuffer(charz,offset:0,index:2)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:2,height:1,depth:1)
	command.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	command.endEncoding()
	code.commit()
	code.waitUntilCompleted()

	var count = 0
	for expected:Int8 in [
		0,16,32,48,80,0,4,16,16,0,1,63,
		0,16,32,48,80,0,4,16,16,3,4,65] {
		let actual:Int8 = charz!.contents().load(fromByteOffset:count,as:Int8.self)
		if (expected != actual) {
			print("mismatch count(\(count)): expected(\(expected)) != actual(\(actual))")
		} else {
			print("match count(\(count)): expected(\(expected)) == actual(\(actual))")
		}
		count = count + 1
	}

	print("between compute and render")

	guard let vertex = library.makeFunction(name:"vertex_simple") else {
		print("cannot make vertex"); return 0}
	guard let fragment = library.makeFunction(name:"fragment_render") else {
		print("cannot make fragment"); return 0}
	let color = MTLClearColor(red: 0.0, green: 104.0/255.0, blue: 55.0/255.0, alpha: 1.0)
	if let temp = layer?.nextDrawable() {
		drawable = temp} else {print("cannot make drawable"); return 0}

	pass = MTLRenderPassDescriptor()
	pass.colorAttachments[0].texture = drawable.texture
	pass.colorAttachments[0].loadAction = .clear
	pass.colorAttachments[0].clearColor = color

	let pipe = MTLRenderPipelineDescriptor()
	pipe.vertexFunction = vertex
	pipe.fragmentFunction = fragment
	pipe.colorAttachments[0].pixelFormat = .bgra8Unorm
	if let temp = try? device.makeRenderPipelineState(descriptor:pipe) {
		render = temp} else {print("cannot make render"); return 0}

	guard let recode = queue.makeCommandBuffer() else {
		print("cannot make recode"); return 0}
	guard let encode = recode.makeRenderCommandEncoder(descriptor:pass) else {
		print("cannot make encode"); return 0}
	encode.setRenderPipelineState(render)
	encode.setVertexBuffer(pointz,offset:0,index:0)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:3)
	encode.endEncoding()
	recode.present(drawable)
	recode.commit()

	print("after render")
	return 1
}

func getMode() -> share.Mode
{
	// let shader = cb.state.12!.pointee.user!.pointee.shader
	let client:UnsafeMutablePointer<share.Client> =
	unwrap(Mirror(reflecting: cb.state).descendant(Int(Memory.User.rawValue))!)!
	let user:UnsafeMutablePointer<share.Mode> = client.pointee.user!
	return user.pointee
}
func getFacet() -> MTLBuffer?
{
	return nil // TODO
}
func getVertex() -> MTLBuffer?
{
	return nil // TODO
}
func getIndex() -> MTLBuffer?
{
	return nil // TODO
}
func getArray() -> [share.Array]
{
	return [] // TODO
}

func swiftDraw()
{
	let shader = getMode().shader
	if (shader == share.Track) {

		guard let code = queue.makeCommandBuffer() else {callError();return}
		for array in getArray() {
			guard let encode = code.makeRenderCommandEncoder(descriptor:pass) else {callError();return}
			encode.setRenderPipelineState(render)
			encode.setVertexBuffer(getFacet(),offset:0,index:0)
			encode.setVertexBuffer(getVertex(),offset:0,index:1)
			encode.setVertexBuffer(getIndex(),offset:0,index:2)
			// TODO set file buffer
			// TODO set state buffer with altered tag
			encode.drawPrimitives(
				type:.triangle,
				vertexStart:Int(array.idx),
				vertexCount:Int(array.siz))
			encode.endEncoding()
		}
		code.present(drawable)
		code.commit()

	} else if (shader == share.Display) {

		guard let code = queue.makeCommandBuffer() else {callError();return}
		for array in getArray() {
			guard let encode = code.makeComputeCommandEncoder() else {callError();return}
			encode.setComputePipelineState(compute)
			encode.setBuffer(getFacet(),offset:0,index:0)
			encode.setBuffer(getVertex(),offset:0,index:1)
			let offset = Int(array.idx)*MemoryLayout<share.Index>.size
			encode.setBuffer(getIndex(),offset:offset,index:2)
			// TODO set file buffer
			// TODO set state buffer with altered tag
			// TODO set allocated result buffer
			let groups = MTLSize(width:1,height:1,depth:1)
			// TODO use groups if threads too large
			let threads = MTLSize(width:Int(array.siz),height:1,depth:1)
			encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
			encode.endEncoding()
			// TODO add callback to read result
		}
		code.commit()

	}
}

// MAIN
	let argc = CommandLine.arguments.count
	let argv = CommandLine.arguments
	if (argc == 4) {cb.hub = pipeInit(argv[1],argv[2])}
	cb.zub = openPipe()
	cb.tub = openPipe()
	cb.mub = openPipe();
	if (cb.zub < 0 || cb.tub < 0 || cb.mub < 0 || (argc == 4 && cb.hub < 0)) {callError()}
	planeInit(Int32(argc))
	bothJump(cb.err,cb.zub)
	bothJump(cb.err,cb.tub)
	bothJump(cb.err,cb.mub)
	if (argc == 4) {bothJump(cb.err,cb.hub)}
	threadInit()
	if (swiftInit() != 0) {cb.call()}
	writeInt(1,cb.zub)
	cb.done()
	threadDone()
