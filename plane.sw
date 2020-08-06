/*
*    plane.sw
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

import face
import share
import AppKit
import Metal
import MetalKit

var device:MTLDevice!
var combine:MTKView!
var layer: CAMetalLayer!
var view:NSView!
var window:NSWindow!
var queue:MTLCommandQueue!
var render:MTLRenderPipelineState!
var compute:MTLComputePipelineState!
var depth:MTLDepthStencilState!
var threads:MTLSize!

var facet = Pend()
var vertex = Pend()
var index = Pend()
var object = Pend()
var form = Pend()
var pierce = Pend()

var point = NSPoint(x:0.0,y:0.0)

struct Form
{
	var basis:share.Linear
	var subject:share.Affine
	var feature:share.Affine
	var feather:share.Vector
	var arrow:share.Vector
	var siz:uint
	var hand:uint
	var tag:uint
	var pad:uint
}
struct Pierce
{
	var valid:Bool = false
	var pad:(uint,uint,uint) = fromZero()
	var point:share.Vector = fromZero()
	var normal:share.Vector = fromZero()
}

struct Pend
{
	var pend:MTLBuffer!
	var last:MTLBuffer!
	mutating func set(_ ptr: UnsafeRawPointer, _ range: Range<Int>)
	{
		if (pend == nil && last != nil) {
			pend = device.makeBuffer(bytes:last.contents(),length:last.length)
			last = nil
		}
		if (pend == nil && range.lowerBound == 0) {
			pend = device.makeBuffer(bytes:ptr,length:range.upperBound)
			return
		}
		if (pend == nil) {
			pend = device.makeBuffer(length:range.upperBound)
		}
		let base:Int = range.lowerBound
		let size:Int = range.upperBound-range.lowerBound
		pend.contents().advanced(by:base).copyMemory(from:ptr,byteCount:size)
	}
	mutating func get() -> MTLBuffer
	{
		if (last == nil) {
			last = pend
			pend = nil
		}
		if (last == nil) {
			last = device.makeBuffer(length:0)
		}
		return last
	}
	mutating func rmw(_ ptr: UnsafeRawPointer, _ range: Range<Int>) -> MTLBuffer
	{
		let buf = get()
		let base:Int = range.lowerBound
		let size:Int = range.upperBound-range.lowerBound
		buf.contents().advanced(by:base).copyMemory(from:ptr,byteCount:size)
		buf.didModifyRange(range)
		return buf
	}
	mutating func new(_ len:Int) -> MTLBuffer
	{
		pend = nil
		last = device.makeBuffer(length:len)
		return last
	}
}
func setPierce() -> Int
{
	let siz = Int(getClient(share.Triangle).siz)
	let zero = Pierce()
	let vals = toList(zero,siz)
	let size = MemoryLayout<Pierce>.size*siz
	pierce.set(vals,0..<size)
	return siz
}
func setForm()
{
	let elem = Form(
		basis:getClient(share.Basis).basis!.pointee,
		subject:getClient(share.Subject).subject!.pointee,
		feature:getClient(share.Feature).feature!.pointee,
		feather:getClient(share.Feather).feather!.pointee,
		arrow:getClient(share.Arrow).arrow!.pointee,
		siz:UInt32(getClient(share.Cloud).siz),
		hand:UInt32(getClient(share.Hand).hand),
		tag:0,pad:0)
	form.set([elem],0..<MemoryLayout<Form>.size)
}
func setTag(_ tag:uint) -> MTLBuffer
{
	let base:Int = MemoryLayout<Form>.offset(of:\Form.tag)!
	let limit:Int = MemoryLayout<Form>.offset(of:\Form.pad)!
	return form.rmw([tag],base..<limit)
}
func getMode() -> share.Mode
{
	return getClient(share.User).user!.pointee
}
func getArray() -> [share.Array]
{
	let client = getClient(share.Range)
	return fromRaw(client.range,Int(client.siz))
}
func getClient(_ mem:share.Memory) -> share.Client
{
	let client:UnsafeMutablePointer<share.Client> =
	fromAny(Mirror(reflecting: cb.state).descendant(Int(mem.rawValue))!)!
	return client.pointee
}
func noWarn<T>(_ val:T) -> T?
{
	return val
}
func noWarn<T>(_ opt:T?) -> T?
{
	return nil
}

func swiftKey(event:NSEvent) -> NSEvent?
{
	guard let str:String = event.characters else {return nil}
	let unicode = str.unicodeScalars
	let key = Int(unicode[unicode.startIndex].value)
	if (key == 27) {if (cb.esc == 0) {cb.esc = 1}}
	else if (key == 13) {if (cb.esc == 1) {cb.esc = 2}}
	else {if (key == 32) {_ = swiftRight(event:event)}; cb.esc = 0}
	print("key(\(key)) esc(\(cb.esc))")
	if (cb.esc >= 2) {NSApp.terminate(nil)}
	return nil
}
func swiftLeft(event:NSEvent) -> NSEvent?
{
	let rect:CGRect = layer.frame
	if (NSPointInRect(point,rect)) {
		cb.click(0)
	}
	return event
}
func swiftRight(event:NSEvent) -> NSEvent?
{
	let rect:CGRect = layer.frame
	if (NSPointInRect(point,rect)) {
		cb.click(1)
	}
	return event
}
func swiftMove(event:NSEvent) -> NSEvent?
{
	point = NSEvent.mouseLocation
	let frame:CGRect = window.frame
	point.x = point.x - NSMinX(frame)
	point.y = point.y - NSMinY(frame)
	let rect:CGRect = layer.frame
	if (NSPointInRect(point,rect)) {
		cb.move(Double(point.x),Double(point.y))
	}
	return event
}
func swiftRoll(event:NSEvent) -> NSEvent?
{
	cb.roll(Double(event.deltaX),Double(event.deltaY))
	return event
}
func swiftAlarm(event:NSEvent) -> NSEvent?
{
	NSEvent.stopPeriodicEvents()
	cb.wake()
	return nil
}
func swiftWake(event:NSEvent) -> NSEvent?
{
	if (cb.full() != 0) {
		NSEvent.startPeriodicEvents(afterDelay: 1000.0*NANO2SEC, withPeriod: 0.0)
		return event
	}
	if (cb.read() != 0) {
		cb.proc()
		cb.wake()
	}
	return nil
}
func swiftSize(_: Notification)
{
	let rect:CGRect = layer.frame
	combine.frame = rect
	cb.size(Double(NSMaxX(rect)),Double(NSMaxY(rect)))
}
func swiftClose(_: Notification)
{
	NSApp.terminate(nil)
}
func swiftReady(_ buffer:MTLBuffer, _ size:Int)
{
	let pierces:[Pierce] = fromRaw(buffer.contents(),size)
	var found:Pierce = Pierce()
	for pierce in pierces {
		if (pierce.valid && (!found.valid || pierce.point.val.2 < found.point.val.2)) {
			found = pierce
		}
	}
	toMutablee(found.point,found.normal,{(pnt,nml) in cb.write(pnt,nml)})
}
func swiftWarp(xpos:Double, ypos:Double)
{
	let frame:CGRect = window.frame
	let coord = CGPoint(x:NSMinX(frame)+point.x,y:NSMinY(frame)+point.y)
    CGWarpMouseCursorPosition(coord);	
}
func swiftCall()
{
	NSApp.run()
}
func swiftWake()
{
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
		atStart:false)
}
func swiftDone()
{
	print("done")
}
func swiftEvent(_ type:NSEvent.EventTypeMask, _ handler: @escaping (_:NSEvent) -> NSEvent?)
{
	NSEvent.addLocalMonitorForEvents(matching:type,handler:handler)
}
func swiftNotify(_ type:Notification.Name, _ handler: @escaping (_:Notification)->Void)
{
	NotificationCenter.default.addObserver(
		forName: type,
		object: nil,
		queue: OperationQueue.main,
		using: handler)
}

func loopInit()
{
	cb.call = swiftCall
	cb.wake = swiftWake
	swiftEvent(.periodic,swiftAlarm)
}
func loopDone()
{
}
func swiftInit() -> Int32
{
	cb.warp = swiftWarp
	cb.full = swiftFull
	cb.dma = swiftDma
	cb.draw = swiftDraw
	cb.done = swiftDone
	swiftEvent(.keyDown,swiftKey)
	swiftEvent(.leftMouseDown,swiftLeft)
	swiftEvent(.rightMouseDown,swiftRight)
	swiftEvent(.mouseMoved,swiftMove)
	swiftEvent(.scrollWheel,swiftRoll)
	swiftEvent(.applicationDefined,swiftWake)
	swiftNotify(NSWindow.didResizeNotification,swiftSize)
	swiftNotify(NSWindow.willCloseNotification,swiftClose)
	let _ = NSApplication.shared
	NSApp.setActivationPolicy(.regular)
	NSApp.activate(ignoringOtherApps: true)
	if let temp = MTLCreateSystemDefaultDevice() {
		device = temp} else {print("cannot make device"); return 0}
	let rect = NSMakeRect(0, 0, 640, 480)
	if let temp = noWarn(MTKView(frame:rect,device:device)) {
		combine = temp} else {print("cannot make combine"); return 0}
	let color = MTLClearColor(red: 0.0, green: 104.0/255.0, blue: 55.0/255.0, alpha: 1.0)
	combine.clearColor = color
	combine.colorPixelFormat = .bgra8Unorm
	combine.depthStencilPixelFormat = .depth32Float
	combine.clearDepth = 0.0 // clip xy -1 to 1; z 0 to 1
	if let temp = noWarn(CAMetalLayer()) {
		layer = temp} else {print("cannot make layer"); return 0}
	layer.device = device
	layer.pixelFormat = .bgra8Unorm
	layer.framebufferOnly = true
	layer.frame = rect
	if let temp = noWarn(NSView(frame:rect)) {
		view = temp} else {print("cannot make view"); return 0}
	view.layer = layer
	view.addSubview(combine)
	let mask:NSWindow.StyleMask = [.titled, .closable, .miniaturizable, .resizable]
	if let temp = noWarn(NSWindow(contentRect: rect, styleMask: mask, backing: .buffered, defer: true)) {
		window = temp} else {print("cannot make window"); return 0}
	window.title = "plane"
	window.makeKeyAndOrderFront(nil)
	window.contentView = view
	if let temp = device.makeCommandQueue() {
		queue = temp} else {print("cannot make queue"); return 0}
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot make library"); return 0}
	guard let kernel_debug = library.makeFunction(name:"kernel_debug") else {
		print("cannot make kernel_debug"); return 0;}
	guard let vertex_simple = library.makeFunction(name:"vertex_simple") else {
		print("cannot make vertex_simple"); return 0}
	guard let vertex_render = library.makeFunction(name:"vertex_render") else {
		print("cannot make vertex_render"); return 0}
	guard let fragment_render = library.makeFunction(name:"fragment_render") else {
		print("cannot make fragment_render"); return 0}
	guard let kernel_pierce = library.makeFunction(name:"kernel_pierce") else {
		print("cannot make kernel_pierce"); return 0}
	guard let debug = try? device.makeComputePipelineState(function:kernel_debug) else {
		print("cannot make debug"); return 0}
	guard let pipe = noWarn(MTLRenderPipelineDescriptor()) else {
		print("cannot make pipe"); return 0}
	pipe.vertexFunction = vertex_simple
	pipe.fragmentFunction = fragment_render
	pipe.colorAttachments[0].pixelFormat = .bgra8Unorm
	pipe.depthAttachmentPixelFormat = .depth32Float
	guard let hello = try? device.makeRenderPipelineState(descriptor:pipe) else {
		print("cannot make hello"); return 0}
	pipe.vertexFunction = vertex_render
	if let temp = try? device.makeRenderPipelineState(descriptor:pipe) {
		render = temp} else {print("cannot make render"); return 0}
	if let temp = try? device.makeComputePipelineState(function:kernel_pierce) {
		compute = temp} else {print("cannot make compute"); return 0;}
    guard let stencil = noWarn(MTLDepthStencilDescriptor()) else {
    	print("cannot make stencil"); return 0}
    stencil.depthCompareFunction = .greater // left hand rule; z thumb to observer
    stencil.isDepthWriteEnabled = true
    if let temp = device.makeDepthStencilState(descriptor: stencil) {
    	depth = temp} else {print("cannot make depth"); return 0}
    if let temp = noWarn(device.maxThreadsPerThreadgroup) {
    	threads = temp} else {print("cannot make thread"); return 0}

	var plane0 = share.Facet(); plane0.versor = 7; plane0.tag = 63
	var plane1 = share.Facet(); plane1.versor = 9; plane1.tag = 65
	let planes = [plane0,plane1]
	let planez = device.makeBuffer(bytes:planes,length:MemoryLayout<share.Facet>.size*2)
	// yellow
	var point0 = share.Facet(); point0.plane = (0.0,1.0,0.6); point0.color.0 = (1.0,1.0,0.0,1.0)
	var point1 = share.Facet(); point1.plane = (-1.0,-1.0,0.6); point1.color.0 = (1.0,1.0,0.0,1.0)
	var point2 = share.Facet(); point2.plane = (1.0,-1.0,0.6); point2.color.0 = (1.0,0.5,0.0,1.0)
	// orange
	var point3 = share.Facet(); point3.plane = (0.0,-1.2,0.4); point3.color.0 = (1.0,0.5,0.0,1.0)
	var point4 = share.Facet(); point4.plane = (1.2,1.2,0.4); point4.color.0 = (1.0,0.5,0.0,1.0)
	var point5 = share.Facet(); point5.plane = (-1.2,1.2,0.4); point5.color.0 = (1.0,1.0,0.0,1.0)
	let points = [point0,point1,point2,point3,point4,point5]
	let pointz = device.makeBuffer(bytes:points,length:MemoryLayout<share.Facet>.size*6)
	let charz = device.makeBuffer(length:1000)
	var array0 = share.Vertex(); array0.plane = (0,1,2)
	var array1 = share.Vertex(); array1.plane = (3,4,5)
	let arrays = [array0,array1]
	let arrayz = device.makeBuffer(bytes:arrays,length:MemoryLayout<share.Vertex>.size*2)

	print("before debug")

	if (true) {
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return 0}
	guard let encode = code.makeComputeCommandEncoder() else {
		print("cannot make encode"); return 0}
	encode.setComputePipelineState(debug)
	encode.setBuffer(planez,offset:0,index:0)
	encode.setBuffer(arrayz,offset:0,index:1)
	encode.setBuffer(charz,offset:0,index:2)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:2,height:1,depth:1)
	encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	encode.endEncoding()
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
	count = count + 1}}

	print("between debug and hello")

	if (true) {
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return 0}
	guard let desc = combine.currentRenderPassDescriptor else {
		print("cannot make desc"); return 0}
	guard let encode = code.makeRenderCommandEncoder(descriptor:desc) else {
		print("cannot make encode"); return 0}
	encode.setRenderPipelineState(hello)
	encode.setDepthStencilState(depth)
	encode.setVertexBuffer(pointz,offset:0,index:0)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:6)
	encode.endEncoding()
    guard let draw = combine.currentDrawable else {
    	print("cannot make draw"); return 0}
	code.present(draw)
	code.commit()}

	print("after hello")
	return 1
}
func swiftFull() -> Int32
{
	return 0 // TODO count inuse buffers
}
func swiftDma(_ mem:share.Memory)
{
	// TODO dma from cb.state to MTLBuffer
}
func swiftDraw()
{
	let shader = getMode().shader
	if (shader == share.Display) {
		setForm()
		guard let code = queue.makeCommandBuffer() else {callError();return}
		for array in getArray() {
			guard let desc = combine.currentRenderPassDescriptor else {callError();return}
			guard let encode = code.makeRenderCommandEncoder(descriptor:desc) else {callError();return}
			encode.setRenderPipelineState(render)
			encode.setDepthStencilState(depth)
			encode.setVertexBuffer(facet.get(),offset:0,index:0)
			encode.setVertexBuffer(vertex.get(),offset:0,index:1)
			encode.setVertexBuffer(index.get(),offset:0,index:2)
			encode.setVertexBuffer(object.get(),offset:0,index:3)
			encode.setVertexBuffer(setTag(UInt32(array.tag)),offset:0,index:4)
			encode.drawPrimitives(
				type:.triangle,
				vertexStart:Int(array.idx),
				vertexCount:Int(array.siz))
			encode.endEncoding()
		}
	    guard let draw = combine.currentDrawable else {callError();return}
        code.present(draw)
		code.commit()
	} else if (shader == share.Track) {
		setForm();
		let size = setPierce()
		guard let code = queue.makeCommandBuffer() else {callError();return}
		for array in getArray() {
		var offset = Int(array.idx)*MemoryLayout<share.Vertex>.size
		var nums:[Int] = []
		var pers:[Int] = []
		let quotient = Int(array.siz)/threads.width
		let remainder = Int(array.siz)%threads.width
		if (quotient > 0) {
			nums.append(quotient)
			pers.append(threads.width)}
		if (remainder > 0) {
			nums.append(1)
			pers.append(remainder)}
		for (n,p) in zip(nums,pers) {
			guard let encode = code.makeComputeCommandEncoder() else {callError();return}
			encode.setComputePipelineState(compute)
			encode.setBuffer(facet.get(),offset:0,index:0)
			encode.setBuffer(vertex.get(),offset:0,index:1)
			encode.setBuffer(index.get(),offset:offset,index:2)
			encode.setBuffer(object.get(),offset:0,index:3)
			encode.setBuffer(setTag(UInt32(array.tag)),offset:0,index:4)
			encode.setBuffer(pierce.get(),offset:0,index:5)
			let num = MTLSize(width:n,height:1,depth:1)
			let per = MTLSize(width:p,height:1,depth:1)
			encode.dispatchThreadgroups(num,threadsPerThreadgroup:per)
			encode.endEncoding()
			offset += n*p*MemoryLayout<share.Vertex>.size
		}}
		code.addCompletedHandler({(buffer:MTLCommandBuffer) in swiftReady(pierce.get(),size)})
		code.commit()
	}
}

// MAIN
	let argc = CommandLine.arguments.count
	let argv = CommandLine.arguments
	if (argc == 4) {cb.hub = pipeInit(argv[1],argv[2])}
	cb.zub = openPipe()
	cb.tub = openPipe()
	if (cb.zub < 0 || cb.tub < 0 || (argc == 4 && cb.hub < 0)) {callError()}
	shareInit(Int32(argc))
	bothJump(cb.err,cb.zub)
	bothJump(cb.err,cb.tub)
	if (argc == 4) {bothJump(cb.err,cb.hub)}
	loopInit()
	threadInit()
	if (swiftInit() != 0) {cb.call()}
	cb.done()
	threadDone()
	loopDone()
	shareDone()
