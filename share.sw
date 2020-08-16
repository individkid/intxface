/*
*    share.sw
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

var device:MTLDevice!
var layer: CAMetalLayer!
var view:NSView!
var delegate:WindowDelegate!
var window:NSWindow!
var queue:MTLCommandQueue!
var render:MTLRenderPipelineState!
var descriptor:MTLRenderPassDescriptor!
var depth:MTLDepthStencilState!
var compute:MTLComputePipelineState!
var threads:MTLSize!
var drag:NSPoint!

var triangle = Pend<share.Facet>()
var corner = Pend<share.Vertex>()
var frame = Pend<Int32>()
var base = Pend<Int32>()
var object = Pend<share.Affine>()
var cloud = Pend<share.Vector>()
var form = Pend<Form>()
var pierce = Pend<Pierce>()

var lock = [Refer]()
var count = Int(0)

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
class WindowDelegate : NSObject, NSWindowDelegate
{
	func windowDidResize(_ notification: Notification)
	{
		swiftSize()
	}
	func windowShouldClose(_ sender: NSWindow) -> Bool
	{
		NSApp.stop(nil)
		return true
	}
	func windowWillMove(_ notification: Notification)
	{
		if (drag == nil) {drag = getPoint()}
	}
}
class Refer
{
	var lock:Int = 0
}
class Pend<T>
{
	var pend:MTLBuffer!
	var last:MTLBuffer!
	var refer:Refer!
	func set(_ ptr: UnsafeRawPointer, _ range: Range<Int>)
	{
		let unit:Int = MemoryLayout<T>.size
		let length:Int = range.upperBound+(unit-range.upperBound%unit)%unit;
		if (pend == nil && last != nil && last.length < length) {
			pend = device.makeBuffer(length:length)
			pend.contents().copyMemory(from:last.contents(),byteCount:last.length)
			last = nil
		}
		if (pend == nil && last != nil && refer.lock == 0)
		{
			pend = last
			last = nil
		}
		if (pend == nil && last != nil) {
			pend = device.makeBuffer(bytes:last.contents(),length:last.length)
			last = nil
		}
		if (pend == nil) {
			pend = device.makeBuffer(length:length)
		}
		let base:Int = range.lowerBound
		let size:Int = range.upperBound-range.lowerBound
		pend.contents().advanced(by:base).copyMemory(from:ptr,byteCount:size)
	}
	func set(_ val: [T], _ index: Int)
	{
		let siz = MemoryLayout<T>.size
		let base = siz*index
		let limit = base+siz*val.count
		let ptr = UnsafeMutablePointer<T>.allocate(capacity:siz*val.count)
		for (count,elem) in zip(0..<val.count,val) {ptr[count] = elem}
		set(UnsafeRawPointer(ptr),base..<limit)
		ptr.deallocate()
	}
	func set<S>(_ val: S, _ index: Int, _ field: Int)
	{
		let siz = MemoryLayout<S>.size
		let size = MemoryLayout<T>.size
		let base = size*index+field
		let limit = base+siz
		let ptr = UnsafeMutablePointer<S>.allocate(capacity:siz)
		ptr[0] = val
		set(UnsafeRawPointer(ptr),base..<limit)
		ptr.deallocate()
	}
	func set<S>(_ val: S, _ field: Int)
	{
		set(val,0,field)
	}
	func set(_ val: T, _ index: Int)
	{
		set([val],index)
	}
	func set(_ val: [T])
	{
		set(val,0)
	}
	func set(_ val: T)
	{
		set([val])
	}
	func get() -> MTLBuffer
	{
		if (last == nil && pend != nil) {
			refer = Refer()
			last = pend
			pend = nil
		}
		if (last == nil) {
			refer = Refer()
			last = device.makeBuffer(length:0)
		}
		lock.append(refer)
		refer.lock += 1
		return last
	}
	func get(_ len:Int) -> MTLBuffer
	{
		let unit:Int = MemoryLayout<T>.size
		let length:Int = len+(unit-len%unit)%unit;
		if (pend != nil && pend.length == length) {
			refer = Refer()
			last = pend
			pend = nil
		} else {
			refer = Refer()
			last = device.makeBuffer(length:length)
			pend = nil
		}
		lock.append(refer)
		refer.lock += 1
		return last
	}
}
func retLock() -> MTLCommandBufferHandler
{
	let temp = lock
	lock = []
	return {(MTLCommandBuffer) in for ref in temp {ref.lock -= 1}}
}
func retReady(_ size: Int) -> MTLCommandBufferHandler
{
	let last = pierce.get()
	return {(MTLCommandBuffer) in setReady(last,size)}
}
func setReady(_ buffer:MTLBuffer, _ size:Int)
{
	var found:Pierce = Pierce()
	var index = Int(-1)
	let pierces:[Pierce] = fromRaw(buffer.contents(),size)
	for (pierce,object) in zip(pierces,0..<size) {
		if (pierce.valid && (!found.valid || pierce.point.val.2 < found.point.val.2)) {
			found = pierce
			index = object
		}
	}
	toMutablee(found.point,found.normal,{(pnt,nml) in cb.write(pnt,nml,Int32(index))})
}
func retCount() -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in count -= 1; cb.wake()}
}
func setPierce() -> Int?
{
	guard let client = getClient(share.Pierce) else {return nil}
	let siz = Int(client.siz)
	let zero = Pierce()
	let vals = toList(zero,siz)
	pierce.set(vals,0..<siz)
	return siz
}
func getMode() -> share.Mode?
{
	return getClient(share.User)?.user?.pointee
}
func getRange() -> [share.Array]?
{
	guard let client = getClient(share.Range) else {return nil}
	if (client.siz == 0) {return []}
	return fromRaw(client.range,Int(client.siz))
}
func getActive() -> [share.Array]?
{
	guard let client = getClient(share.Active) else {return nil}
	if (client.siz == 0) {return []}
	return fromRaw(client.active,Int(client.siz))
}
func getClient(_ mem:share.Memory) -> share.Client?
{
	let any = Mirror(reflecting: cb.state).descendant(Int(mem.rawValue))
	let client:UnsafeMutablePointer<share.Client>? = fromAny(any)
	return client?.pointee
}
func getPoint() -> NSPoint
{
	var point = NSEvent.mouseLocation
	let frame:CGRect = window.frame
	point.x = point.x - NSMinX(frame)
	point.y = point.y - NSMinY(frame)
	return point
}
func setEvent(_ type:NSEvent.EventTypeMask, _ handler: @escaping (_:NSEvent) -> NSEvent?)
{
	NSEvent.addLocalMonitorForEvents(matching:type,handler:handler)
}

func loopInit()
{
	cb.call = loopCall
	cb.wake = loopWake
}
func loopCall()
{
	NSApp.run()
}
func loopWake()
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
func loopDone()
{
	print("loopDone")
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
	if (cb.esc >= 2) {NSApp.stop(nil)}
	return nil
}
func swiftLeft(event:NSEvent) -> NSEvent?
{
	let point = getPoint()
	let rect:CGRect = layer.frame
	if (NSPointInRect(point,rect)) {
		cb.click(0)
	}
	return event
}
func swiftRight(event:NSEvent) -> NSEvent?
{
	let point = getPoint()
	let rect:CGRect = layer.frame
	if (NSPointInRect(point,rect)) {
		cb.click(1)
	}
	return event
}
func swiftMove(event:NSEvent) -> NSEvent?
{
	let point = getPoint()
	let rect:CGRect = layer.frame
	if (NSPointInRect(point,rect)) {
		cb.move(Double(point.x),Double(point.y))
		// cb.move(Double(point.x),Double(point.y)) // TODO test
	}
	return event
}
func swiftRoll(event:NSEvent) -> NSEvent?
{
	cb.roll(Double(event.deltaX),Double(event.deltaY))
	// cb.roll(Double(event.deltaX),Double(event.deltaY)) // TODO test
	// let point = getPoint() // TODO test
	// cb.move(Double(point.x),Double(point.y)) // TODO test
	return event
}
func swiftCheck(event:NSEvent) -> NSEvent?
{
	if (cb.full() != 0) {
		return nil
	}
	if (cb.read() != 0) {
		cb.proc()
		cb.wake()
	}
	return nil
}
func swiftDrag(event:NSEvent) -> NSEvent?
{
	if (drag == nil) {return event}
	var point = NSEvent.mouseLocation
	point.x -= drag.x; point.y -= drag.y
	cb.drag(Double(point.x),Double(point.y))
	return event
}
func swiftClear(event:NSEvent) -> NSEvent?
{
	drag = nil
	return event
}
func swiftSize()
{
	let rect:CGRect = layer.frame
	cb.size(Double(NSMaxX(rect)),Double(NSMaxY(rect)))
	let frame:CGRect = window.frame
	cb.drag(Double(NSMinX(frame)),Double(NSMinY(frame)))
}
func swiftInit()
{
	let _ = NSApplication.shared
	NSApp.setActivationPolicy(.regular)
	NSApp.activate(ignoringOtherApps: true)
	if let temp = MTLCreateSystemDefaultDevice() {
		device = temp} else {print("cannot make device"); return}
	let rect = NSMakeRect(0, 0, CGFloat(WINWIDE), CGFloat(WINHIGH))
	if let temp = noWarn(CAMetalLayer()) {
		layer = temp} else {print("cannot make layer"); return}
	layer.device = device
	layer.pixelFormat = .bgra8Unorm
	layer.framebufferOnly = true
	layer.frame = rect
	if let temp = noWarn(NSView(frame:rect)) {
		view = temp} else {print("cannot make view"); return}
	view.layer = layer
	if let temp = noWarn(WindowDelegate()) {
		delegate = temp} else {print("cannot make delegate"); return}
	let mask:NSWindow.StyleMask = [.titled, .closable, .miniaturizable, .resizable]
	if let temp = noWarn(NSWindow(contentRect: rect, styleMask: mask, backing: .buffered, defer: true)) {
		window = temp} else {print("cannot make window"); return}
	window.title = "plane"
	window.makeKeyAndOrderFront(nil)
	window.contentView = view
	window.delegate = delegate
	if let temp = device.makeCommandQueue() {
		queue = temp} else {print("cannot make queue"); return}
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot make library"); return}
	guard let vertex_render = library.makeFunction(name:"vertex_render") else {
		print("cannot make vertex_render"); return}
	guard let fragment_render = library.makeFunction(name:"fragment_render") else {
		print("cannot make fragment_render"); return}
	guard let kernel_pierce = library.makeFunction(name:"kernel_pierce") else {
		print("cannot make kernel_pierce"); return}
	guard let pipe = noWarn(MTLRenderPipelineDescriptor()) else {
		print("cannot make pipe"); return}
	pipe.vertexFunction = vertex_render
	pipe.fragmentFunction = fragment_render
	pipe.colorAttachments[0].pixelFormat = .bgra8Unorm
	pipe.depthAttachmentPixelFormat = .depth32Float
	if let temp = try? device.makeRenderPipelineState(descriptor:pipe) {
		render = temp} else {print("cannot make render"); return}
	if let temp = noWarn(MTLRenderPassDescriptor()) {
    	descriptor = temp} else {print("cannot make descriptor"); return}
	let color = MTLClearColor(red: 0.0, green: 104.0/255.0, blue: 55.0/255.0, alpha: 1.0)
	descriptor.colorAttachments[0].clearColor = color
	descriptor.colorAttachments[0].loadAction = .clear
	descriptor.depthAttachment.clearDepth = 0.0 // clip xy -1 to 1; z 0 to 1
    guard let desc = noWarn(MTLDepthStencilDescriptor()) else {
    	print("cannot make desc"); return}
    desc.depthCompareFunction = .greater // left hand rule; z thumb to observer
    desc.isDepthWriteEnabled = true
    if let temp = device.makeDepthStencilState(descriptor: desc) {
    	depth = temp} else {print("cannot make depth"); return}
	if let temp = try? device.makeComputePipelineState(function:kernel_pierce) {
		compute = temp} else {print("cannot make compute"); return;}
    if let temp = noWarn(device.maxThreadsPerThreadgroup) {
    	threads = temp} else {print("cannot make thread"); return}
	cb.warp = swiftWarp
	cb.dma = swiftDma
	cb.draw = swiftDraw
	cb.full = swiftFull
	cb.done = swiftDone
	setEvent(.keyDown,swiftKey)
	setEvent(.leftMouseDown,swiftLeft)
	setEvent(.rightMouseDown,swiftRight)
	setEvent(.mouseMoved,swiftMove)
	setEvent(.scrollWheel,swiftRoll)
	setEvent(.applicationDefined,swiftCheck)
	setEvent(.leftMouseDragged,swiftDrag)
	setEvent(.leftMouseUp,swiftClear)
	cb.curs(Double(NSMinX(rect)),Double(NSMinY(rect)),
		Double(NSMaxX(rect)),Double(NSMaxY(rect)))
}
func swiftWarp(xpos:Double, ypos:Double)
{
	let point = getPoint()
	let frame:CGRect = window.frame
	let coord = CGPoint(x:NSMinX(frame)+point.x,y:NSMinY(frame)+point.y)
    CGWarpMouseCursorPosition(coord);	
}
func swiftWhole<T>(_ pend:Pend<T>, _ ptr:UnsafePointer<T>, _ src:Int32, _ dst:Int32, _ siz:Int32)
{
	let size = MemoryLayout<T>.size
	let base = Int(dst)*size
	let limit = (Int(dst)+Int(siz))*size
	pend.set(ptr.advanced(by:Int(src)),base..<limit)
}
func swiftPart<T,S>(_ pend:Pend<T>, _ ptr:UnsafePointer<S>, _ src:Int32, _ dst:Int32, _ fld:PartialKeyPath<T>)
{
	guard let field = MemoryLayout<T>.offset(of:fld) else {cb.err(#file,#line,-1);return}
	let part = MemoryLayout<S>.size
	let size = MemoryLayout<T>.size
	let base = Int(dst)*size+field
	let limit = Int(dst)*size+field+part
	pend.set(ptr.advanced(by:Int(src)),base..<limit)
}
func swiftPart<T,S>(_ pend:Pend<T>, _ val:S, _ dst:Int32, _ fld:PartialKeyPath<T>)
{
	guard let field = MemoryLayout<T>.offset(of:fld) else {cb.err(#file,#line,-1);return}
	let part = MemoryLayout<S>.size
	let size = MemoryLayout<T>.size
	let base = Int(dst)*size+field
	let limit = Int(dst)*size+field+part
	toPointre(val,{(ptr) in pend.set(ptr,base..<limit)})
}
func swiftDma(_ mem:share.Memory, _ idx:Int32, _ siz:Int32)
{
	guard let client = getClient(mem) else {cb.err(#file,#line,-1);return}
	switch (mem) {
	case (share.Triangle): swiftWhole(triangle,client.triangle,idx,idx,siz)
	case (share.Corner): swiftWhole(corner,client.corner,idx,idx,siz)
	case (share.Frame): swiftWhole(frame,client.frame,idx,idx,siz)
	case (share.Base): swiftWhole(base,client.base,idx,idx,siz)
	case (share.Basis): swiftPart(form,client.basis,idx,Int32(0),\Form.basis)
	case (share.Subject): swiftPart(form,client.subject,idx,Int32(0),\Form.subject)
	case (share.Object): swiftWhole(object,client.object,idx,idx,siz)
	case (share.Feature): swiftPart(form,client.feature,idx,Int32(0),\Form.feature)
	case (share.Render):
	if (idx == 0 && siz > 0) {swiftPart(form,client.render,idx,Int32(0),\Form.feather)}
	if (idx+siz > 1) {swiftPart(form,client.render,idx+1,Int32(0),\Form.arrow)}
	case (share.Pierce):
	if (idx == 0 && siz > 0) {swiftPart(form,client.pierce,idx,Int32(0),\Form.feather)}
	if (idx+siz > 1) {swiftPart(form,client.pierce,idx+1,Int32(0),\Form.arrow)}
	case (share.Cloud): swiftWhole(cloud,client.cloud,idx,idx,siz)
	swiftPart(form,client.siz,Int32(0),\Form.siz)
	case (share.User): swiftPart(form,client.user.pointee.hand,Int32(0),\Form.hand)
	default: cb.err(#file,#line,-1);return}
}
func swiftDraw()
{
	guard let shader = getMode()?.shader else {cb.err(#file,#line,-1);return}
	if (shader == share.Display) {
		guard let code = queue.makeCommandBuffer() else {cb.err(#file,#line,-1);return}
		guard let range = getRange() else {cb.err(#file,#line,-1);return}
		guard let field = MemoryLayout<Form>.offset(of:\Form.tag) else {cb.err(#file,#line,-1);return}
	    guard let draw = layer.nextDrawable() else {cb.err(#file,#line,-1);return}
		descriptor.colorAttachments[0].texture = draw.texture
		if (range.count == 0) {
			guard let encode = code.makeRenderCommandEncoder(descriptor:descriptor) else {cb.err(#file,#line,-1);return}
			encode.endEncoding()
		}
		for array in range {
			form.set(UInt32(array.tag),field)
			guard let encode = code.makeRenderCommandEncoder(descriptor:descriptor) else {cb.err(#file,#line,-1);return}
			encode.setRenderPipelineState(render)
			encode.setDepthStencilState(depth)
			encode.setVertexBuffer(triangle.get(),offset:0,index:0)
			encode.setVertexBuffer(corner.get(),offset:0,index:1)
			encode.setVertexBuffer(frame.get(),offset:0,index:2)
			encode.setVertexBuffer(object.get(),offset:0,index:3)
			encode.setVertexBuffer(form.get(),offset:0,index:4)
			encode.drawPrimitives(
				type:.triangle,
				vertexStart:Int(array.idx),
				vertexCount:Int(array.siz))
			encode.endEncoding()
		}
		code.present(draw)
		code.addScheduledHandler(retLock())
		code.addCompletedHandler(retCount())
		count += 1
		code.commit()
	} else if (shader == share.Track) {
		guard let size = setPierce() else {cb.err(#file,#line,-1);return}
		guard let code = queue.makeCommandBuffer() else {cb.err(#file,#line,-1);return}
		guard let active = getActive() else {cb.err(#file,#line,-1);return}
		guard let field = MemoryLayout<Form>.offset(of:\Form.tag) else {cb.err(#file,#line,-1);return}
		for array in active {
			form.set(UInt32(array.tag),field)
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
				guard let encode = code.makeComputeCommandEncoder() else {cb.err(#file,#line,-1);return}
				encode.setComputePipelineState(compute)
				encode.setBuffer(triangle.get(),offset:0,index:0)
				encode.setBuffer(corner.get(),offset:0,index:1)
				encode.setBuffer(base.get(),offset:offset,index:2)
				encode.setBuffer(object.get(),offset:0,index:3)
				encode.setBuffer(form.get(),offset:0,index:4)
				encode.setBuffer(pierce.get(),offset:0,index:5)
				let num = MTLSize(width:n,height:1,depth:1)
				let per = MTLSize(width:p,height:1,depth:1)
				encode.dispatchThreadgroups(num,threadsPerThreadgroup:per)
				encode.endEncoding()
				offset += n*p*MemoryLayout<share.Vertex>.size
			}
		}
		code.addCompletedHandler(retReady(size))
		code.addScheduledHandler(retLock())
		code.addCompletedHandler(retCount())
		count += 1
		code.commit()
	}
}
func swiftFull() -> Int32
{
	if (count < 3) {return 0}
	return 1
}
func swiftDone()
{
	print("swiftDone")
}

	for arg in CommandLine.arguments {
	shareArg(arg)}
	shareInit() // can write to pipes
	cb.start() // can schedule events
	loopInit() // can read from pipes
	threadInit() // will schedule pipe reads
	cb.call() // will handle events
	cb.done()
	threadDone()
	loopDone()
	shareDone()
