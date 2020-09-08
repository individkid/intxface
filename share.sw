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
var window:NSWindow!
var queue:MTLCommandQueue!
var render:MTLRenderPipelineState!
var param:MTLRenderPassDescriptor!
var depth:MTLDepthStencilState!
var compute:MTLComputePipelineState!
var threads:MTLSize!

var triangle = Pend<share.Facet>()
var corner = Pend<share.Vertex>()
var frame = Pend<share.Index>()
var base = Pend<CInt>()
var object = Pend<share.Affine>()
var cloud = Pend<share.Vector>()
var form = Pend<Form>()
var pierce = Pend<Pierce>()

var lock = [Refer]()
var count = Int(0)
let event = getEvent()

struct Form
{
	var basis:share.Linear
	var subject:share.Affine
	var feature:share.Affine
	var feather:share.Vector
	var arrow:share.Vector
	var focal:share.Vector
	var picture:share.Vector
	var siz:uint
	var hand:uint
	var pad0:uint
	var pad1:uint
}
struct Pierce
{
	var valid:Bool = false
	var pad:(uint,uint,uint) = (0,0,0)
	var point:share.Vector = share.Vector(val:(0.0,0.0,0.0,0.0))
	var normal:share.Vector = share.Vector(val:(0.0,0.0,0.0,0.0))
}
class Refer
{
	var lock:Int = 0
}

func toMutable<T>(_ list:[T], _ fnc:(_:Int,_:UnsafeMutablePointer<T>)->Void)
{
	let ptr = UnsafeMutablePointer<T>.allocate(capacity:list.count)
	for (val,idx) in zip(list,Swift.Array(0..<list.count)) {ptr[idx] = val}
	fnc(list.count,ptr)
	ptr.deallocate()
}
class Pend<T>
{
	var pend:MTLBuffer!
	var last:MTLBuffer!
	var refer:Refer!
	func set(_ ptr: UnsafeRawPointer, _ range: Range<Int>)
	{
		let len:Int = range.upperBound
		let unit:Int = MemoryLayout<T>.size
		let length:Int = len+(unit-len%unit)%unit;
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
		if (pend != nil) {
			let base:Int = range.lowerBound
			let size:Int = range.upperBound-range.lowerBound
			pend.contents().advanced(by:base).copyMemory(from:ptr,byteCount:size)
		}
	}
	func set(_ vals: [T], _ index: Int)
	{
		let siz = MemoryLayout<T>.size
		let base = siz*index
		let limit = base+siz*vals.count
		toMutable(vals) {(len,ptr) in
		set(UnsafePointer<T>(ptr),base..<limit)}
	}
	func set(_ val: [T]?, _ index: Int)
	{
		guard let vals = val else {return}
		set(vals,index)
	}
	func set<S>(_ val: S, _ field: PartialKeyPath<T>)
	{
		guard let fld = MemoryLayout<T>.offset(of:field) else {cb.err(#file,#line,-1);return}
		let siz = MemoryLayout<S>.size
		toMutable([val]) {(len,ptr) in
		set(UnsafePointer<S>(ptr),fld..<fld+siz)}
	}
	func get() -> MTLBuffer
	{
		if (last == nil && pend != nil) {
			refer = Refer()
			last = pend
			pend = nil
		}
		if (last != nil) {
			lock.append(refer)
			refer.lock += 1
		}
		return last
	}
}
func setPierce() -> Int?
{
	guard let client = getClient(share.Base,0) else {return nil}
	let siz = Int(client.siz)
	let zero = Pierce()
	let vals = Swift.Array(repeating: zero, count: siz)
	pierce.set(vals,0)
	return siz
}
func getClient(_ mem:share.Memory, _ siz:Int) -> share.Client?
{
	let any = Mirror(reflecting: cb.state).descendant(Int(mem.rawValue))
	let client = any as? UnsafeMutablePointer<share.Client>
	guard let temp = client?.pointee else {return nil}
	if (temp.siz < siz) {return nil}
	if (temp.mem != mem) {return nil}
	return temp
}
func getMemory<T>(_ mem:share.Memory, _ idx:Int, _ len:Int, _ fnc:(share.Client) -> UnsafeMutablePointer<T>?) -> [T]?
{
	guard let client = getClient(mem,idx+len) else {return nil}
	guard let ptr = fnc(client) else {return []}
	return Swift.Array(0..<len).map() {(sub) in ptr[idx+sub]}
}
func getMemory<T>(_ mem:share.Memory, _ idx:Int, _ fnc:(share.Client) -> UnsafeMutablePointer<T>?) -> T?
{
	guard let client = getClient(mem,idx+1) else {return nil}
	guard let ptr = fnc(client) else {return nil}
	return ptr[idx]
}
func getMemory<T>(_ mem:share.Memory, _ fnc:(share.Client) -> UnsafeMutablePointer<T>?) -> [T]?
{
	guard let client = getClient(mem,0) else {return nil}
	let len = Int(client.siz)
	guard let ptr = fnc(client) else {return []}
	return Swift.Array(0..<len).map() {(sub) in ptr[sub]}
}
func getRect() -> NSRect
{
	if let temp = window {
		return temp.contentRect(forFrameRect:temp.frame)
	} else {
		return NSMakeRect(0.0, 0.0, CGFloat(WINWIDE), CGFloat(WINHIGH))
	}
}
func getPoint() -> NSPoint
{
	var point = NSEvent.mouseLocation
	let frame:CGRect = window.frame
	point.x = point.x - NSMinX(frame)
	point.y = point.y - NSMinY(frame)
	return point
}
func getTexture(_ rect:NSRect) -> MTLTexture?
{
	let text = MTLTextureDescriptor()
	text.height = Int(rect.height)
	text.width = Int(rect.width)
	text.pixelFormat = .depth32Float
	text.storageMode = .private
	return device.makeTexture(descriptor:text)
}
func getCheck() -> Bool
{
	if (cb.full() != 0) {
		return true
	}
	if (cb.read() != 0) {
		cb.proc()
		cb.wake()
		return true
	}
	return false
}

func setEvent(_ type:NSEvent.EventTypeMask, _ handler: @escaping (_:NSEvent) -> NSEvent?)
{
	NSEvent.addLocalMonitorForEvents(matching:type,handler:handler)
}
func getLock() -> MTLCommandBufferHandler
{
	let temp = lock
	lock = []
	return {(MTLCommandBuffer) in for ref in temp {ref.lock -= 1}}
}
func getReady(_ size: Int) -> MTLCommandBufferHandler
{
	if (size == 0) {return {(MTLCommandBuffer) in swiftEmpty()}}
	let last = pierce.get()
	return {(MTLCommandBuffer) in swiftReady(last,size)}
}
func getCount() -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in count -= 1; cb.wake()}
}
class getEvent : NSObject, NSWindowDelegate
{
	func windowShouldClose(_ sender: NSWindow) -> Bool
	{
		loopStop()
		return true
	}
	func windowDidResize(_ notification: Notification)
	{
		swiftSize()
	}
}

func swiftReady(_ buffer:MTLBuffer, _ size:Int)
{
	if (buffer.length < size*MemoryLayout<Pierce>.size) {cb.err(#file,#line,-1);return}
	var found:Pierce = Pierce()
	guard let cursor = getMemory(share.Archer,0,{$0.archer}) else {cb.err(#file,#line,-1);return}
	let xpos = cursor.val.0; let ypos = cursor.val.1
	found.point.val.0 = xpos; found.normal.val.0 = 0.0
	found.point.val.1 = ypos; found.normal.val.1 = 0.0
	found.point.val.2 = 0.0; found.normal.val.2 = 1.0
	var index = size
	let raw = buffer.contents()
	let siz = MemoryLayout<Pierce>.size
	let pierces:[Pierce] = Swift.Array(0..<size).map() {(sub) in
	raw.advanced(by:sub*siz).load(as:Pierce.self)}
	for (pierce,plane) in zip(pierces,0..<size) {
		if (pierce.valid && (!found.valid || pierce.point.val.2 > found.point.val.2)) {
			found = pierce
			index = plane
		}
	}
	toMutable([found.point])
		{(points:Int,point:UnsafeMutablePointer<share.Vector>) in
	toMutable([found.normal])
		{(normals:Int,normal:UnsafeMutablePointer<share.Vector>) in
	cb.write(point,normal,CInt(index))
		}}
}
func swiftEmpty()
{
	let found:Pierce = Pierce()
	guard let object = getClient(Object,1) else {cb.err(#file,#line,-1);return}
	let index = Int(object.siz)
	toMutable([found.point])
		{(points:Int,point:UnsafeMutablePointer<share.Vector>) in
	toMutable([found.normal])
		{(normals:Int,normal:UnsafeMutablePointer<share.Vector>) in
	cb.write(point,normal,CInt(index))
		}}
}
func swiftSize()
{
	let rect = window.contentRect(forFrameRect:window.frame)
	let wide = Double(rect.width)
	let high = Double(rect.height)
	cb.drag(0.0,0.0,wide/2.0,high/2.0)
	let size = CGSize(width:rect.width,height:rect.height)
	layer.drawableSize = size
	if let temp = getTexture(rect) {
		param.depthAttachment.texture = temp} else {return}
	while (getCheck()) {}
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
	if (cb.esc >= 2) {loopStop()}
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
	}
	return event
}
func swiftRoll(event:NSEvent) -> NSEvent?
{
	cb.roll(Double(event.deltaX),Double(event.deltaY))
	return event
}
func swiftCheck(event:NSEvent) -> NSEvent?
{
	let _ = getCheck()
	return nil
}

func swiftInit()
{
	device = MTLCreateSystemDefaultDevice()
	let rect = NSMakeRect(
		CGFloat(cb.conf(share.PictureMinX)), CGFloat(cb.conf(share.PictureMinY)),
		CGFloat(cb.conf(share.PictureWide)), CGFloat(cb.conf(share.PictureHigh)))
	layer = CAMetalLayer()
	layer.device = device
	layer.pixelFormat = .bgra8Unorm
	layer.framebufferOnly = true
	layer.frame = rect
	view = NSView(frame:rect)
	view.wantsLayer = true
	view.layer = layer
	let mask:NSWindow.StyleMask = [.titled, .closable, .miniaturizable, .resizable]
	window = NSWindow(contentRect: rect, styleMask: mask, backing: .buffered, defer: true)
	window.title = "plane"
	window.makeKeyAndOrderFront(nil)
	window.contentView = view
	window.delegate = event
	queue = device.makeCommandQueue()
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot make library");cb.err(#file,#line,-1);return}
	guard let vertex_render = library.makeFunction(name:"vertex_render") else {
		print("cannot make vertex_render");cb.err(#file,#line,-1);return}
	guard let fragment_render = library.makeFunction(name:"fragment_render") else {
		print("cannot make fragment_render");cb.err(#file,#line,-1);return}
	guard let kernel_pierce = library.makeFunction(name:"kernel_pierce") else {
		print("cannot make kernel_pierce");cb.err(#file,#line,-1);return}
	let pipe = MTLRenderPipelineDescriptor()
	pipe.vertexFunction = vertex_render
	pipe.fragmentFunction = fragment_render
	pipe.colorAttachments[0].pixelFormat = .bgra8Unorm
	pipe.depthAttachmentPixelFormat = .depth32Float
	render = try? device.makeRenderPipelineState(descriptor:pipe)
	let color = MTLClearColor(red: 0.0, green: 104.0/255.0, blue: 55.0/255.0, alpha: 1.0)
	param = MTLRenderPassDescriptor()
	param.colorAttachments[0].clearColor = color
	param.colorAttachments[0].storeAction = .store
	param.depthAttachment.clearDepth = 0.0 // clip xy -1 to 1; z 0 to 1
	param.depthAttachment.storeAction = .dontCare
	param.depthAttachment.texture = getTexture(rect)
    let desc = MTLDepthStencilDescriptor()
    desc.depthCompareFunction = .greater // left hand rule; z thumb to observer
    desc.isDepthWriteEnabled = true
    depth = device.makeDepthStencilState(descriptor: desc)
	compute = try? device.makeComputePipelineState(function:kernel_pierce)
    threads = device.maxThreadsPerThreadgroup
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
}
func swiftWarp(xpos:Double, ypos:Double)
{
	let frame:CGRect = window.frame
	guard let screen:NSRect = NSScreen.main?.frame else {cb.err(#file,#line,-1);return}
	let coord = CGPoint(x:NSMinX(frame)+CGFloat(xpos),y:screen.maxY-NSMinY(frame)-CGFloat(ypos))
    CGWarpMouseCursorPosition(coord);	
}
func swiftDma(_ mem:share.Memory, _ idx:CInt, _ siz:CInt)
{
	switch (mem) {
	case (share.Triangle):
	triangle.set(getMemory(mem,Int(idx),Int(siz),{$0.triangle}),Int(idx))
	case (share.Corner):
	corner.set(getMemory(mem,Int(idx),Int(siz),{$0.corner}),Int(idx))
	case (share.Frame):
	frame.set(getMemory(mem,Int(idx),Int(siz),{$0.frame}),Int(idx))
	case (share.Base):
	base.set(getMemory(mem,Int(idx),Int(siz),{$0.base}),Int(idx))
	case (share.Basis):
	form.set(getMemory(mem,0,{$0.basis}),\Form.basis)
	case (share.Subject):
	form.set(getMemory(mem,0,{$0.subject}),\Form.subject)
	case (share.Object):
	object.set(getMemory(mem,Int(idx),Int(siz),{$0.object}),Int(idx))
	case (share.Feature):
	form.set(getMemory(mem,0,{$0.feature}),\Form.feature)
	case (share.Render):
	form.set(getMemory(mem,0,{$0.render}),\Form.focal)
	form.set(getMemory(mem,1,{$0.render}),\Form.picture)
	case (share.Archer):
	form.set(getMemory(mem,0,{$0.archer}),\Form.feather)
	form.set(getMemory(mem,1,{$0.archer}),\Form.arrow)
	case (share.Cloud):
	let memory = getMemory(mem,Int(idx),Int(siz),{$0.cloud})
	cloud.set(memory,Int(idx))
	if let temp = memory {
	form.set(temp.count,\Form.siz)} else {
	form.set(0,\Form.siz)}
	case (share.User):
	if let temp = getMemory(mem,0,{$0.user}) {
	form.set(temp.hand,\Form.hand)} else {
	form.set(0,\Form.hand)}
	default: cb.err(#file,#line,-1);return}
}
func swiftDraw(_ shader:share.Shader)
{
	switch (shader) {
	case (share.Display):
	guard let code = queue.makeCommandBuffer() else {cb.err(#file,#line,-1);return}
    guard let draw = layer.nextDrawable() else {cb.err(#file,#line,-1);return}
	param.colorAttachments[0].texture = draw.texture
	param.colorAttachments[0].loadAction = .clear
	param.depthAttachment.loadAction = .clear
	guard let range:[share.Array] = getMemory(share.Range,{$0.range}) else {cb.err(#file,#line,-1);return}
	if (range.count == 0) {
		guard let encode = code.makeRenderCommandEncoder(descriptor:param) else {cb.err(#file,#line,-1);return}
		encode.endEncoding()
	}
	for array in range {
		guard let encode = code.makeRenderCommandEncoder(descriptor:param) else {cb.err(#file,#line,-1);return}
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
		param.colorAttachments[0].loadAction = .load
		param.depthAttachment.loadAction = .load
	}
	code.present(draw)
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()
	case (share.Track):
	guard let size = setPierce() else {cb.err(#file,#line,-1);return}
	guard let code = queue.makeCommandBuffer() else {cb.err(#file,#line,-1);return}
	guard let active = getMemory(share.Active,{$0.active}) else {cb.err(#file,#line,-1);return}
	for array in active {
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
	code.addCompletedHandler(getReady(size))
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()
	default: cb.err(#file,#line,-1);return}
}
func swiftFull() -> CInt
{
	if (count < 3) {return 0}
	return 1
}
func swiftDone()
{
	print("swiftDone")
}

func loopConf(_ config:share.Config) -> Double
{
	let rect = getRect()
	guard let screen:NSRect = NSScreen.main?.frame else {cb.err(#file,#line,-1);return 0.0}
	let wide = rect.maxX-rect.minX
	let high = rect.maxY-rect.minY
	switch (config) {
	case (share.PictureMinX): return Double(-wide/2.0)
	case (share.PictureMinY): return Double(-high/2.0)
	case (share.PictureWide): return Double(wide)
	case (share.PictureHigh): return Double(high)
	case (share.DefaultWide): return Double(WINWIDE)
	case (share.DefaultHigh): return Double(WINHIGH)
	case (share.DefaultDeep): return Double(WINDEEP)
	case (share.DefaultLong): return Double(WINLONG)
	case (share.DefaultStop): return Double(WINSTOP)
	case (share.DefaultUnit): return Double(LENGTH)
	case (share.DefaultPole): return Double(ANGLE)
	case (share.DefaultBase): return Double(POWER)
	case (share.ScreenWide): return Double(screen.maxX)
	case (share.ScreenHigh): return Double(screen.maxY)
	case (share.LeverDeep): return Double(TIPDEEP)
	default: cb.err(#file,#line,-1);return 0.0}
}
func loopInit()
{
	let _ = NSApplication.shared
	NSApp.setActivationPolicy(.accessory)
	NSApp.activate(ignoringOtherApps: true)
    cb.conf = loopConf
	cb.call = loopCall
	cb.wake = loopWake
}
func loopCall()
{
	NSApp.run()
}
func loopStop()
{
	NSApp.stop(nil)
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

	for arg in CommandLine.arguments {
	shareArg(arg)}
	loopInit() // can read from pipes
	shareInit() // can write to pipes
	cb.start() // can schedule events
	threadInit() // will schedule pipe reads
	cb.call() // will handle events
	threadDone()
	cb.done()
	shareDone()
	loopDone()
