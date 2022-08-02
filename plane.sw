import argx
import type
import plane
import face
import AppKit
import Metal

var device:MTLDevice!
var layer:CAMetalLayer!
var view:NSView!
var window:NSWindow!
var queue:MTLCommandQueue!
var render:MTLRenderPipelineState!
var param:MTLRenderPassDescriptor!
var depth:MTLDepthStencilState!
var compute:MTLComputePipelineState!
var threads:MTLSize!

var lock = [Refer]()
var count = Int(0)
let event = getEvent()

var triangle = Pend<Triangle>()
var numeric = Pend<Numeric>()
var vertex = Pend<Vertex>()
var matrix = Pend<Matrix>()
var swarm = Pend<Vector>()
var texture = Pend<Vector>()
var uniform = Pend<Uniform>()
var pierce = Pend<Pierce>()
var array = [Ranje]()
var size = 0 // TODO use planeConfig(Pierce*)

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
	func set(_ vals:[T], _ index: Int, _ length : Int)
	{
		var sub = 0
		var idx = index%length
		while (vals.count-sub > length) {
			set(Array(vals[sub...(sub+length-idx-1)]),idx)
			sub = sub + length - idx; idx = 0}
		set(Array(vals[sub...(vals.count-1)]),idx)
	}
	func set(_ val: [T]?, _ index: Int)
	{
		guard let vals = val else {return}
		set(vals,index)
	}
	func set<S>(_ val: S, _ field: PartialKeyPath<T>)
	{
		guard let fld = MemoryLayout<T>.offset(of:field) else {exitErr(#file,#line,-1);return}
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
func getRect() -> NSRect
{
	if let temp = window {
		return temp.contentRect(forFrameRect:temp.frame)
	} else {
		return NSMakeRect(0.0, 0.0, 
			CGFloat(planeConfig(WindowWide)),
			CGFloat(planeConfig(WindowHigh)))
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
func getLock() -> MTLCommandBufferHandler
{
	let temp = lock
	lock = []
	return {(MTLCommandBuffer) in for ref in temp {ref.lock -= 1}}
}
func getReady() -> MTLCommandBufferHandler
{
	if (size == 0) {return {(MTLCommandBuffer) in planeEmpty()}}
	let last = pierce.get()
	return {(MTLCommandBuffer) in planeReady(UnsafeMutablePointer<Pierce>(OpaquePointer(last.contents())),Int32(size))}
}
func getCount() -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in count -= 1; planeWake(Int32(count))}
}
func getThird(_ range: Ranje) -> Ranje
{
	var result = Ranje()
	result.idx = range.idx/3
	result.siz = range.siz/3
	return result
}
class getEvent : NSObject, NSWindowDelegate
{
	func windowShouldClose(_ sender: NSWindow) -> Bool
	{
		// loopStop()
		return true
	}
	func windowDidResize(_ notification: Notification)
	{
		// swiftSize()
	}
}
func setEvent(_ type:NSEvent.EventTypeMask, _ handler: @escaping (_:NSEvent) -> NSEvent?)
{
	NSEvent.addLocalMonitorForEvents(matching:type,handler:handler)
}
func swiftInit()
{
	for arg in CommandLine.arguments {useArgument(arg)}
	device = MTLCreateSystemDefaultDevice()
	let rect = NSMakeRect(
		CGFloat(planeConfig(WindowLeft)), CGFloat(planeConfig(WindowBase)),
		CGFloat(planeConfig(WindowWide)), CGFloat(planeConfig(WindowHigh)))
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
	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"planeG.so") else {
		print("cannot make library");exitErr(#file,#line,-1);return}
	guard let vertex_render = library.makeFunction(name:"vertex_render") else {
		print("cannot make vertex_render");exitErr(#file,#line,-1);return}
	guard let fragment_render = library.makeFunction(name:"fragment_render") else {
		print("cannot make fragment_render");exitErr(#file,#line,-1);return}
	guard let kernel_pierce = library.makeFunction(name:"kernel_pierce") else {
		print("cannot make kernel_pierce");exitErr(#file,#line,-1);return}
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
	// setEvent(.keyDown,swiftKey)
	// setEvent(.leftMouseUp,swiftLeft)
	// setEvent(.rightMouseUp,swiftRight)
	// setEvent(.leftMouseDown,swiftActive)
	// setEvent(.rightMouseDown,swiftActive)
	// setEvent(.mouseMoved,swiftMove)
	// setEvent(.scrollWheel,swiftRoll)
	// setEvent(.applicationDefined,swiftCheck)
}
func swiftMemory(_ ptr: UnsafeMutablePointer<Client>?)
{
	let client = ptr!.pointee
	let siz = Int(client.siz)
	let idx = Int(client.idx)
	switch (client.mem) {
	case (Trianglez): triangle.set(Swift.Array(0..<siz).map() {(sub) in client.tri![sub]},idx)
	case (Numericz): numeric.set(Swift.Array(0..<siz).map() {(sub) in client.num![sub]},idx)
	case (Vertexz): vertex.set(Swift.Array(0..<siz).map() {(sub) in client.vtx![sub]},idx)
	case (Piercez): pierce.set(Swift.Array(repeating: Pierce(), count: siz),idx)
	case (Swarmz): swarm.set(Swift.Array(0..<siz).map() {(sub) in client.swa![sub]},idx)
	case (Texturez): texture.set(Swift.Array(0..<siz).map() {(sub) in client.tex![sub]},idx)
	case (Basisz): uniform.set(client.bas![0],\Uniform.bas)
	case (Allmatz): uniform.set(client.all![0],\Uniform.all)
	case (Fewmatz): matrix.set(Swift.Array(0..<siz).map() {(sub) in client.few![sub]},idx)
	case (Onematz): uniform.set(client.one![0],\Uniform.one)
	case (Ranjez): array = Swift.Array(0..<siz).map() {(sub) in client.rng![sub]}
	case (Configurez): for sub in Swift.Array(0..<siz) {switch client.cfg![sub] {
		case (UniformFeather): uniform.set(client.val![sub],\Uniform.fea)
		case (UniformArrow): uniform.set(client.val![sub],\Uniform.arw)
		case (UniformPicture): uniform.set(client.val![sub],\Uniform.pic)
		case (UniformFocal): uniform.set(client.val![sub],\Uniform.foc)
		case (UniformIndex): uniform.set(Int(client.val![sub]),\Uniform.idx)
		case (UniformSize): uniform.set(Int(client.val![sub]),\Uniform.siz)
		case (TriangleSize): uniform.set(Int(client.val![sub]),\Uniform.tsz)
		case (TriangleLimit): uniform.set(Int(client.val![sub]),\Uniform.tlm)
		case (TriangleIndex): uniform.set(Int(client.val![sub]),\Uniform.tid)
		case (NumericSize): uniform.set(Int(client.val![sub]),\Uniform.nsz)
		case (NumericLimit): uniform.set(Int(client.val![sub]),\Uniform.nlm)
		case (NumericIndex): uniform.set(Int(client.val![sub]),\Uniform.nid)
		case (VertexSize): uniform.set(Int(client.val![sub]),\Uniform.vsz)
		case (VertexLimit): uniform.set(Int(client.val![sub]),\Uniform.vlm)
		case (VertexIndex): uniform.set(Int(client.val![sub]),\Uniform.vid)
		case (PierceSize): uniform.set(Int(client.val![sub]),\Uniform.psz)
		case (PierceLimit): uniform.set(Int(client.val![sub]),\Uniform.plm)
		case (PierceIndex): uniform.set(Int(client.val![sub]),\Uniform.pid)
		case (ObjectSize): uniform.set(Int(client.val![sub]),\Uniform.osz)
		case (ObjectLimit): uniform.set(Int(client.val![sub]),\Uniform.olm)
		case (ObjectIndex): uniform.set(Int(client.val![sub]),\Uniform.oid)
		case (SwarmSize): uniform.set(Int(client.val![sub]),\Uniform.ssz)
		case (SwarmLimit): uniform.set(Int(client.val![sub]),\Uniform.slm)
		case (SwarmIndex): uniform.set(Int(client.val![sub]),\Uniform.sid)
		case (TextureSize): uniform.set(Int(client.val![sub]),\Uniform.xsz)
		case (TextureLimit): uniform.set(Int(client.val![sub]),\Uniform.xlm)
		case (TextureIndex): uniform.set(Int(client.val![sub]),\Uniform.xid)
		default: break}}
	default: exitErr(#file,#line,-1)}
}
func swiftDraw(_ shader: Shader)
{
	switch (shader) {
	case (Dipoint):
	guard let code = queue.makeCommandBuffer() else {exitErr(#file,#line,-1);return}
    guard let draw = layer.nextDrawable() else {exitErr(#file,#line,-1);return}
	param.colorAttachments[0].texture = draw.texture
	param.colorAttachments[0].loadAction = .clear
	param.depthAttachment.loadAction = .clear
	if (array.count == 0) {
		guard let encode = code.makeRenderCommandEncoder(descriptor:param) else {exitErr(#file,#line,-1);return}
		encode.endEncoding()
	}
	for range in array { // use planeConfig(DrawStart) and planeConfig(DrawStop)
		guard let encode = code.makeRenderCommandEncoder(descriptor:param) else {exitErr(#file,#line,-1);return}
		encode.setRenderPipelineState(render)
		encode.setDepthStencilState(depth)
		encode.setVertexBuffer(triangle.get(),offset:0,index:0)
		encode.setVertexBuffer(numeric.get(),offset:0,index:1)
		encode.setVertexBuffer(vertex.get(),offset:0,index:2)
		encode.setVertexBuffer(matrix.get(),offset:0,index:3)
		encode.setVertexBuffer(uniform.get(),offset:0,index:4)
		encode.drawPrimitives(
			type:.triangle,
			vertexStart:Int(range.idx),
			vertexCount:Int(range.siz))
		encode.endEncoding()
		param.colorAttachments[0].loadAction = .load
		param.depthAttachment.loadAction = .load
	}
	code.present(draw)
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()
	case (Adpoint):
	guard let code = queue.makeCommandBuffer() else {exitErr(#file,#line,-1);return}
	for range in array { // use planeConfig(DrawStart) and planeConfig(DrawStop)
		var offset = (Int(range.idx)*MemoryLayout<Triangle>.size)/3
		var nums:[Int] = []
		var pers:[Int] = []
		let quotient = (Int(range.siz)/threads.width)/3
		let remainder = (Int(range.siz)%threads.width)/3
		if (quotient > 0) {
			nums.append(quotient)
			pers.append(threads.width)}
		if (remainder > 0) {
			nums.append(1)
			pers.append(remainder)}
		for (n,p) in zip(nums,pers) {
			guard let encode = code.makeComputeCommandEncoder() else {exitErr(#file,#line,-1);return}
			encode.setComputePipelineState(compute)
			encode.setBuffer(triangle.get(),offset:offset,index:0)
			encode.setBuffer(numeric.get(),offset:0,index:1)
			encode.setBuffer(vertex.get(),offset:0,index:2)
			encode.setBuffer(matrix.get(),offset:0,index:3)
			encode.setBuffer(uniform.get(),offset:0,index:4)
			encode.setBuffer(pierce.get(),offset:0,index:5)
			let num = MTLSize(width:n,height:1,depth:1)
			let per = MTLSize(width:p,height:1,depth:1)
			encode.dispatchThreadgroups(num,threadsPerThreadgroup:per)
			encode.endEncoding()
			offset += n*p*MemoryLayout<Triangle>.size
		}
	}
	code.addCompletedHandler(getReady())
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()
	default: exitErr(#file,#line,-1);return
	}
}

// MAIN

	planeInit(swiftInit,swiftMemory,swiftDraw)
