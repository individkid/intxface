import face
import type
import plane
import AppKit
import Metal

var device:MTLDevice!
var layer:CAMetalLayer!
var view:NSView!
var window:NSWindow!
var queue:MTLCommandQueue!
var render = [MTLRenderPipelineState?](repeating: nil, count: Int(Micros.rawValue))
var param = [MTLRenderPassDescriptor?](repeating: nil, count: Int(Micros.rawValue))
var depth = [MTLDepthStencilState?](repeating: nil, count: Int(Micros.rawValue))
var compute = [MTLComputePipelineState?](repeating: nil, count: Int(Micros.rawValue))
var threads = [MTLSize?](repeating: nil, count: Int(Micros.rawValue))

var lock1 = Lock()
var pool1 = Pool()
var lock2 = Lock()
var pool2 = Pool()
var count = Int(0)
var delta = 0.0

var triangle = Pend<Triangle>(lock1,pool1)
var numeric = Pend<Numeric>(lock1,pool1)
var vertex = Pend<Vertex>(lock1,pool1)
var subject = Pend<Matrix>(lock1,pool1)
var object = Pend<Matrix>(lock1,pool1)
var element = Pend<Matrix>(lock1,pool1)
var swarm = Pend<Vector>(lock1,pool1)
var texture = Pend<Vector>(lock1,pool1)
var textual = Pend<Int32>(lock1,pool1)
var basis = Pend<Basis>(lock1,pool1)
var uniform = Pend<Uniform>(lock1,pool1)
var pierce = Pend<Pierce>(lock2,pool2)
var array = [Slice]()
let semaphor = DispatchSemaphore(value:0)
var goon = true

func getRect() -> NSRect
{
	let orig = window.frame.origin
	let size = view.frame.size
	let full = NSScreen.main!.visibleFrame.size
	let w = size.width/2.0
	let h = size.height/2.0
	let x = orig.x+w-full.width/2.0
	let y = orig.y+h-full.height/2.0
	return NSMakeRect(x,y,w,h)
}
func getPoint() -> NSPoint
{
	let orig = NSEvent.mouseLocation
	let full = NSScreen.main!.visibleFrame.size
	let x = orig.x-full.width/2.0
	let y = orig.y-full.height/2.0
	return NSMakePoint(x,y)
}
func getSize() -> NSSize
{
	return NSScreen.main!.visibleFrame.size
}
func setFrame()
{
	layer.frame = view.frame
}
class Refer
{
	var lock:Int = 0
	var pool:Pool!
	init(_ l:Int, _ p: Pool)
	{
		lock = l
		pool = p
	}
}
func toMutable<T>(_ list:[T], _ fnc:(_:Int,_:UnsafeMutablePointer<T>)->Void)
{
	let ptr = UnsafeMutablePointer<T>.allocate(capacity:list.count)
	for (val,idx) in zip(list,Swift.Array(0..<list.count)) {ptr[idx] = val}
	fnc(list.count,ptr)
	ptr.deallocate()
}
func toArray<T>(_ list: [T], _ vals: [T], _ index: Int, _ limit: Int) -> [T]
{
	var result = list
	var todo = vals.count
	var done = 0
	var idx = index
	var bas = 0
	while (idx < 0) {idx = idx + limit}
	while (idx >= limit) {idx = idx - limit}
	if (idx > result.count) {
		todo = todo + idx - result.count
		bas = (vals.count - (idx - result.count) % vals.count) % vals.count
	}
	while (done < todo) {
		var lim = vals.count
		if (idx + lim - bas > limit) {
			lim = limit - idx + bas
		}
		if (idx < result.count && idx + lim - bas > result.count) {
			lim = result.count - idx + bas
		}
		if (idx < result.count) {
			result[idx...(idx+lim-bas - 1)] = vals[bas...(lim-1)]
		} else {
			result.append(contentsOf:vals[bas...(lim-1)])
		}
		done = done + lim - bas
		idx = (idx + lim - bas) % limit
		bas = lim % vals.count
	}
	return result
}
class Lock
{
	var lock = [Refer]()
}
class Pool
{
	var pool = [Int]()
}
class Pend<T>
{
	var pend:Int!
	var last:Int!
	var lock:Lock!
	var pool:Pool!
	var keep:[MTLBuffer]
	var refer:[Refer]
	init(_ l: Lock, _ p: Pool)
	{
		refer = [Refer]()
		lock = l
		pool = p
		keep = [MTLBuffer]()
	}
	func make(_ length:Int) -> Int?
	{
		if (length == 0) {return nil}
		if (pool.pool.count == 0) {
			pool.pool.append(keep.count)
			keep.append(device.makeBuffer(length:length)!)
			refer.append(Refer(refer.count,pool))
		}
		if (keep[pool.pool[0]].length < length) {
			keep[pool.pool[0]] = device.makeBuffer(length:length)!
		}
		return pool.pool.removeFirst()
	}
	func set(_ ptr: UnsafeRawPointer, _ range: Range<Int>)
	{
		let len:Int = range.upperBound
		let unit:Int = MemoryLayout<T>.size
		let length:Int = len+(unit-len%unit)%unit;
		if (pend == nil && last != nil) {
			pend = make(keep[last].length)
			keep[pend].contents().copyMemory(from:keep[last].contents(),byteCount:keep[last].length)
			last = nil
		}
		if (pend == nil) {
			pend = make(length)
		}
		if (pend != nil) {
			let base:Int = range.lowerBound
			let size:Int = range.upperBound-range.lowerBound
			keep[pend].contents().advanced(by:base).copyMemory(from:ptr,byteCount:size)
		}
	}
	func set(_ vals: [T], _ index: Int)
	{
		let siz = MemoryLayout<T>.size
		let base = siz*index
		let limit = base+siz*vals.count
		toMutable(vals) {(len,ptr) in
		set(ptr,base..<limit)}
	}
	func set(_ vals:[T], _ index: Int, _ length : Int)
	{
		var sub = 0
		var idx = index
		if (length <= 0) {print("invalid buffer length");exitErr(#file,#line);return}
		while (idx < 0) {idx = idx + length}
		while (idx >= length) {idx = idx - length}
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
		guard let fld = MemoryLayout<T>.offset(of:field) else {exitErr(#file,#line);return}
		let siz = MemoryLayout<S>.size
		toMutable([val]) {(len,ptr) in
		set(ptr,fld..<fld+siz)}
	}
	func get() -> MTLBuffer?
	{
		if (last == nil && pend != nil) {
			last = pend
			pend = nil
		}
		if (last != nil) {
			lock.lock.append(refer[last])
		}
		if (last == nil) {
			return nil
		} else {
			return keep[last]
		}
	}
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
	let temp = lock1.lock
	lock1.lock = []
	return {(MTLCommandBuffer) in for ref in temp {ref.pool.pool.append(ref.lock)}}
}
func setLock()
{
	for ref in lock2.lock {ref.pool.pool.append(ref.lock)}
	lock2.lock = []
}
func getReady() -> MTLCommandBufferHandler
{
	let size = planeInfo(TriangleSize)
	if (size == 0) {return {(MTLCommandBuffer) in planeReady(nil)}}
	if let last = pierce.get() {
		return {(MTLCommandBuffer) in planeReady(UnsafeMutablePointer<Pierce>(OpaquePointer(last.contents()))); setLock()}
	} else {
		return {(MTLCommandBuffer) in setLock()}
	}
}
func getCount(_ code:MTLCommandBuffer) -> MTLCommandBufferHandler
{
	count += 1
	return {(MTLCommandBuffer) in count -= 1; planeSafe(Threads,Waits,RegisterDone)}
}
func getDraw() -> CAMetalDrawable?
{
	return layer.nextDrawable() // TODO get off screen drawable if layer is nil
}
class getApplication : NSObject, NSApplicationDelegate
{
	internal func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool
	{
		return false
	}
}
class getDelegate : NSObject, NSWindowDelegate
{
	internal func windowShouldClose(_ sender: NSWindow) -> Bool
	{
		return false
	}
	func windowWillClose(_ notification: Notification)
	{
		planeSafe(Window,Stop,Configures)
	}
	func windowDidResize(_ notification: Notification)
	{
		CATransaction.begin()
		CATransaction.setDisableActions(true)
		setFrame()
		CATransaction.commit()
		planeSafe(Threads,Waits,WindowLeft)
	}
	func windowWillMove(_ notification: Notification)
	{
		Task(priority:.medium,operation:{
			while (NSEvent.pressedMouseButtons != 0) {
			try! await Task.sleep(nanoseconds:1_000_000)
			windowDidResize(notification)}})
	}
}
class getView : NSView
{
	override func mouseDown(with event: NSEvent)
	{
		planeSafe(Threads,Waits,CursorClick)
	}
	override func mouseMoved(with event: NSEvent)
	{
		uniform.set(Int(getPoint().x),\Uniform.lon)
		uniform.set(Int(getPoint().y),\Uniform.lat)
		planeSafe(Threads,Waits,CursorLeft)
	}
	override func scrollWheel(with event: NSEvent)
	{
		delta += event.deltaY
		planeSafe(Threads,Waits,CursorAngle)
	}
	override func keyDown(with event: NSEvent)
	{
		print("keyDown event.characters")
	}
}
func swiftInit()
{
	for arg in CommandLine.arguments {planeAddarg(arg)}
	let application = getApplication()
	NSApplication.shared.delegate = application
	device = MTLCreateSystemDefaultDevice()
}
func swiftOpen()
{
	let rect = NSMakeRect(
		CGFloat(planeInfo(WindowLeft)), CGFloat(planeInfo(WindowBase)),
		CGFloat(planeInfo(WindowWide)), CGFloat(planeInfo(WindowHigh)))

	layer = CAMetalLayer()
	layer.device = device
	layer.pixelFormat = .bgra8Unorm
	layer.framebufferOnly = true
	layer.frame = rect

	view = getView(frame:rect)
	view.wantsLayer = true
	view.layer!.addSublayer(layer)
	let tracking = NSTrackingArea(rect:rect,options:[.mouseMoved,.mouseEnteredAndExited,.inVisibleRect,.activeInKeyWindow],owner:view)
	view.addTrackingArea(tracking)
	setFrame()

	let mask:NSWindow.StyleMask = [.titled,.closable,.miniaturizable,.resizable]
	window = NSWindow(contentRect: rect, styleMask: mask, backing: .buffered, defer: false)
	window.title = "plane"
	window.contentView = view
	let delegate = getDelegate()
	window.delegate = delegate
	window.makeKeyAndOrderFront(nil)
}
func swiftLoad()
{
	let rect = NSMakeRect(
		CGFloat(planeInfo(WindowLeft)), CGFloat(planeInfo(WindowBase)),
		CGFloat(planeInfo(WindowWide)), CGFloat(planeInfo(WindowHigh)))
	queue = device.makeCommandQueue()
	let url = URL(fileURLWithPath: "metal.metallib")
	guard let library:MTLLibrary = try? device.makeLibrary(URL:url) else {
		fputs("plane: cannot load library: metal.metallib\n",stderr);exitErr(#file,#line);return}
	for sub in 0...Int(Micros.rawValue-1) {let shader = Micro(UInt32(sub)); switch (shader) {
	case (Dipoint): // fallthrough case (Diplane):
	guard let vertex_render = library.makeFunction(name:String(cString: Initial__Micro__Code_Str(Dipoint)(Corner)!)) else {
		print("cannot make vertex_render");exitErr(#file,#line);return}
	guard let fragment_render = library.makeFunction(name:String(cString: Initial__Micro__Code_Str(Dipoint)(Fragment)!)) else {
		print("cannot make fragment_render");exitErr(#file,#line);return}
	let pipe = MTLRenderPipelineDescriptor()
	pipe.vertexFunction = vertex_render
	pipe.fragmentFunction = fragment_render
	pipe.colorAttachments[0].pixelFormat = .bgra8Unorm
	pipe.depthAttachmentPixelFormat = .depth32Float
	render[sub] = try? device.makeRenderPipelineState(descriptor:pipe)
	let color = MTLClearColor(red: 0.0, green: 104.0/255.0, blue: 55.0/255.0, alpha: 1.0)
	param[sub] = MTLRenderPassDescriptor()
	param[sub]!.depthAttachment.clearDepth = -1.0 // clip xyz -1 to 1
	param[sub]!.depthAttachment.texture = getTexture(rect)
	param[sub]!.depthAttachment.storeAction = .dontCare
	param[sub]!.depthAttachment.loadAction = .clear
	param[sub]!.colorAttachments[0].clearColor = color
	param[sub]!.colorAttachments[0].storeAction = .store
	param[sub]!.colorAttachments[0].loadAction = .clear
    let desc = MTLDepthStencilDescriptor()
    desc.depthCompareFunction = .less // right hand rule; z thumb away from observer
    desc.isDepthWriteEnabled = true
    depth[sub] = device.makeDepthStencilState(descriptor: desc)
	case (Adpoint): // fallthrough case (Adplane):
	guard let kernel_pierce = library.makeFunction(name:String(cString: Initial__Micro__Str(Micro(UInt32(sub)))!)) else {
	 	print("cannot make kernel_pierce");exitErr(#file,#line);return}
	compute[sub] = try? device.makeComputePipelineState(function:kernel_pierce)
    threads[sub] = device.maxThreadsPerThreadgroup
    default: break}}
}
func swiftThread()
{
	while (goon) {semaphor.wait(); planeMain()}
}
func swiftStart()
{
	DispatchQueue.main.async(execute:DispatchWorkItem(block:swiftThread))
	NSApp.setActivationPolicy(NSApplication.ActivationPolicy.accessory)
	NSApp.activate(ignoringOtherApps:true)
	NSApp.run()
	planeSafe(Process,Stop,Configures)
}
func swiftMemory(_ ptr: UnsafeMutablePointer<Center>?)
{
	let center = ptr!.pointee
	let siz = Int(center.siz)
	let idx = Int(center.idx)
	switch (center.mem) {
	case (Trianglez): triangle.set(Swift.Array(0..<siz).map() {(sub) in center.tri![sub]},idx-Int(planeInfo(TriangleBase)),Int(planeInfo(TriangleSize)))
	case (Numericz): numeric.set(Swift.Array(0..<siz).map() {(sub) in center.num![sub]},idx-Int(planeInfo(NumericBase)),Int(planeInfo(NumericSize)))
	case (Vertexz): vertex.set(Swift.Array(0..<siz).map() {(sub) in center.vtx![sub]},idx-Int(planeInfo(VertexBase)),Int(planeInfo(VertexSize)))
	case (Allmatz): subject.set(Swift.Array(0..<siz).map() {(sub) in center.all![sub]},idx-Int(planeInfo(SubjectBase)),Int(planeInfo(SubjectSize)))
	case (Fewmatz): object.set(Swift.Array(0..<siz).map() {(sub) in center.few![sub]},idx-Int(planeInfo(ObjectBase)),Int(planeInfo(ObjectSize)))
	case (Onematz): element.set(Swift.Array(0..<siz).map() {(sub) in center.one![sub]},idx-Int(planeInfo(ElementBase)),Int(planeInfo(ElementSize)))
	case (Swarmz): swarm.set(Swift.Array(0..<siz).map() {(sub) in center.swa![sub]},idx-Int(planeInfo(SwarmBase)),Int(planeInfo(SwarmSize)))
	case (Texturez): texture.set(Swift.Array(0..<siz).map() {(sub) in center.tex![sub]},idx-Int(planeInfo(TextureBase)),Int(planeInfo(TextureSize)))
	case (Textualz): textual.set(Swift.Array(0..<siz).map() {(sub) in center.tur![sub]},idx-Int(planeInfo(TextualBase)),Int(planeInfo(TextualSize)))
	case (Basisz): basis.set(Swift.Array(0..<siz).map() {(sub) in center.bas![sub]},idx-Int(planeInfo(BasisBase)),Int(planeInfo(BasisSize)))
	case (Piercez): pierce.set(Swift.Array(0..<siz).map() {(sub) in center.pie![sub]},idx-Int(planeInfo(PierceBase)),Int(planeInfo(PierceSize)))
	case (Slicez): array = toArray(array,Swift.Array(0..<siz).map() {(sub) in center.rng![sub]},idx-Int(planeInfo(SliceBase)),Int(planeInfo(SliceSize)))
	case (Configurez): for sub in Swift.Array(0..<siz) {switch center.cfg![sub] {
		case (UniformAll): uniform.set(Int(center.val![sub]),\Uniform.all)
		case (UniformOne): uniform.set(Int(center.val![sub]),\Uniform.one)
		case (UniformLeft): uniform.set(Int(center.val![sub]),\Uniform.lon)
		case (UniformBase): uniform.set(Int(center.val![sub]),\Uniform.lat)
		case (UniformProj): uniform.set(Int(center.val![sub]),\Uniform.pro)
		case (UniformIndex): uniform.set(Int(center.val![sub]),\Uniform.idx)
		case (UniformBasis): uniform.set(Int(center.val![sub]),\Uniform.use)
		case (UniformSize): uniform.set(Int(center.val![sub]),\Uniform.siz)
		default: break}}
	default: exitErr(#file,#line);return}
}
func swiftMain(_ proc: Thread, _ wait: Wait)
{
	switch(wait) {
	case (Start): switch (proc) {
		case (Process): swiftStart()
		case (Graphics): swiftLoad()
		case (Window): swiftOpen()
		default: break}
	case (Stop): switch (proc) {
		case (Process): goon = false; NSApp.stop(nil)
		case (Graphics): queue = nil
		case (Window): window.close()
		default: break}
	default: exitErr(#file,#line)}	
}
func swiftSafe()
{
	semaphor.signal()
}
func swiftInfo(_ query: Configure) -> Int32
{
	switch (query) {
	case (RegisterDone): return Int32(count)
	case (WindowLeft): return Int32(getRect().origin.x)
	case (WindowBase): return Int32(getRect().origin.y)
	case (WindowWide): return Int32(getRect().size.width)
	case (WindowHigh): return Int32(getRect().size.height)
	case (CursorLeft): return Int32(getPoint().x)
	case (CursorBase): return Int32(getPoint().y)
	case (CursorNear): return Int32(getSize().width*2.0)
	case (CursorAngle): let temp = Int32(delta); delta -= Double(temp); return temp
	case (CursorClick): return Int32(NSEvent.pressedMouseButtons)
	default: exitErr(#file,#line);return 0}
}
func swiftDraw(_ shader: Micro, _ base: Int32, _ limit: Int32)
{
	let sub = Int(shader.rawValue)
	guard let code = queue?.makeCommandBuffer() else {return}
	if (Int(base) < 0 || Int(base) >= Int(limit) || array.count < Int(limit)) {
		let dip = Int(Dipoint.rawValue)
    	guard let draw = getDraw() else {exitErr(#file,#line);return}
		param[dip]!.colorAttachments[0].texture = draw.texture
		guard let encode = code.makeRenderCommandEncoder(descriptor:param[dip]!) else {exitErr(#file,#line);return}
		encode.endEncoding()
		code.present(draw)
	} else {switch (shader) {
	case (Dipoint): fallthrough case (Diplane):
    guard let draw = getDraw() else {exitErr(#file,#line);return}
	param[sub]!.colorAttachments[0].texture = draw.texture
	for range in array[Int(base)..<Int(limit)] {
		guard let encode = code.makeRenderCommandEncoder(descriptor:param[sub]!) else {exitErr(#file,#line);return}
		encode.setRenderPipelineState(render[sub]!)
		encode.setDepthStencilState(depth[sub]!)
		if let tmp = triangle.get() {encode.setVertexBuffer(tmp,offset:0,index:0)}
		if let tmp = numeric.get() {encode.setVertexBuffer(tmp,offset:0,index:1)}
		if let tmp = vertex.get() {encode.setVertexBuffer(tmp,offset:0,index:2)}
		if let tmp = subject.get() {encode.setVertexBuffer(tmp,offset:0,index:3)}
		if let tmp = object.get() {encode.setVertexBuffer(tmp,offset:0,index:4)}
		if let tmp = element.get() {encode.setVertexBuffer(tmp,offset:0,index:5)}
		if let tmp = basis.get() {encode.setVertexBuffer(tmp,offset:0,index:6)}
		if let tmp = uniform.get() {encode.setVertexBuffer(tmp,offset:0,index:7)}
		if let tmp = texture.get() {encode.setVertexBuffer(tmp,offset:0,index:8)}
		if let tmp = textual.get() {encode.setVertexBuffer(tmp,offset:0,index:9)}
		encode.drawPrimitives(
			type:.triangle,
			vertexStart:Int(range.idx*3),
			vertexCount:Int(range.siz*3))
		encode.endEncoding()
		param[sub]!.colorAttachments[0].loadAction = .load
		param[sub]!.depthAttachment.loadAction = .load
	}
	code.present(draw)
	case (Adpoint): fallthrough case (Adplane):
	for range in array[Int(base)..<Int(limit)] {
		var offset = Int(range.idx)*MemoryLayout<Pierce>.size
		var nums:[Int] = []
		var pers:[Int] = []
		let quotient = Int(range.siz)/threads[sub]!.width
		let remainder = Int(range.siz)%threads[sub]!.width
		if (quotient > 0) {
			nums.append(quotient)
			pers.append(threads[sub]!.width)}
		if (remainder > 0) {
			nums.append(1)
			pers.append(remainder)}
		for (n,p) in zip(nums,pers) {
			guard let encode = code.makeComputeCommandEncoder() else {exitErr(#file,#line);return}
			encode.setComputePipelineState(compute[sub]!)
			if let tmp = triangle.get() {encode.setBuffer(tmp,offset:0,index:0)}
			if let tmp = numeric.get() {encode.setBuffer(tmp,offset:0,index:1)}
			if let tmp = vertex.get() {encode.setBuffer(tmp,offset:0,index:2)}
			if let tmp = subject.get() {encode.setBuffer(tmp,offset:0,index:3)}
			if let tmp = object.get() {encode.setBuffer(tmp,offset:0,index:4)}
			if let tmp = element.get() {encode.setBuffer(tmp,offset:0,index:5)}
			if let tmp = basis.get() {encode.setBuffer(tmp,offset:0,index:6)}
			if let tmp = uniform.get() {encode.setBuffer(tmp,offset:0,index:7)}
			if let tmp = pierce.get() {encode.setBuffer(tmp,offset:offset,index:8)}
			let num = MTLSize(width:n,height:1,depth:1)
			let per = MTLSize(width:p,height:1,depth:1)
			encode.dispatchThreadgroups(num,threadsPerThreadgroup:per)
			encode.endEncoding()
			offset += n*p*MemoryLayout<Pierce>.size
		}
	}
	code.addCompletedHandler(getReady())
	default: exitErr(#file,#line);return}}
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount(code))
	code.commit()
}

// MAIN

	planeInit(swiftInit,swiftMemory,swiftSafe,swiftMain,swiftInfo,swiftDraw)
