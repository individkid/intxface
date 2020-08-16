/*
*    planra.sw
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

func retDebug(_ charz:MTLBuffer, _ a:Int8, _ b:Int8, _ c:Int8, _ d:Int8) -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in
	var index = 0
	for expected:Int8 in [
	0,16,32,48,80,0,4,16,16,0,a,b,
	0,16,32,48,80,0,4,16,16,3,c,d] {
	let actual:Int8 = charz.contents().load(fromByteOffset:index,as:Int8.self)
	if (expected != actual) {
		print("mismatch index(\(index)): expected(\(expected)) != actual(\(actual))")
	} else {
		print("match index(\(index)): expected(\(expected)) == actual(\(actual))")
	}
	index = index + 1}}
}
func planraInit()
{
	let _ = swiftInit()

	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot make library"); return}
	guard let kernel_debug = library.makeFunction(name:"kernel_debug") else {
		print("cannot make kernel_debug"); return;}
	guard let vertex_simple = library.makeFunction(name:"vertex_simple") else {
		print("cannot make vertex_simple"); return}
	guard let fragment_render = library.makeFunction(name:"fragment_render") else {
		print("cannot make fragment_render"); return}
	guard let debug = try? device.makeComputePipelineState(function:kernel_debug) else {
		print("cannot make debug"); return}
	guard let pipe = noWarn(MTLRenderPipelineDescriptor()) else {
		print("cannot make pipe"); return}
	pipe.vertexFunction = vertex_simple
	pipe.fragmentFunction = fragment_render
	pipe.colorAttachments[0].pixelFormat = .bgra8Unorm
	pipe.depthAttachmentPixelFormat = .depth32Float
	guard let hello = try? device.makeRenderPipelineState(descriptor:pipe) else {
		print("cannot make hello"); return}

	var plane0 = share.Facet(); plane0.versor = 8; plane0.tag = 64
	var plane1 = share.Facet(); plane1.versor = 8; plane1.tag = 64
	let planes = [plane0,plane1];
	triangle.set(planes)
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
	print("Form.tag \(MemoryLayout<Form>.offset(of:\Form.tag)!)")
	print("Facet.tag \(offsetFacetTag())")

	print("before debug")

	for (a,b,c,d):(Int8,Int8,Int8,Int8) in [(8,64,8,64),(7,63,9,65)] {
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return}
	guard let encode = code.makeComputeCommandEncoder() else {
		print("cannot make encode"); return}
	encode.setComputePipelineState(debug)
	encode.setBuffer(triangle.get(),offset:0,index:0)
	encode.setBuffer(arrayz,offset:0,index:1)
	encode.setBuffer(charz,offset:0,index:2)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:2,height:1,depth:1)
	encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	encode.endEncoding()
	code.addCompletedHandler(retDebug(charz!,a,b,c,d))
	code.addScheduledHandler(retLock())
	code.addCompletedHandler(retCount())
	count += 1
	code.commit()
	// code.waitUntilScheduled()
	triangle.set(Int32(63),Int(offsetFacetTag()))
	triangle.set(Int32(65),1,Int(offsetFacetTag()))
	triangle.set(Int32(7),Int(offsetFacetVersor()))
	triangle.set(Int32(9),1,Int(offsetFacetVersor()))
	print("before \(count)")
	code.waitUntilCompleted()
	print("after \(count)")}

	print("between debug and hello")

	if (true) {
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return}
    guard let draw = layer.nextDrawable() else {
    	print("cannot make draw"); return}
	descriptor.colorAttachments[0].texture = draw.texture
	guard let encode = code.makeRenderCommandEncoder(descriptor:descriptor) else {
		print("cannot make encode"); return}
	encode.setRenderPipelineState(hello)
	encode.setDepthStencilState(depth)
	encode.setVertexBuffer(pointz,offset:0,index:0)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:6)
	encode.endEncoding()
	code.present(draw)
	code.addScheduledHandler(retLock())
	code.addCompletedHandler(retCount())
	count += 1
	code.commit()}

	print("after hello")
}

// MAIN
	cb.start = planraInit
