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

func getDebug(_ charz:MTLBuffer, _ a:Int32, _ b:Int32, _ c:Int32, _ d:Int32) -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in
	var index = 0
	for expected:Int32 in [
	0,255,500,256,-256,500,-256,-256,500,0,0,0,
	0,-256,400,256,256,400,-256,256,400,1,1,1] {
	let actual:Int32 = charz.contents().load(fromByteOffset:index,as:Int32.self)
	if (expected != actual) {
		print("mismatch index(\(index)): expected(\(expected)) != actual(\(actual))")
	} else {
		print("match index(\(index)): expected(\(expected)) == actual(\(actual))")
	}
	index = index + MemoryLayout<Int32>.size}}
}
func planraDraw1(_ shader:share.Shader)
{
	form.set(getRender(0),\Form.feather)
	form.set(getRender(1),\Form.arrow)
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return}
    guard let draw = layer.nextDrawable() else {
		print("cannot make draw"); return}
	descriptor.colorAttachments[0].texture = draw.texture
	guard let encode = code.makeRenderCommandEncoder(descriptor:descriptor) else {
		print("cannot make encode"); return}
	encode.setRenderPipelineState(render)
	encode.setDepthStencilState(depth)
	encode.setVertexBuffer(triangle.get(),offset:0,index:0)
	encode.setVertexBuffer(form.get(),offset:0,index:1)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:6)
	encode.endEncoding()
	code.present(draw)
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()
}
func planraDraw0(_ shader:share.Shader)
{
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
	if let temp = try? device.makeRenderPipelineState(descriptor:pipe) {
		render = temp} else {print("cannot make render"); return}

	let white = (Float(0.0),Float(0.0),Float(0.0),Float(1.0))
	let yellow = (Float(1.0),Float(1.0),Float(0.0),Float(1.0))
	let orange = (Float(1.0),Float(0.5),Float(0.0),Float(1.0))
	let allwhite = (white,white,white)
	var plane0 = share.Facet(); plane0.versor = 2
	var plane1 = share.Facet(); plane1.versor = 2
	var plane2 = share.Facet(); plane2.versor = 1 // (0,256,0),(0,256,128),(128,0,0)
	var plane3 = share.Facet(); plane3.versor = 1 // (0,256,0),(0,256,128),(128,512,0)
	var plane4 = share.Facet(); plane4.versor = 1 // (0,-256,0),(0,-256,128),(128,-256,0)
	var plane5 = share.Facet(); plane5.versor = 1 // (0,-256,0),(0,-256,128),(128,0,0)
	var plane6 = share.Facet(); plane6.versor = 1 // (0,-256,0),(0,-256,128),(128,-512,0)
	var plane7 = share.Facet(); plane7.versor = 1 // (0,256,0),(0,256,128),(128,256,0)
	plane0.plane = (500.0,500.0,500.0); plane1.plane = (400.0,400.0,400.0)
	plane2.plane = (256.0,256.0,0.0); plane3.plane = (256.0,256.0,512.0); plane4.plane = (-256.0,-256.0,-256.0)
	plane5.plane = (-256.0,-256.0,0.0); plane6.plane = (-256.0,-256.0,-512.0); plane7.plane = (256.0,256.0,256.0)
	plane0.poly = 0; plane1.poly = 0
	plane2.poly = 0; plane3.poly = 0; plane4.poly = 0
	plane5.poly = 0; plane6.poly = 0; plane7.poly = 0
	plane0.tag = 0; plane1.tag = 0
	plane2.tag = 1; plane3.tag = 1; plane4.tag = 1
	plane5.tag = 1; plane6.tag = 1; plane7.tag = 1
	plane0.point = (0,1,2); plane1.point = (3,4,5)
	plane2.point = (0,1,6); plane3.point = (0,2,6); plane4.point = (1,2,6)
	plane5.point = (3,4,6); plane6.point = (3,5,6); plane7.point = (4,5,6)
	plane0.color = (orange,yellow,yellow); plane1.color = (yellow,orange,orange)
	plane2.color = allwhite; plane3.color = allwhite; plane4.color = allwhite
	plane5.color = allwhite; plane6.color = allwhite; plane7.color = allwhite
	let planes = [plane0,plane1,plane2,plane3,plane4,plane5,plane6,plane7]
	triangle.set(planes)
	var vertex0 = share.Vertex(); vertex0.plane = (0,2,3)
	var vertex1 = share.Vertex(); vertex1.plane = (0,2,4)
	var vertex2 = share.Vertex(); vertex2.plane = (0,3,4)
	var vertex3 = share.Vertex(); vertex3.plane = (1,5,6)
	var vertex4 = share.Vertex(); vertex4.plane = (1,5,7)
	var vertex5 = share.Vertex(); vertex5.plane = (1,6,7)
	corner.set([vertex0,vertex1,vertex2,vertex3,vertex4,vertex5])
	frame.set([0,1,2,3,4,5])

	var point0 = share.Facet(); point0.plane = (0.0,256.0,500.0); point0.color.0 = yellow
	var point1 = share.Facet(); point1.plane = (-256.0,-256.0,500.0); point1.color.0 = yellow
	var point2 = share.Facet(); point2.plane = (256.0,-256.0,500.0); point2.color.0 = orange
	var point3 = share.Facet(); point3.plane = (-256.0,256.0,400.0); point3.color.0 = orange
	var point4 = share.Facet(); point4.plane = (256.0,256.0,400.0); point4.color.0 = orange
	var point5 = share.Facet(); point5.plane = (0.0,-256.0,400.0); point5.color.0 = yellow
	let points = [point0,point1,point2,point3,point4,point5]
	let charz = device.makeBuffer(length:1000)
	print("Form.tag \(MemoryLayout<Form>.offset(of:\Form.tag)!)")
	print("Facet.tag \(offsetFacetTag())")

	print("before debug")

	form.set(getRender(0),\Form.feather)
	form.set(getRender(1),\Form.arrow)
	form.set(UInt32(0),\Form.tag)
	for (a,b,c,d):(Int32,Int32,Int32,Int32) in [(0,0,0,0),(0,0,0,0)] {
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return}
	guard let encode = code.makeComputeCommandEncoder() else {
		print("cannot make encode"); return}
	encode.setComputePipelineState(debug)
	encode.setBuffer(triangle.get(),offset:0,index:0)
	encode.setBuffer(corner.get(),offset:0,index:1)
	encode.setBuffer(frame.get(),offset:0,index:2)
	encode.setBuffer(object.get(),offset:0,index:3)
	encode.setBuffer(form.get(),offset:0,index:4)
	encode.setBuffer(charz,offset:0,index:5)
	let groups = MTLSize(width:1,height:1,depth:1)
	let threads = MTLSize(width:2,height:1,depth:1)
	encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
	encode.endEncoding()
	code.addCompletedHandler(getDebug(charz!,a,b,c,d))
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()
	// TEST code.waitUntilScheduled()
	// plane2.plane = (48.0,48.0,48.0)
	// triangle.set(plane2,2)
	print("before \(count)")
	code.waitUntilCompleted()
	print("after \(count)")}

	print("between debug and hello")

	triangle.set(points)
	planraDraw1(share.Display)

	print("after hello")

	cb.draw = planraDraw1
}
func planraInit()
{
	swiftInit()
	cb.draw = planraDraw0
}

// MAIN
	cb.start = planraInit
