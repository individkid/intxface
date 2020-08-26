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

var charz:MTLBuffer!
var debug:MTLComputePipelineState!
var once:Bool = false

func getDebug(_ charz:MTLBuffer, _ a:Int32, _ b:Int32, _ c:Int32, _ d:Int32) -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in
	var index = 0
	for expected:Int32 in [
	0,255,500,256,-256,500,-256,-256,500,10,5,0,
	0,-256,400,256,256,400,-256,256,400,10,10,0] {
	let actual:Int32 = charz.contents().load(fromByteOffset:index,as:Int32.self)
	if (expected != actual) {
		print("mismatch index(\(index)): expected(\(expected)) != actual(\(actual))")
	} else {
		print("match index(\(index)): expected(\(expected)) == actual(\(actual))")
	}
	index = index + MemoryLayout<Int32>.size}}
}
func planraDraw(_ shader:share.Shader)
{
	guard let temp = getClient(Frame) else {print("cannot get frame"); return}
	if (temp.siz < 6) {return}
	if (shader == share.Track && !once) {
	once = true
	form.set(getRender(0),\Form.feather)
	form.set(getRender(1),\Form.arrow)
	form.set(UInt32(0),\Form.tag)
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
	code.addCompletedHandler(getDebug(charz!,0,0,0,0))
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()}
	if (shader == share.Display) {
	form.set(getRender(0),\Form.feather)
	form.set(getRender(1),\Form.arrow)
	guard let code = queue.makeCommandBuffer() else {
		print("cannot make code"); return}
    guard let draw = layer.nextDrawable() else {
		print("cannot make draw"); return}
	param.colorAttachments[0].texture = draw.texture
	guard let encode = code.makeRenderCommandEncoder(descriptor:param) else {
		print("cannot make encode"); return}
	encode.setRenderPipelineState(render)
	encode.setDepthStencilState(depth)
	encode.setVertexBuffer(triangle.get(),offset:0,index:0)
	encode.setVertexBuffer(corner.get(),offset:0,index:1)
	encode.setVertexBuffer(frame.get(),offset:0,index:2)
	encode.setVertexBuffer(object.get(),offset:0,index:3)
	encode.setVertexBuffer(form.get(),offset:0,index:4)
	encode.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:6)
	encode.endEncoding()
	code.present(draw)
	code.addScheduledHandler(getLock())
	code.addCompletedHandler(getCount())
	count += 1
	code.commit()}
}
func planraInit()
{
	print("Form.tag \(MemoryLayout<Form>.offset(of:\Form.tag)!)")
	print("Facet.tag \(offsetFacetTag())")

	swiftInit()
	cb.draw = planraDraw

	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot make library"); return}
	guard let kernel_debug = library.makeFunction(name:"kernel_debug") else {
		print("cannot make kernel_debug"); return;}
	if let temp = try? device.makeComputePipelineState(function:kernel_debug) {
		debug = temp} else {print("cannot make debug"); return}
	charz = device.makeBuffer(length:1000)

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
	plane0.color = (yellow,orange,yellow); plane1.color = (orange,yellow,orange)
	plane2.color = allwhite; plane3.color = allwhite; plane4.color = allwhite
	plane5.color = allwhite; plane6.color = allwhite; plane7.color = allwhite

	var vertex0 = share.Vertex(); vertex0.plane = (0,2,3)
	var vertex1 = share.Vertex(); vertex1.plane = (0,2,4)
	var vertex2 = share.Vertex(); vertex2.plane = (0,3,4)
	var vertex3 = share.Vertex(); vertex3.plane = (1,5,6)
	var vertex4 = share.Vertex(); vertex4.plane = (1,5,7)
	var vertex5 = share.Vertex(); vertex5.plane = (1,6,7)

	let planes = [plane0,plane1,plane2,plane3,plane4,plane5,plane6,plane7]
	let vertexs = [vertex0,vertex1,vertex2,vertex3,vertex4,vertex5]
	let indexs:[Int32] = [0,1,2,3,4,5]

	toMutablss(planes,[Copy,Dma2],{(ptr,fnc) in debugFacet(Triangle,0,8,2,ptr,fnc)})
	toMutablss(vertexs,[Copy,Dma2],{(ptr,fnc) in debugVertex(Corner,0,6,2,ptr,fnc)})
	toMutablss(indexs,[Copy,Dma2,Gpu1,Gpu0],{(ptr,fnc) in debugInt(Frame,0,6,4,ptr,fnc)})
}

// MAIN
	cb.start = planraInit
