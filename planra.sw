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

var debug:MTLComputePipelineState!
var checks:MTLBuffer!
var clients:[share.Client]!
var facets:[share.Facet]!
var vertexs:[share.Vertex]!
var indexs:[share.Index]!
var bases:[Int32]!
var ranges:[share.Array]!
var actives:[share.Array]!
var once:Bool = false

func getDebug(_ checks:MTLBuffer, _ a:CInt, _ b:CInt, _ c:CInt, _ d:CInt) -> MTLCommandBufferHandler
{
	return {(MTLCommandBuffer) in
	var index = 0
	for expected:CInt in [
	0,2,3,0,2,4,0,3,4,0,0,0,
	1,5,6,1,5,7,1,6,7,0,0,0] {
	let actual:CInt = checks.contents().load(fromByteOffset:index,as:CInt.self)
	if (expected != actual) {
		print("mismatch index(\(index)): expected(\(expected)) != actual(\(actual))")
	} else {
		print("match index(\(index)): expected(\(expected)) == actual(\(actual))")
	}
	index = index + MemoryLayout<CInt>.size}}
}
func planraDraw(_ shader:share.Shader)
{
	guard let temp = getMemory(share.Frame,{$0.frame}) else {print("cannot make frame"); return}
	if (shader == share.Track && temp.count >= 6 && !once) {
		once = true
		guard let code = queue.makeCommandBuffer() else {
			print("cannot make code"); return}
		guard let encode = code.makeComputeCommandEncoder() else {
			print("cannot make encode"); return}
		encode.setComputePipelineState(debug)
		encode.setBuffer(triangle.get(),offset:0,index:0)
		encode.setBuffer(corner.get(),offset:0,index:1)
		encode.setBuffer(base.get(),offset:0,index:2)
		encode.setBuffer(object.get(),offset:0,index:3)
		encode.setBuffer(form.get(),offset:0,index:4)
		encode.setBuffer(checks,offset:0,index:5)
		let groups = MTLSize(width:1,height:1,depth:1)
		let threads = MTLSize(width:2,height:1,depth:1)
		encode.dispatchThreadgroups(groups,threadsPerThreadgroup:threads)
		encode.endEncoding()
		code.addCompletedHandler(getDebug(checks!,0,0,0,0))
		code.addScheduledHandler(getLock())
		code.addCompletedHandler(getCount())
		count += 1
		code.commit()
	} else {
		swiftDraw(shader)
	}
}
func planraInit()
{
	print("Form.hand \(MemoryLayout<Form>.offset(of:\Form.hand)!)")
	print("Facet.tag \(offsetFacetTag())")

	swiftInit()
	cb.draw = planraDraw

	guard let library:MTLLibrary = try? device.makeLibrary(filepath:"plane.so") else {
		print("cannot make library"); return}
	guard let kernel_debug = library.makeFunction(name:"kernel_debug") else {
		print("cannot make kernel_debug"); return;}
	if let temp = try? device.makeComputePipelineState(function:kernel_debug) {
		debug = temp} else {print("cannot make debug"); return}
	checks = device.makeBuffer(length:1024)

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

	var index0 = share.Index(); index0.point = 0; index0.tag = 0
	var index1 = share.Index(); index1.point = 1; index1.tag = 0
	var index2 = share.Index(); index2.point = 2; index2.tag = 0
	var index3 = share.Index(); index3.point = 3; index3.tag = 0
	var index4 = share.Index(); index4.point = 4; index4.tag = 0
	var index5 = share.Index(); index5.point = 5; index5.tag = 0

	var range0 = share.Array(); range0.idx = 0; range0.siz = 3
	var range1 = share.Array(); range1.idx = 3; range1.siz = 3

	var active0 = share.Array(); active0.idx = 0; active0.siz = 2;

	clients = []
	facets = [plane0,plane1,plane2,plane3,plane4,plane5,plane6,plane7]
	vertexs = [vertex0,vertex1,vertex2,vertex3,vertex4,vertex5]
	indexs = [index0,index1,index2,index3,index4,index5]
	bases = [0,1]
	ranges = [range0,range1]
	actives = [active0]

	toMutable(clients)
		{(len:Int,client:UnsafeMutablePointer<share.Client>) in
	toMutable(facets)
		{(ptrs:Int,ptr:UnsafeMutablePointer<share.Facet>) in
	toMutable([Copy,Dma1])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	atomicFacet(Triangle,0,Int32(ptrs),Int32(fncs),ptr,fnc,0,client)
		{(num:CInt,client:UnsafeMutablePointer<share.Client>?) in
	toMutable(vertexs)
		{(ptrs:Int,ptr:UnsafeMutablePointer<share.Vertex>) in
	toMutable([Copy,Dma1])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	atomicVertex(Corner,0,Int32(ptrs),Int32(fncs),ptr,fnc,num,client)
		{(num:CInt,client:UnsafeMutablePointer<share.Client>?) in
	toMutable(indexs)
		{(ptrs:Int,ptr:UnsafeMutablePointer<share.Index>) in
	toMutable([Copy,Dma1])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	atomicIndex(Frame,0,Int32(ptrs),Int32(fncs),ptr,fnc,num,client)
		{(num:CInt,client:UnsafeMutablePointer<share.Client>?) in
	toMutable(bases)
		{(ptrs:Int,ptr:UnsafeMutablePointer<Int32>) in
	toMutable([Copy,Dma1])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	atomicInt(Base,0,Int32(ptrs),Int32(fncs),ptr,fnc,num,client)
		{(num:CInt,client:UnsafeMutablePointer<share.Client>?) in
	toMutable(ranges)
		{(ptrs:Int,ptr:UnsafeMutablePointer<share.Array>) in
	toMutable([Copy])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	atomicArray(Range,0,Int32(ptrs),Int32(fncs),ptr,fnc,num,client)
		{(num:CInt,client:UnsafeMutablePointer<share.Client>?) in
	toMutable(actives)
		{(ptrs:Int,ptr:UnsafeMutablePointer<share.Array>) in
	toMutable([Copy])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	atomicArray(Active,0,Int32(ptrs),Int32(fncs),ptr,fnc,num,client)
		{(num:CInt,client:UnsafeMutablePointer<share.Client>?) in
	toMutable([Copy,Atom,Gpu1,Gpu0])
		{(fncs:Int,fnc:UnsafeMutablePointer<share.Function>) in
	clientClient(Process,0,num,Int32(fncs),client,fnc)
		}}}}}}}}}}}}}}}}}}}}
}

// MAIN
	cb.start = planraInit
