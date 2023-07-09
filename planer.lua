dofile("type.lua")
ident = wrapType("Planez","planerLua","metalSw")
os.execute("sleep 1")
center = {["mem"]="Configurez",["siz"]=4,["idx"]=0,["slf"]=0,
	["cfg"]={"PierceSize","TriangleSize","VertexSize","SliceSize"},
	["val"]={1,1,3,1}}
writeCenter(center,ident)
initialize = hideCenter("Center(mem:Memory(Piercez)siz:Int(1)idx:Int(0)slf:Int(0)pie[0]:Pierce("..
	"fix[0]:Old(0.000000)fix[1]:Old(0.100000)fix[2]:Old(0.200000)fix[3]:Old(0.300000)"..
	"nml[0]:Old(1.000000)nml[1]:Old(1.100000)nml[2]:Old(1.200000)nml[3]:Old(1.300000)"..
	"vld:Int32(1)idx:Int32(0)pad[0]:Int32(0)pad[1]:Int32(0)))")
writeCenter(initialize,ident)
triangle = hideCenter("Center(mem:Memory(Trianglez)siz:Int(1)idx:Int(0)slf:Int(0)tri[0]:Triangle("..
	"vtx[0]:Int32(0)vtx[1]:Int32(1)vtx[2]:Int32(2)vtx[3]:Int32(3)"..
	"num:Int32(0)tex:Int32(0)pol:Int32(0)pad:Int32(0)))")
writeCenter(triangle,ident)
vertex = hideCenter("Center(mem:Memory(Vertexz)siz:Int(3)idx:Int(0)slf:Int(0)"..
	"vtx[0]:Vertex(vec[0]:Old(-1.0)vec[1]:Old(0.0)vec[2]:Old(0.0)vec[3]:Old(1.0)"..
		"ref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)pad:Int32(0))"..
	"vtx[1]:Vertex(vec[0]:Old(0.0)vec[1]:Old(1.0)vec[2]:Old(0.0)vec[3]:Old(1.0)"..
		"ref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)pad:Int32(0))"..
	"vtx[2]:Vertex(vec[0]:Old(1.0)vec[1]:Old(-1.0)vec[2]:Old(0.0)vec[3]:Old(1.0)"..
		"ref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)pad:Int32(0)))")
writeCenter(vertex,ident)
slice = hideCenter("Center(mem:Memory(Slicez)siz:Int(1)idx:Int(0)slf:Int(0)rng[0]:Slice(idx:Int(0)siz:Int(1)))")
writeCenter(slice,ident)
limit = {["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=0,["cfg"]={"ArgumentLimit"},["val"]={1}}
writeCenter(limit,ident)
expected = hideCenter("Center(mem:Memory(Piercez)siz:Int(1)idx:Int(0)slf:Int(0)pie[0]:Pierce("..
	"fix[0]:Old(0.000000)fix[1]:Old(0.000000)fix[2]:Old(0.000000)fix[3]:Old(0.000000)"..
	"nml[0]:Old(0.000000)nml[1]:Old(0.000000)nml[2]:Old(-1.000000)nml[3]:Old(0.000000)"..
	"vld:Int32(1)idx:Int32(0)pad[0]:Int32(0)pad[1]:Int32(0)))")
os.execute("sleep 1")
center = {["mem"]="Piercez",["siz"]=0,["idx"]=0,["slf"]=0}
writeCenter(center,ident)
result = readCenter(ident)
if showCenter(expected,"") ~= showCenter(result,"") then io.stderr:write("planerLua: mismatch\n"); os.exit(-1) end
center = {["mem"]="Configurez",["siz"]=1,["idx"]=0,["slf"]=0,["cfg"]={"RegisterOpen"},["val"]={0}}
writeCenter(center,ident)
if waitExit() < 0 then io.stderr:write("planerLua: bad exit status: metalSw\n"); os.exit(-1) end
