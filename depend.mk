datxC.o: datx.c datx.h proto.h type.h
faceC.o: face.c face.h proto.h
faceCpp.o: face.cpp face.h proto.h wrap.h
facer.log: facer.lua facerC facerHs facerLua luax.so
facerC: faceC.o facerC.o protoC.o
facerC.o: face.h facer.c proto.h
facerHs: face.hs faceC.o facer.hs protoC.o
facerLua: facer.lua
fileC: faceC.o fileC.o protoC.o typeC.o
fileC.o: face.h file.c proto.h type.h
filer.log: fileC filer.lua filerLua luax.so type.lua
filerLua: filer.lua
fmtxC.o: fmtx.c
fragmentColorG: vulkan.g
fragmentPierceG: vulkan.g
fragmentRelateG: vulkan.g
hole: holeC
holeC: faceC.o holeC.o protoC.o typeC.o
holeC.o: face.h hole.c proto.h type.h
line: lineCpp
lineCpp: faceC.o lineCpp.o protoC.o typeC.o
lineCpp.o: face.h line.cpp proto.h type.h
luax.so: faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o wrapCpp.o
luaxC.o: face.h luax.c luax.h proto.h
luaxCpp.o: luax.cpp luax.h proto.h wrap.h
metxC.o: metx.c metx.h
planeC.o: datx.h face.h fmtx.h metx.h plane.c plane.h proto.h stlx.h sugx.h type.h
planra.log: planraC
planraC: datxC.o faceC.o planraC.o protoC.o typeC.o
planraC.o: datx.h face.h luax.h metx.h plane.h planra.c proto.h type.h
protoC.o: proto.c proto.h
share: shareC
shareC: datxC.o faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o shareC.o typeC.o wrapCpp.o
shareC.o: datx.h face.h luax.h proto.h share.c type.h
space: spaceHs
spaceHs: face.hs faceC.o naive.hs protoC.o space.hs type.hs
spacer.log: luax.so spaceHs spacer.lua spacerLua type.lua
spacerLua: spacer.lua
spacra.log: spacraHs
spacraHs: face.hs faceC.o naive.hs protoC.o spacra.hs type.hs
stlxCpp.o: proto.h stlx.cpp stlx.h
sugxC.o: proto.h stlx.h sugx.c sugx.h type.h
type.c: luax.so show.lua type.gen
type.h: luax.so show.lua type.gen
type.hs: luax.so show.lua type.gen
type.lua: luax.so show.lua type.gen
typeC.o: face.h type.c type.h
typer.c: luax.so show.lua test.lua typer.gen
typer.h: luax.so show.lua test.lua typer.gen
typer.hs: luax.so show.lua test.lua
typer.log: typer.lua typerC typerHs typerLua
typer.lua: luax.so show.lua test.lua typer.gen
typerC: faceC.o protoC.o typerC.o
typerC.o: face.h typer.c typer.h
typerHs: face.hs faceC.o protoC.o typer.gen
typerLua: typer.gen
typra.log: luax.so show.lua test.lua typra.lua typraLua
typraLua: typra.lua
vertexConstG: vulkan.g
vertexCoplaneG: vulkan.g
vertexFetchG: vulkan.g
vertexFillG: vulkan.g
vertexVertexG: vulkan.g
vulkan.log:  fragmentColorG fragmentPierceG fragmentRelateG texture.jpg vertexConstG vertexCoplaneG vertexFetchG vertexFillG vertexVertexG vulkanCpp
vulkanCpp: datxC.o faceC.o fmtxC.o metxC.o planeC.o protoC.o stlxCpp.o sugxC.o typeC.o vulkanCpp.o
vulkanCpp.o: face.h plane.h proto.h stlx.h type.h vulkan.cpp
wrapCpp.o: proto.h wrap.cpp wrap.h
