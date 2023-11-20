datxC.o: datx.c datx.h face.h proto.h type.h
faceC.o: face.c face.h proto.h
faceCpp.o: face.cpp face.h proto.h wrap.h
facer.log: facer.lua facerC facerHs facerLua luax.so
facerC: faceC.o facer.c protoC.o
facerC.o: face.h proto.h
facerHs: face.hs faceC.o facer.hs protoC.o
facerLua: facer.lua
fileC: faceC.o file.c protoC.o typeC.o
fileC.o: face.h proto.h type.h
filer.log: fileC filer.lua filerLua luax.so type.lua
filerLua: filer.lua
hole: holeC
holeC: faceC.o hole.c protoC.o typeC.o
holeC.o: face.h proto.h type.h
line: lineCpp
lineCpp: faceC.o lineCpp.o protoC.o typeC.o
lineCpp.o: face.h line.cpp proto.h type.h
luax.so: faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o wrapCpp.o
luaxC.o: face.h luax.c luax.h proto.h
luaxCpp.o: luax.cpp luax.h proto.h wrap.h
metal: metalSw
metal.metallib: metal.g
metalSw: datxC.o faceC.o faceCpp.o luaxC.o luaxCpp.o metal.sw metxC.o planeC.o planeCpp.o protoC.o typeC.o wrapCpp.o
metalSw.o: face.h plane.h proto.h type.h
metxC.o: metx.c metx.h
page: pageSw
pageSw: page.sw
pipe: pipeSw
pipeSw: pipe.sw
planeC.o: datx.h face.h luax.h metx.h plane.c plane.h proto.h type.h
planeCpp.o: plane.cpp proto.h wrap.h
planer.log: luax.so metal.metallib metalSw planer.lua planerLua type.lua
planerLua: planer.lua
planra.log: planraC
planraC: datxC.o faceC.o planra.c protoC.o typeC.o
planraC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h
protoC.o: proto.c proto.h
share: shareC
shareC: datxC.o faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o share.c typeC.o wrapCpp.o
shareC.o: datx.h face.h luax.h proto.h type.h
space: spaceHs
spaceHs: face.hs faceC.o naive.hs protoC.o space.hs type.hs
spacer.log: luax.so spaceHs spacer.lua spacerLua type.lua
spacerLua: spacer.lua
spacra.log: spacraHs
spacraHs: face.hs faceC.o naive.hs protoC.o spacra.hs type.hs
type.c: luax.so show.lua type.gen
type.h: luax.so show.lua type.gen
type.hs: luax.so show.lua type.gen
type.lua: luax.so show.lua type.gen
typeC.o: face.h proto.h type.c type.h
typer.c: luax.so show.lua test.lua typer.gen
typer.h: luax.so show.lua test.lua typer.gen
typer.hs: luax.so show.lua test.lua typer.gen
typer.log: luax.so typer.lua typerC typerHs typerLua
typer.lua: luax.so show.lua test.lua typer.gen
typerC: faceC.o protoC.o typer.c
typerC.o: face.h proto.h typer.h
typerHs: face.hs faceC.o protoC.o typer.hs
typerLua: typer.lua
typra.log: luax.so show.lua test.lua typra.lua typraLua
typraLua: typra.lua
wrapCpp.o: proto.h wrap.cpp wrap.h
