datxC.o: datx.c datx.h face.h proto.h type.h
face.so: faceC.o luaxC.o protoC.o wrapCpp.o
faceC.o: face.c face.h luax.h proto.h wrap.h
facer.log: face.so facer.lua facerC facerHs facerLua
facerC: faceC.o facer.c luaxC.o protoC.o wrapCpp.o
facerC.o: face.h proto.h
facerHs: face.hs faceC.o facer.hs protoC.o
facerLua: facer.lua
fileC: faceC.o file.c luaxC.o protoC.o typeC.o wrapCpp.o
fileC.o: face.h proto.h type.h
filer.log: face.so fileC filer.lua filerLua type.lua
filerLua: filer.lua
hole: holeC
holeC: faceC.o hole.c luaxC.o protoC.o typeC.o wrapCpp.o
holeC.o: face.h proto.h type.h
line: lineCpp
lineCpp: faceC.o lineCpp.o luaxC.o protoC.o typeC.o wrapCpp.o
lineCpp.o: face.h line.cpp proto.h type.h
luax.so: faceC.o luaxC.o protoC.o wrapCpp.o
luaxC.o: luax.c luax.h proto.h wrap.h
metal: metalSw
metal.metallib: metal.g
metalSw: datxC.o faceC.o luaxC.o metal.sw metxC.o planeC.o protoC.o typeC.o wrapCpp.o
metalSw.o: face.h plane.h proto.h type.h
metxC.o: metx.c metx.h
page: pageSw
pageSw: page.sw
pipe: pipeSw
pipeSw: pipe.sw
planeC.o: datx.h face.h luax.h metx.h plane.c plane.h proto.h type.h wrap.h
planer.log: face.so metal.metallib metalSw planer.lua planerLua type.lua
planerLua: planer.lua
planra.log: planraC
planraC: datxC.o faceC.o luaxC.o planra.c protoC.o typeC.o wrapCpp.o
planraC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h wrap.h
protoC.o: proto.c proto.h
share: shareC
shareC: datxC.o faceC.o luaxC.o protoC.o share.c typeC.o wrapCpp.o
shareC.o: datx.h face.h luax.h proto.h type.h wrap.h
space: spaceHs
spaceHs: face.hs faceC.o naive.hs protoC.o space.hs type.hs
spacer.log: face.so spaceHs spacer.lua spacerLua type.lua
spacerLua: spacer.lua
spacra.log: spacraHs
spacraHs: face.hs faceC.o naive.hs protoC.o spacra.hs type.hs
type.c: face.so luax.so show.lua type.gen
type.h: face.so luax.so show.lua type.gen
type.hs: face.so luax.so show.lua type.gen
type.lua: face.so luax.so show.lua type.gen
typeC.o: face.h proto.h type.c type.h
typer.c: face.so luax.so show.lua test.lua typer.gen
typer.h: face.so luax.so show.lua test.lua typer.gen
typer.hs: face.so luax.so show.lua test.lua typer.gen
typer.log: face.so typer.lua typerC typerHs typerLua
typer.lua: face.so luax.so show.lua test.lua typer.gen
typerC: faceC.o luaxC.o protoC.o typer.c wrapCpp.o
typerC.o: face.h proto.h typer.h
typerHs: face.hs faceC.o protoC.o typer.hs
typerLua: typer.lua
typra.log: face.so luax.so show.lua test.lua typra.lua typraLua
typraLua: typra.lua
wrapCpp.o: face.h luax.h proto.h wrap.cpp wrap.h
