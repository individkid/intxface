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
hole: holeC
holeC: faceC.o holeC.o protoC.o typeC.o
holeC.o: face.h hole.c proto.h type.h
line: lineCpp
lineCpp: faceC.o lineCpp.o protoC.o typeC.o
lineCpp.o: face.h line.cpp proto.h type.h
luax.so: faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o wrapCpp.o
luaxC.o: face.h luax.c luax.h proto.h
luaxCpp.o: luax.cpp luax.h proto.h wrap.h
protoC.o: proto.c proto.h
share: shareC
shareC: datxC.o faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o shareC.o typeC.o wrapCpp.o
shareC.o: datx.h face.h luax.h proto.h share.c type.h
sharer.log: luax.so shareC sharer.lua sharerLua type.lua
sharerLua: sharer.lua
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
typeC.o: face.h type.c type.h
typer.c: luax.so show.lua test.lua typer.gen
typer.h: luax.so show.lua test.lua typer.gen
typer.hs: luax.so show.lua test.lua typer.gen
typer.log: luax.so typer.lua typerC typerHs typerLua
typer.lua: luax.so show.lua test.lua typer.gen
typerC: faceC.o protoC.o typerC.o
typerC.o: face.h typer.c typer.h
typerHs: face.hs faceC.o protoC.o typer.hs
typerLua: typer.lua
typra.log: luax.so show.lua test.lua typra.lua typraLua
typraLua: typra.lua
wrapCpp.o: proto.h wrap.cpp wrap.h
