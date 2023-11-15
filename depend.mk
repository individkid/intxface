datxC.o: datx.c datx.h face.h proto.h type.h
face.so: faceC.o facer.c luaxC.o protoC.o show.lua wrapCpp.o
faceC.o: face.c face.h luax.c luax.h proto.c proto.h wrap.h
facer.log: face.so facer.lua facerC facerHs facerLua
facerC: faceC.o facer.c facer.hs luaxC.o protoC.o wrapCpp.o
facerC.o: face.h proto.h
facerHs: face.hs faceC.o facer.hs protoC.o
facerLua: facer.lua
fileC: faceC.o file.c luax.c luax.h luaxC.o proto.c protoC.o type.gen typeC.o wrap.h wrapCpp.o
fileC.o: face.h proto.h type.h
filer.log: fileC filer.lua filerLua planra.c type.lua
filerLua: filer.lua
holeC: faceC.o line.cpp luaxC.o protoC.o typeC.o wrapCpp.o
lineCpp: faceC.o luaxC.o pipe.sw protoC.o typeC.o wrapCpp.o
luax.so: faceC.o luaxC.o protoC.o show.lua wrapCpp.o
luaxC.o: luax.c luax.h proto.c proto.h wrap.cpp wrap.h
metal.metallib: metal.g
metalSw: datx.h datxC.o face.c faceC.o luax.h luaxC.o metal.sw metx.h metxC.o planeC.o protoC.o type.gen typeC.o wrap.h wrapCpp.o
metalSw.o: datx.c face.h metx.c plane.c plane.h proto.h type.h
metxC.o: metx.c metx.h
pageSw: share.c
pipeSw: page.sw
planeC.o: datx.h face.h luax.h metx.h plane.c plane.h proto.h type.h wrap.h
planer.log: metal.metallib metalSw naive.hs planer.lua planerLua spacra.hs
planerLua: planer.lua
planra.log: metal.sw planraC
planraC: datxC.o faceC.o luax.c luaxC.o planra.c protoC.o type.gen typeC.o wrapCpp.o
planraC.o: datx.c datx.h face.h luax.h metx.h plane.h proto.h type.h wrap.h
protoC.o: proto.c proto.h wrap.cpp
shareC: datxC.o faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
spaceHs: face.h face.hs faceC.o luax.c luax.h naive.hs proto.h protoC.o space.hs type.gen type.hs wrap.h
spacer.log: hole.c spaceHs spacer.lua spacerLua
spacerLua: spacer.lua
spacra.log: space.hs spacraHs
spacraHs: face.h face.hs faceC.o luax.c luax.h naive.hs proto.h protoC.o spacra.hs type.gen type.hs wrap.h
type.c: face.c type.gen
type.dep: face.so luax.so show.lua
type.h: type.gen
type.hs: type.gen
type.lua: type.gen
typeC.o: face.c face.h luax.h proto.h type.c type.gen type.h wrap.h
typer.c: typer.gen
typer.dep: face.so luax.so show.lua test.lua
typer.h: typer.gen
typer.hs: typer.gen
typer.log: file.c type.gen typer.lua typerC typerHs typerLua
typer.lua: typer.gen
typerC: faceC.o luax.c luax.h luaxC.o protoC.o test.lua typer.c typer.gen wrap.h wrapCpp.o
typerC.o: face.h proto.h typer.h
typerHs: face.h face.hs faceC.o luax.c luax.h proto.h protoC.o test.lua typer.gen typer.hs wrap.h
typerLua: typer.lua
typra.log: luax.so show.lua test.lua typer.gen typra.lua typraLua
typraLua: typra.lua
wrapCpp.o: face.h luax.h proto.h wrap.cpp wrap.h
