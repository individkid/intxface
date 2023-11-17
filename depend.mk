datxC.o: datx.h face.h proto.h type.h
face.so: faceC.o luaxC.o protoC.o wrapCpp.o
faceC.o: face.h luax.h proto.h wrap.h
facer.log: face.so facer.lua facerC facerHs facerLua
facerC: face.hs faceC.o facerC.o luaxC.o protoC.o wrapCpp.o
facerC.o: face.h proto.h
facerHs: face.hs faceC.o protoC.o
fileC: faceC.o fileC.o luax.h luaxC.o protoC.o typeC.o wrap.h wrapCpp.o
fileC.o: face.h proto.h type.h
filer.log: datx.h fileC filer.lua filerLua metx.h plane.h type.lua
holeC: faceC.o line.cpp luaxC.o protoC.o typeC.o wrapCpp.o
holeC.o: face.h luax.h proto.h wrap.h
lineCpp: faceC.o luaxC.o pipe.sw protoC.o typeC.o wrapCpp.o
luax.so: faceC.o protoC.o show.lua test.lua wrapCpp.o
luaxC.o: luax.h proto.h wrap.h
metalSw: datxC.o faceC.o luax.h luaxC.o metalSw.o metxC.o planeC.o protoC.o typeC.o wrap.h wrapCpp.o
metalSw.o: datx.h face.h metx.h plane.h proto.h type.h
metxC.o: metx.h
pageSw: share.c
pipeSw: page.sw
planeC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h wrap.h
planer.log: metal.metallib metalSw naive.hs planer.lua planerLua
planra.log: planraC
planraC: datxC.o faceC.o luaxC.o planraC.o protoC.o typeC.o wrapCpp.o
planraC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h wrap.h
protoC.o: proto.h
shareC: datxC.o faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
spaceHs: face.h face.hs faceC.o luax.h naive.hs proto.h protoC.o type.hs wrap.h
spacer.log: hole.c spaceHs spacer.lua spacerLua
spacra.log: spacraHs
spacraHs: face.h face.hs faceC.o luax.h naive.hs proto.h protoC.o type.hs wrap.h
type.dep: face.so luax.so show.lua
type.h: type.gen
type.hs: type.gen
typeC.o: face.h proto.h type.h
typer.dep: face.so luax.so show.lua test.lua
typer.hs: typer.gen
typer.log: type.gen typer.lua typerC typerHs typerLua
typerC: faceC.o luaxC.o protoC.o wrapCpp.o
typerC.o: face.h proto.h typer.h
typerHs: face.h face.hs faceC.o luax.h proto.h protoC.o wrap.h
typra.log: luax.so show.lua test.lua typer.gen typra.lua typraLua
wrapCpp.o: face.h luax.h proto.h wrap.h
