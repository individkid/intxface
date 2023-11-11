datxC.o: datx.h type.h
face.so: luaxC.o protoC.o wrapCpp.o
faceC.o: face.h luax.h proto.h wrap.h
facer.log: face.so facerC facerHs facerLua
facerC: faceC.o luaxC.o protoC.o wrapCpp.o
facerC.o: face.h proto.h
facerHs: face.hs faceC.o protoC.o
fileC: faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
fileC.o: face.h proto.h type.h
filer.log: fileC filerLua type.lua
holeC: faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
lineCpp: faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
luax.so: faceC.o protoC.o wrapCpp.o
luaxC.o: luax.h proto.h wrap.h
metalSw: datxC.o faceC.o luaxC.o metxC.o planeC.o protoC.o typeC.o wrapCpp.o
metalSw.o: face.h plane.h proto.h type.h
metxC.o: metx.h
planeC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h wrap.h
planer.log: metal.metallib metalSw planerLua type.lua
planra.log: planraC
planraC: datxC.o faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
planraC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h wrap.h
protoC.o: proto.h
shareC: datxC.o faceC.o luaxC.o protoC.o typeC.o wrapCpp.o
spacra.log: spacraHs
spacer.log: spacerLua spaceHs
spacraHs: face.hs naive.hs type.hs faceC.o protoC.o wrapCpp.o
spaceHs: face.hs naive.hs type.hs faceC.o protoC.o wrapCpp.o
type.dep: face.so luax.so show.lua
typeC.o: type.h
typer.dep: face.so luax.so show.lua test.lua
typer.log: typerC typerHs typerLua
typerC: faceC.o luaxC.o protoC.o wrapCpp.o
typerC.o: typer.h
typerHs: face.hs faceC.o protoC.o
typra.log: face.so luax.so show.lua test.lua typraLua
wrapCpp.o: face.h luax.h proto.h wrap.h
