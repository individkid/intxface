datxC.o: datx.h type.h
face.so: luaxC.o protoC.o
faceC.o: face.h luax.h proto.h
facer.log: face.so facerC facerHs facerLua
facerC: faceC.o luaxC.o protoC.o
facerC.o: face.h proto.h
facerHs: face.hs faceC.o protoC.o
fileC: faceC.o luaxC.o protoC.o typeC.o
fileC.o: face.h proto.h type.h
filer.log: fileC filerLua type.lua
holeC: faceC.o luaxC.o protoC.o typeC.o
holeC.o: face.h proto.h type.h
lineCpp: faceC.o luaxC.o protoC.o typeC.o
lineCpp.o: face.h proto.h type.h
luax.so: protoC.o
luaxC.o: luax.h proto.h
metalSw: datxC.o faceC.o luaxC.o metxC.o planeC.o protoC.o typeC.o
metalSw.o: face.h plane.h proto.h type.h
metxC.o: metx.h
planeC.o: datx.h face.h luax.h metx.h plane.h proto.h type.h
planer.log: metal.metallib metalSw planerLua type.lua
planra.log: planraC
planraC: datxC.o faceC.o luaxC.o protoC.o typeC.o
planraC.o: datx.h proto.h
protoC.o: proto.h
shareC: datxC.o faceC.o luaxC.o protoC.o typeC.o
shareC.o: datx.h face.h proto.h type.h
spaceHs: face.hs faceC.o naive.hs protoC.o type.hs
spacra.log: spacraHs
spacraHs: naive.hs
type.dep: face.so luax.so show.lua
typeC.o: type.h
typer.dep: face.so luax.so show.lua test.lua
typer.log: typerC typerHs typerLua
typerC: faceC.o luaxC.o protoC.o
typerC.o: typer.h
typerHs: face.hs faceC.o protoC.o
typra.log: face.so luax.so show.lua test.lua typraLua
