faceC.o: face.h
facer.log: facerC facerHs facerLua
facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
facerLua: face.so
fileC: faceC.o typeC.o
fileC.o: face.h type.h
filer.log: fileC filerLua
filerLua: face.so type.lua
holeC: faceC.o typeC.o
holeC.o: face.h type.h
lineCpp: faceC.o typeC.o
lineCpp.o: face.h type.h
planeC.o: face.h metic.h plane.h share.h type.h
planeSw: faceC.o planeC.o typeC.o
planeSw.o: face.h plane.h type.h
spaceHs: face.hs faceC.o naive.hs type.hs
spacra.log: spacraHs
spacraHs: naive.hs
type.c: type.src
type.h: type.src
type.hs: type.src
type.lua: type.src
typeC.o: face.h type.h
typer.c: type.src typra.src
typer.h: type.src typra.src
typer.hs: type.src typra.src
typer.log: typerC typerHs typerLua typerSw
typer.lua: type.src typra.src
typer.sw: type.src typra.src
typerC: faceC.o
typerC.o: face.h typer.h
typerHs: face.hs faceC.o
typerLua: face.so
typerSw: faceC.o
typerSw.o: face.h
typra.log: typraLua
typraLua: type.src typra.src
