base.c: type.src
base.h: type.src
baseC.o: base.h face.h
faceC.o: face.h
facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
facerLua: face.so
fileC: baseC.o faceC.o typeC.o
fileC.o: base.h face.h type.h
filerLua: face.so type.lua
lineC: baseC.o faceC.o typeC.o
lineC.o: base.h face.h type.h
meticC.o: metic.h
planeG.o: plane.h
planeSw: baseC.o faceC.o meticC.o plane.so shareC.o typeC.o
planeSw.o: base.h face.h metic.h share.h type.h
shareC.o: base.h face.h metic.h share.h type.h
spaceHs: baseC.o face.hs faceC.o naive.hs type.hs typeC.o
tradeC: baseC.o faceC.o typeC.o
tradeC.o: base.h face.h type.h
type.c: type.src
type.h: type.src
type.hs: type.src
type.lua: type.src
typeC.o: base.h face.h type.h
typer.c: type.src typra.src
typer.h: type.src typra.src
typer.hs: type.src typra.src
typer.lua: type.src typra.src
typer.sw: type.src typra.src
typerC: baseC.o faceC.o
typerC.o: base.h face.h typer.h
typerHs: face.hs faceC.o
typerLua: face.so
typerSw: faceC.o
typerSw.o: face.h
typraLua: type.src typra.src
