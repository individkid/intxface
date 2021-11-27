faceC.o: face.h
facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
facerLua: face.so
fileC: faceC.o typeC.o
fileC.o: face.h type.h
filerLua: face.so type.lua
lineC: faceC.o typeC.o
lineC.o: face.h type.h
meticC.o: metic.h
planeG.o: plane.h
planeSw: faceC.o meticC.o plane.so shareC.o typeC.o
planeSw.o: face.h metic.h share.h share.sw type.h
planraSw: faceC.o meticC.o plane.so shareC.o typeC.o
planraSw.o: face.h metic.h share.h share.sw type.h
shareC.o: face.h metic.h share.h type.h
spaceHs: face.hs faceC.o naive.hs type.hs typeC.o
spacraHs: naive.hs
holeC: faceC.o typeC.o
holeC.o: face.h type.h
type.c: type.src
type.h: type.src
type.hs: type.src
type.lua: type.src
type.sw: type.src
typeC.o: face.h type.h
typer.c: type.src typra.src
typer.h: type.src typra.src
typer.hs: type.src typra.src
typer.lua: type.src typra.src
typer.sw: type.src typra.src
typerC: faceC.o typeC.o
typerC.o: face.h type.h typer.h
typerHs: face.hs faceC.o
typerLua: face.so
typerSw: faceC.o
typerSw.o: face.h
typraLua: type.src typra.src
