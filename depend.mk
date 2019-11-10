facerC: facerC.o faceC.o
facerC.o: face.h
fileC: baseC.o typeC.o faceC.o fileC.o
fileC.o: type.h base.h face.h
facerLua: facerC facerHs facerLua facer.lua face.so
typerLua: face.so typer.lua
filerLua: filer.lua file face.so type.lua
faceC.o: face.h
facerHs: faceC.o face.hs facer.hs
typeC.o: face.h base.h type.h
typeGen: type.gen show.lua
typerC: baseC.o typerC.o faceC.o
typerC.o: base.h typer.h face.h
typerGen: typer.gen test.lua show.lua
baseC.o: face.h base.h
typraLua: typra.lua test.lua show.lua
file: typeC.o faceC.o baseC.o fileC.o
typerHs: typer.hs faceC.o face.hs
