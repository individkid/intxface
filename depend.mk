facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
faceC.o: face.h
typerGen: typer.gen show.lua test.lua
fileC: typeC.o baseC.o faceC.o
fileC.o: face.h type.h base.h
typraLua: typra.lua show.lua test.lua
typeGen: type.gen show.lua
typerHs: face.hs faceC.o
typerLua: typer.lua face.so
baseC.o: face.h base.h
facerLua: facer.lua facerC facerHs facerLua face.so
typerC: faceC.o baseC.o
typerC.o: base.h face.h typer.h
file: fileC
filerLua: filer.lua face.so type.lua file
typeC.o: base.h face.h type.h
