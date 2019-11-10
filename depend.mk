typerHs: face.hs
file: fileC
facerHs: face.hs
fileC: faceC.o typeC.o
fileC.o: base.h face.h type.h
facerLua: facer.lua facerC facerLua facerHs face.so
typerLua: typer.lua face.so
typeGen: type.gen show.lua
typerC: baseC.o faceC.o
typerC.o: base.h typer.h face.h
filerLua: filer.lua face.so file type.lua
typerGen: typer.gen test.lua show.lua
facerC: faceC.o
facerC.o: face.h
faceC.o: face.h
typeC.o: base.h face.h type.h
typraLua: typra.lua test.lua show.lua
baseC.o: base.h face.h
