typraLua: typra.lua test.lua show.lua
filerLua: filer.lua face.so file type.lua
typerC: baseC.o faceC.o
typerC.o: typer.h face.h base.h
baseC.o: face.h base.h
typeGen: type.gen show.lua
facerHs: faceC.o face.hs
facerLua: facer.lua face.so facerLua facerC facerHs
typerGen: typer.gen test.lua show.lua
facerC: faceC.o
facerC.o: face.h
typerLua: typer.lua face.so
faceC.o: face.h
file: fileC
fileC: baseC.o faceC.o typeC.o
fileC.o: base.h face.h type.h
typeC.o: face.h type.h base.h
typerHs: faceC.o face.hs
