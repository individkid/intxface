baseC.o: base.h face.h
faceC.o: face.h
facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
facerLua: face.so facer.lua facerC facerHs facerLua
file: fileC
fileC: baseC.o faceC.o typeC.o
fileC.o: base.h face.h type.h
filerLua: face.so file filer.lua type.lua
typeC.o: base.h face.h type.h
typeGen: show.lua type.gen
typerC: baseC.o faceC.o
typerC.o: base.h face.h typer.h
typerGen: show.lua test.lua typer.gen
typerHs: face.hs faceC.o
typerLua: face.so typer.lua
typraLua: show.lua test.lua typra.lua
