baseC.o: base.h face.h
baseGen: show.lua
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
typeC.o: base.h face.h type.h
typeGen: show.lua
typerC: baseC.o faceC.o
typerC.o: base.h face.h typer.h
typerGen: show.lua test.lua
typerHs: face.hs faceC.o
typerLua: face.so
typraLua: show.lua test.lua
