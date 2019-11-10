fileC: faceC.o fileC.o baseC.o typeC.o
fileC.o: type.h base.h face.h
typraLua: show.lua test.lua typra.lua
facerHs: faceC.o face.hs facer.hs
typeGen: type.gen show.lua
baseC.o: base.h face.h
typerHs: faceC.o face.hs typer.hs
typerGen: show.lua typer.gen test.lua
typeC.o: type.h face.h base.h
facerLua: face.so facer.lua
typerC: faceC.o typerC.o baseC.o
typerC.o: typer.h base.h face.h
faceC.o: face.h
facerC: faceC.o facerC.o
facerC.o: face.h
typerLua: face.so typer.lua
