typeC.o: base.h type.h face.h
typerC: faceC.o typerC.o baseC.o
typerC.o: base.h typer.h face.h
facerHs: faceC.o face.hs facer.hs
typerLua: face.so typer.lua
typeGen: show.lua type.gen
typraLua: typra.lua test.lua show.lua
fileC: faceC.o fileC.o baseC.o typeC.o
fileC.o: base.h type.h face.h
typerHs: faceC.o face.hs typer.hs
faceC.o: face.h
facerC: faceC.o facerC.o
facerC.o: face.h
baseC.o: face.h base.h
typerGen: typer.gen test.lua show.lua
