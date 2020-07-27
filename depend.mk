base.c: type.src
base.h: type.src
baseC.o: base.h face.h
containC.o: contain.h
faceC.o: face.h
facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
facerLua: face.so
fileC: baseC.o faceC.o typeC.o
fileC.o: base.h face.h type.h
filerLua: face.so type.lua
glfwC.o: base.h face.h metic.h share.h type.h
lineC: baseC.o faceC.o typeC.o
lineC.o: base.h face.h type.h
metalC.o: base.h face.h metic.h share.h type.h
meticC.o: metic.h
modelC.o: base.h face.h metic.h share.h type.h
openglC.o: base.h contain.h face.h metic.h share.h type.h
planeG.o: plane.h
planeSw: baseC.o faceC.o glfwC.o metalC.o meticC.o plane.so shareC.o typeC.o
planeSw.o: base.h face.h metic.h share.h type.h type.sw
shareC.o: base.h face.h metic.h share.h type.h
spaceHs: baseC.o face.hs faceC.o naive.hs type.hs typeC.o
tradeC: baseC.o faceC.o typeC.o
tradeC.o: base.h face.h type.h
typeC.o: base.h face.h type.h
typeSw.o: face.h
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
vulkanC.o: base.h face.h metic.h share.h type.h
contain.c: base.h type.h
