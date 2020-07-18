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
planeSw: baseC.o containC.o faceC.o glfwC.o metalC.o meticC.o modelC.o openglC.o shareC.o typeC.o vulkanC.o
planeSw.o: base.h face.h metic.h share.h type.h
shareC.o: base.h face.h metic.h share.h type.h
spaceHs: face.hs faceC.o naive.hs type.hs
tradeC: baseC.o faceC.o typeC.o
tradeC.o: base.h face.h type.h
typeC.o: base.h face.h type.h
typeSw.o: face.h
typerC: baseC.o faceC.o
typerC.o: base.h face.h typer.h
typerHs: face.hs faceC.o
typerLua: face.so
typerSw: faceC.o
typerSw.o: face.h
typraLua: type.src typra.src
vulkanC.o: base.h face.h metic.h share.h type.h
contain.c: base.h type.h
