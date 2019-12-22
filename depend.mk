arithmeticC.o: arithmetic.h
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
glfwC.o: plane.h
lineC: baseC.o faceC.o typeC.o
lineC.o: base.h face.h type.h
metalC.o: plane.h
modelC.o: plane.h
openglC.o: plane.h
planeC: arithmeticC.o baseC.o faceC.o glfwC.o metalC.o modelC.o openglC.o typeC.o vulkanC.o
planeC.o: arithmetic.h base.h face.h plane.h type.h
spaceHs: face.hs faceC.o naive.hs type.hs
tradeC: baseC.o faceC.o typeC.o
tradeC.o: base.h face.h type.h
typeC.o: base.h face.h type.h
typeGen: show.lua
typerC: baseC.o faceC.o
typerC.o: base.h face.h typer.h
typerGen: show.lua test.lua
typerHs: face.hs faceC.o
typerLua: face.so
typraLua: show.lua test.lua
vulkanC.o: plane.h
