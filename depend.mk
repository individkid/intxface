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
openglC.o: plane.h
planeC: baseC.o faceC.o glfwC.o metalC.o openglC.o typeC.o vulkanC.o
planeC.o: base.h face.h plane.h type.h
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
