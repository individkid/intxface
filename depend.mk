baseC.o: base.h face.h
baseGen: show.lua
containC.o: contain.h face.h
faceC.o: face.h
facerC: faceC.o
facerC.o: face.h
facerHs: face.hs faceC.o
facerLua: face.so
fileC: baseC.o faceC.o typeC.o
fileC.o: base.h face.h type.h
filerLua: face.so type.lua
glfwC.o: base.h face.h metic.h plane.h type.h
lineC: baseC.o faceC.o typeC.o
lineC.o: base.h face.h type.h
metalC.o: base.h face.h metic.h plane.h type.h
meticC.o: metic.h
modelC.o: base.h face.h metic.h plane.h type.h
openglC.o: base.h contain.h face.h metic.h plane.h type.h
planeC: baseC.o containC.o faceC.o glfwC.o metalC.o meticC.o modelC.o openglC.o typeC.o vulkanC.o
planeC.o: base.h face.h metic.h plane.h type.h
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
vulkanC.o: base.h face.h metic.h plane.h type.h
containC.o: base.h type.h
