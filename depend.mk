baseC.o: base.h face.h
typeC.o: base.h type.h face.h
faceC.o: face.h
facerC: facerC.o faceC.o
facerC.o: face.h
typerC: baseC.o typerC.o faceC.o
typerC.o: base.h typer.h face.h
fileC: baseC.o typeC.o fileC.o faceC.o
fileC.o: base.h face.h type.h
facerHs: face.so facer.hs face.hs
typerHs: face.so face.hs typer.hs
