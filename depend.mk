facerC: facerC.o faceC.o
facerC.o: face.h
fileC: typeC.o baseC.o fileC.o faceC.o
fileC.o: base.h face.h type.h
faceC.o: face.h
typeC.o: base.h face.h type.h
typerC: typerC.o baseC.o faceC.o
typerC.o: base.h face.h typer.h
baseC.o: face.h base.h
