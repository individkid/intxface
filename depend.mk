faceC.o: face.c face.h proto.h
faceCpp.o: face.cpp face.h proto.h wrap.h
file: fileC
fileC: faceC.o fileC.o fileCtypeC.o protoC.o
fileC.o: face.h file.c proto.h type.h
fileCtypeC.o: face.h fileCtype.c proto.h type.h
luax.so: faceC.o faceCpp.o luaxC.o luaxCpp.o protoC.o wrapCpp.o
luaxC.o: face.h luax.c luax.h proto.h
luaxCpp.o: luax.cpp luax.h proto.h wrap.h
protoC.o: proto.c proto.h
type.h: luax.so show.lua type.gen
wrapCpp.o: proto.h wrap.cpp wrap.h
