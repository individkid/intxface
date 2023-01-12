.SECONDARY:
.SUFFIXES:
.DELETE_ON_ERROR:

all: facer.log typra.log typer.log filer.log planer.log sharer.log spacra.log hole line plane space share core page

INCLUDEPATH = -I/usr/local/include
LIBRARYPATH = -L/usr/local/lib
LIBRARIES = -llua -lportaudio
UNAME = $(shell uname)

ifeq ($(UNAME),Linux)
	CXX = g++
	CC = gcc
	GHC = ghc
	SWC = oops
	GC = oops
	EXT = x
endif
ifeq ($(UNAME),Darwin)
	CXX = clang++
	CC = clang
	GHC = ghc
	SWC = swiftc
	GC = xcrun
	EXT = y
endif

# lua depend.lua
include depend.mk$(EXT)

facer.log:
	./facerC > facer.log
	./facerHs >> facer.log
	./facerLua >> facer.log
typra.log:
	./typraLua > typra.log
typer.log:
	./typerC > typer.log
filer.log:
	rm -f *.-- .*.-- ..*.-- ...*.--
	./filerLua > filer.log
planer.log:
	./planerLua > planer.log
sharer.log:
	./sharerLua > sharer.log
spacra.log:
	./spacraHs > spacra.log

%: %C
	ln -f $< $@
%: %Cpp
	ln -f $< $@
%: %Hs
	ln -f $< $@
%: %Lua
	ln -f $< $@
%: %Sw
	ln -f $< $@

%C: %C.o
	$(CXX) -o $@ $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}
%Cpp: %Cpp.o
	$(CXX) -o $@ $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}
%Hs: %.hs
	$(GHC) -o $@ $< $(filter %C.o %Cpp.o,$^) -v0 ${LIBRARIES} ${LIBRARYPATH}
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Sw: %Sw.o
	$(SWC) -o $@ $< $(filter %C.o %.so,$^) ${LIBRARIES} ${LIBRARYPATH}

ifeq ($(UNAME),Darwin)
%.metallib: %G.o
	$(GC) -sdk macosx metallib -o $@ $<
endif
%Cpp.so: %Cpp.o
	$(CXX) -o $@ -fPIC -shared $^ ${LIBRARIES} ${LIBRARYPATH}
%.so: %C.o
	$(CC) -o $@ -fPIC -shared $^ ${LIBRARIES} ${LIBRARYPATH}

%C.o: %.c
	$(CC) -o $@ -c $< ${INCLUDEPATH}
%Cpp.o: %.cpp
	$(CXX) -o $@ -c $< ${INCLUDEPATH}
%Sw.o: %.sw
	cat $(filter-out $<, $(filter %.sw,$^)) $< | $(SWC) -o $@ -I . -c -
%G.o: %.metal
	$(GC) -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<

%.cpp: %.cpp$(EXT)
	cp $< $@
%.sw: %.sw$(EXT)
	cp $< $@
ifeq ($(UNAME),Darwin)
%.metal: %.g$(EXT)
	cp $< $@
endif

%.dep: %.gen
	lua $< $@
%.h: %.dep
	lua $*.gen $@
%.c: %.dep
	lua $*.gen $@
%.cpp: %.dep
	lua $*.gen $@
%.hs: %.dep
	lua $*.gen $@
%.lua: %.dep
	lua $*.gen $@
%.sw: %.dep
	lua $*.gen $@

.PHONY:
clean:
	rm -f type.h type.c type.hs type.lua type.sw
	rm -f typer.h typer.c typer.hs typer.lua typer.sw
	rm -f plane.sw plane.cpp plane.g
	rm -f core.sw page.sw
	rm -f typra facer typer filer planra spacra
	rm -f hole file line plane space share
	rm -f core page
	rm -f *C *M *Cpp *Hs *Lua *Sw
	rm -f *.err *.out *.log *.tmp *.cp *.ls *.rm
	rm -f *.-- .*.-- ..*.-- ...*.--
	rm -f *.o *.so *.hi *_stub.h *.metal *.metallib *.dep
	rm -rf depend

