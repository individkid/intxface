.SECONDARY:
.SUFFIXES:
.DELETE_ON_ERROR:

all: type.dep typer.dep facer.log typra.log typer.log filer.log planer.log spacra.log hole line plane space spacra

LIBRARIES = -llua -lportaudio
UNAME = $(shell uname)

ifeq ($(UNAME),Linux)
	CXX = g++
	CC = gcc
	GHC = ghc
	AGC = agda
	SWC = oops
	GC = oops
	EXT = x
endif
ifeq ($(UNAME),Darwin)
	CXX = clang++
	CC = clang
	GHC = ghc
	AGC = agda
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
spacra.log:
	./spacraHs > spacra.log

%: %C
	ln -f $< $@
%: %Cpp
	ln -f $< $@
%: %Hs
	ln -f $< $@
%: %A
	ln -f $< $@
%: %Lua
	ln -f $< $@
%: %M
	ln -f $< $@
%: %Sw
	ln -f $< $@

%C: %C.o
	$(CXX) -L/usr/local/lib -o $@ $(filter %C.o,$^) ${LIBRARIES}
%Cpp: %Cpp.o
	$(CXX) -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES}
%Hs: %.hs
	$(GHC) -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES} -v0
%A: %.agda
	$(AGC) --compile --ghc-flag=-o --ghc-flag=$@ $<
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%M: %M.o
	$(CXX) -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES}
%Sw: %Sw.o
	$(SWC) -o $@ $< $(filter %C.o,$^) -L /usr/local/lib ${LIBRARIES}

%.so: %C.o
	$(CC) -L/usr/local/lib -o $@ -fPIC -shared $^ -llua
%G.so: %G.o
	$(GC) -sdk macosx metallib -o $@ $<

%C.o: %.c
	$(CC) -o $@ -c $< -I /usr/local/include
%Cpp.o: %.cpp
	$(CC) -o $@ -c $< -I /usr/local/include
%M.o: %.m
	$(CC) -o $@ -c $< -I /usr/local/include
%Sw.o: %.sw
	cat $(filter-out $<, $(filter %.sw,$^)) $< | $(SWC) -o $@ -I . -c -
%G.o: %.metal
	$(GC) -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<

%.agda: %.a
	cp $< $@
%.cpp: %.cpp$(EXT)
	cp $< $@
%.sw: %.sw$(EXT)
	cp $< $@
%.g: %.g$(EXT)
	cp $< $@
%.metal: %.g
	cp $< $@

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
%.m: %.dep
	lua $*.gen $@
%.sw: %.dep
	lua $*.gen $@
%.g: %.dep
	lua $*.gen $@

.PHONY:
clean:
	rm -f type.h type.c type.hs type.lua type.sw
	rm -f typer.h typer.c typer.hs typer.lua typer.sw
	rm -f plane.sw plane.cpp plane.g
	rm -f typra facer typer filer planra spacra
	rm -f hole file line plane space
	rm -f *C *M *Cpp *Hs *A *Lua *Sw
	rm -f *.err *.out *.log *.tmp *.cp *.ls *.rm
	rm -f *.-- .*.-- ..*.-- ...*.--
	rm -f *.o *.so *.hi *_stub.h a.* *.metal *.dep
	rm -rf *.agda *.agdai MAlonzo depend
	rm -f type main help

