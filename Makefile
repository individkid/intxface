.SECONDARY:
.SUFFIXES:
.DELETE_ON_ERROR:

all: facer.log typra.log typer.log filer.log planer.log spacra.log hole line plane space spacra

LIBRARIES = -llua -lportaudio
UNAME = $(shell uname)

ifeq ($(UNAME),Linux)
	CXX = g++
	CC = gcc
	EXT = x
endif
ifeq ($(UNAME),Darwin)
	CXX = clang++
	CC = clang
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
ifeq ($(UNAME),Darwin)
%: %M
	ln -f $< $@
%: %Sw
	ln -f $< $@
endif

%C: %C.o
	$(CXX) -L/usr/local/lib -o $@ $(filter %C.o,$^) ${LIBRARIES}
%Cpp: %Cpp.o
	$(CXX) -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES}
%Hs: %.hs
	ghc -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES} -v0
%A: %.agda
	agda --compile --ghc-flag=-o --ghc-flag=$@ $<
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
ifeq ($(UNAME),Darwin)
%M: %M.o
	$(CXX) -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES}
%Sw: %Sw.o
	swiftc -o $@ $< $(filter %C.o,$^) -L /usr/local/lib ${LIBRARIES}
endif

%.so: %C.o
	$(CC) -L/usr/local/lib -o $@ -fPIC -shared $^ -llua
ifeq ($(UNAME),Darwin)
%G.so: %G.o
	xcrun -sdk macosx metallib -o $@ $<
endif

%C.o: %.c
	$(CC) -o $@ -c $< -I /usr/local/include
%Cpp.o: %.cpp
	$(CC) -o $@ -c $< -I /usr/local/include
ifeq ($(UNAME),Darwin)
%M.o: %.m
	$(CC) -o $@ -c $< -I /usr/local/include
%Sw.o: %.sw
	cat $(filter-out $<, $(filter %.sw,$^)) $< | swiftc -o $@ -I . -c -
%G.o: %.metal
	xcrun -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<
endif

%.agda: %.a
	cp $< $@
ifeq ($(UNAME),Linux)
%.cpp: %.cpp$(EXT)
	cp $< $@
endif
ifeq ($(UNAME),Darwin)
%.sw: %.sw$(EXT)
	cp $< $@
%.metal: %.g
	cp $< $@
endif

%.h: %.gen
	lua $< $@
%.c: %.gen
	lua $< $@
%.cpp: %.gen
	lua $< $@
%.hs: %.gen
	lua $< $@
%.lua: %.gen
	lua $< $@
ifeq ($(UNAME),Darwin)
%.m: %.gen
	lua $< $@
%.sw: %.gen
	lua $< $@
%.g: %.gen
	lua $< $@
endif

.PHONY:
clean:
	rm -f type.h type.c type.hs type.lua type.sw
	rm -f typer.h typer.c typer.hs typer.lua typer.sw
	rm -f plane.sw plane.cpp
	rm -f typra facer typer filer planra spacra
	rm -f hole file line plane space
	rm -f *C *M *Cpp *Hs *A *Lua *Sw
	rm -f *.err *.out *.log *.tmp *.cp *.ls *.rm
	rm -f *.-- .*.-- ..*.-- ...*.--
	rm -f *.o *.so *.hi *_stub.h a.* *.metal
	rm -rf *.agda *.agdai MAlonzo depend
	rm -f type main help

