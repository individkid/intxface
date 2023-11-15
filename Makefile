.SECONDARY:
.SUFFIXES:
.DELETE_ON_ERROR:

all: facer.log typra.log typer.log filer.log planra.log planer.log spacra.log spacer.log holeC lineCpp metalSw spaceHs pipeSw pageSw shareC

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
endif
ifeq ($(UNAME),Darwin)
	CXX = clang++
	CC = clang
	GHC = ghc
	SWC = swiftc
	GC = xcrun
endif

# lua depend.lua
# TODO add intermediate depends on externs; generate only used externs
include depend.mk

facer.log:
	@echo make: $@
	@./facerC > facer.log
	@./facerHs >> facer.log
	@./facerLua >> facer.log
typra.log:
	@echo make: $@
	@./typraLua > typra.log
typer.log:
	@echo make: $@
	@./typerC > typer.log
filer.log:
	@echo make: $@
	@rm -f *.--; rm -f .*.--; rm -f ..*.--; rm -f ...*.--
	@./filerLua > filer.log
planra.log:
	@echo make: $@
	@./planraC > planra.log
planer.log:
	@echo make: $@
	@./planerLua > planer.log
spacra.log:
	@echo make: $@
	@./spacraHs > spacra.log
spacer.log:
	@echo make: $@
	@./spacerLua > spacer.log

%: %C
	@echo make: $@
	@ln -f $< $@
%: %Cpp
	@echo make: $@
	@ln -f $< $@
%: %Hs
	@echo make: $@
	@ln -f $< $@
%: %Lua
	@echo make: $@
	@ln -f $< $@
%: %Sw
	@echo make: $@
	@ln -f $< $@

%C: %C.o
	@echo make: $@
	@$(CXX) -o $@ $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}
%Cpp: %Cpp.o
	@echo make: $@
	@$(CXX) -o $@ $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}
%Hs: %.hs
	@echo make: $@
	@$(GHC) -o $@ $< $(filter %C.o %Cpp.o,$^) -v0 ${LIBRARIES} ${LIBRARYPATH}
%Lua: %.lua
	@echo make: $@
	@echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Sw: %Sw.o
	@echo make: $@
	@$(SWC) -o $@ $< $(filter %C.o %.so %Cpp.o,$^) -lc++ ${LIBRARIES} ${LIBRARYPATH}

%Cpp.so: %Cpp.o
	@echo make: $@
	@$(CXX) -o $@ -fPIC -shared $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}
%.so: %C.o
	@echo make: $@
	@$(CXX) -o $@ -fPIC -shared $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}

%C.o: %.c
	@echo make: $@
	@$(CC) -o $@ -c $< ${INCLUDEPATH}
%Cpp.o: %.cpp
	@echo make: $@
	@$(CXX) -o $@ -c $< -std=c++11 ${INCLUDEPATH}
%Sw.o: %.sw
	@echo make: $@
	@cat $(filter-out $<, $(filter %.sw,$^)) $< | $(SWC) -o $@ -I . -c -

ifeq ($(UNAME),Darwin)
%.metallib: %G.o
	@echo make: $@
	@$(GC) -sdk macosx metallib -o $@ $<
%G.o: %.metal
	@echo make: $@
	@$(GC) -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<
%.metal: %.g
	@echo make: $@
	@cp $< $@
endif

%.dep: %.gen
	@echo make: $@
	@lua $< $@
%.h: %.dep
	@echo make: $@
	@lua $*.gen $@
%.c: %.dep
	@echo make: $@
	@lua $*.gen $@
%.cpp: %.dep
	@echo make: $@
	@lua $*.gen $@
%.hs: %.dep
	@echo make: $@
	@lua $*.gen $@
%.lua: %.dep
	@echo make: $@
	@lua $*.gen $@

.PHONY:
clean:
	rm -f type.h type.c type.hs type.lua type.sw
	rm -f typer.h typer.c typer.hs typer.lua typer.sw
	rm -f typra facer typer filer planra spacra
	rm -f hole file line metal space share pipe page
	rm -rf subdir.* stderr.* stdout.*
	rm -f *C *M *Cpp *Hs *Lua *Sw
	rm -f *.err *.out *.log *.tmp *.cp *.ls *.rm
	rm -f *.--; rm -f .*.--; rm -f ..*.--; rm -f ...*.--
	rm -f *.o *.so *.hi *_stub.h *.metal *.metallib *.dep

