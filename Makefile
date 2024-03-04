.SECONDARY:
.SUFFIXES:
.DELETE_ON_ERROR:

UNAME = $(shell uname)
all: facer.log typra.log typer.log filer.log planra.log spacra.log spacer.log sharer.log hole line planraCpp vulkan.vsv vulkan.fsv space share

ifeq ($(UNAME),Linux)
LIBRARIES = -llua -lportaudio -lglfw -lvulkan
CXX = g++
CC = gcc
GHC = ghc
SWC = oops
GC = oops
endif
ifeq ($(UNAME),Darwin)
LIBRARIES = -llua -lportaudio
# -ldl -lpthread -lX11 -lXrandr
CXX = clang++
CC = clang
GHC = ghc
SWC = swiftc
GC = xcrun
endif
INCLUDEPATH = -I/usr/local/include
LIBRARYPATH = -L/usr/local/lib

# lua depend.lua
include depend.mk

facer.log:
	./facerC > facer.log
	./facerHs >> facer.log
	./facerLua >> facer.log
typra.log:
	./typraLua > typra.log
typer.log:
	./typerC > typer.log
filer.log:
	rm -f *.--; rm -f .*.--; rm -f ..*.--; rm -f ...*.--
	./filerLua > filer.log
planra.log:
	./planraC > planra.log
planer.log:
	./planerLua > planer.log
spacra.log:
	./spacraHs > spacra.log
spacer.log:
	./spacerLua > spacer.log
sharer.log:
	./sharerLua > sharer.log

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
	$(CXX) -o $@ $(filter %C.o %Cpp.o,$^) -std=c++17 -O2 ${LIBRARIES} ${LIBRARYPATH}
%Hs: %.hs
	$(GHC) -o $@ $(filter %.hs %C.o %Cpp.o,$^) -v0 ${LIBRARIES} ${LIBRARYPATH}
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Sw: %Sw.o
	$(SWC) -o $@ $< $(filter %C.o %.so %Cpp.o,$^) -lc++ ${LIBRARIES} ${LIBRARYPATH}

ifeq ($(UNAME),Linux)
%.so: %C.o
	$(CXX) -o $@ -shared $(filter %C.o %Cpp.o,$^)
endif
ifeq ($(UNAME),Darwin)
%.so: %C.o
	$(CXX) -o $@ -shared $(filter %C.o %Cpp.o,$^) ${LIBRARIES} ${LIBRARYPATH}
endif

%C.o: %.c
	$(CC) -o $@ -fPIC -D_GNU_SOURCE -c $< ${INCLUDEPATH}
ifeq ($(UNAME),Linux)
%Cpp.o: %.cpp
	$(CXX) -o $@ -c -fPIC $< ${INCLUDEPATH}
%Cpp.o: %Cpp.mk
	$(CXX) -o $@ -c -fPIC `cat $<` ${INCLUDEPATH}
endif
ifeq ($(UNAME),Darwin)
%Cpp.o: %.cpp
	$(CXX) -o $@ -c -fPIC $< -std=c++11 ${INCLUDEPATH}
endif
%Sw.o: %.sw
	cat $(filter-out $<, $(filter %.sw,$^)) $< | $(SWC) -o $@ -I . -c -

ifeq ($(UNAME),Darwin)
%.metallib: %G.o
	$(GC) -sdk macosx metallib -o $@ $<
%G.o: %.metal
	$(GC) -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<
%.metal: %.g
	cp $< $@
endif
ifeq ($(UNAME),Linux)
%.vsv: %.vg
	glslc -fshader-stage=vert $< -o $@
%.fsv: %.fg
	glslc -fshader-stage=frag $< -o $@
endif

%.h: %.gen %.mk
	lua $*.gen $@
%.c: %.gen %.mk
	lua $*.gen $@ $*.mk
%.cpp: %.gen %.mk
	lua $*.gen $@
%.hs: %.gen %.mk
	lua $*.gen $@
%.lua: %.gen %.mk
	lua $*.gen $@

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
	rm -f *.o *.so *.hi *_stub.h
	rm -f *.metal *.metallib *.vsv *.fsv

