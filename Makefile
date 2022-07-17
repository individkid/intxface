.SECONDARY:
all: facer.log typra.log typer.log filer.log spacra.log hole line plane space spacra

.SUFFIXES:

LIBRARIES = -llua -lportaudio

ifndef DEPEND
# lua depend.lua > depend.mk
include depend.mk
endif

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
spacra.log:
	./spacraHs > spacra.log

%: %C
	ln -f $< $@
%: %M
	ln -f $< $@
%: %Cpp
	ln -f $< $@
%: %Hs
	ln -f $< $@
%: %A
	ln -f $< $@
%: %Lua
	ln -f $< $@
%: %Sw
	ln -f $< $@

%C: %C.o
	clang++ -L/usr/local/lib -o $@ $(filter %C.o,$^) ${LIBRARIES}
%M: %M.o
	clang++ -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES}
%Cpp: %Cpp.o
	clang++ -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES}
%Hs: %.hs
	ghc -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES} -v0
%A: %.agda
	agda --compile --ghc-flag=-o --ghc-flag=$@ $<
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Sw: %Sw.o
	swiftc -o $@ $< $(filter %C.o,$^) -L /usr/local/lib ${LIBRARIES}

%.so: %C.o
	clang -L/usr/local/lib -o $@ -fPIC -shared $^ -llua
%G.so: %G.o
	xcrun -sdk macosx metallib -o $@ $<

%C.o: %.c
	clang -o $@ -c $< -I /usr/local/include
%M.o: %.m
	clang -o $@ -c $< -I /usr/local/include
%Cpp.o: %.cpp
	clang -o $@ -c $< -I /usr/local/include
%Sw.o: %.sw
	cat $(filter-out $<, $(filter %.sw,$^)) $< | swiftc -o $@ -I . -c -
%G.o: %.metal
	xcrun -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<

%.metal: %.g
	cp $< $@
%.agda: %.a
	cp $< $@

%.h: %.gen
	lua $< $@
%.c: %.gen
	lua $< $@
%.m: %.gen
	lua $< $@
%.cpp: %.gen
	lua $< $@
%.hs: %.gen
	lua $< $@
%.lua: %.gen
	lua $< $@
%.sw: %.gen
	lua $< $@
%.g: %.gen
	lua $< $@

.PHONY:
clean:
	rm -f type.h type.c type.hs type.lua type.sw
	rm -f typer.h typer.c typer.hs typer.lua typer.sw
	rm -f typra facer typer filer planra spacra
	rm -f hole file line plane space
	rm -f *C *M *Cpp *Hs *A *Lua *Sw
	rm -f *.err *.out *.log *.tmp *.cp *.ls *.rm
	rm -f *.-- .*.-- ..*.-- ...*.--
	rm -f *.o *.so *.hi *_stub.h a.* *.metal
	rm -rf *.agda *.agdai MAlonzo depend
	rm -f type main help

