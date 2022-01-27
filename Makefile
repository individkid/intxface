.SECONDARY:
all: facer.log typra.log typer.log filer.log spacra.log hole line plane space spacra

LIBRARIES = -llua -lportaudio

ifndef DEPEND
# lua depend.lua > depend.mk
include depend.mk
endif

facer.log: facerC facerHs facerLua
	./facerC > facer.log
	./facerHs >> facer.log
	./facerLua >> facer.log
typra.log: typraLua
	./typraLua > typra.log
typer.log: typerC typerHs typerLua typerSw
	./typerC > typer.log
filer.log: filerLua file
	rm -f *.-- .*.-- ..*.-- ...*.--
	./filerLua > filer.log
spacra.log: spacra
	./spacra > spacra.log

%: %C
	ln -f $< $@
%: %Hs
	ln -f $< $@
%: %Lua
	ln -f $< $@
%: %Sw
	ln -f $< $@

%C: %C.o
	clang++ -L/usr/local/lib -o $@ $(filter %C.o,$^) ${LIBRARIES}
%Hs: %.hs
	ghc -L/usr/local/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES} -v0 2> $*.out
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
%C.o: %.m
	clang -o $@ -c $< -I /usr/local/include
%C.o: %.cpp
	clang -o $@ -c $< -I /usr/local/include
%Sw.o: %.sw
	cat $(filter %.sw,$^) | swiftc -o $@ -I . -c -
%G.o: %.metal
	xcrun -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<

%.metal: %.g
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
	rm -f *C *Hs *Lua *Sw
	rm -f *.err *.out *.log *.tmp
	rm -f *.-- .*.-- ..*.-- ...*.--
	rm -f *.o *.so *.hi *_stub.h a.* *.metal
	rm -f depend type main help

