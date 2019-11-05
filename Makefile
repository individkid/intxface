test: facer.log typra.log typer.log filer.log

facerC.log: facerC facerHs facerLua
	./facerC > facerC.log

facer.log: facerC facerHs facerLua
	./facerC > facer.log
	./facerHs >> facer.log
	./facerLua >> facer.log

typra.log: typra
	./typra > typra.log

typer.log: typerC typerHs typerLua
	./typerC > typer.log

filer.log: filer
	./filer > filer.log

%C: %.c faceC.o
	clang -o $@ $^ -llua
fileC lineC planeC: typeC.o
%: %C
	ln -f $< $@

%Hs: %.hs face.hs faceC.o
	ghc -o $@ $< faceC.o -llua -v0 2> $*.out
spaceHs: type.hs
%: %Hs
	ln -f $< $@

wrap = echo '\#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Gen: %.gen
	$(wrap)
%Lua: %.lua
	$(wrap)
%: %Lua
	ln -f $< $@
facerLua: face.so
typraLua typerGen: show.lua test.lua
typerLua: face.so type.so
typeGen: show.lua
filerLua: face.so type.so file
linerLua playLua ballLua: face.so type.so file line
planerLua sculptLua playLua ballLua: face.so type.so file plane
spacerLua sculptLua printLua playLua ballLua: face.so type.so file space

%.so: %C.o
	clang -o $@ -fPIC -shared $< -llua

%C.o: %.c %.h
	clang -o $@ -c $< -I /usr/local/include/lua

%.h %.c %.hs %.lua: %Gen
	./$< $@

.PHONY: clean
clean:
	rm -f file line plane space *C *Hs *Lua *Gen
	rm -f log type.h type.c type.hs type.lua typer.*
	rm -f *.out *.log *.o *.so *.hi *_stub.h *.txt
