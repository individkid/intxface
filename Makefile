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

%Lua: %.inc type.inc face.so
	echo '#!/usr/bin/env lua' > $@
	echo 'dofile "'$<'"' >> $@
	chmod +x $@
filerLua sculptLua printLua playLua ballLua: type.so file
%: %Lua
	ln -f $< $@
typra.inc: typer.inc
	ln -f $< $@

%.so: %C.o
	clang -o $@ -fPIC -shared $< -llua

%C.o: %.c %.h
	clang -o $@ -c $< -I /usr/local/include/lua

%.h %.c %.hs %.lua: % %.inc type.inc
	./$< $@

.PHONY: clean
clean:
	rm -f file line plane space *C *Hs *Lua
	rm -f log type.h type.c type.hs type.lua typer.*
	rm -f *.out *.log *.o *.so *.hi *_stub.h *.txt
