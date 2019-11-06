all: facer.log typra.log typer.log filer.log

facer.log: facerC facerHs facerLua
	./facerC > facer.log
	./facerHs >> facer.log
	./facerLua >> facer.log
	cat $@

typra.log: typraLua
	./typraLua > typra.log
	cat $@

typer.log: typerC typerHs typerLua
	./typerC > typer.log
	cat $@

filer.log: filerLua
	./filerLua > filer.log
	cat $@

%C: %.c
	clang -o $@ $(filter-out %.h,$^) -llua
facerC: faceC.o face.h
typerC: faceC.o face.h typer.h
fileC: faceC.o typeC.o face.h type.h
lineC: faceC.o typeC.o face.h type.h
planeC: faceC.o typeC.o face.h type.h
%: %C
	ln -f $< $@

%Hs: %.hs
	ghc -o $@ $< $(filter-out %.hs,$^) -llua -v0 2> $*.out
facerHs: face.hs faceC.o
typerHs: face.hs faceC.o
spaceHs: type.hs face.hs faceC.o
%: %Hs
	ln -f $< $@

%Gen: %.gen
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
typerGen: show.lua test.lua
typeGen: show.lua

%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%: %Lua
	ln -f $< $@
facerLua: face.so
typraLua: show.lua test.lua
typerLua: face.so typer.lua
filerLua: face.so type.lua file
linerLua: face.so type.lua file line
planerLua: face.so type.lua file plane
spacerLua: face.so type.lua file space
sculptLua: face.so type.lua file plane space
printLua: face.so type.lua file space
playLua: face.so type.lua file line plane space
ballLua: face.so type.lua file line plane space

%.so: %C.o
	clang -o $@ -fPIC -shared $^ -llua
%C.o: %.c %.h
	clang -o $@ -c $< -I /usr/local/include/lua
%.h: %Gen
	./$< $@
%.c: %Gen
	./$< $@
%.hs: %Gen
	./$< $@
%.lua: %Gen
	./$< $@

.PHONY: clean
clean:
	rm -f type.h type.c type.hs type.lua
	rm -f typer.h typer.c typer.hs typer.lua
	rm -f typra filer file line plane space 
	rm -f *C *Hs *Lua *Gen *.out *.log *.txt
	rm -f *.o *.so *.hi *_stub.h
