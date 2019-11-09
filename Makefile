#    Makefile
#    Copyright (C) 2019  Paul Coelho
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

%C: %C.o
	clang -o $@ $^ -llua
facerC: faceC.o
typerC: faceC.o baseC.o
fileC: faceC.o baseC.o typeC.o
lineC: faceC.o baseC.o typeC.o
planeC: faceC.o baseC.o typeC.o
%: %C
	ln -f $< $@

%Hs: %.hs
	ghc -o $@ $< $(filter-out %.hs,$^) -llua -v0 2> $*.out
facerHs: face.hs faceC.o
typerHs: face.hs faceC.o
spaceHs: face.hs faceC.o type.hs
%: %Hs
	ln -f $< $@

%Gen: %.gen
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
typerGen: show.lua test.lua
typeGen: show.lua
baseGen: show.lua

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
%C.o: %.c
	clang -o $@ -c $< -I /usr/local/include/lua
faceC.o: face.h
facerC.o: face.h
baseC.o: face.h base.h
typeC.o: face.h base.h type.h
typerC.o: face.h base.h typer.h
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
	rm -f base.h base.c
	rm -f type.h type.c type.hs type.lua
	rm -f typer.h typer.c typer.hs typer.lua
	rm -f typra filer file line plane space
	rm -f depend facer show test type typer
	rm -f *C *Hs *Lua *Gen *.err *.out *.log *.txt
	rm -f *.o *.so *.hi *_stub.h
