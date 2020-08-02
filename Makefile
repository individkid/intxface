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

.SECONDARY:
all: facer.log typra.log typer.log filer.log line plane space trade

ifndef DEPEND
# lua depend.lua > depend.mk
include depend.mk
endif

facer.log: facerC facerHs facerLua
	./facerC > facer.log
	./facerHs >> facer.log
	./facerLua >> facer.log
	cat $@
typra.log: typraLua
	./typraLua > typra.log
	cat $@
typer.log: typerC typerHs typerLua typerSw
	./typerC > typer.log
	cat $@
filer.log: filerLua file
	rm -f *.txt .*.txt ..*.txt ...*.txt
	./filerLua > filer.log
	cat $@

%: %C
	ln -f $< $@
%: %Hs
	ln -f $< $@
%: %Lua
	ln -f $< $@
%: %Sw
	ln -f $< $@

FRAMEWORKS = -framework CoreFoundation -framework CoreGraphics -framework OpenGL -framework Metal -framework QuartzCore
LIBRARIES = -llua -lportaudio -lglfw -lGLEW -lMoltenVK
%C: %C.o
	clang++ -o $@ $(filter %C.o,$^) ${LIBRARIES} ${FRAMEWORKS}
%Hs: %.hs
	ghc -L/usr/lib -o $@ $< $(filter %C.o,$^) ${LIBRARIES} -v0 2> $*.out
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Sw: %Sw.o
	swiftc -o $@ $< $(filter %C.o,$^) -L /usr/local/lib ${LIBRARIES} ${FRAMEWORKS}

%.so: %C.o
	clang -o $@ -fPIC -shared $^ -llua
%.so: %G.o
	xcrun -sdk macosx metallib -o $@ $<

%C.o: %.c
	clang -o $@ -c $< -I /usr/local/include/lua -I /usr/local/Cellar/molten-vk/1.0.34/libexec/include
%C.o: %.m
	clang -o $@ -c $< -I /usr/local/include/lua -I /usr/local/Cellar/molten-vk/1.0.34/libexec/include
%C.o: %.cpp
	clang -o $@ -c $< -I /usr/local/include/lua -I /usr/local/Cellar/molten-vk/1.0.34/libexec/include
%Sw.o: %.sw
	cat $(filter %.sw,$^) | swiftc -o $@ -I . -c -
%G.o: %.metal
	xcrun -sdk macosx metal -O2 -std=macos-metal2.2 -o $@ -c $<

%.h: %.gen
	lua $< $@
%.c: %.gen
	lua $< $@
%.hs: %.gen
	lua $< $@
%.lua: %.gen
	lua $< $@
%.sw: %.gen
	lua $< $@
%.metal: %.g
	cp $< $@

.PHONY:
clean:
	rm -f base.h base.c contain.h contain.c
	rm -f type.h type.c type.hs type.lua type.sw
	rm -f typer.h typer.c typer.hs typer.lua typer.sw
	rm -f typra facer typer filer
	rm -f trade file line plane space
	rm -f *C *Hs *Lua *Sw
	rm -f *.err *.out *.log
	rm -f *.txt .*.txt ..*.txt ...*.txt
	rm -f *.o *.so *.hi *_stub.h a.* *.metal
	rm -f depend opengl type main

