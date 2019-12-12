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
typer.log: typerC typerHs typerLua
	./typerC > typer.log
	cat $@
filer.log: filerLua file
	rm -f *.txt .*.txt ..*.txt ...*.txt
	./filerLua > filer.log
	cat $@

%C: %C.o
	clang++ -o $@ $(filter %C.o,$^) -llua -lportaudio -lglfw3 -lMoltenVK
%Hs: %.hs
	ghc -L/usr/lib -o $@ $< $(filter %C.o,$^) -llua -lportaudio -lglfw3 -lMoltenVK -v0 2> $*.out
%Gen: %.gen
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@
%Lua: %.lua
	echo '#!/usr/bin/env lua' > $@ ; echo 'dofile "'$<'"' >> $@ ; chmod +x $@

%: %C
	ln -f $< $@
%: %Hs
	ln -f $< $@
%: %Lua
	ln -f $< $@

%.so: %C.o
	clang -o $@ -fPIC -shared $^ -llua
%C.o: %.c
	clang -o $@ -c $< -I /usr/local/include/lua -I /usr/local/Cellar/molten-vk/1.0.34/libexec/include/vulkan
%C.o: %.cpp
	clang -o $@ -c $< -I /usr/local/include/lua -I /usr/local/Cellar/molten-vk/1.0.34/libexec/include/vulkan

%.h: %Gen
	./$< $@
%.c: %Gen
	./$< $@
%.hs: %Gen
	./$< $@
%.lua: %Gen
	./$< $@

.PHONY:
clean:
	rm -f base.h base.c
	rm -f type.h type.c type.hs type.lua
	rm -f typer.h typer.c typer.hs typer.lua
	rm -f typra facer typer filer
	rm -f trade file line plane space
	rm -f *C *Hs *Lua *Gen *.err *.out *.log
	rm -f *.txt .*.txt ..*.txt ...*.txt
	rm -f *.o *.so *.hi *_stub.h
