/*
*    trade.c
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "type.h"
#include "base.h"
#include "face.h"
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>

jmp_buf errbuf = {0};
int mapping[NUMINET][NUMOPEN] = {0}; // to NUMOPEN
int address[NUMINET][NUMOPEN] = {0}; // to NUMINET
int number[NUMINET] = {0}; // to NUMOPEN
enum {Unused,Cluster,System,Server,Client} layer[NUMINET] = {0};
// only fub on System layer
// only hub on Cluster layer
// might have several sub on Server or Client layers

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int rub = 0; // unused listen
	int fub = 0; // file system
	int hub = 0; // process cluster
	int sub = 0;
	struct File file = {0};
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((fub = forkExec("file")) < 0) ERROR(exiterr,-1);
	if ((rub = openInet(0,argv[3])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub); bothJump(huberr,fub); bothJump(huberr,rub);
	layer[hub] = Cluster; layer[fub] = System; layer[rub] = Unused;
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(&file,sub);
	int snd = rub;
	if (layer[sub] == Cluster && file.act == NewThd && file.num == 3) {
	if (pollInet(file.ptr[1],file.ptr[2])) snd = checkInet(file.ptr[1],file.ptr[2]);
	else {snd = openInet(file.ptr[1],file.ptr[2]); layer[snd] = Server;}}
	else if (layer[sub] == Unused) {snd = fub; layer[sub] = Client;}
	else if (file.act == NewThd) snd = fub;
	if (snd != rub) {
	address[sub][file.idx] = snd;
	int num = mapping[sub][file.idx] = number[snd]++;
	address[snd][num] = sub;
	mapping[snd][num] = file.idx;}
	// forward command
	int idx = address[sub][file.idx];
	file.idx = mapping[sub][file.idx];
	writeFile(&file,idx);}}}
	return -1;
}
