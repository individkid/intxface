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
int hub2net[NUMFILE] = {0};
int net2idx[NUMINET][NUMFILE] = {0};
int net2len[NUMINET] = {0};
int hub2idx[NUMFILE] = {0};
int sys2len = 0;
int sys2net[NUMFILE] = {0};
int sys2idx[NUMFILE] = {0};

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
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(&file,sub);
	int idx = file.idx;
	if (sub == hub && file.act == NewThd && file.num == 3 && !checkinet(file.ptr[1],file[ptr[2]])) {
	// open file to server
	sub = hub2net[idx] = openInet(file.ptr[1],file[2]);
	file.idx = net2idx[sub][idx] = net2len[sub]++;}
	else if (sub == hub && hub2net[idx]) {
	// forward command from cluster to server
	sub = hub2net[idx];
	file.idx = net2idx[sub][idx];}
	else if (sub == hub && file.act == NewThd) {
	// open file to system
	sub = fub;
	file.idx = hub2idx[idx] = sys2len++;}
	else if (sub == hub) {
	// forward command from cluster to system
	sub = fub;
	file.idx = sys2idx[idx];}
	else if (sub == fub && sys2net[idx]) {
	// forward response from system to client
	sub = sys2net[idx];
	file.idx = sys2idx[idx];}
	else if (sub == fub) {
	// forward response from system to cluster
	sub = hub;
	file.idx = sys2idx[idx];}
	else if (file.act == NewThd) {
	// open file for client
	sys2net[idx] = sub = fub;
	sys2idx[file.idx = net2idx[idx] = sys2len++] = idx;}
	else if (hub2net[idx]) {
	// forward response from server to cluster
	sub = hub;
	file.idx = sys2idx[idx];}
	else {
	// forward command from client to system
	sub = fub;
	file.idx = net2idx[idx];}
	writeFile(&file,sub);}}}
	return -1;
}
