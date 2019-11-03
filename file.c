/*
*    file.c
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
#include "face.h"
#include <pthread.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/errno.h>

#define NUMFILE 64
#define INFINITE 1000000000ull
#define FILESIZE 4096
#define CMDSIZE 4
#define NAMES 4
#define GIVEN 3
#define NAMED 2
#define HELPER 1
#define CONTROL 0

long long identifier = 0;
jmp_buf errbuf = {0};
jmp_buf jmpbuf[NUMFILE] = {0};
int number[NUMOPEN] = {0};

enum Stage {
	Move,
	Rreq,
	Wreq,
	Rrsp,
	Wrsp,
	Wlck,
	Rlck,
	Send,
	Done,
	Stages
};
struct Thread {
	pthread_t thread;
	enum Stage stage;
	char *name;
	int anonym;
	int named;
	int given;
	int helper;
	int control;
	int seqnum;
	off_t config;
	off_t append;
	int idx;
	off_t loc;
	int num;
	char *buf;
	int total;
	char buffer[CMDSIZE*BUFSIZE];
	int size[CMDSIZE];
	char *pointer[CMDSIZE];
	struct File command;
	struct File response;
};

void create(char *ptr, int idx, struct Thread *thread);
void clean(struct Thread *thread);
void move(struct Thread *thread);
void rreq(struct Thread *thread);
void wreq(struct Thread *thread);
void rrsp(struct Thread *thread);
void wrsp(struct Thread *thread);
void wlck(struct Thread *thread);
void rlck(struct Thread *thread);
void send(struct Thread *thread);
void jump(struct Thread *thread);

void spokerr(int arg)
{
	longjmp(jmpbuf[number[arg]],1);
}

void huberr(int arg)
{
	longjmp(errbuf,1);
}

void filerr(int arg)
{
	longjmp(jmpbuf[arg],1);
}

void exiterr(int arg)
{
	exit(arg);
}

void nonote(int idx)
{
}

void construct(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] != 0) ERROR(exiterr,-1)
	if (command->num != 1) ERROR(exiterr,-1)
	struct Thread *new = malloc(sizeof(struct Thread));
	if (new == 0) ERROR(exiterr,-1)
	struct Thread init = {0};
	*new = init;
	thread[command->idx] = new;
	create(command->ptr[0],command->idx,new);
}

void normal(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num <= 0) ERROR(exiterr,-1)
	writeFile(command,thread[command->idx]->named);
}

void normish(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub == face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num <= 0) ERROR(exiterr,-1)
	writeFile(command,face);
}

void final(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num != 0) ERROR(exiterr,-1)
	command->loc = identifier;
	writeFile(command,thread[command->idx]->named);
}

void finish(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub == face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num != 0) ERROR(exiterr,-1)
	clean(thread[command->idx]);
	free(thread[command->idx]);
	thread[command->idx] = 0;
	writeFile(command,face);
}

void error(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (command->num != 0) ERROR(exiterr,-1)
	command->act = EndThd;
	command->loc = identifier;
	for (int i = 0; i < NUMFILE; i++) if (thread[i])
	writeFile(command,thread[i]->named);
	command->act = PrcEnd;
	writeFile(command,face);
}

void errish(struct File *command, int sub, int face, struct Thread **thread)
{
	struct File temp = {0};
	temp.num = 0;
	finish(&temp,sub,face,thread);
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int face = 0;
	int sub = 0;
	struct File command = {0};
	struct Thread *thread[NUMFILE] = {0};
	identifier = getpid(); // TODO add seconds-since-publish
	if ((face = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,face);
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(&command,sub);
	switch (command.act) {
	// initialize idx Thread with file name
	case (NewThd): construct(&command,sub,face,thread); break;
	// forward to idx Thread through named pipe
	case (CmdThd): normal(&command,sub,face,thread); break;
	// forward from number[sub] Thread to process pipe
	case (ThdCmd): normish(&command,sub,face,thread); break;
	// forward identifier to idx Thread through named pipe
	case (EndThd): final(&command,sub,face,thread); break;
	// forward last from number[sub] Thread to process pipe
	case (ThdEnd): finish(&command,sub,face,thread); break;
	// forward to each Thread and send last to process pipe
	case (EndPrc): error(&command,sub,face,thread); return 0;
	default: ERROR(exiterr,-1)}} ERROR(exiterr,-1)}
	// send last from number[sub] Thread to process pipe
	errish(&command,sub,face,thread);}
	return -1;
}

void *file(void *arg)
{
	struct Thread *thread = (struct Thread *)arg;
	if (setjmp(jmpbuf[thread->idx]) == 0)
	while (1) switch (thread->stage) {
	// reopen helper
	// neof helper, goto Rreq
	// eof helper, goto Rrsp
	case (Move): move(thread); break;
	// read helper
	// kill command to self, goto Done
	// kill command to other, neof helper, goto Rreq
	// kill command to other, eof helper, goto Rrsp
	// past command, goto Wreq
	// future command, goto Rrsp
	case (Rreq): rreq(thread); break;
	// write anonym
	// neof helper, goto Rreq
	// eof helper, goto Rrsp
	case (Wreq): wreq(thread); break;
	// read given
	// eof given, nothing read, goto Wlck
	// eof given, something read, goto Wrsp
	// neof given, enough read, goto Wrsp
	// neof given, not enough read, goto Rrsp
	case (Rrsp): rrsp(thread); break;
	// write anonym
	case (Wrsp): wrsp(thread); break;
	case (Wlck): wlck(thread); break;
	case (Rlck): rlck(thread); break;
	case (Send): send(thread); break;
	case (Done): return 0;
	default: ERROR(exiterr,-1)}
	jump(thread); return 0;
}

#define self thread->thread
#define stage thread->stage
#define name thread->name
#define anonym thread->anonym
#define named thread->named
#define given thread->given
#define helper thread->helper
#define control thread->control
#define seqnum thread->seqnum
#define config thread->config
#define append thread->append
#define thdidx thread->idx
#define thdloc thread->loc
#define thdnum thread->num
#define thdbuf thread->buf
#define total thread->total
#define buffer thread->buffer
#define size thread->size
#define pointer thread->pointer
#define cmd thread->command
#define rsp thread->response
#define cmdact thread->command.act
#define cmdidx thread->command.idx
#define cmdloc thread->command.loc
#define cmdnum thread->command.num
#define cmdsiz thread->command.siz
#define cmdptr thread->command.ptr
#define rspact thread->response.act
#define rspidx thread->response.idx
#define rsploc thread->response.loc
#define rspnum thread->response.num
#define rspsiz thread->response.siz
#define rspptr thread->response.ptr

void create(char *ptr, int sub, struct Thread *thread)
{
	if ((name = malloc(strlen(ptr)+NAMES)) == 0) ERROR(huberr,-1)
	for (int i = 0; i < NAMES-1; i++) name[i] = '.';
	strcpy(name+NAMES-1,ptr);
	if ((anonym = openPipe()) < 0) ERROR(huberr,-1)
	if ((named = openFifo(name+NAMED)) < 0) ERROR(huberr,-1)
	if ((helper = openFile(name+HELPER)) < 0) ERROR(huberr,-1)
	if ((given = openFile(name+GIVEN)) < 0) ERROR(huberr,-1)
	if ((control = openFile(name+CONTROL)) < 0) ERROR(huberr,-1)
	number[anonym] = thdidx; number[named] = thdidx;
	number[helper] = thdidx; number[given] = thdidx;
	number[control] = thdidx;
	readJump(huberr,anonym); writeJump(huberr,named);
	writeJump(spokerr,anonym); readJump(spokerr,named);
	bothJump(spokerr,helper); bothJump(spokerr,given);
	bothJump(spokerr,control);
	append = 0; config = 0; total = 0; thdnum = 0;
	thdidx = sub; thdbuf = buffer; thdloc = config; stage = Move;
	if (pthread_create(&self,0,file,thread) < 0) ERROR(huberr,-1)
}

void clean(struct Thread *thread)
{
	if (pthread_join(self,0) < 0) ERROR(huberr,-1)
	closeIdent(anonym);
	closeIdent(named);
	closeIdent(helper);
	closeIdent(given);
	closeIdent(control);
	number[anonym] = 0; number[named] = 0;
	number[helper] = 0; number[given] = 0;
	number[control] = 0;
}

void move(struct Thread *thread)
{
	wrlkwFile(0,1,control);
	if (append >= FILESIZE && unlink(name+HELPER) < 0) ERROR(filerr,thdidx)
	moveIdent(openFile(name+GIVEN),helper);
	if (pollFile(helper)) {
		int saved = seqnum;
		seqnum = readInt(helper);
		if (seqnum != saved + 1) {config = 0; seekFile(0,given);}
	} else {
		seqnum = seqnum + 1;
		writeInt(seqnum,helper);
	}
	unlkFile(0,1,control);
	append = sizeof(int);
	if (pollFile(helper)) {
		stage = Rreq; return;}
	stage = Rrsp;
}

void rreq(struct Thread *thread)
{
	readFile(&cmd,helper);
	if (cmdact == EndThd && cmdloc == identifier) {
		stage = Done; return;}
	if (cmdact == EndThd && pollFile(helper)) {
		stage = Rreq; return;}
	if (cmdact == EndThd) {
		stage = Rrsp; return;}
	for (int i = 0; i < cmdnum; i++) total += cmdsiz[i];
	if (config >= cmdloc + total) {
		stage = Wreq; return;}
	stage = Rrsp;
}

void wreq(struct Thread *thread)
{
	writeFile(&cmd,anonym);
	total = 0;
	if (pollFile(helper)) {
		stage = Rreq; return;}
	stage = Rrsp;
}

void rrspf(char *ptr, int siz, void *arg);
void rrsp(struct Thread *thread)
{
	rdlkwFile(config,BUFSIZE,given);
	seekFile(config,given);
	readStr(rrspf,thread,given);
}
void rrspf(char *ptr, int trm, void *arg)
{
	struct Thread *thread = arg;
	int siz = strlen(ptr)+trm;
	unlkFile(config,BUFSIZE,given);
	if (siz == 0 && thdnum == 0) {
		stage = Wlck; return;}
	if (siz == 0) {
		stage = Wrsp; return;}
	strcpy(thdbuf,ptr);
	pointer[thdnum] = thdbuf;
	size[thdnum] = siz;
	thdnum++; thdbuf += siz; config += siz;
	if (thdnum >= CMDSIZE) {
		stage = Wrsp; return;}
}

void wrsp(struct Thread *thread)
{
	rspact = ThdCmd;
	rspidx = thdidx;
	rsploc = thdloc;
	rspnum = thdnum;
	rspsiz = size;
	rspptr = pointer;
	writeFile(&rsp,anonym);
	thdnum = 0; thdbuf = buffer; thdloc = config;
	if (total > 0 && config >= cmdloc + total) {
		stage = Wreq; return;}
	if (total == 0 && pollFile(helper)) {
		stage = Rreq; return;}
	stage = Rrsp;
}

void wlck(struct Thread *thread)
{
	if (wrlkFile(append,INFINITE,helper) == 0) {
		stage = Rlck; return;}
	if (pollFile(helper)) {
		unlkFile(append,INFINITE,helper);
		stage = Rlck; return;}
	readFile(&cmd,named);
	if (cmdnum == 0 && cmdloc == identifier) {
		stage = Done; return;}
	off_t loc = cmdloc;
	seekFile(loc,given);
	for (int i = 0; i < cmdnum; i++) {
		wrlkwFile(loc,cmdsiz[i],given);
		int trm = (cmdsiz[i]>strlen(cmdptr[i]));
		writeStr(cmdptr[i],trm,given);
		unlkFile(loc,cmdsiz[i],given);
		loc += cmdsiz[i]+trm;}
	writeFile(&cmd,helper);
	unlkFile(append,INFINITE,helper);
	append += sizeFile(&cmd);
	stage = Send;
}

void rlck(struct Thread *thread)
{
	rdlkwFile(append,1,helper);
	unlkFile(append,1,helper);
	if (!pollFile(helper)) {
		stage = Wlck; return;}
	readFile(&cmd,helper);
	append += sizeFile(&cmd);
	if (cmdnum == 0 && cmdloc == identifier) stage = Done;
	else if (cmdnum == 0) {
		stage = Wlck; return;}
	stage = Send;
}

void send(struct Thread *thread)
{
	writeFile(&cmd,anonym);
	if (append >= FILESIZE) {
		stage = Move; return;}
	stage = Wlck;
}

void jump(struct Thread *thread)
{
	rspact = ThdEnd;
	rspidx = thdidx;
	rspnum = 0;
	writeJump(0,anonym);
	writeFile(&rsp,anonym);
}
