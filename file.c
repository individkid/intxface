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
	int total;
	int size;
	int idx;
	int num;
	char *buf;
	off_t start;
	char buffer[CMDSIZE*BUFSIZE];
	int bufsiz[CMDSIZE];
	char *bufptr[CMDSIZE];
	struct File command;
	struct File response;
};

void spokerr(int arg);
void huberr(int arg);
void filerr(int arg);
void exiterr(int arg);
void nonote(int idx);
void construct(char *ptr, int idx, struct Thread **thread);
void destroy(struct File *command, int sub, struct Thread **thread);
void normish(struct File *command, struct Thread **thread);
void normal(struct File *command, int sub);
void finish(struct File *command, struct Thread **thread);
void final(struct Thread **thread);
void error(int sub);
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

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int face = 0;
	struct File command = {0};
	struct Thread *thread[NUMFILE] = {0};
	identifier = getpid(); // TODO add seconds-since-publish
	if ((face = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if (setjmp(errbuf) == 0) {
	bothJump(huberr,face);
	for (int sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(&command,sub);
	int cmd = (sub == face);
	int vld = (command.idx >= 0 && command.idx < NUMFILE);
	int new = (vld && thread[command.idx] == 0);
	int non = (command.num == 0);
	int one = (command.num == 1);
	if (cmd && vld && new && one) construct(*command.ptr,command.idx,thread);
	else if (cmd && vld && new) error(thread[sub]->anonym);
	else if (cmd && vld && non) finish(&command,thread);
	else if (cmd && vld) normish(&command,thread);
	else if (cmd) {final(thread); return 0;}
	else if (non) destroy(&command,face,thread);
	else normal(&command,face);}}
	error(face); return -1;
}

void *file(void *arg)
{
	struct Thread *thread = (struct Thread *)arg;
	if (setjmp(jmpbuf[thread->idx]) == 0)
	while (1) switch (thread->stage) {
	case (Move): move(thread); break;
	case (Rreq): rreq(thread); break;
	case (Wreq): wreq(thread); break;
	case (Rrsp): rrsp(thread); break;
	case (Wrsp): wrsp(thread); break;
	case (Wlck): wlck(thread); break;
	case (Rlck): rlck(thread); break;
	case (Send): send(thread); break;
	case (Done): return 0;
	default: ERROR(exiterr,-1)}
	jump(thread); return 0;
}

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

void construct(char *ptr, int idx, struct Thread **thread)
{
	struct Thread init = {0};
	struct Thread *new = malloc(sizeof(struct Thread));
	if (new == 0) ERROR(huberr,-1)
	*new = init;
	if (thread[idx]) ERROR(exiterr,-1)
	thread[idx] = new;
	create(ptr,idx,new);
}

void destroy(struct File *command, int sub, struct Thread **thread)
{
	int idx = command->idx;
	clean(thread[idx]);
	free(thread[idx]);
	thread[idx] = 0;
	normal(command,sub);
}

void normish(struct File *command, struct Thread **thread)
{
	writeFile(command,thread[command->idx]->named);
}

void normal(struct File *command, int sub)
{
	command->idx = number[sub];
	writeFile(command,sub);
}

void finish(struct File *command, struct Thread **thread)
{
	command->loc = identifier;
	writeFile(command,thread[command->idx]->named);
}

void final(struct Thread **thread)
{
	struct File command = {0};
	command.num = 0;
	for (int i = 0; i < NUMFILE; i++) if (thread[i]) {
	command.idx = i; finish(&command,thread);}
}

void error(int sub)
{
	struct File command = {0};
	command.idx = -1;
	command.num = 0;
	writeFile(&command,sub);
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
#define cfgsiz thread->size
#define appsiz thread->total
#define thdidx thread->idx
#define thdnum thread->num
#define thdbuf thread->buf
#define cfgloc thread->start
#define buffer thread->buffer
#define bufsiz thread->bufsiz
#define bufptr thread->bufptr
#define cmd thread->command
#define rsp thread->response
#define cmdidx thread->command.idx
#define cmdloc thread->command.loc
#define cmdnum thread->command.num
#define cmdsiz thread->command.siz
#define cmdptr thread->command.ptr
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
	bothJump(spokerr,control); readNote(nonote,given);
	append = 0; config = 0; cfgsiz = 0; appsiz = 0; thdnum = 0;
	thdidx = sub; thdbuf = buffer; cfgloc = config; stage = Move;
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
	if (pollFile(helper)) stage = Rreq;
	else stage = Rrsp;
}

void rreq(struct Thread *thread)
{
	readFile(&cmd,helper);
	for (int i = 0; i < cmdnum; i++) cfgsiz += cmdsiz[i];
	if (config >= cmdloc + cfgsiz) stage = Wreq;
	else stage = Rrsp;	
}

void wreq(struct Thread *thread)
{
	writeFile(&cmd,anonym);
	cfgsiz = 0;
	if (pollFile(helper)) stage = Rreq;
	else stage = Rrsp;
}

void rrsp(struct Thread *thread)
{
	rdlkwFile(config,BUFSIZE,given);
	char *ptr = readStr(given);
	unlkFile(config,BUFSIZE,given);
	int siz = strlen(ptr)+checkStr(given);
	if (siz == 0 && thdnum == 0) {stage = Wlck; return;}
	if (siz == 0) {stage = Wrsp; return;}
	strcpy(thdbuf,ptr);
	bufptr[thdnum] = thdbuf;
	bufsiz[thdnum] = siz;
	thdnum++; thdbuf += siz; appsiz += siz; config += siz;
	if (appsiz >= CMDSIZE) stage = Wrsp;
}

void wrsp(struct Thread *thread)
{
	rspidx = thdidx;
	rsploc = cfgloc;
	rspnum = thdnum;
	rspsiz = bufsiz;
	rspptr = bufptr;
	writeFile(&rsp,anonym);
	thdnum = 0; thdbuf = buffer; appsiz = 0; cfgloc = config;
	if (pollFile(helper)) stage = Rreq;
	else stage = Rrsp;
}

void wlck(struct Thread *thread)
{
	if (wrlkFile(append,INFINITE,helper) == 0) {
		stage = Rlck; return;}
	if (pollFile(helper)) {
		unlkFile(append,INFINITE,helper);
		stage = Rlck; return;}
	readFile(&cmd,named);
	off_t loc = cmdloc;
	seekFile(loc,given);
	for (int i = 0; i < cmdnum; i++) {
		wrlkwFile(loc,cmdsiz[i],given);
		writeBuf(cmdptr[i],cmdsiz[i],given);
		unlkFile(loc,cmdsiz[i],given);
		loc += cmdsiz[i];}
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
	stage = Send;
}

void send(struct Thread *thread)
{
	writeFile(&cmd,anonym);
	if (cmdnum == 0 && cmdloc == identifier) {
		stage = Done; return;}
	if (append >= FILESIZE) stage = Move;
	else stage = Wlck;
}

void jump(struct Thread *thread)
{
	writeJump(0,anonym);
	cmdnum = 0;
	writeFile(&cmd,anonym);
}
