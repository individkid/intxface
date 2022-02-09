// share state change

#include "face.h"
#include "type.h"
#include <pthread.h>
#include <dlfcn.h>
#include <time.h>
#include <setjmp.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>

long long identifier = 0;
jmp_buf errbuf = {0};
jmp_buf jmpbuf[NUMFILE] = {0};
int number[NUMOPEN] = {0};
int cmdsize = CMDSIZE;
extern int bufsize;

enum Name {
	Control,
	Helper,
	Named,
	Given,
	Names
};
enum Stage {
	Open,
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
typedef void (*cbtype)(char *,const char*,int);
struct Thread {
	pthread_t thread; // wait after end notice
	enum Stage stage; // goto labels
	char *name; // three leading dots for four names
	int anonym; // config/update from given/helper/named
	int named; // block to append to helper
	int given; // read to eof, random write
	int helper; // poll or block at eof
	int control; // lock for replacing helper
	int seqnum; // first int in helper
	off_t config; // location in given
	off_t append; // location in helper
	int idx; // constant per thread
	off_t loc; // saved from config when num is zero
	int num; // indicates if response is pending
	char *buf; // saved from buffer when num is zero
	int total; // indicates if command is valid
	char buffer[CMDSIZE*BUFSIZE]; // read from given
	int size[CMDSIZE]; // per num delta in buffer
	char *pointer[CMDSIZE]; // per num into buffer
	int opcsiz; // size of rmw buffer
	char opcbuf[CMDSIZE*BUFSIZE]; // rmw buffer
	cbtype opcode[Opcodes]; // how to change given fields
	struct File command; // from helper or named to anonym
	struct File response; // from given to anonym
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

void spokerr(const char *str, int num, int arg)
{
	longjmp(jmpbuf[number[arg]],1);
}

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void filerr(const char *str, int num, int arg)
{
	longjmp(jmpbuf[arg],1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

void nonote(const char *str, int num, int idx)
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

void function(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num != 2) ERROR(exiterr,-1)
	void *lib = 0;
	if ((lib = dlopen(command->ptr[0], RTLD_LAZY)) == 0) ERROR(huberr,-1)
	thread[command->idx]->opcode[command->opc] = dlsym(lib,command->ptr[1]);
	if (thread[command->idx]->opcode[command->opc] == 0) ERROR(huberr,-1)
	if (dlclose(lib) < 0) ERROR(huberr,-1)
}

void configure(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (command->num != 2) ERROR(exiterr,-1)
	int val;
	val = sscanf(command->ptr[0],"%d",&bufsize); if (val != 1) ERROR(huberr,-1)
	val = sscanf(command->ptr[1],"%d",&cmdsize); if (val != 1) ERROR(huberr,-1)
}

void normal(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num <= 0) ERROR(exiterr,-1)
	command->pid = identifier;
	writeFile(command,thread[command->idx]->named);
}

void normish(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub == face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	if (command->num <= 0) ERROR(exiterr,-1)
	if (command->pid != identifier) command->pid = 0;
	writeFile(command,face);
}

void final(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	command->pid = identifier;
	writeFile(command,thread[command->idx]->named);
}

void finish(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub == face) ERROR(exiterr,-1)
	if (!(command->idx >= 0 && command->idx < NUMFILE)) ERROR(exiterr,-1)
	if (thread[command->idx] == 0) ERROR(exiterr,-1)
	clean(thread[command->idx]);
	free(thread[command->idx]);
	thread[command->idx] = 0;
	writeFile(command,face);
}

void error(struct File *command, int sub, int face, struct Thread **thread)
{
	if (sub != face) ERROR(exiterr,-1)
	command->act = EndThd;
	command->pid = identifier;
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
	identifier = ((long long)getpid()<<(sizeof(long long)/2))+(long long)time(0);
	if ((face = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,face);
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	readFile(&command,sub);
	switch (command.act) {
	// initialize idx Thread with file name
	case (NewThd): construct(&command,sub,face,thread); break;
	// set up callback for read modify write
	case (FncThd): function(&command,sub,face,thread); break;
	// change granularity of command data
	case (TstThd): configure(&command,sub,face,thread); break;
	// forward to idx Thread through named pipe
	case (CmdThd): case (CfgThd): normal(&command,sub,face,thread); break;
	// forward from number[sub] Thread to process pipe
	case (ThdCmd): case (ThdCfg): normish(&command,sub,face,thread); break;
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
	// Open: Move:
	// reopen helper
	// from Send, goto Wlck
	// goto Rrsp
	case (Open): case (Move): move(thread); break;
	// Rreq:
	// read helper
	// kill to self, goto Done
	// kill to other, neof helper, goto Rreq
	// kill to other, eof helper, goto Rrsp
	// past command, goto Wreq
	// future command, goto Rrsp
	case (Rreq): rreq(thread); break;
	// Wreq:
	// write anonym
	// neof helper, goto Rreq
	// eof helper, goto Rrsp
	case (Wreq): wreq(thread); break;
	// Rrsp:
	// read given
	// eof given, none read, goto Wlck
	// eof given, some read, goto Wrsp
	// neof given, all read, goto Wrsp
	// neof given, nall read, goto Rrsp
	case (Rrsp): rrsp(thread); break;
	// Wrsp:
	// write anonym
	// command read, past command, goto Wreq
	// command read, future command, goto Rrsp
	// command unread, neof helper, goto Rreq
	// command unread, eof helper, goto Rrsp
	case (Wrsp): wrsp(thread); break;
	// Wlck:
	// helper locked, goto Rlck, wlock helper
	// neof helper, unlock helper, goto Rlck
	// read named
	// kill to self, goto Done
	// write given
	// write helper
	// unlock helper
	// goto Send
	case (Wlck): wlck(thread); break;
	// Rlck:
	// rlock helper
	// unlock helper
	// eof helper, goto Wlck
	// read helper
	// kill to self, goto Done
	// kill to other, goto Wlck
	// goto Send
	case (Rlck): rlck(thread); break;
	// Send:
	// write anonym
	// too big, goto Move
	// more room, goto Wlck
	case (Send): send(thread); break;
	// Done:
	case (Done): return 0;
	default: ERROR(exiterr,-1)}
	// longjmp:
	// kill anonym
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
#define opcsiz thread->opcsiz
#define opcbuf thread->opcbuf
#define opcode thread->opcode
#define cmd thread->command
#define rsp thread->response

void create(char *ptr, int sub, struct Thread *thread)
{
	if ((name = malloc(strlen(ptr)+Names)) == 0) ERROR(huberr,-1)
	for (int i = 0; i < Names-1; i++) name[i] = '.';
	strcpy(name+Names-1,ptr);
	if ((anonym = openPipe()) < 0) ERROR(huberr,-1)
	if ((named = openFifo(name+Named)) < 0) ERROR(huberr,-1)
	if ((helper = openFile(name+Helper)) < 0) ERROR(huberr,-1)
	if ((given = openFile(name+Given)) < 0) ERROR(huberr,-1)
	if ((control = openFile(name+Control)) < 0) ERROR(huberr,-1)
	number[anonym] = thdidx; number[named] = thdidx;
	number[helper] = thdidx; number[given] = thdidx;
	number[control] = thdidx;
	readJump(huberr,anonym); writeJump(huberr,named);
	writeJump(spokerr,anonym); readJump(spokerr,named);
	bothJump(spokerr,helper); bothJump(spokerr,given);
	bothJump(spokerr,control);
	append = 0; config = 0; total = 0; thdnum = 0;
	thdidx = sub; thdbuf = buffer; thdloc = config;
	stage = Open;
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
	// reopen helper
	wrlkwFile(0,1,control);
	if (append >= FILESIZE && unlink(name+Helper) < 0) ERROR(filerr,thdidx)
	moveIdent(openFile(name+Helper),helper);
	if (pollFile(helper)) {
		int saved = seqnum;
		seqnum = readInt(helper);
		if (seqnum != saved + 1) {config = 0; seekFile(0,given);}
	} else {
		seqnum = seqnum + 1;
		writeInt(seqnum,helper);
	}
	unlkFile(0,1,control);
	append = checkFile(helper);
	seekFile(append,helper);
	// from Send, goto Wlck
	if (stage == Move) {
		stage = Wlck; return;}
	// goto Rrsp
	stage = Rrsp;
}

void rreq(struct Thread *thread)
{
	// read helper
	readFile(&cmd,helper);
	// kill to self, goto Done
	if (cmd.act == EndThd && cmd.pid == identifier) {
		stage = Done; return;}
	// kill to other, neof helper, goto Rreq
	if (cmd.act == EndThd && pollFile(helper)) {
		stage = Rreq; return;}
	// kill to other, eof helper, goto Rrsp
	if (cmd.act == EndThd) {
		stage = Rrsp; return;}
	for (int i = 0; i < cmd.num; i++) total += cmd.siz[i];
	// past command, goto Wreq
	if (config >= cmd.loc + total) {
		stage = Wreq; return;}
	// future command, goto Rrsp
	stage = Rrsp;
}

void wreq(struct Thread *thread)
{
	// write anonym
	writeFile(&cmd,anonym);
	total = 0;
	// neof helper, goto Rreq
	if (pollFile(helper)) {
		stage = Rreq; return;}
	// eof helper, goto Rrsp
	stage = Rrsp;
}

void rrspf(const char *ptr, int siz, void *arg);
void rrsp(struct Thread *thread)
{
	// read given
	rdlkwFile(config,bufsize,given);
	seekFile(config,given);
	if (pollFile(given)) readStr(rrspf,thread,given);
	else rrspf("",0,thread);
}
void rrspf(const char *ptr, int trm, void *arg)
{
	struct Thread *thread = arg;
	int siz = strlen(ptr)+trm;
	unlkFile(config,bufsize,given);
	// eof given, none read, goto Wlck
	if (siz == 0 && thdnum == 0) {
		stage = Wlck; return;}
	// eof given, some read, goto Wrsp
	if (siz == 0) {
		stage = Wrsp; return;}
	strcpy(thdbuf,ptr);
	pointer[thdnum] = thdbuf;
	size[thdnum] = siz;
	thdnum++; thdbuf += siz; config += siz;
	// neof given, all read, goto Wrsp
	if (thdnum >= cmdsize) {
		stage = Wrsp; return;}
	// neof given, nall read, goto Rrsp
	stage = Rrsp;
}

void wrsp(struct Thread *thread)
{
	// write anonym
	rsp.act = ThdCfg;
	rsp.idx = thdidx;
	rsp.loc = thdloc;
	rsp.num = thdnum;
	rsp.siz = size;
	rsp.ptr = pointer;
	writeFile(&rsp,anonym);
	thdnum = 0; thdbuf = buffer; thdloc = config;
	// command read, past command, goto Wreq
	if (total > 0 && config >= cmd.loc + total) {
		stage = Wreq; return;}
	// command read, future command, goto Rrsp
	if (total > 0) {
		stage = Rrsp; return;}
	// command unread, neof helper, goto Rreq
	if (pollFile(helper)) {
		stage = Rreq; return;}
	// command unread, eof helper, goto Rrsp
	stage = Rrsp;
}

void wlckf(const char *str, int trm, void *arg)
{
	struct Thread *thread = arg;
	strcpy(opcbuf+opcsiz,str);
	opcsiz += strlen(str)+trm;
}
void wlck(struct Thread *thread)
{
	// helper locked, goto Rlck, wlock helper
	if (wrlkFile(append,INFINITE,helper) == 0) {
		stage = Rlck; return;}
	// neof helper, unlock helper, goto Rlck
	if (pollFile(helper)) {
		unlkFile(append,INFINITE,helper);
		stage = Rlck; return;}
	// read named
	readFile(&cmd,named);
	// kill to self, goto Done
	if (cmd.act == EndThd && cmd.pid == identifier) {
		stage = Done; return;}
	if (cmd.act == CfgThd) cmd.loc = config;
	// write given
	int siz = 0;
	for (int i = 0; i < cmd.num; i++) siz += cmd.siz[i];
	if (opcode[cmd.opc]) {
		opcsiz = 0;
		while (opcsiz < siz) readStr(wlckf,thread,given);
		(*opcode[cmd.opc])(buffer,opcbuf,siz);
	}
	seekFile(cmd.loc,given);
	wrlkwFile(cmd.loc,siz,given);
	for (int i = 0; i < cmd.num; i++) {
		int trm = (cmd.siz[i]>strlen(cmd.ptr[i]));
		writeStr(cmd.ptr[i],trm,given);}
	unlkFile(cmd.loc,siz,given);
	// write helper
	writeFile(&cmd,helper);
	// unlock helper
	unlkFile(append,INFINITE,helper);
	append += sizeFile(&cmd);
	// goto Send
	stage = Send;
}

void rlck(struct Thread *thread)
{
	// rlock helper
	rdlkwFile(append,1,helper);
	// unlock helper
	unlkFile(append,1,helper);
	// eof helper, goto Wlck
	if (!pollFile(helper)) {
		stage = Wlck; return;}
	// read helper
	readFile(&cmd,helper);
	append += sizeFile(&cmd);
	// kill to self, goto Done
	if (cmd.act == EndThd && cmd.pid == identifier) {
		stage = Done; return;}
	// kill to other, goto Wlck
	if (cmd.act == EndThd) {
		stage = Wlck; return;}
	// goto Send
	stage = Send;
}

void send(struct Thread *thread)
{
	// write anonym
	if (cmd.act == CmdThd || cmd.act == CfgThd) cmd.act = ThdCmd;
	writeFile(&cmd,anonym);
	// too big, goto Move
	if (append >= FILESIZE) {
		stage = Move; return;}
	// more room, goto Wlck
	stage = Wlck;
}

void jump(struct Thread *thread)
{
	rsp.act = ThdEnd;
	rsp.idx = thdidx;
	rsp.num = 0;
	writeJump(0,anonym);
	writeFile(&rsp,anonym);
}
