#include "plane.h"
#include "face.h"
#include "metic.h"
#include "type.h"
#include "share.h"
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/errno.h>

struct Kernel {
	int valid; // optimized
	struct Matrix compose; // optimization
	struct Matrix maintain; // change to points
	struct Matrix written; // portion written
	struct Matrix towrite; // portion to write
	struct Vector fixed; // fixed point
	struct Vector start; // start axis
	struct Vector current; // current axis
	float angle; // roller delta
};
struct Kernel subject = {0};
struct Kernel *object = {0};
struct Kernel facet = {0};
struct Indicate *indicate = {0};
struct Argument *argument = {0};
struct Machine *machine = {0};
float configure[Configures] = {0};
int internal = 0;
int external = 0;
const char *input = 0;
const char *output = 0;
int goon = 0;
uftype callDma = 0;
vftype callWake = 0;
rftype callInfo = 0;
wftype callDraw = 0;

void planeAlize(float *dir, const float *vec) // normalize
{
}
void planeCross(float *axe, const float *fix, const float *pic, const float *cur)
{
}
void planeXtate(float *mat, const float *fix, const float *pic, const float *cur, float ang) // rotate
{
}
void planeXlate(float *mat, const float *fix, const float *pic, const float *cur, float ang) // translate
{
}
void planeScale(float *mat, const float *fix, const float *pic, const float *cur, float ang)
{
}
void planeFocal(float *mat, const float *pos, const float *siz)
{
}
void planeMachine(enum Atwill atwill, enum Command command, enum Memory memory)
{
	int current = configure[MachineCurrent];
	int limit = configure[MachineSize];
	struct Machine *mptr = machine;
	struct Indicate *cate = 0;
	struct Argument *ment = 0;
	for (int i = 0; i < limit; i++) {
		struct Machine *temp = machine+i;
		if (temp->idx == current) mptr = temp;
		if (temp->idx == current && temp->atw == atwill) mptr = temp;
		if (temp->idx == current && temp->atw == atwill && temp->cmd == command && temp->mem == memory) mptr = temp;}
	cate = indicate+mptr->ind;
	switch (cate->bet) {
	case (Manip): break; // TODO change indicated matrix
	case (Permute): break; // TODO send indicated pierce point
	case (Complete): break; // TODO send indicated memory
	case (Debug): break; // TODO emulate user action
	default: break;}
	ment = argument+mptr->arg;
	callDraw(ment->sha,ment->srt,ment->stp);
	configure[MachineCurrent] = mptr->nxt;
}
void *planeThread(void *arg)
{
	while (goon) {
	struct Client client = {0};
	int sub = pselectAny(0,1<<external);
	if (sub != external) ERROR(exitErr,0);
	readClient(&client,external);
	writeClient(&client,internal);
	callWake();}
	return 0;
}
void planeInit(vftype init, vftype run, uftype dma, vftype wake, rftype info, wftype draw)
{
	pthread_t pthread;
	init(); // this calls planeArgument
	callDma = dma;
	callWake = wake;
	callInfo = info;
	callDraw = draw;
	internal = openPipe();
	external = pipeInit(input,output);
	goon = 1;
	if (pthread_create(&pthread,0,planeThread,0) != 0) ERROR(exitErr,0);
	run();
	goon = 0;
	if (pthread_join(pthread,0) != 0) ERROR(exitErr,0);
}
void planeArgument(const char *str)
{
	if (!input) input = str;
	else if (!output) output = str;
	else ERROR(exitErr,0);
}
float planeConfig(enum Configure cfg)
{
	return configure[cfg];
}
void planeWake(enum Query hint)
{
	switch (hint) {
	case (WakeCall): {
	struct Client client = {0};
	int sub = pselectAny(0,1<<internal);
	if (sub != internal) ERROR(exitErr,0);
	readClient(&client,internal);
	switch (client.cmd) {
	case (SetCmd): {
	switch (client.mem) {
	case (Trianglez):
	case (Numericz):
	case (Vertexz): {
	callDma(&client);
	planeMachine(Atpipe,client.cmd,client.mem);
	break;}
	default: break;}
	break;}
	default: break;}
	break;}
	default: break;}
}
void planeReady(struct Pierce *pierce, int size)
{
}
