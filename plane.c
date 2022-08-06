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
#include <string.h>

struct Kernel {
	int valid; // optimized
	struct Matrix compose; // optimization
	struct Matrix maintain; // change to points
	struct Matrix written; // portion written
	struct Matrix towrite; // portion to write
	struct Vector fixed; // fixed point
	float angle; // cumulative angle
};
struct Kernel subject = {0};
struct Kernel *object = {0};
struct Kernel facet = {0};
struct Indicate *indicate = {0};
struct Argument *argument = {0};
struct Machine *machine = {0};
float configure[Configures] = {0};
char collect[BUFSIZE] = {0};
float angle = 0;
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
void planeCross(float *axe, const float *fix, const float *pic)
{
}
void planeXtate(float *mat, const float *fix, const float *pic, float ang) // rotate
{
}
void planeXlate(float *mat, const float *fix, const float *pic, float ang) // translate
{
}
void planeScale(float *mat, const float *fix, const float *pic, float ang)
{
}
void planeFocal(float *mat, const float *pos, const float *siz)
{
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
	struct Client client = {0};
	struct Matrix matrix = {0};
	struct Vector vector = {0};
	enum Command command = Commands;
	enum Memory memory = Memorys;
	int state = configure[MachineState]; // TODO use Index and Limit
	int limit = configure[MachineSize]; // TODO use Index and Limit
	struct Machine *mptr = machine;
	switch (hint) {
	case (KeyPress): {
	// TODO accumulate into collect
	break;}
	case (WakeCall): {
	int sub = pselectAny(0,1<<internal);
	if (sub != internal) ERROR(exitErr,0);
	readClient(&client,internal);
	command = client.cmd;
	memory = client.mem;
	switch (command) {
	case (SetCmd): {
	switch (memory) {
	case (Trianglez):
	case (Numericz):
	case (Vertexz): {
	callDma(&client);
	break;}
	default: break;}
	break;}
	default: break;}
	break;}
	default: break;}
	for (int i = 0; i < limit; i++) { // TODO use Index and Limit
	struct Machine *temp = machine+i;
	if (temp->idx == state) mptr = temp;
	if (temp->idx == state && temp->que == hint) mptr = temp;
	if (temp->idx == state && temp->que == hint && temp->cmd == command && temp->mem == memory) mptr = temp;
	if (temp->idx == state && temp->que == hint && temp->cmd == command && temp->mem == memory && strcmp(temp->str, collect) == 0) mptr = temp;}
	if (mptr->ind >= 0 && mptr->ind < configure[IndicateSize]) { // TODO use Index and Limit
	struct Indicate *cate = indicate+mptr->ind;
	switch (cate->bet) {
	case (Manip): {
	switch (cate->sel) {
	case (Universe): {
	switch (cate->xfm) {
	case (Translate): {
	vector.vec[0] = callInfo(CursorLeft); vector.vec[1] = callInfo(CursorBase); subject.angle += callInfo(RollerChange);
	planeXlate(copymat(matrix.mat,subject.compose.mat,4),subject.fixed.vec,vector.vec,subject.angle);
	client.mem = Allmatz; client.siz = 1; client.idx = 0; client.one = &matrix;
	callDma(&client);
	break;}
	default: break;}
	break;}
	default: break;}
	break;}
	case (Gesture): {
	switch (cate->sel) {
	case (Universe): {
	// TODO consult README for what to do before updating subject.fixed
	// TODO search through pierce points to get pierce for subject.fixed
	subject.angle = 0;
	break;}
	default: break;}
	break;}
	case (Permute): break; // TODO send indicated pierce point
	case (Complete): break; // TODO send indicated memory
	case (Debug): break; // TODO emulate user action
	default: break;}}
	if (mptr->arg >= 0 && mptr->arg < configure[ArgumentSize]) { // TODO use Index and Limit
	struct Argument *ment = argument+mptr->arg;
	callDraw(ment->sha,ment->srt,ment->stp);}
	if (mptr->nxt >= 0 && mptr->nxt < configure[MachineSize]) { // TODO use Index and Limit
	configure[MachineState] = mptr->nxt;}
}
void planeReady(struct Pierce *pierce, int size)
{
	// TODO save pierce points for use by planeWake
}
