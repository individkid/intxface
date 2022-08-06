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
struct Kernel element = {0};
struct Indicate *indicate = {0};
struct Argument *argument = {0};
struct Machine *machine = {0};
float configure[Configures] = {0};
char collect[BUFSIZE] = {0};
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
typedef void (*planeXform)(float *mat, const float *fix, const float *pic, float ang);
void planeXtate(float *mat, const float *fix, const float *pic, float ang) // rotate
{
}
void planeXlate(float *mat, const float *fix, const float *pic, float ang) // translate
{
}
void planeScale(float *mat, const float *fix, const float *pic, float ang)
{
}
void planeFocal(float *mat, const float *fix, const float *pic, float ang)
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
	struct Vector fixed = {0};
	float angle = 0;
	struct Kernel *kernel = 0;
	planeXform transform = 0;
	enum Command command = Commands;
	enum Memory memory = Memorys;
	int state = configure[MachineState];
	struct Machine *mptr = machine;
	switch (hint) {
		case (DrawDone): break;
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
				case (Trianglez): case (Numericz): case (Vertexz): {
				callDma(&client);
				break;}
				default: break;}
			break;}
			case (GetCmd): break;
			default: break;}
		break;}
		case (ButtonRight): case (ButtonLeft): break;
		case (RollerChange): break;
		case (CursorLeft): case (CursorBase): break;
		case (PictureLeft): case (PictureBase): case (PictureWide): case (PictureHigh): break;
		default: break;}
	for (int i = configure[MachineIndex]; i < configure[MachineLimit]; i++) {
		struct Machine *temp = machine+i%(int)configure[MachineSize];
		if (temp->idx == state) mptr = temp;
		if (temp->idx == state && (temp->que & (1<<hint))) mptr = temp;
		if (temp->idx == state && (temp->que & (1<<hint)) && temp->cmd == command && temp->mem == memory) mptr = temp;
		if (temp->idx == state && (temp->que & (1<<hint)) && temp->cmd == Commands && strcmp(temp->str, collect) == 0) mptr = temp;}
	if (mptr->ind >= configure[IndicateIndex] && mptr->ind < configure[IndicateLimit]) {
		struct Indicate *cate = indicate+mptr->ind%(int)configure[IndicateSize];
		switch (cate->bet) {
			case (Manip): {
			switch (cate->sel) {
				case (Subject): memory = Allmatz; client.all = &matrix; client.idx = 0; kernel = &subject; break;
				case (Object): memory = Fewmatz; client.few = &matrix; client.idx = (int)configure[UniformIndex]; kernel = object+client.idx%(int)configure[ObjectSize]; break;
				case (Element): memory = Onematz; client.one = &matrix; client.idx = 0; kernel = &element; break;
				default: break;}
			switch (cate->xfm) {
				case (Translate): transform = planeXlate; break;
				case (Rotate): transform = planeXtate; break;
				case (Scale): transform = planeScale; break;
				case (Zoom): transform = planeFocal; break; // TODO add normalize step to shaders
				default: break;}
			vector.vec[0] = callInfo(CursorLeft); vector.vec[1] = callInfo(CursorBase); angle = kernel->angle += callInfo(RollerChange);
			copymat(matrix.mat,kernel->compose.mat,4); copyvec(fixed.vec,kernel->fixed.vec,4);
			transform(matrix.mat,fixed.vec,vector.vec,angle);
			client.mem = memory; client.siz = 1;
			callDma(&client);
			break;}
			case (Gesture): {
			switch (cate->sel) {
				case (Subject): {
				// TODO consult README for what to do before updating subject.fixed
				// TODO search through pierce points to get pierce for subject.fixed
				subject.angle = 0;
				break;}
				case (Object): break;
				case (Element): break;
				default: break;}
			break;}
			case (Permute): break; // TODO send indicated pierce point
			case (Complete): break; // TODO send indicated memory
			case (Debug): break; // TODO emulate user action
			default: break;}}
	if (mptr->arg >= configure[ArgumentIndex] && mptr->arg < configure[ArgumentLimit]) {
		struct Argument *ment = argument+mptr->arg%(int)configure[ArgumentSize];
		callDraw(ment->sha,ment->srt,ment->stp);}
	if (mptr->nxt >= configure[MachineIndex] && mptr->nxt < configure[MachineLimit]) {
		configure[MachineState] = mptr->nxt;}
}
void planeReady(struct Pierce *pierce, int size)
{
	// TODO save pierce points for use by planeWake
}
