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
void planeCross(float *axe, const float *fix, const float *cur)
{
}
typedef void (*planeXform)(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang);
void planeXtate(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang) // rotate
{
}
void planeXlate(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang) // translate
{
}
void planeScale(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang)
{
}
void planeFocal(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang)
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
	struct Vector picture = {0};
	struct Vector corner = {0};
	struct Vector vector = {0};
	struct Vector fixed = {0};
	enum Configure array[2] = {0};
	float angle = 0;
	struct Kernel *kernel = 0;
	planeXform transform = 0;
	enum Command command = Commands;
	enum Memory memory = Memorys;
	int state = configure[MachineState];
	int index = configure[MachineIndex];
	while (index >= configure[MachineIndex] && index < configure[MachineLimit]) {
		struct Machine *mptr = machine+index%(int)configure[MachineSize];
		int number = mptr->ind;
		int found = 0;
		if (mptr->idx != state) {index = mptr->nxt; continue;}
		switch (mptr->qua) {
			case (ByQuery): if (mptr->que == hint) found = 1; break;
			case (ByCommand): if (mptr->cmd == command) found = 1; break;
			case (ByMemory): if (mptr->mem == memory) found = 1; break;
			case (ByStr): if (strcmp(mptr->str,collect) == 0) found = 1; break;
			default: break;}
		if (!found) {index++; continue;}
		if (mptr->bef != mptr->idx) {state = mptr->bef; index = configure[MachineIndex]; continue;}
		while (number < configure[IndicateIndex] && number >= configure[IndicateLimit]) {
			struct Indicate *iptr = indicate+number%(int)configure[IndicateSize];
			switch (iptr->bet) {
				case (Read): readClient(&client,internal); command = client.cmd; memory = client.mem; break;
				case (Write): callDma(&client); break;
				case (Send): writeClient(&client,external); break;
				case (Manip): {
				switch (iptr->sel) {
					case (Subject): memory = Allmatz; client.all = &matrix; client.idx = 0; kernel = &subject; break;
					case (Object): memory = Fewmatz; client.few = &matrix; client.idx = (int)configure[UniformIndex]; kernel = object+client.idx%(int)configure[ObjectSize]; break;
					case (Element): memory = Onematz; client.one = &matrix; client.idx = 0; kernel = &element; break;
					default: break;}
				switch (iptr->xfm) {
					case (Translate): transform = planeXlate; break;
					case (Rotate): transform = planeXtate; break;
					case (Scale): transform = planeScale; break;
					case (Zoom): transform = planeFocal; break; // TODO add normalize step to shaders
					default: break;}
				picture.vec[0] = callInfo(PictureLeft); picture.vec[1] = callInfo(PictureBase);
				corner.vec[0] = callInfo(PictureWide); corner.vec[1] = callInfo(PictureHigh);
				vector.vec[0] = callInfo(CursorLeft); vector.vec[1] = callInfo(CursorBase); angle = kernel->angle += callInfo(RollerChange);
				copymat(matrix.mat,kernel->compose.mat,4); copyvec(fixed.vec,kernel->fixed.vec,4);
				transform(matrix.mat,picture.vec,corner.vec,fixed.vec,vector.vec,angle);
				client.mem = memory; client.siz = 1;
				break;}
				case (Gesture): {
				switch (iptr->sel) {
					case (Subject): {
					// TODO consult README for what to do before updating subject.fixed
					// TODO search through pierce points to get pierce for subject.fixed
					subject.angle = 0;
					break;}
					case (Object): break;
					case (Element): break;
					default: break;}
				break;}
				case (Permute): break; // TODO fill in Piercez client
				case (Complete): break; // TODO fill in indicated memory
				case (Debug): break; // TODO emulate user action
				case (Draw): callDraw(iptr->sha,iptr->srt,iptr->stp); break;
				case (Reconfig): {
				vector.vec[0] = configure[array[0] = iptr->cfg] = iptr->val;
				client.cfg = array; client.val = vector.vec; client.idx = 0; client.mem = Configurez; client.siz = 1;
				break;}
				case (Take): {
				switch (iptr->que) {
					case (CursorLeft): case (CursorBase): {
					vector.vec[0] = configure[array[0] = UniformLeft] = callInfo(CursorLeft);
					vector.vec[1] = configure[array[1] = UniformBase] = callInfo(CursorBase);
					client.cfg = array; client.val = vector.vec; client.idx = 0; client.mem = Configurez; client.siz = 2;
					break;}
					default: break;}
				break;}
				default: break;}
			number = iptr->nxt;}
		if (mptr->aft != mptr->idx) {state = mptr->aft; index = configure[MachineIndex]; continue;}}
}
void planeReady(struct Pierce *pierce, int size)
{
	// TODO save pierce points for use by planeWake
}
