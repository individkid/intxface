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
struct Matrix **planeMatrix(struct Client *client)
{
	switch ((int)configure[MachineSelect]) {
		case (0): return &client->all;
		case (1): return &client->few;
		case (2): return &client->one;
		defalut: break;}
	return 0;
}
int planeIndex()
{
	switch ((int)configure[MachineSelect]) {
		case (0): return 0;
		case (1): return (int)configure[UniformIndex];
		case (2): return 0;
		default: break;}
	return 0;
}
enum Memory planeMemory()
{
	switch ((int)configure[MachineSelect]) {
		case (0): return Allmatz;
		case (1): return Fewmatz;
		case (2): return Onematz;
		defalut: break;}
	return Memorys;
}
struct Kernel *planeKernel()
{
	switch ((int)configure[MachineSelect]) {
		case (0): return &subject;
		case (1): return object+planeIndex();
		case (2): return &element;
		default: break;}
	return 0;
}
planeXform planeFunc()
{
	switch ((int)configure[MachineXform]) {
		case (0): return planeXlate;
		case (1): return planeXtate;
		case (2): return planeScale;
		case (3): return planeFocal; // TODO add normalize step to shaders
		default: break;}
	return 0;
}
enum Shader planeShader()
{
	switch ((int)configure[MachineShader]) {
		case (0): return Dipoint;
		case (1): return Diplane;
		case (2): return Adpoint;
		case (3): return Adplane;
		case (4): return Copoint;
		case (5): return Coplane;
		default: break;}
	return Shaders;
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
void planeWake(enum Configure hint)
{
	struct Matrix matrix = {0};
	struct Vector vector = {0};
	struct Client client = {0};
	char collect[BUFSIZE] = {0};
	configure[MachineLine] = configure[MachineIndex];
	configure[MachineHint] = hint;
	while (configure[MachineLine] >= configure[MachineIndex] && configure[MachineLine] < configure[MachineLimit]) {
		struct Machine *mptr = machine+(int)configure[MachineLine]%(int)configure[MachineSize];
		int next = (int)configure[MachineLine]+1;
		switch (mptr->xfr) {
			case (Read): readClient(&client,internal); break; // read internal pipe
			case (Write): writeClient(&client,external); break; // write external pipe
			case (Save): { // client, pierce, or query to configure
			switch (mptr->cfg) {
				case (MachineCommand): configure[MachineCommand] = client.cmd; break;
				case (MachineMemory): configure[MachineMemory] = client.mem; break;
				case (MachineSelf): configure[MachineSelf] = client.slf; break;
				case (UniformLeft): configure[UniformLeft] = 0/*TODO pierce*/; break;
				case (UniformBase): configure[UniformBase] = 0/*TODO pierce*/; break;
				case (UniformIndex): configure[UniformIndex] = 0/*TODO pierce*/; break;
				case (WindowLeft): configure[WindowLeft] = callInfo(WindowLeft); break;
				case (WindowBase): configure[WindowBase] = callInfo(WindowBase); break;
				case (WindowWide): configure[WindowWide] = callInfo(WindowWide); break;
				case (WindowHigh): configure[WindowHigh] = callInfo(WindowHigh); break;
				case (CursorLeft): configure[CursorLeft] = callInfo(CursorLeft); break;
				case (CursorBase): configure[CursorBase] = callInfo(CursorBase); break;
				case (CursorAngle): configure[CursorAngle] +=/*accumulate*/ callInfo(CursorAngle); break;
				default: break;}
			break;}
			case (Force): configure[mptr->cfg] = mptr->val; break; // machine to configure
			case (Setup): { // configure to client
			if (client.mem != Configurez) {
				freeClient(&client);
				client.cmd = configure[MachineResponse];
				client.mem = Configurez; client.idx = 0; client.siz = 0;}
			if (mptr->idx >= client.siz) {
				allocConfigure(&client.cfg,mptr->idx+1);
				allocOld(&client.val,mptr->idx+1);
				client.siz = mptr->idx+1;}
			client.cfg[mptr->idx] = mptr->cfg; client.val[mptr->idx] = mptr->val;
			break;}
			case (Manip): { // kernel to client
			struct Vector picture = {0};
			struct Vector corner = {0};
			struct Vector fixed = {0};
			struct Vector cursor = {0};
			float angle = 0;
			client.cmd = configure[MachineResponse];
			client.mem = planeMemory();
			client.idx = planeIndex();
			client.siz = 1; client.slf = 0;
			*planeMatrix(&client) = &matrix;
			picture.vec[0] = configure[WindowLeft]; picture.vec[1] = configure[WindowBase];
			corner.vec[0] = configure[WindowWide]; corner.vec[1] = configure[WindowHigh];
			cursor.vec[0] = configure[CursorLeft]; cursor.vec[1] = configure[CursorBase];
			angle = configure[CursorAngle];
			copymat(matrix.mat,planeKernel()->compose.mat,4);
			copyvec(fixed.vec,planeKernel()->fixed.vec,4);
			planeFunc()(matrix.mat,picture.vec,corner.vec,fixed.vec,cursor.vec,angle);
			break;}
			case (Lead): break; // TODO configure to kernel
			case (Merge): break; // TODO echo to kernel
			case (Follow): break; // TODO other to kernel
			case (Compose): break; // TODO make kernel valid
			case (Give): callDma(&client); break; // dma to gpu
			case (Keep): { // dma to cpu
			switch (client.mem) {
				case (Machinez): break; // TODO wrap on MachineSize
				default: break;}
			break;}
			case (Draw): callDraw(planeShader(),configure[MachineStart],configure[MachineStop]); break; // start shader
			case (Equal): if (configure[mptr->cfg] == mptr->val) next = mptr->idx; break; // jump if equal
			case (Noteq): if (configure[mptr->cfg] != mptr->val) next = mptr->idx; break; // jump not equal
			case (Less): if (configure[mptr->cfg] < mptr->val) next = mptr->idx; break; // jump if less
			case (More): if (configure[mptr->cfg] > mptr->val) next = mptr->idx; break; // jump if more
			case (Goto): next = mptr->idx; break; // jump regardless
			default: break;}
		configure[MachineLine] = next;}
}
void planeReady(struct Pierce *pierce, int size)
{
	// TODO save pierce points for use by planeWake
}
