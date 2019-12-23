/*
*    plane.c
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

#define EXTERN
#include "plane.h"
#include <pthread.h>

enum API {None, Metal, Vulkan, Opengl, Model};
int sub = 0;
int hub = 0;
int zub = 0;
int esc = 0;
jmp_buf jmpbuf = {0};
pthread_mutex_t mutex = {0};
pthread_cond_t cond = {0};
pthread_t pthread = {0};
GLFWwindow *window = 0;
struct Client *client = 0;
struct Client *state[Memorys] = {0};
struct Client *saved[Memorys] = {0};
struct Client *inject[NUMJECT] = {0};
int enject = 0;
int deject = 0;
enum API api = 0;
int full = 0;

void huberr(const char *str, int num, int arg)
{
	longjmp(jmpbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	glfwTerminate();
	printf("exiterr (%s) (%d)\n",str,num); fflush(stdout);
	exit(arg);
}

int callread(int argc)
{
	if (enject != deject) {
	allocClient(&client,0);
	client = inject[deject];
	inject[deject] = 0;
	deject = (deject+1)%NUMJECT;}
	int vld = 0;
	if (argc == 4) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (sub >= 0) {readClient(client,sub); sub = -1; vld = 1;}
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}
	return vld;
}

float *clientMat(struct Client *client, int idx)
{
	switch (client->mem) {
	case (Subject): return &state[Subject]->subject[0].val[0][0];
	case (Object): return &state[Object]->object[idx].val[0][0];
	case (Feature): return &state[Feature]->feature[0].val[0][0];
	default: ERROR(exiterr,-1);}
	return 0;
}

void clientRmw0()
{
	// state[idx] = client[0]*saved[idx]
	float *save = clientMat(saved[client->mem],client->idx);
	float *give = clientMat(client,0);
	copymat(save,timesmat(save,give,4),4);
}

void clientRmw1()
{
	// A = B*C
	// A' = B*C'
	// B = A/C
	// A' = (A/C)*C'
	// saved[idx] = 1/saved[idx]
	// state[idx] = state[idx]*saved[idx]
	// saved[idx] = client[0]
	// state[idx] = state[idx]*client[0]
	float *save = clientMat(saved[client->mem],client->idx); invmat(save,4);
	float *stat = clientMat(state[client->mem],client->idx); timesmat(stat,save,4);
	float *give = clientMat(client,0); copymat(save,give,4); timesmat(stat,give,4);
}

#define INDEXED(ENUM,FIELD) \
	if (client->mem == ENUM && ptr[client->mem] && client->siz < ptr[client->mem]->siz) \
	{memcpy(&ptr[ENUM]->FIELD[client->idx],client->FIELD,client->siz*sizeof(*client->FIELD)); return;}
void clientCopy(struct Client **ptr)
{
	INDEXED(Basis,basis);
	INDEXED(Object,object);
	INDEXED(Cloud,cloud);
	allocClient(&ptr[client->mem],0); ptr[client->mem] = client;
}

void process()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): clientRmw0(); break;
	case (Rmw1): clientRmw1(); break;
	case (Copy): clientCopy(state); break;
	case (Save): clientCopy(saved); break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): break;
	case (Post): break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void produce()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): break;
	case (Post): {
	struct Pierce *ptr = 0;
	for (int i = 0; i < state[Sightline]->siz; i++)
	if (ptr == 0 || state[Sightline]->sightline[i].order < ptr->order)
	ptr = &state[Sightline]->sightline[i];
	if (ptr) state[Face]->face = ptr->ident;
	else state[Face]->face = state[Triangle]->siz;
	break;}
	case (Port): {
	struct Metric metric = {0};
	metric.src = Plane;
	metric.plane = state[client->mem];
	writeMetric(&metric,hub);
	break;}
	default: ERROR(exiterr,-1);}
}

void *thread(void *arg)
{
	int tmp = 0;
	int gon = 1;
	while (gon) {
	for (tmp = waitAny(); tmp >= 0 && gon; tmp = waitAny()) {
	if (tmp == zub) gon = 0; else {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	sub = tmp; glfwPostEmptyEvent();
	if (pthread_cond_wait(&cond,&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}}}
	return 0;
}

void threadInit(int argc, char **argv)
{
	if (argc == 4) {sub = -1;
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((zub = openPipe()) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub); bothJump(huberr,zub);
	if (pthread_mutex_init(&mutex,0) != 0) ERROR(exiterr,-1);
	if (pthread_cond_init(&cond,0) != 0) ERROR(exiterr,-1);
	if (pthread_create(&pthread,0,thread,0) != 0) ERROR(exiterr,-1);}
}

void threadDone(int argc)
{
	if (argc == 4) {writeInt(1,zub);
	if (pthread_join(pthread,0) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_destroy(&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_cond_destroy(&cond) != 0) ERROR(exiterr,-1);}
}

int main(int argc, char **argv)
{
	printf("uint32_t(%d) int(%d) GL_INT(%d) float(%d) GL_FLOAT(%d)\n",(int)sizeof(uint32_t),(int)sizeof(int),(int)sizeof(GL_INT),(int)sizeof(float),(int)sizeof(GL_FLOAT));

	threadInit(argc,argv);
	windowInit(argc,argv);
	if (metalInit()) api = Metal;
	else if (vulkanInit()) api = Vulkan;
	else if (openglInit()) api = Opengl;
	else if (modelInit()) api = Model;
	else api = None;

	while (esc < 2 && !glfwWindowShouldClose(window)) {
	if (setjmp(jmpbuf) == 0) {
	while(esc < 2 && !glfwWindowShouldClose(window)) {
	switch (api) {
	case (None): break;
	case (Metal): full = metalFull(); break;
	case (Vulkan): full = vulkanFull(); break;
	case (Opengl):  full = openglFull(); break;
	case (Model):  full = modelFull(); break;}
	if (full) {
	glfwWaitEventsTimeout(1000.0*NANO2SEC);
	continue;}
	if (callread(argc)) {
	process();
	switch (api) { // redraw changed buffers uniforms
	case (None): break;
	case (Metal): metalDraw(); break;
	case (Vulkan): vulkanDraw(); break;
	case (Opengl):  openglDraw(); break;
	case (Model):  modelDraw(); break;}
	produce();
	glfwPollEvents();
	continue;}
	glfwWaitEvents();}}} // send changed metrics with feedback info

	switch (api) {
	case (None): break;
	case (Metal): metalDone(); break;
	case (Vulkan): vulkanDone(); break;
	case (Opengl):  openglDone(); break;
	case (Model):  modelDone(); break;}
	windowDone();
	threadDone(argc);

	return 0;
}
