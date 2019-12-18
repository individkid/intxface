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

enum API {None, Metal, Vulkan, Opengl};

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
struct Client *saved0[Memorys] = {0};
struct Client *saved1[Memorys] = {0};
enum API api = 0;

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
	// TODO get from user input inject
	int vld = 0;
	if (argc == 4) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (sub >= 0) {readClient(client,sub); sub = -1; vld = 1;}
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}
	return vld;
}

void clientCompose(struct Client *res, struct Client *one, struct Client *oth)
{
}

void clientDelta(struct Client *res, struct Client *one, struct Client *oth)
{
}

void process()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Check): break;
	case (Rdma): break;
	case (Rmw0): {
	struct Client *compose = 0; allocClient(&compose,1);
	clientCompose(compose,client,saved0[client->mem]);
	allocClient(&state[client->mem],0); state[client->mem] = compose; break;}
	case (Rmw1): {
	struct Client *delta = 0; allocClient(&delta,1);
	clientDelta(delta,state[client->mem],saved1[client->mem]);
	allocClient(&saved1[client->mem],0); saved1[client->mem] = client;
	struct Client *compose = 0; allocClient(&compose,1);
	clientCompose(compose,delta,client); allocClient(&delta,0);
	allocClient(&state[client->mem],0); state[client->mem] = compose; break;}
	case (Copy): {
	allocClient(&state[client->mem],0); state[client->mem] = client; break;}
	case (Save0): {
	allocClient(&saved0[client->mem],0); saved0[client->mem] = client; break;}
	case (Save1): {
	allocClient(&saved1[client->mem],0); saved1[client->mem] = client; break;}
	case (Dma): break;
	case (Report): break;
	case (Render): break;
	default: ERROR(exiterr,-1);}
}

void produce()
{
	// send Metric to steer scripts, update other users,
	//  change modes, sculpt topology, report state
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
	printf("uint32_t(%d) int(%d) GL_INT(%d)\n",(int)sizeof(uint32_t),(int)sizeof(int),(int)sizeof(GL_INT));

	threadInit(argc,argv);
	windowInit(argc,argv);
	if (metalInit()) api = Metal;
	else if (vulkanInit()) api = Vulkan;
	else if (openglInit()) api = Opengl;
	else api = None;

	while (esc < 2 && !glfwWindowShouldClose(window)) {
	if (setjmp(jmpbuf) == 0) {
	while(esc < 2 && !glfwWindowShouldClose(window)) {
	glfwWaitEvents();
	if (callread(argc)) { // from other processes
	switch (api) {
	case (None): break;
	case (Metal): if (!metalCheck()) continue; break;
	case (Vulkan): if (!vulkanCheck()) continue; break;
	case (Opengl):  if (!openglCheck()) continue; break;}
	process();
	switch (api) { // redraw changed buffers uniforms
	case (None): break;
	case (Metal): metalDraw(); break;
	case (Vulkan): vulkanDraw(); break;
	case (Opengl):  openglDraw(); break;}
	produce();}}}} // send changed metrics with feedback info

	switch (api) {
	case (None): break;
	case (Metal): metalDone(); break;
	case (Vulkan): vulkanDone(); break;
	case (Opengl):  openglDone(); break;}
	windowDone();
	threadDone(argc);

	return 0;
}
