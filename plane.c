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
struct Client client = {0};
pthread_t pthread = {0};
GLFWwindow* window = 0;
int vertexBufferChanged = 0;
int elementBufferChanged = 0;
// TODO uniformBufferChanged;
enum API api = 0;

int tags = 0; // how many tags to render
int topes = 0; // how many files/polytopes there are
float basis[3][3][3] = {0}; // client copy of foot points
struct Affine affine = {0}; // client copy of transformation
int vertices = 0; // how many points
int verticez = 0; // how much memory allocated
struct Vertex *vertex = 0; // client copy of shader input
int facets = 0; // how many triangles
int facetz = 0; // how much memory allocated
struct Facet *facet = 0; // client copy of point triples
struct User user = {0};

int tag = 0; // which triangles are being rendered
int tope = 0; // which tope is being transformed
int plane = 0; // which plane is being transformed
// TODO feather arrow and feedback
struct Affine saved = {0}; // from when sent to file
float fixed[3] = {0}; // from when pierce point clicked
float moved[2] = {0}; // from when mouse moved
float rolled = {0}; // from when roller adjusted

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
	int vld = 0;
	if (argc == 4) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (sub >= 0) {readClient(&client,sub); sub = -1; vld = 1;}
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}
	return vld;
}

void process()
{
	switch (client.mem) {
	case (Uniform): break;
	case (Corner):
	while (client.idx >= verticez) {
	verticez = (verticez ? verticez*2 : 1);
	if ((vertex = realloc(vertex,verticez)) == 0) ERROR(exiterr,-1);}
	memcpy(&vertex[client.idx],&client.vtx,sizeof(client.vtx));
	vertexBufferChanged = 1;
	break;
	case (Triangle): break;
	case (Usage): break;
	default: ERROR(exiterr,-1);}
}

void produce()
{
	// send Metric to steer scripts, update other users,
	//  change modes, sculpt topology, report state
}

void handle()
{
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

void threadDestroy(int argc)
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
	handle(); // apply arguments from user input callbacks
	if (callread(argc)) process(); // from other processes
	switch (api) { // redraw changed buffers uniforms
	case (None): break;
	case (Metal): metalDraw(); break;
	case (Vulkan): vulkanDraw(); break;
	case (Opengl):  openglDraw(); break;}
	produce(); // send changed metrics with feedback info
	vertexBufferChanged = elementBufferChanged = 0;}}}

	switch (api) {
	case (None): break;
	case (Metal): metalDestroy(); break;
	case (Vulkan): vulkanDestroy(); break;
	case (Opengl):  openglDestroy(); break;}
	windowDestroy();
	threadDestroy(argc);

	return 0;
}
