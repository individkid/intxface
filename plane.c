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

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#include "type.h"
#include "base.h"
#include "face.h"
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/errno.h>

jmp_buf jmpbuf = {0};
pthread_mutex_t mutex = {0};
pthread_cond_t cond = {0};
int sub = 0;
int hub = 0;
int zub = 0;

struct Vertex {
	int vtxid;
	int tag[3];
	float plane[3][3];
	int versor[3];
	float coord[3][2];
	float color[3][3];
	int texid[3];
	int facid[3];
	int matid;
};
struct Facet {
	int facid;
	int vtxid[3];
};
struct Affine {
	float view[4][4];
	float tope[NUMFILE][4][4];
	float face[4][4];
};

int tope = 0;
int topes = 0;
struct Affine affine = {0};
int vertices = 0;
struct Vertex *vertex = 0;
int facsts = 0;
struct Facet *facets = 0;
enum Matrix matrix = Picture;
enum Click click = Refine;
enum Move move = Rotate;
enum Roll roll = Cylinder;
enum Type type = Command;
enum Mode mode = Initial;

struct Affine saved = {0};
float fixed[3] = {0};
float moved[2] = {0};
float rolled = {0};

void huberr(const char *str, int num, int arg)
{
	longjmp(jmpbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
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

int main(int argc, char **argv)
{
	pthread_t pthread = {0};
	if (argc == 4) {sub = -1;
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((zub = openPipe()) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub); bothJump(huberr,zub);
	if (pthread_mutex_init(&mutex,0) != 0) ERROR(exiterr,-1);
	if (pthread_cond_init(&cond,0) != 0) ERROR(exiterr,-1);
	if (pthread_create(&pthread,0,thread,0) != 0) ERROR(exiterr,-1);}

	glfwInit();
	glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
	GLFWwindow* window = glfwCreateWindow(800, 600, "Vulkan window", 0, 0);
	uint32_t extensionCount = 0;
	vkEnumerateInstanceExtensionProperties(0, &extensionCount, 0);
	printf("%d extensions supported\n",extensionCount); fflush(stdout);

	int vld = 0;
	struct Client client = {0};
	while (!glfwWindowShouldClose(window)) {
	if (setjmp(jmpbuf) == 0) {
	while(!glfwWindowShouldClose(window)) {
	glfwPollEvents();
	// send Metric to steer scripts, update other users,
	//  change modes, sculpt topology, report state
	if (argc == 4) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (sub >= 0) {readClient(&client,sub); sub = -1; vld = 1;}
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}
	if (vld) {vld = 0; switch (client.mem) {
	case (Buffer): break;
	case (Shader): break;
	default: ERROR(exiterr,-1);}}}}}

	glfwDestroyWindow(window);
	glfwTerminate();

	if (argc == 4) {writeInt(1,zub);
	if (pthread_join(pthread,0) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_destroy(&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_cond_destroy(&cond) != 0) ERROR(exiterr,-1);}
	return 0;
}
