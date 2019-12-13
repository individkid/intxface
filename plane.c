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

jmp_buf errbuf = {0};

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

void *clientmem(void *arg)
{
	char **argv = arg;
	int hub = 0;
	int sub = 0;
	struct Client *client = (struct Client*)malloc(sizeof(struct Client));
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub);
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	readClient(client,sub);
	switch (client->mem) {
	case (Buffer): break;
	case (Shader): break;
	default: ERROR(exiterr,-1);}
	client = (struct Client*)malloc(sizeof(struct Client));}}}
	return 0;
}

int main(int argc, char **argv)
{
	pthread_t thread = {0};
	if (argc == 4) {
	if (pthread_create(&thread,0,clientmem,argv) != 0) ERROR(exiterr,-1);}

	glfwInit();

	glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
	GLFWwindow* window = glfwCreateWindow(800, 600, "Vulkan window", 0, 0);

	uint32_t extensionCount = 0;
	vkEnumerateInstanceExtensionProperties(0, &extensionCount, 0);

	printf("%d extensions supported",extensionCount);

	while(!glfwWindowShouldClose(window)) {
	glfwPollEvents();}

	glfwDestroyWindow(window);

	glfwTerminate();

	if (argc == 4) {
	if (pthread_join(thread,0) != 0) ERROR(exiterr,-1);}

	return 0;
}
