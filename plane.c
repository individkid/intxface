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
#include <stdint.h>
#include <string.h>
#include <sys/errno.h>

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
VkInstance instance = {0};
VkDebugUtilsMessengerEXT debugMessenger;

struct Vertex {
	int32_t tag[3]; // layout (location=0) in ivec3 tag;
	float plane[3][3]; // layout (location=1) in vec3 plane[3];
	int32_t versor[3]; // layout (location=4) in ivec3 versor;
	float coord[3][2]; // layout (location=5) in vec2 coord[3];
	float color[4][3]; // layout (location=8) in vec4 color[3];
	int32_t texid[3]; // layout (location=11) in ivec3 texid;
	int32_t facid[3]; // layout (location=12) in ivec3 facid;
	int32_t matid; // layout (location=13) in int matid;
};
struct Facet {
	int32_t vtxid[3]; // just in time filtered per tag
};
struct Affine {
	float view[4][4]; // all polytopes at once
	float tope[NUMFILE][4][4]; // individual polytopes
	float face[4][4]; // individual plane
};

int tag = 0; // which triangles are being rendered
int tope = 0; // which tope is being transformed
int plane = 0; // which plane is being transformed
int topes = 0; // how many files/polytopes there are
float basis[3][3][3]; // client copy of foot points
struct Affine affine = {0}; // client copy of transformation
int vertices = 0; // how many points
struct Vertex *vertex = 0; // client copy of shader input
int facets = 0; // how many triangles
struct Facet *facet = 0; // client copy of point triples
enum Matrix matrix = Picture; // transformation collection
enum Click click = Transform; // what mouse click does
enum Move move = Rotate; // what mouse move does
enum Roll roll = Cylinder; // what mouse roller does
enum Type type = Command; // what keyboard does
enum Mode mode = Initial; // changed by left/right click

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
	printf("exiterr (%s) (%d)\n",str,num); fflush(stdout);
	exit(arg);
}

int valid(int argc)
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
	case (Corner): break;
	case (Triangle): break;
	case (Usage): break;
	default: ERROR(exiterr,-1);}
}

void produce()
{
	// send Metric to steer scripts, update other users,
	//  change modes, sculpt topology, report state
}

void displayKey(struct GLFWwindow* ptr, int key, int scancode, int action, int mods)
{
    if (action == 1) printf("GLFW key %d %d %d %d\n",key,scancode,action,mods);
    if (key == 256 && action == 1) {if (esc == 0) esc = 1;}
    else if (key == 257 && action == 1) {if (esc == 1) esc = 2;}
    else if (action == 1) esc = 0;
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

VkBool32 debugCallback(
	VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
	VkDebugUtilsMessageTypeFlagsEXT messageType,
	const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData,
	void* pUserData) {
	printf("validation layer (%s)\n",pCallbackData->pMessage);
	return VK_FALSE;
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

void windowInit(int argc, char **argv)
{
	glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
	const char *name = (argc == 4 ? argv[3] : argv[0]);
	window = glfwCreateWindow(WINWIDE, 600, name, 0, 0);
	glfwSetKeyCallback(window, displayKey);
}

void vulkanInit()
{
	uint32_t extensionPropertyCount = 0;
	if (vkEnumerateInstanceExtensionProperties(0,&extensionPropertyCount,0) != VK_SUCCESS) ERROR(exiterr,-1);
	VkExtensionProperties extensionProperties[extensionPropertyCount];
	if (vkEnumerateInstanceExtensionProperties(0,&extensionPropertyCount,extensionProperties) != VK_SUCCESS) ERROR(exiterr,-1);
	for (int i = 0; i < extensionPropertyCount; i++) printf("extension (%s)\n",extensionProperties[i].extensionName);

	VkApplicationInfo appInfo = {0};
	appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
	appInfo.pApplicationName = "Manipulate";
	appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
	appInfo.pEngineName = "No Engine";
	appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
	appInfo.apiVersion = VK_API_VERSION_1_0;

	VkInstanceCreateInfo createInfo = {0};
	createInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
	createInfo.pApplicationInfo = &appInfo;
	uint32_t glfwExtensionCount = 0;
	const char** glfwExtensions = {0};
	glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);
	uint32_t extensionCount = glfwExtensionCount;
	const char* extensions[extensionCount+1];
	memcpy(extensions,glfwExtensions,sizeof(glfwExtensions[0])*glfwExtensionCount);
	//extensions[extensionCount++] = "VK_EXT_debug_utils";
	createInfo.enabledExtensionCount = extensionCount;
	createInfo.ppEnabledExtensionNames = extensions;
	createInfo.enabledLayerCount = 0;

	if (vkCreateInstance(&createInfo, 0, &instance) != VK_SUCCESS) ERROR(exiterr,-1);

	/*
	PFN_vkCreateDebugUtilsMessengerEXT func = (PFN_vkCreateDebugUtilsMessengerEXT)
		vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
	VkDebugUtilsMessengerCreateInfoEXT debugCreateInfo = {};
	debugCreateInfo.sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
	debugCreateInfo.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
	debugCreateInfo.messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
	debugCreateInfo.pfnUserCallback = debugCallback;
	debugCreateInfo.pUserData = 0;
	if (func == 0) ERROR(exiterr,-1);
	if (func(instance, &debugCreateInfo, 0, &debugMessenger) != VK_SUCCESS) ERROR(exiterr,-1);
	*/

	uint32_t physicalDeviceCount = 0;
	if (vkEnumeratePhysicalDevices(instance,&physicalDeviceCount,0) != VK_SUCCESS) ERROR(exiterr,-1);
	VkPhysicalDevice physicalDevice[physicalDeviceCount];
	if (vkEnumeratePhysicalDevices(instance,&physicalDeviceCount,physicalDevice) != VK_SUCCESS) ERROR(exiterr,-1);
	for (int i = 0; i < physicalDeviceCount; i++) {
	VkPhysicalDeviceProperties physicalDeviceProperties = {0};
	vkGetPhysicalDeviceProperties(physicalDevice[i],&physicalDeviceProperties);
	printf("device (%s) vertex shader attributes (%d)\n",physicalDeviceProperties.deviceName,
		physicalDeviceProperties.limits.maxVertexInputAttributes);}
}

void windowDestroy()
{
	glfwDestroyWindow(window);
}

void vulkanDestroy()
{
	/*
	PFN_vkDestroyDebugUtilsMessengerEXT func = (PFN_vkDestroyDebugUtilsMessengerEXT)
	vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
	if (func == 0) {ERROR(exiterr,-1);} else func(instance, debugMessenger, 0);
	*/

	vkDestroyInstance(instance, 0);
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
	threadInit(argc,argv);
	glfwInit();
	windowInit(argc,argv);
	vulkanInit();

	while (esc < 2 && !glfwWindowShouldClose(window)) {
	if (setjmp(jmpbuf) == 0) {
	while(esc < 2 && !glfwWindowShouldClose(window)) {
	glfwPollEvents();
	produce();
	if (valid(argc))
	process();}}}

	vulkanDestroy();
	windowDestroy();
	glfwTerminate();
	threadDestroy(argc);

	return 0;
}
