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

#include <GL/glew.h>
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

enum Shader {Render,Present,Pierce,Cloud,Shaders};

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
GLuint vertexArray = 0;
GLuint programID = 0;
GLuint vertexBuffer = 0;
GLuint elementBuffer = 0;
int vertexBufferChanged = 0;
int elementBufferChanged = 0;
int vulkan = 0;
VkInstance instance = {0};
VkDebugUtilsMessengerEXT debug = {0};
VkSurfaceKHR surface = {0};
VkPhysicalDevice physical = {0};
VkDevice logical = {0};
int valid[Shaders] = {0};
uint32_t family[Shaders] = {0};
VkQueue queue[Shaders] = {0};
VkSwapchainKHR swap = {0};

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
	uint32_t vtxid[3]; // just in time filtered per tag
};
struct Affine {
	float view[4][4]; // all polytopes at once
	float tope[NUMFILE][4][4]; // individual polytopes
	float face[4][4]; // individual plane
};

int tags = 0; // how many tags to render
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

int tag = 0; // which triangles are being rendered
int tope = 0; // which tope is being transformed
int plane = 0; // which plane is being transformed
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

VkBool32 debugCallback(
	VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
	VkDebugUtilsMessageTypeFlagsEXT messageType,
	const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData,
	void* pUserData) {
	printf("validation layer (%s)\n",pCallbackData->pMessage);
	return VK_FALSE;
}

void vulkanInit(int argc, char **argv)
{
	glfwInit();
	glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
	const char *name = (argc == 4 ? argv[3] : argv[0]);
	if ((window = glfwCreateWindow(WINWIDE, WINHIGH, name, 0, 0)) == 0) ERROR(exiterr,-1);
	glfwSetKeyCallback(window, displayKey);

	uint32_t extensionPropertyCount = 0;
	if (vkEnumerateInstanceExtensionProperties(0,&extensionPropertyCount,0) != VK_SUCCESS) ERROR(exiterr,-1);
	VkExtensionProperties extensionProperties[extensionPropertyCount];
	if (vkEnumerateInstanceExtensionProperties(0,&extensionPropertyCount,extensionProperties) != VK_SUCCESS) ERROR(exiterr,-1);
	uint32_t extensionCount = 0;
	const char* extensions[extensionPropertyCount];
	for (int i = 0; i < extensionPropertyCount; i++) extensions[extensionCount++] = extensionProperties[i].extensionName;
	for (int i = 0; i < extensionCount; i++) printf("extension (%s)\n",extensions[i]);

	// create instance
	VkApplicationInfo appInfo = {0};
	appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
	appInfo.pApplicationName = "Manipulate";
	appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
	appInfo.pEngineName = "No Engine";
	appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
	appInfo.apiVersion = VK_API_VERSION_1_0;
	VkInstanceCreateInfo instanceCreateInfo = {0};
	instanceCreateInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
	instanceCreateInfo.pApplicationInfo = &appInfo;
	/*
	uint32_t glfwExtensionCount = 0;
	const char** glfwExtensions = {0};
	glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);
	memcpy(&extensions[extensionCount],glfwExtensions,sizeof(glfwExtensions[0])*glfwExtensionCount);
	extensionCount += glfwExtensionCount;
	//extensions[extensionCount++] = "VK_EXT_debug_utils";
	extensions[extensionCount++] = "VK_KHR_surface";
	extensions[extensionCount++] = "VK_KHR_swapchain";
	extensions[extensionCount++] = "VK_MVK_macos_surface";
	extensions[extensionCount++] = "VK_MVK_moltenvk";
	instanceCreateInfo.enabledExtensionCount = extensionCount;
	instanceCreateInfo.ppEnabledExtensionNames = extensions;
	*/
	instanceCreateInfo.enabledExtensionCount = extensionCount;
	instanceCreateInfo.ppEnabledExtensionNames = extensions;
	instanceCreateInfo.enabledLayerCount = 0;
	if (vkCreateInstance(&instanceCreateInfo, 0, &instance) != VK_SUCCESS) ERROR(exiterr,-1);

	// setup debug messenger
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
	if (func(instance, &debugCreateInfo, 0, &debug) != VK_SUCCESS) ERROR(exiterr,-1);
	*/

	// create surface
	// if (!glfwVulkanSupported()) ERROR(exiterr,-1);
    // if (glfwCreateWindowSurface(instance, window, 0, &surface) != VK_SUCCESS) ERROR(exiterr,-1);

	// pick physical device
	uint32_t physicalDeviceCount = 0;
	if (vkEnumeratePhysicalDevices(instance,&physicalDeviceCount,0) != VK_SUCCESS) ERROR(exiterr,-1);
	VkPhysicalDevice physicalDevice[physicalDeviceCount];
	if (vkEnumeratePhysicalDevices(instance,&physicalDeviceCount,physicalDevice) != VK_SUCCESS) ERROR(exiterr,-1);
	for (int i = 0; i < physicalDeviceCount; i++) {
	VkPhysicalDeviceProperties physicalDeviceProperties = {0};
	vkGetPhysicalDeviceProperties(physicalDevice[i],&physicalDeviceProperties);
	printf("device (%s) vertex shader attributes (%d)\n",physicalDeviceProperties.deviceName,
		physicalDeviceProperties.limits.maxVertexInputAttributes);}
	if (physicalDeviceCount) physical = physicalDevice[0];

	// create logical device
	uint32_t queueFamilyCount = 0;
	vkGetPhysicalDeviceQueueFamilyProperties(physical, &queueFamilyCount, 0);
	VkQueueFamilyProperties queueFamilies[queueFamilyCount];
	vkGetPhysicalDeviceQueueFamilyProperties(physical, &queueFamilyCount, queueFamilies);
	for (int i = 0; i < queueFamilyCount; i++) {
	VkBool32 presentSupport = VK_FALSE;
	// vkGetPhysicalDeviceSurfaceSupportKHR(physical, i, surface, &presentSupport);
	if (presentSupport && !valid[Present]) {
	family[Present] = i; valid[Present] = 1;}
	if ((queueFamilies[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) && !valid[Render]) {
	family[Render] = i; valid[Render] = 1;}}
	uint32_t queueCreateInfoCount = 0;
	VkDeviceQueueCreateInfo queueCreateInfo[Shaders] = {0};
	float queuePriority = 1.0f;
	for (int i = 0; i < Shaders; i++) if (valid[i]) {
	queueCreateInfo[queueCreateInfoCount].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
	queueCreateInfo[queueCreateInfoCount].queueFamilyIndex = family[i];
	queueCreateInfo[queueCreateInfoCount].queueCount = 1;
	queueCreateInfo[queueCreateInfoCount].pQueuePriorities = &queuePriority;
	queueCreateInfoCount++;}
	VkPhysicalDeviceFeatures deviceFeatures = {0};
	VkDeviceCreateInfo deviceCreateInfo = {0};
	deviceCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
	deviceCreateInfo.pQueueCreateInfos = queueCreateInfo;
	deviceCreateInfo.queueCreateInfoCount = queueCreateInfoCount;
	deviceCreateInfo.pEnabledFeatures = &deviceFeatures;
	deviceCreateInfo.enabledExtensionCount = 0;
	deviceCreateInfo.enabledLayerCount = 0;
	if (vkCreateDevice(physical, &deviceCreateInfo, 0, &logical) != VK_SUCCESS) ERROR(exiterr,-1);
	for (int i = 0; i < Shaders; i++) if (valid[i]) {
	vkGetDeviceQueue(logical, family[i], 0, &queue[i]);}

	// create swap chain
	/*
	VkSwapchainCreateInfoKHR swapCreateInfo = {0};
	if (vkCreateSwapchainKHR(logical, &swapCreateInfo, 0, &swap) != VK_SUCCESS) ERROR(exiterr,-1);
	*/

	// continue https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Surface-format
}

GLuint loadShaders(const char *vs, const char *gs, const char *fs)
{
	return 0;
}

#define VERTEX(FIELD) ((void*)&(((struct Vertex *)0)->FIELD))
void openglInit(int argc, char **argv)
{
	glfwInit();
	glfwWindowHint(GLFW_SAMPLES, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // To make MacOS happy; should not be needed
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	const char *name = (argc == 4 ? argv[3] : argv[0]);
	if ((window = glfwCreateWindow(WINWIDE, WINHIGH, name, 0, 0)) == 0) ERROR(exiterr,-1);
	glfwSetKeyCallback(window, displayKey);
	glfwMakeContextCurrent(window);
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS); 
	glGenVertexArrays(1, &vertexArray);
	glBindVertexArray(vertexArray);
	programID = loadShaders("plane.vs","plane.gs","plane.fs");
	glGenBuffers(1, &vertexBuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
	GLuint index = 0;
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(tag[0]));
	for (int i = 0; i < 3; i++)
	glVertexAttribPointer(index++,3,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(plane[i][0]));
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(versor[0]));
	for (int i = 0; i < 3; i++)
	glVertexAttribPointer(index++,2,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(coord[i][0]));
	for (int i = 0; i < 3; i++)
	glVertexAttribPointer(index++,4,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(coord[i][0]));
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(texid[0]));
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(facid[0]));
	glVertexAttribIPointer(index++,1,GL_INT,sizeof(struct Vertex),VERTEX(matid));
	for (int i = 0; i < index; i++)
	glEnableVertexAttribArray(i);
	glBindVertexArray(0);
	for (int i = 0; i < index; i++)
	glDisableVertexAttribArray(i);
	glGenBuffers(1, &elementBuffer);
}

void vulkanDraw()
{
}

void openglDraw()
{
	if (vertexBufferChanged) {
	glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(struct Vertex)*vertices, vertex, GL_STATIC_DRAW);}
	if (elementBufferChanged) {
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementBuffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(struct Facet)*facets, facet, GL_STATIC_DRAW);}
	if (vertexBufferChanged || elementBufferChanged) {
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programID);
	glBindVertexArray(vertexArray);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementBuffer);
	glDrawElements(GL_TRIANGLES,facets,GL_UNSIGNED_INT,(void*)0);
	glfwSwapBuffers(window);}
}

void openglDestroy()
{
	glDeleteBuffers(1, &elementBuffer);
	glDeleteBuffers(1, &vertexBuffer);
	glDeleteVertexArrays(1, &vertexArray);
	glDeleteProgram(programID);
}

void vulkanDestroy()
{
	//vkDestroySwapchainKHR(logical, swap, 0);
	vkDestroyDevice(logical, 0);
	vkDestroySurfaceKHR(instance, surface, 0);
	/*
	PFN_vkDestroyDebugUtilsMessengerEXT func = (PFN_vkDestroyDebugUtilsMessengerEXT)
	vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
	if (func == 0) {ERROR(exiterr,-1);} else func(instance, debug, 0);
	*/
	vkDestroyInstance(instance, 0);
	glfwDestroyWindow(window);
	glfwTerminate();
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
	vulkan = glfwVulkanSupported();
	printf("vulkan (%d)\n",vulkan);
	threadInit(argc,argv);
	if (vulkan) vulkanInit(argc,argv);
	else openglInit(argc,argv);

	while (esc < 2 && !glfwWindowShouldClose(window)) {
	if (setjmp(jmpbuf) == 0) {
	while(esc < 2 && !glfwWindowShouldClose(window)) {
	glfwWaitEvents();
	if (vulkan) vulkanDraw();
	else openglDraw();
	produce();
	if (callread(argc))
	process();}}}

	if (vulkan) vulkanDestroy();
	else openglDestroy();
	threadDestroy(argc);

	return 0;
}
