/*
*    vulkan.c
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
#include "plane.h"
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>

enum Shader {Render,Present,Pierce,Bounce,Shaders};
VkInstance instance = {0};
VkDebugUtilsMessengerEXT debug = {0};
VkSurfaceKHR surface = {0};
VkPhysicalDevice physical = {0};
VkDevice logical = {0};
int valid[Shaders] = {0};
uint32_t family[Shaders] = {0};
VkQueue queue[Shaders] = {0};
VkSwapchainKHR swap = {0};

VkBool32 debugCallback(
	VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
	VkDebugUtilsMessageTypeFlagsEXT messageType,
	const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData,
	void* pUserData) {
	printf("validation layer (%s)\n",pCallbackData->pMessage);
	return VK_FALSE;
}

int vulkanInit()
{
	if (!glfwVulkanSupported()) return 0;
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
	return 1;
}

void vulkanDraw()
{
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
}
