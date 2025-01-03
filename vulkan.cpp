#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#define GLM_FORCE_DEPTH_ZERO_TO_ONE
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

#include <iostream>
#include <fstream>
#include <algorithm>
#include <chrono>
#include <vector>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include <limits>
#include <array>
#include <deque>
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
extern "C" {
#include "proto.h"
#include "face.h"
#include "type.h"
#include "plane.h"
#include "fmtx.h"
};

struct SafeState {
    sem_t semaphore;
    SafeState(int val) {
        if (sem_init(&semaphore, 0, val) != 0) {std::cerr << "failed to create semaphore!" << std::endl; exit(-1);}
    }
    ~SafeState() {
        if (sem_destroy(&semaphore) != 0) {std::cerr << "cannot destroy semaphore!" << std::endl; exit(-1);}
    }
    void wait() {
        if (sem_wait(&semaphore) != 0) {std::cerr << "cannot wait for semaphore!" << std::endl; exit(-1);}
    }
    void post() {
        if (sem_post(&semaphore) != 0) {std::cerr << "cannot post to semaphore!" << std::endl; exit(-1);}
    }
    bool trywait() {
        int tryval = sem_trywait(&semaphore);
        if (tryval != 0 && errno != EAGAIN) {std::cerr << "cannot trywait for semaphore!" << std::endl; exit(-1);}
        return (tryval == 0);
    }
    int get() {
        int sval;
        if (sem_getvalue(&semaphore,&sval) != 0) {std::cerr << "cannot get semaphore!" << std::endl; exit(-1);}
        return sval;
    }
};

struct MainState;
struct ChangeState {
    MainState *main;
    wftype wrapDone;
    SafeState safe;
    int config[Configures];
    ChangeState(MainState*main) : main(main), wrapDone(0), safe(1), config{0}
    {std::cout << "ChangeState" << std::endl;}
    ~ChangeState() {std::cout << "~ChangeState" << std::endl;}
    int get(Configure cfg) {
        if (cfg < 0 || cfg >= Configures) {std::cerr << "invalid get cfg!" << std::endl; exit(-1);}
        return config[cfg];
    }
    void set(Configure cfg, int val) {
        if (cfg < 0 || cfg >= Configures) {std::cerr << "invalid set cfg!" << std::endl; exit(-1);}
        config[cfg] = val;
    }
    int read(Configure cfg) {
        if (cfg < 0 || cfg >= Configures) {std::cerr << "invalid read cfg!" << std::endl; exit(-1);}
        safe.wait(); int val = config[cfg]; safe.post(); return val;
    }
    void write(Configure cfg, int val) {
        if (cfg < 0 || cfg >= Configures) {std::cerr << "invalid write cfg!" << std::endl; exit(-1);}
        safe.wait(); config[cfg] = val; safe.post();
    }
    void wots(Configure cfg, int val) {
        if (cfg < 0 || cfg >= Configures) {std::cerr << "invalid wots cfg!" << std::endl; exit(-1);}
        safe.wait(); config[cfg] |= val; safe.post();
    }
    void wotc(Configure cfg, int val) {
        if (cfg < 0 || cfg >= Configures) {std::cerr << "invalid wotc cfg!" << std::endl; exit(-1);}
        safe.wait(); config[cfg] &= ~val; safe.post();
    }
    void async(Async bit);
    void loop();
    int copy(Center *);
    void done(int pass, Center *);
};

// TODO define glfw callbacks that cast void* to ChangeState*
struct WindowState {
    const uint32_t WIDTH = 800;
    const uint32_t HEIGHT = 600;
    GLFWwindow* const window;
    WindowState(ChangeState *change) :
        window(createWindow(WIDTH,HEIGHT))
        {std::cout << "WindowState" << std::endl;}
    ~WindowState() {
        std::cout << "~WindowState" << std::endl;
        glfwDestroyWindow(window);
        glfwTerminate();
    }
    static GLFWwindow* createWindow(uint32_t WIDTH, uint32_t HEIGHT) {
        glfwInit();
        glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
        return glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
    }
};

struct VulkanState {
    static const char *validationLayers[];
    VkDebugUtilsMessengerCreateInfoEXT info;
    VkInstance instance;
    VkDebugUtilsMessengerEXT debug;
    VkSurfaceKHR surface;
    VulkanState(GLFWwindow* window) :
        info(createInfo(validationLayers)),
        instance(createInstance(info,validationLayers)),
        debug(createDebug(instance,info,validationLayers)),
        surface(createSurface(instance, window))
        {std::cout << "VulkanState" << std::endl;}
    ~VulkanState() {
        std::cout << "~VulkanState" << std::endl;
        vkDestroySurfaceKHR(instance, surface, nullptr);
        auto func = (PFN_vkDestroyDebugUtilsMessengerEXT)
        vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
        if (func != nullptr) func(instance, debug, nullptr);
        vkDestroyInstance(instance, nullptr);
    }
    static VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
        VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData) {
        std::cout << "validation layer: " << pCallbackData->pMessage << std::endl;
        return VK_FALSE;
    }
    static VkDebugUtilsMessengerCreateInfoEXT createInfo(const char **validationLayers) {
        VkDebugUtilsMessengerCreateInfoEXT info{};
        if (!validationLayers) return info;
        info.sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
        info.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
        info.messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
        info.pfnUserCallback = debugCallback;
        return info;
    }
    static VkInstance createInstance(VkDebugUtilsMessengerCreateInfoEXT info, const char **validationLayers) {
        if (validationLayers) {
            uint32_t count; vkEnumerateInstanceLayerProperties(&count, nullptr);
            std::vector<VkLayerProperties> availableLayers(count);
            vkEnumerateInstanceLayerProperties(&count, availableLayers.data());
            for (const char **name = validationLayers; *name; name++) {
            bool found = false; for (uint32_t i = 0; i < count; i++)
            if (strcmp(*name, availableLayers[i].layerName) == 0) {found = true; break;}
            if (!found) {std::cerr << "validation layers requested, but not available!" << std::endl; exit(-1);}}}
        VkApplicationInfo appInfo{};
        appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
        appInfo.pApplicationName = "plane";
        appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
        appInfo.pEngineName = "No Engine";
        appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
        appInfo.apiVersion = VK_API_VERSION_1_0;
        VkInstanceCreateInfo createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
        createInfo.pApplicationInfo = &appInfo;
        const char **ptr = 0; uint32_t count = 0; uint32_t size = 0;
        ptr = glfwGetRequiredInstanceExtensions(&count);
        if (validationLayers) size = count+1; else size = count;
        std::vector<const char *> extensions(size);
        for (uint32_t i = 0; i < count; i++) extensions[i] = ptr[i];
        if (validationLayers) extensions[count] = VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
        createInfo.enabledExtensionCount = size;
        createInfo.ppEnabledExtensionNames = extensions.data();
        if (validationLayers) {
            createInfo.enabledLayerCount = 0;
            for (const char **name = validationLayers; *name; name++) createInfo.enabledLayerCount++;
            createInfo.ppEnabledLayerNames = validationLayers;
            createInfo.pNext = (VkDebugUtilsMessengerCreateInfoEXT*) &info;} else {
            createInfo.enabledLayerCount = 0;
            createInfo.pNext = nullptr;}
        VkInstance instance;
        if (vkCreateInstance(&createInfo, nullptr, &instance) != VK_SUCCESS)
        {std::cerr << "failed to create instance!" << std::endl; exit(-1);}
        return instance;
    }
    static VkDebugUtilsMessengerEXT createDebug(VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info,
        const char **validationLayers) {
        VkDebugUtilsMessengerEXT debug{};
        if (!validationLayers) return debug;
        auto func = (PFN_vkCreateDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
        if (func == nullptr || func(instance, &info, nullptr, &debug) != VK_SUCCESS)
        {std::cerr << "failed to set up debug messenger!" << std::endl; exit(-1);}
        return debug;
    }
    static VkSurfaceKHR createSurface(VkInstance instance, GLFWwindow* window) {
        VkSurfaceKHR surface;
        if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
        {std::cerr << "failed to create window surface!" << std::endl; exit(-1);}
        return surface;
    }
};
#ifdef NDEBUG
const char *VulkanState::validationLayers[] = {0};
#else
const char *VulkanState::validationLayers[] = {"VK_LAYER_KHRONOS_validation",0};
#endif

struct PhysicalState {
    static const char *deviceExtensions[];
    VkPhysicalDevice device;
    uint32_t graphicsFamily;
    uint32_t presentFamily;
    VkPhysicalDeviceProperties properties;
    VkSurfaceFormatKHR surfaceFormat;
    VkPresentModeKHR presentMode;
    VkPhysicalDeviceMemoryProperties memProperties;
    PhysicalState(VkInstance instance, VkSurfaceKHR surface) :
        device(createDevice(instance,surface,deviceExtensions)),
        graphicsFamily(findGraphicsFamily(surface,device)),
        presentFamily(findPresentFamily(surface,device)),
        properties(findProperties(device)),
        surfaceFormat(chooseSwapSurfaceFormat(surface,device)),
        presentMode(chooseSwapPresentMode(surface,device)),
        memProperties(findMemoryProperties(device))
        {std::cout << "PhysicalState" << std::endl;}
    ~PhysicalState() {std::cout << "~PhysicalState" << std::endl;}
    static bool foundIndices(VkSurfaceKHR surface, VkPhysicalDevice device) {
        bool foundGraphics = false; bool foundPresent = false;
        uint32_t count = 0; vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);
        std::vector<VkQueueFamilyProperties> queueFamilies(count);
        vkGetPhysicalDeviceQueueFamilyProperties(device, &count, queueFamilies.data());
        uint32_t i = 0; for (const auto& queueFamily : queueFamilies) {
            VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i++, surface, &support);
            if (support) foundPresent = true;
            if (queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT) foundGraphics = true;
            if (foundGraphics && foundPresent) return true;}
        return false;
    }
    static bool foundDetails(VkSurfaceKHR surface, VkPhysicalDevice device) {
        uint32_t surfaceFormats = 0; vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &surfaceFormats, nullptr);
        uint32_t presentModes = 0; vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &presentModes, nullptr);
        return (surfaceFormats > 0 && presentModes > 0);
    }
    static VkPhysicalDevice createDevice(VkInstance instance, VkSurfaceKHR surface, const char **deviceExtensions) {
        VkPhysicalDevice retdev;
        uint32_t count = 0; vkEnumeratePhysicalDevices(instance, &count, nullptr);
        if (count == 0) {std::cerr << "failed to find GPUs with Vulkan support!" << std::endl; exit(-1);}
        std::vector<VkPhysicalDevice> devices(count);
        vkEnumeratePhysicalDevices(instance, &count, devices.data());
        bool found = false; for (const auto& device : devices) {
            if (!foundIndices(surface,device)) continue;
        if (!foundDetails(surface,device)) continue;
            count = 0; vkEnumerateDeviceExtensionProperties(device, nullptr, &count, nullptr);
            std::vector<VkExtensionProperties> properties(count);
            vkEnumerateDeviceExtensionProperties(device, nullptr, &count, properties.data());
            bool found0 = true; for (const char **name = deviceExtensions; *name; name++) {
            bool found1 = false; for (uint32_t i = 0; i < count; i++)
            if (strcmp(*name,properties[i].extensionName) == 0)
            {found1 = true; break;} if (!found1) {found0 = false; break;}} if (!found0) continue;
            VkPhysicalDeviceFeatures supportedFeatures;
            vkGetPhysicalDeviceFeatures(device, &supportedFeatures);
            if (!supportedFeatures.samplerAnisotropy) continue;
            found = true; retdev = device; break;}
        if (!found) {std::cerr << "failed to find a suitable GPU!" << std::endl; exit(-1);}
        return retdev;
    }
    static uint32_t findGraphicsFamily(VkSurfaceKHR surface, VkPhysicalDevice device) {
        uint32_t count = 0; vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);
        std::vector<VkQueueFamilyProperties> queueFamilies(count);
        vkGetPhysicalDeviceQueueFamilyProperties(device, &count, queueFamilies.data());
        uint32_t i = 0; for (const auto& queueFamily : queueFamilies) {
            VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &support);
            if (support && (queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT)) return i; i++;}
        i = 0; for (const auto& queueFamily : queueFamilies) {
            if (queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT) return i; i++;}
        return 0;
    }
    static uint32_t findPresentFamily(VkSurfaceKHR surface, VkPhysicalDevice device) {
        uint32_t count = 0; vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);
        std::vector<VkQueueFamilyProperties> queueFamilies(count);
        vkGetPhysicalDeviceQueueFamilyProperties(device, &count, queueFamilies.data());
        uint32_t i = 0; for (const auto& queueFamily : queueFamilies) {
            VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &support);
            if (support && (queueFamily.queueFlags & VK_QUEUE_GRAPHICS_BIT)) return i; i++;}
        i = 0; for (const auto& queueFamily : queueFamilies) {
            VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &support);
            if (support) return i; i++;}
        return 0;
    }
    static VkPhysicalDeviceProperties findProperties(VkPhysicalDevice device) {
        VkPhysicalDeviceProperties properties{};
        vkGetPhysicalDeviceProperties(device, &properties);
        return properties;
    }
    static VkSurfaceFormatKHR chooseSwapSurfaceFormat(VkSurfaceKHR surface, VkPhysicalDevice device) {
        uint32_t count = 0; vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, nullptr);
        std::vector<VkSurfaceFormatKHR> formats(count);
        if (count != 0) vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, formats.data());
        for (const auto& format : formats) {
        if (format.format == VK_FORMAT_B8G8R8A8_SRGB && format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
        return format;}}
        return formats[0];
    }
    static VkPresentModeKHR chooseSwapPresentMode(VkSurfaceKHR surface, VkPhysicalDevice device) {
        uint32_t count = 0; vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, nullptr);
        std::vector<VkPresentModeKHR> presentModes(count);
        if (count != 0) vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, presentModes.data());
        for (const auto& presentMode : presentModes) {
        if (presentMode == VK_PRESENT_MODE_MAILBOX_KHR) {
        return presentMode;}}
        return VK_PRESENT_MODE_FIFO_KHR;
    }
    static VkPhysicalDeviceMemoryProperties findMemoryProperties(VkPhysicalDevice device) {
        VkPhysicalDeviceMemoryProperties memProperties;
        vkGetPhysicalDeviceMemoryProperties(device, &memProperties);
        return memProperties;
    }
};
const char *PhysicalState::deviceExtensions[] = {VK_KHR_SWAPCHAIN_EXTENSION_NAME,0};

struct LogicalState {
    VkDevice device;
    VkQueue graphics;
    VkQueue present;
    VkCommandPool commandPool;
    VkFormat imageFormat;
    VkFormat depthFormat;
    VkRenderPass renderPass;
    static constexpr VkFormat candidates[] = {VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT};
    LogicalState(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily, VkSurfaceFormatKHR surfaceFormat,
        const char **validationLayers, const char **deviceExtensions) :
        device(createDevice(physicalDevice,graphicsFamily,presentFamily,validationLayers,deviceExtensions)),
        graphics(createQueue(device,graphicsFamily)),
        present(createQueue(device,presentFamily)),
        commandPool(createCommandPool(device,graphicsFamily)),
        imageFormat(surfaceFormat.format),
        depthFormat(findSupportedFormat(physicalDevice, candidates, sizeof(candidates)/sizeof(VkFormat),
            VK_IMAGE_TILING_OPTIMAL, VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)),
        renderPass(createRenderPass(device,imageFormat,depthFormat))
        {std::cout << "LogicalState" << std::endl;}
    ~LogicalState() {
        std::cout << "~LogicalState" << std::endl;
        vkDestroyRenderPass(device, renderPass, nullptr);
        vkDestroyCommandPool(device, commandPool, nullptr);
        vkDestroyDevice(device, nullptr);
    }
    static VkDevice createDevice(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily,
        const char **validationLayers, const char **deviceExtensions) {
        VkDevice device;
        std::vector<VkDeviceQueueCreateInfo> queueCreateInfos;
        uint32_t queueFamilies[] = {graphicsFamily, presentFamily};
        float queuePriority = 1.0f;
        for (uint32_t i = 0; i < (graphicsFamily == presentFamily ? 1 : 2); i++) {
            VkDeviceQueueCreateInfo queueCreateInfo{};
            queueCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
            queueCreateInfo.queueFamilyIndex = queueFamilies[i];
            queueCreateInfo.queueCount = 1;
            queueCreateInfo.pQueuePriorities = &queuePriority;
            queueCreateInfos.push_back(queueCreateInfo);}
        VkPhysicalDeviceFeatures deviceFeatures{};
        deviceFeatures.samplerAnisotropy = VK_TRUE;
        VkDeviceCreateInfo createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
        createInfo.queueCreateInfoCount = static_cast<uint32_t>(queueCreateInfos.size());
        createInfo.pQueueCreateInfos = queueCreateInfos.data();
        createInfo.pEnabledFeatures = &deviceFeatures;
        createInfo.enabledExtensionCount = 0;
        for (const char **name = deviceExtensions; *name; name++) createInfo.enabledExtensionCount++;
        createInfo.ppEnabledExtensionNames = deviceExtensions;
        if (validationLayers) {
            createInfo.enabledLayerCount = 0;
            for (const char **name = validationLayers; *name; name++) createInfo.enabledLayerCount++;
            createInfo.ppEnabledLayerNames = validationLayers;}
        else createInfo.enabledLayerCount = 0;
        if (vkCreateDevice(physicalDevice, &createInfo, nullptr, &device) != VK_SUCCESS)
        {std::cerr << "failed to create logical device!" << std::endl; exit(-1);}
        return device;
    }
    static VkQueue createQueue(VkDevice device, uint32_t family) {
        VkQueue queue;
        vkGetDeviceQueue(device, family, 0, &queue);
        return queue;
    }
    static VkCommandPool createCommandPool(VkDevice device, uint32_t family) {
        VkCommandPool pool;
        VkCommandPoolCreateInfo commandPoolInfo{};
        commandPoolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
        commandPoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
        commandPoolInfo.queueFamilyIndex = family;
        if (vkCreateCommandPool(device, &commandPoolInfo, nullptr, &pool) != VK_SUCCESS)
        {std::cerr << "failed to create graphics command pool!" << std::endl; exit(-1);}
        return pool;
    }
    static VkFormat findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size,
        VkImageTiling tiling, VkFormatFeatureFlags features) {
        for (int i = 0; i < size; i++) {
        VkFormatProperties props;
        vkGetPhysicalDeviceFormatProperties(physicalDevice, candidates[i], &props);
        if (tiling == VK_IMAGE_TILING_LINEAR && (props.linearTilingFeatures & features) == features) return candidates[i];
        else if (tiling == VK_IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures & features) == features) return candidates[i];}
        {std::cerr << "failed to find supported format!" << std::endl; exit(-1);}
    }
    static VkRenderPass createRenderPass(VkDevice device, VkFormat imageFormat, VkFormat depthFormat) {
        VkAttachmentDescription colorAttachment{};
        colorAttachment.format = imageFormat;
        colorAttachment.samples = VK_SAMPLE_COUNT_1_BIT;
        colorAttachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        colorAttachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        colorAttachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        colorAttachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        colorAttachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        colorAttachment.finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        VkAttachmentDescription depthAttachment{};
        depthAttachment.format = depthFormat;
        depthAttachment.samples = VK_SAMPLE_COUNT_1_BIT;
        depthAttachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        depthAttachment.storeOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        depthAttachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        depthAttachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        depthAttachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        depthAttachment.finalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        VkAttachmentReference colorAttachmentRef{};
        colorAttachmentRef.attachment = 0;
        colorAttachmentRef.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        VkAttachmentReference depthAttachmentRef{};
        depthAttachmentRef.attachment = 1;
        depthAttachmentRef.layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        VkSubpassDescription subpass{};
        subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
        subpass.colorAttachmentCount = 1;
        subpass.pColorAttachments = &colorAttachmentRef;
        subpass.pDepthStencilAttachment = &depthAttachmentRef;
        VkSubpassDependency dependency{};
        dependency.srcSubpass = VK_SUBPASS_EXTERNAL;
        dependency.dstSubpass = 0;
        dependency.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT | VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT;
        dependency.srcAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        dependency.dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT | VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
        dependency.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT | VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        std::array<VkAttachmentDescription, 2> attachments = {colorAttachment, depthAttachment};
        VkRenderPassCreateInfo renderPassInfo{};
        renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
        renderPassInfo.attachmentCount = static_cast<uint32_t>(attachments.size());
        renderPassInfo.pAttachments = attachments.data();
        renderPassInfo.subpassCount = 1;
        renderPassInfo.pSubpasses = &subpass;
        renderPassInfo.dependencyCount = 1;
        renderPassInfo.pDependencies = &dependency;
        VkRenderPass renderPass;
        if (vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPass) != VK_SUCCESS)
        {std::cerr << "failed to create render pass!" << std::endl; exit(-1);}
        return renderPass;
    }
};

enum SizeEnum {
    IntSize,
    ExtentSize,
    MicroSize,
    SwapSize,
};
struct SizeState {
    SizeEnum tag;
    int size;
    VkExtent2D extent;
    Micro micro;
    VkSurfaceCapabilitiesKHR capabilities;
    SizeState() {
        tag = IntSize;
        size = 0;
    }
    SizeState(int size) {
        tag = IntSize;
        this->size = size;
    }
    SizeState(VkExtent2D extent) {
        tag = ExtentSize;
        this->extent = extent;
    }
    SizeState(Micro micro) {
        tag = MicroSize;
        this->micro = micro;
    }
    SizeState(VkSurfaceCapabilitiesKHR capabilities) {
        tag = SwapSize;
        this->capabilities = capabilities;
    }
    bool operator==(const SizeState &other) const {
        if (tag == IntSize && other.tag == IntSize &&
        size == other.size) return true;
        if (tag == ExtentSize && other.tag == ExtentSize &&
        extent.width == other.extent.width &&
        extent.height == other.extent.height) return true;
        if (tag == MicroSize && other.tag == MicroSize &&
        micro == other.micro) return true;
        if (tag == SwapSize && other.tag == SwapSize &&
        capabilities.currentExtent.width == other.capabilities.currentExtent.width &&
        capabilities.currentExtent.height == other.capabilities.currentExtent.height) return true;
        return false;
    }
};
std::ostream& operator<<(std::ostream& os, const SizeState& size) {
    switch (size.tag) {default: os << "MicroSize()"; break;
    case (IntSize): os << "IntSize(" << size.size << ")"; break;
    case (ExtentSize): os << "ExtentSize(" << size.extent.width << "," << size.extent.height << ")"; break;
    case (MicroSize): os << "MicroSize(" << size.micro << ")"; break;
    case (SwapSize): os << "SwapSize(" << size.capabilities.currentExtent.width << "," << size.capabilities.currentExtent.height << ")"; break;}
    return os;
}

enum BindEnum {
    SwapBind,
    IndexBind,
    PipelineBind,
    DrawBind,
    BindEnums
};
struct BaseState;
struct BindState {
    virtual void advance() = 0;
    virtual BaseState *buffer() = 0;
    virtual int index() = 0;
    static BindState* self;
    static GLFWwindow* window;
    static VkSurfaceKHR surface;
    static uint32_t graphicsFamily;
    static uint32_t presentFamily;
    static VkFormat imageFormat;
    static VkFormat depthFormat;
    static VkDevice device;
    static VkRenderPass renderPass;
    static VkCommandPool commandPool;
    static int frames;
    static int micro;
    static ChangeState *change;
    static VkPhysicalDevice physical;
    static VkQueue graphics;
    static VkQueue present;
    static VkBufferUsageFlags flags;
    static VkPhysicalDeviceProperties properties;
    static VkPhysicalDeviceMemoryProperties memProperties;
    static VkSurfaceFormatKHR surfaceFormat;
    static VkPresentModeKHR presentMode;
    static int debug;
    const char *name;
    BindState(const char *n,GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice physical,
        VkDevice device, VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode,
        uint32_t graphicsFamily, uint32_t presentFamily, VkFormat imageFormat, VkFormat depthFormat,
        VkRenderPass renderPass, VkPhysicalDeviceMemoryProperties memProperties) : name(n) {
        BindState::self = this;
        BindState::window = window;
        BindState::surface = surface;
        BindState::physical = physical;
        BindState::device = device;
        BindState::surfaceFormat = surfaceFormat;
        BindState::presentMode = presentMode;
        BindState::graphicsFamily = graphicsFamily;
        BindState::presentFamily = presentFamily;
        BindState::imageFormat = imageFormat;
        BindState::depthFormat = depthFormat;
        BindState::renderPass = renderPass;
        BindState::memProperties = memProperties;
        BindState::debug = 0;
    }
    BindState(const char *n, VkCommandPool pool, int frames) : name(n) {
        BindState::self = this;
        BindState::commandPool = pool;
        BindState::frames = frames;
        BindState::micro = 0;
        BindState::debug = 0;
    }
    BindState(const char *n, ChangeState *change) : name(n) {
        BindState::self = this;
        BindState::change = change;
        BindState::debug = 0;
    }
    BindState(const char *n, VkQueue graphics, VkQueue present, VkBufferUsageFlags flags) : name(n) {
        BindState::self = this;
        BindState::graphics = graphics;
        BindState::present = present;
        BindState::flags = flags;
        BindState::debug = 0;
    }
    BindState(const char *n, VkBufferUsageFlags flags) : name(n) {
        BindState::self = this;
        BindState::flags = flags;
        BindState::debug = 0;
    }
    BindState(const char *n, VkPhysicalDeviceProperties properties) : name(n) {
        BindState::self = this;
        BindState::properties = properties;
        BindState::debug = 0;
    }
    BindState(const char *n) : name(n) {
        BindState::self = this;
        BindState::debug = 0;
    }
    static int check(BindEnum typ, Memory mem) {
        if (mem < 0 || mem > Memorys)
        {std::cerr << "invalid Memory bind!" << std::endl; exit(-1);}
        if (typ < 0 || typ > BindEnums)
        {std::cerr << "invalid BindEnum bind!" << std::endl; exit(-1);}
        if (mem == Memorys && typ == BindEnums)
        {std::cerr << "invalid both bind!" << std::endl; exit(-1);}
        if (mem != Memorys && typ != BindEnums)
        {std::cerr << "invalid neither bind!" << std::endl; exit(-1);}
        int idx = (mem == Memorys ? typ : mem+BindEnums);
        if (idx < 0 || idx >= BindEnums+Memorys)
        {std::cerr << "invalid index bind!" << std::endl; exit(-1);}
        return idx;
    }
};
BindState* BindState::self;
ChangeState *BindState::change;
GLFWwindow* BindState::window;
VkSurfaceKHR BindState::surface;
uint32_t BindState::graphicsFamily;
uint32_t BindState::presentFamily;
VkFormat BindState::imageFormat;
VkFormat BindState::depthFormat;
VkDevice BindState::device;
VkRenderPass BindState::renderPass;
VkCommandPool BindState::commandPool;
int BindState::frames;
int BindState::micro;
VkPhysicalDevice BindState::physical;
VkQueue BindState::graphics;
VkQueue BindState::present;
VkBufferUsageFlags BindState::flags;
VkPhysicalDeviceProperties BindState::properties;
VkPhysicalDeviceMemoryProperties BindState::memProperties;
VkSurfaceFormatKHR BindState::surfaceFormat;
VkPresentModeKHR BindState::presentMode;
int BindState::debug;

template <class State, int Size> struct ArrayState : public BindState {
    SafeState safe;
    BindEnum typ;
    Memory mem;
    int idx;
    State state[Size];
    ArrayState(const char *name, BindEnum t, Memory m,
        GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice physical, VkDevice device,
        VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode,
        uint32_t graphicsFamily, uint32_t presentFamily, VkFormat imageFormat, VkFormat depthFormat,
        VkRenderPass renderPass, VkPhysicalDeviceMemoryProperties memProperties) :
        BindState(name,window,surface,physical,device,
        surfaceFormat,presentMode,
        graphicsFamily,presentFamily,imageFormat,depthFormat,
        renderPass,memProperties),
        safe(1), typ(t), mem(m), idx(0) {}
    ArrayState(const char *name, BindEnum t, Memory m,
        VkCommandPool pool, int frames) :
        BindState(name,pool,frames),
        safe(1), typ(t), mem(m), idx(0) {}
    ArrayState(const char *name, BindEnum t, Memory m,
        ChangeState *change) :
        BindState(name,change),
        safe(1), typ(t), mem(m), idx(0) {}
    ArrayState(const char *name, BindEnum t, Memory m,
        VkQueue graphics, VkQueue present, VkBufferUsageFlags flags) :
        BindState(name,graphics,present,flags),
        safe(1), typ(t), mem(m), idx(0) {}
    ArrayState(const char *name, BindEnum t, Memory m,
        VkBufferUsageFlags flags) :
        BindState(name,flags),
        safe(1), typ(t), mem(m), idx(0) {}
    ArrayState(const char *name, BindEnum t, Memory m,
        VkPhysicalDeviceProperties properties) :
        BindState(name,properties),
        safe(1), typ(t), mem(m), idx(0) {}
    ArrayState(const char *name, BindEnum t, Memory m) :
        BindState(name),
        safe(1), typ(t), mem(m), idx(0) {}
    State *derived() {safe.wait(); State *ptr = &state[idx]; safe.post(); return ptr;}
    State *preview() {safe.wait(); State *ptr = &state[(idx+1)%Size]; safe.post(); return ptr;}
    State *preview(int i) {
        if (i < 0 || i >= Size) {std::cerr << "cannot preview! " << i << " " << Size << std::endl; exit(-1);}
        safe.wait(); State *ptr = &state[i]; safe.post(); return ptr;}
    void advance(int i) {
        if (i < 0 || i >= Size) {std::cerr << "cannot advance!" << std::endl; exit(-1);}
        safe.wait(); idx = i; safe.post();}
    void advance() {safe.wait(); idx = (idx+1)%Size; safe.post();}
    BaseState *buffer() {safe.wait(); BaseState *ptr = &state[idx]; safe.post(); return ptr;}
    int index() {return check(typ,mem);}
};
template<class State, class Init, int Size> struct InitState {
    State state[Size];
    InitState(Init func) {for (int i = 0; i < Size; i++) state[i] = func();}
    InitState(State init) {for (int i = 0; i < Size; i++) state[i] = init;}
    State &operator[](int i) {return state[i];}
};
template<class State, class Init> struct HeapState {
    std::vector<State> state;
    HeapState(Init func, int size) : state(size) {for (int i = 0; i < size; i++) state[i] = func();}
    HeapState(State init, int size) : state(size) {for (int i = 0; i < size; i++) state[i] = init;}
    State &operator[](int i) {return state[i];}
};
template<class State> struct ConstState {
    State value;
    ConstState(State value) : value(value) {}
    State operator()() {return value;}
};

enum BaseEnum {
    InitBase, // uninitialized
    BothBase, // enqued for both change
    SizeBase, // enqued for size change
    LockBase, // enqued for data change
    NextBase, // waiting for fence done
    FreeBase, // ready to use or update
    BaseEnums
};
struct BaseState {
    BaseEnum state;
    SizeState size, todo;
    void *ptr; int loc; int siz;
    SafeState safe; int count;
    char debug[64];
    BaseState() : state(InitBase), size(0), todo(0), safe(1), count(0) {
        debug[0] = 0;
    }
    BaseState(const char *name) : state(InitBase), size(0), todo(0), safe(1), count(0) {
        sprintf(debug,"%s%d",name,BindState::debug++);
    }
    bool push(void *ptr, int loc, int siz, SizeState max) { // called in main thread
        safe.wait();
        if (state != InitBase && state != FreeBase) {safe.post(); return false;}
        if (state == FreeBase && count > 0) {safe.post(); return false;}
        this->ptr = ptr; this->loc = loc; this->siz = siz;
        todo = max;
        state = BothBase;
        safe.post();
        return true;
    }
    VkFence sizeup() { // called in separate thread
        safe.wait();
        if (state != BothBase) {std::cerr << "sizeup invalid state! " << state << std::endl; exit(-1);}
        safe.post();
        if (size == todo); else {
        if (size == SizeState(0)); else unsize();
        if ((size = todo) == SizeState(0)); else {std::cout << "sizeup resize " << debug << " " << size << std::endl; resize();}}
        safe.wait();
        state = NextBase;
        safe.post();
        return setup(ptr,loc,siz);
    }
    bool push(SizeState siz) { // called in main thread
        safe.wait();
        if (state != InitBase && state != FreeBase) {safe.post(); return false;}
        if (state == FreeBase && count > 0) {safe.post(); return false;}
        todo = siz;
        state = SizeBase;
        safe.post();
        return true;
    }
    void baseres() { // called in separate thread
        safe.wait();
        if (state != SizeBase) {std::cerr << "baseres invalid state! " << state << "(" << SizeBase << ")" << " " << debug << std::endl; exit(-1);}
        safe.post();
        if (size == todo); else {
        if (size == SizeState(0)); else unsize();
        if ((size = todo) == SizeState(0)); else {std::cout << "baseres resize " << debug << " " << size << std::endl; resize();}}
        safe.wait();
        state = FreeBase;
        safe.post();
    }
    virtual void unsize() = 0;
    virtual void resize() = 0;
    bool push(void *ptr, int loc, int siz) { // called in main thread
        safe.wait();
        if (state != FreeBase || count > 0) {safe.post(); return false;}
        this->ptr = ptr; this->loc = loc; this->siz = siz;
        state = LockBase;
        safe.post();
        return true;
    }
    VkFence basesup() { // called in separate thread
        safe.wait();
        if (state != LockBase) {std::cerr << "setup invalid state!" << std::endl; exit(-1);}
        state = NextBase;
        safe.post();
        return setup(ptr,loc,siz);
    }
    virtual VkFence setup(void *ptr, int loc, int siz) = 0;
    virtual void baseups() { // called in separate thread
        safe.wait();
        if (state != NextBase) {std::cerr << "upset invalid state!" << std::endl; exit(-1);}
        safe.post();
        // if (bind) bind->advance();
        upset();
        safe.wait();
        state = FreeBase;
        safe.post();
    }
    virtual void upset() = 0;
    bool take() {
        safe.wait();
        if (state != FreeBase) {safe.post(); return false;}
        count += 1;
        safe.post();
        return true;
    }
    void give() {
        safe.wait();
        if (state != FreeBase) {std::cerr << "invalid give state!" << std::endl; exit(-1);}
        if (count <= 0) {std::cerr << "invalid free count! " << debug << std::endl; exit(-1);}
        count -= 1;
        safe.post();
    }
    bool check() {
        safe.wait();
        if (state != FreeBase) {safe.post(); return false;}
        safe.post();
        return true;
    }
    virtual VkSwapchainKHR getSwapChain() {std::cerr << "BaseState::swapChain" << std::endl; exit(-1);}
    virtual VkFramebuffer getSwapChainFramebuffer(int i) {std::cerr << "BaseState::swapChainFramebuffer" << std::endl; exit(-1);}
    virtual VkPipeline getGraphicsPipeline() {std::cerr << "BaseState::graphicsPipeline" << std::endl; exit(-1);}
    virtual VkPipelineLayout getPipelineLayout() {std::cerr << "BaseState::pipelineLayout" << std::endl; exit(-1);}
    virtual VkBuffer getBuffer() {std::cerr << "BaseState::buffer" << std::endl; exit(-1);}
    virtual int getRange() {std::cerr << "BaseState::size" << std::endl; exit(-1);}
    virtual VkImageView getTextureImageView() {std::cerr << "BaseState::textureImageView" << std::endl; exit(-1);}
    virtual VkSampler getTextureSampler() {std::cerr << "BaseState::textureSampler" << std::endl; exit(-1);}
    virtual VkDescriptorPool getDescriptorPool() {std::cerr << "BaseState::getDescriptorPool" << std::endl; exit(-1);}
    virtual VkDescriptorSetLayout getDescriptorSetLayout() {std::cerr << "BaseState::getDescriptorSetLayout" << std::endl; exit(-1);}
    virtual VkExtent2D getSwapChainExtent() {std::cerr << "BaseState::getSwapChainExtent" << std::endl; exit(-1);}
    static uint32_t findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags,
        VkPhysicalDeviceMemoryProperties memProperties) {
        for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
        if ((filter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & flags) == flags) return i;
        {std::cerr << "failed to find suitable memory type!" << std::endl; exit(-1);}
    }
    static VkCommandBuffer createCommandBuffer(VkDevice device, VkCommandPool pool) {
        VkCommandBuffer commandBuffer;
        VkCommandBufferAllocateInfo allocInfo{};
        allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
        allocInfo.commandPool = pool;
        allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
        allocInfo.commandBufferCount = (uint32_t)(1);
        if (vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer) != VK_SUCCESS)
        {std::cerr << "failed to allocate command buffers!" << std::endl; exit(-1);}
        return commandBuffer;
    }
    static VkFence createFence(VkDevice device) {
        VkFence fence;
        VkFenceCreateInfo fenceInfo{};
        fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
        fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;
        if (vkCreateFence(device, &fenceInfo, nullptr, &fence) != VK_SUCCESS)
        {std::cerr << "failed to create fence!" << std::endl; exit(-1);}
        return fence;
    }
    static VkSemaphore createSemaphore(VkDevice device) {
        VkSemaphore semaphore;
        VkSemaphoreCreateInfo semaphoreInfo{};
        semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
        if (vkCreateSemaphore(device, &semaphoreInfo, nullptr, &semaphore) != VK_SUCCESS)
        {std::cerr << "failed to create semaphore!" << std::endl; exit(-1);}
        return semaphore;
    }
    static void createImage(VkDevice device, VkPhysicalDevice physical,
        uint32_t width, uint32_t height, VkFormat format, VkImageTiling tiling, VkImageUsageFlags usage,
        VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties,
        VkImage& image, VkDeviceMemory& imageMemory) {
        VkImageCreateInfo imageInfo{};
        imageInfo.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
        imageInfo.imageType = VK_IMAGE_TYPE_2D;
        imageInfo.extent.width = width;
        imageInfo.extent.height = height;
        imageInfo.extent.depth = 1;
        imageInfo.mipLevels = 1;
        imageInfo.arrayLayers = 1;
        imageInfo.format = format;
        imageInfo.tiling = tiling;
        imageInfo.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        imageInfo.usage = usage;
        imageInfo.samples = VK_SAMPLE_COUNT_1_BIT;
        imageInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
        if (vkCreateImage(device, &imageInfo, nullptr, &image) != VK_SUCCESS)
        {std::cerr << "failed to create image!" << std::endl; exit(-1);}
        VkMemoryRequirements memRequirements;
        vkGetImageMemoryRequirements(device, image, &memRequirements);
        VkMemoryAllocateInfo allocInfo{};
        allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        allocInfo.allocationSize = memRequirements.size;
        allocInfo.memoryTypeIndex = findMemoryType(physical, memRequirements.memoryTypeBits, properties, memProperties);
        if (vkAllocateMemory(device, &allocInfo, nullptr, &imageMemory) != VK_SUCCESS)
        {std::cerr << "failed to allocate image memory!" << std::endl; exit(-1);}
        vkBindImageMemory(device, image, imageMemory, 0);
    }
    static VkImageView createImageView(VkDevice device, VkImage image, VkFormat format, VkImageAspectFlags aspectFlags) {
        VkImageViewCreateInfo viewInfo{};
        viewInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        viewInfo.image = image;
        viewInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
        viewInfo.format = format;
        viewInfo.subresourceRange.aspectMask = aspectFlags;
        viewInfo.subresourceRange.baseMipLevel = 0;
        viewInfo.subresourceRange.levelCount = 1;
        viewInfo.subresourceRange.baseArrayLayer = 0;
        viewInfo.subresourceRange.layerCount = 1;
        VkImageView imageView;
        if (vkCreateImageView(device, &viewInfo, nullptr, &imageView) != VK_SUCCESS)
        {std::cerr << "failed to create image view!" << std::endl; exit(-1);}
        return imageView;
    }
};

struct ItemState : public BaseState {
    BindState *bind;
    ItemState(const char *name, BindState *ptr) : bind(ptr) {
        sprintf(debug,"%s%s%d",bind->name,name,BindState::debug++);
    }
    void baseups() { // called in separate thread
        safe.wait();
        if (state != NextBase) {std::cerr << "baseups invalid state! " << state << " " << debug << std::endl; exit(-1);}
        safe.post();
        bind->advance();
        upset();
        safe.wait();
        state = FreeBase;
        safe.post();
    }
    static void createBuffer(VkDevice device, VkPhysicalDevice physical, VkDeviceSize size, VkBufferUsageFlags usage,
        VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties,
        VkBuffer& buffer, VkDeviceMemory& bufferMemory) {
        VkBufferCreateInfo bufferInfo{};
        bufferInfo.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
        bufferInfo.size = size;
        bufferInfo.usage = usage;
        bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
        if (vkCreateBuffer(device, &bufferInfo, nullptr, &buffer) != VK_SUCCESS)
        {std::cerr << "failed to create buffer!" << std::endl; exit(-1);}
        VkMemoryRequirements memRequirements;
        vkGetBufferMemoryRequirements(device, buffer, &memRequirements);
        VkMemoryAllocateInfo allocInfo{};
        allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        allocInfo.allocationSize = memRequirements.size;
        allocInfo.memoryTypeIndex = findMemoryType(physical, memRequirements.memoryTypeBits, properties, memProperties);
        if (vkAllocateMemory(device, &allocInfo, nullptr, &bufferMemory) != VK_SUCCESS)
        {std::cerr << "failed to allocate buffer memory!" << std::endl; exit(-1);}
        vkBindBufferMemory(device, buffer, bufferMemory, 0);
    }
};

enum ThreadEnum {
    FailThread,
    SizeThread,
    SetThread,
    BothThread,
    ThreadEnums
};
struct PushState {
    ThreadEnum tag;
    BaseState *base;
    VkFence fence;
    Center *center;
};
struct ThreadState {
    const VkDevice device;
    ChangeState *change;
    SafeState safe; SafeState wait;
    std::deque<PushState> before;
    std::deque<PushState> after;
    bool done;
    pthread_t thread;
    ThreadState(VkDevice device, ChangeState *change) :
        device(device), change(change),
        safe(1), wait(0), done(false) {
        if (pthread_create(&thread,0,separate,this) != 0)
        {std::cerr << "failed to create thread!" << std::endl; exit(-1);}
        {std::cout << "ThreadState" << std::endl;}
    }
    ~ThreadState() {std::cout << "~ThreadState" << std::endl;}
    bool push(bool pass, ThreadEnum tag, BaseState *base, Center *center) {
        PushState push;
        push.base = base; push.center = center;
        if (!pass) {
        push.tag = FailThread;
        safe.wait(); before.push_back(push); safe.post();
        return false;}
        push.tag = tag;
        safe.wait(); before.push_back(push); safe.post();
        if (wait.get() == 0) wait.post();
        return true;
    }
    bool push(BaseState *base, void *ptr, int loc, int siz, SizeState max, Center *center) {
        return push(base->push(ptr,loc,siz,max),BothThread,base,center);
    }
    bool push(BaseState *base, void *ptr, int loc, int siz, Center *center) {
        return push(base->push(ptr,loc,siz),SetThread,base,center);
    }
    bool push(BaseState *base, SizeState size, Center *center) {
        return push(base->push(size),SizeThread,base,center);
    }
    void push() {
        safe.wait();
        done = true;
        safe.post();
        if (wait.get() == 0) wait.post();
        pthread_join(thread,0);
    }
    bool stage() {
        while (1) {while (1) {safe.wait();
        if (before.empty()) {safe.post(); break;}
        PushState push = before.front(); before.pop_front(); safe.post();
        switch (push.tag) {default: {std::cerr << "invalid push tag!" << std::endl; exit(-1);}
        break; case(FailThread): push.fence = VK_NULL_HANDLE; push.base = 0;
        break; case(SizeThread): push.fence = VK_NULL_HANDLE; push.base->baseres(); push.base = 0;
        break; case(SetThread): push.fence = push.base->basesup();
        break; case(BothThread): push.fence = push.base->sizeup();}
        after.push_back(push);}
        safe.wait(); if (!after.empty()) {safe.post(); break;}
        if (before.empty()) {
        if (done) {safe.post(); return false;}
        else {safe.post(); wait.wait();}}}
        return true;
    }
    static void *separate(void *ptr) {
        struct ThreadState *arg = (ThreadState*)ptr;
        while (arg->stage()) {
        if (arg->after.empty()) {std::cerr << "separate empty after!" << std::endl; exit(-1);}
        PushState push = arg->after.front(); arg->after.pop_front();
        if (push.fence != VK_NULL_HANDLE) {
        VkResult result = vkWaitForFences(arg->device,1,&push.fence,VK_FALSE,NANOSECONDS);
        if (result != VK_SUCCESS) {std::cerr << "cannot wait for fence!" << std::endl; exit(-1);}}
        if (push.base) push.base->baseups();
        int pass = (push.tag == FailThread ? 0 : 1);
        arg->change->done(pass, push.center);
        arg->change->async(FenceAsync);}
        vkDeviceWaitIdle(arg->device);
        return 0;
    }
};

struct SwapState : public BaseState {
    GLFWwindow* window;
    const VkSurfaceKHR surface;
    const VkPhysicalDevice physical;
    const VkDevice device;
    const VkSurfaceFormatKHR surfaceFormat;
    const VkPresentModeKHR presentMode;
    const uint32_t graphicsFamily;
    const uint32_t presentFamily;
    const VkFormat imageFormat;
    const VkFormat depthFormat;
    const VkRenderPass renderPass;
    const VkPhysicalDeviceMemoryProperties memProperties;
    VkSwapchainKHR swapChain;
    // TODO change vectors to number initialized and array of maximum supported size
    std::vector<VkImage> swapChainImages;
    std::vector<VkImageView> swapChainImageViews;
    VkImage depthImage;
    VkDeviceMemory depthImageMemory;
    VkImageView depthImageView;
    std::vector<VkFramebuffer> swapChainFramebuffers;
    SwapState() :
        BaseState("SwapState"),
        window(BindState::window),
        surface(BindState::surface),
        physical(BindState::physical),
        device(BindState::device),
        surfaceFormat(BindState::surfaceFormat),
        presentMode(BindState::presentMode),
        graphicsFamily(BindState::graphicsFamily),
        presentFamily(BindState::presentFamily),
        imageFormat(BindState::imageFormat),
        depthFormat(BindState::depthFormat),
        renderPass(BindState::renderPass),
        memProperties(BindState::memProperties)
        {std::cout << "SwapState" << std::endl;}
    ~SwapState() {push(0); baseres(); std::cout << "~SwapState" << std::endl;}
    VkSwapchainKHR getSwapChain() {return swapChain;}
    VkFramebuffer getSwapChainFramebuffer(int i) {return swapChainFramebuffers[i];}
    VkExtent2D getSwapChainExtent() {return size.capabilities.currentExtent;}
    void resize() {
        swapChain = createSwapChain(surface,device,getSwapChainExtent(),surfaceFormat,presentMode,
            size.capabilities,graphicsFamily,presentFamily);
        createSwapChainImages(device,swapChain,swapChainImages);
        swapChainImageViews.resize(swapChainImages.size());
        for (int i = 0; i < swapChainImages.size(); i++)
        swapChainImageViews[i] = createImageView(device, swapChainImages[i], imageFormat, VK_IMAGE_ASPECT_COLOR_BIT);
        createImage(device, physical, getSwapChainExtent().width, getSwapChainExtent().height, depthFormat,
            VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
            memProperties,/*output*/ depthImage, depthImageMemory);
        depthImageView = createImageView(device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT);
        createFramebuffers(device,getSwapChainExtent(),renderPass,swapChainImageViews,depthImageView,swapChainFramebuffers);
    }
    void unsize() {
        int width = 0, height = 0;
        glfwGetFramebufferSize(window, &width, &height);
        while (width == 0 || height == 0) {
            glfwGetFramebufferSize(window, &width, &height);
            glfwWaitEvents();}
        vkDeviceWaitIdle(device);
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthImageMemory, nullptr);
        for (auto framebuffer : swapChainFramebuffers)
            vkDestroyFramebuffer(device, framebuffer, nullptr);
        for (auto imageView : swapChainImageViews)
            vkDestroyImageView(device, imageView, nullptr);
        vkDestroySwapchainKHR(device, swapChain, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz) {
        return VK_NULL_HANDLE;
    }
    void upset() {
    }
    static VkSwapchainKHR createSwapChain(VkSurfaceKHR surface, VkDevice device, VkExtent2D swapChainExtent,
        VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode,
        VkSurfaceCapabilitiesKHR capabilities, uint32_t graphicsFamily, uint32_t presentFamily) {
        VkSwapchainKHR swapChain;
        uint32_t imageCount = capabilities.minImageCount + 1;
        if (capabilities.maxImageCount > 0 && imageCount > capabilities.maxImageCount)
        imageCount = capabilities.maxImageCount;
        VkSwapchainCreateInfoKHR createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
        createInfo.surface = surface;
        createInfo.minImageCount = imageCount;
        createInfo.imageFormat = surfaceFormat.format;
        createInfo.imageColorSpace = surfaceFormat.colorSpace;
        createInfo.imageExtent = swapChainExtent;
        createInfo.imageArrayLayers = 1;
        createInfo.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
        uint32_t queueFamilies[] = {graphicsFamily, presentFamily};
        if (graphicsFamily != presentFamily) {
            createInfo.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
            createInfo.queueFamilyIndexCount = 2;
            createInfo.pQueueFamilyIndices = queueFamilies;}
        else createInfo.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
        createInfo.preTransform = capabilities.currentTransform;
        createInfo.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
        createInfo.presentMode = presentMode;
        createInfo.clipped = VK_TRUE;
        if (vkCreateSwapchainKHR(device, &createInfo, nullptr, &swapChain) != VK_SUCCESS)
        {std::cerr << "failed to create swap chain!" << std::endl; exit(-1);}
        return swapChain;
    }
    static void createSwapChainImages(VkDevice device, VkSwapchainKHR swapChain, std::vector<VkImage> &swapChainImages) {
        uint32_t imageCount;
        vkGetSwapchainImagesKHR(device, swapChain, &imageCount, nullptr);
        swapChainImages.resize(imageCount);
        vkGetSwapchainImagesKHR(device, swapChain, &imageCount, swapChainImages.data());
    }
    static void createFramebuffers(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
        std::vector<VkImageView> swapChainImageViews, VkImageView depthImageView,
        std::vector<VkFramebuffer> &swapChainFramebuffers) {
        swapChainFramebuffers.resize(swapChainImageViews.size());
        for (size_t i = 0; i < swapChainImageViews.size(); i++) {
        std::array<VkImageView, 2> attachments = {swapChainImageViews[i],depthImageView};
        VkFramebufferCreateInfo framebufferInfo{};
        framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        framebufferInfo.renderPass = renderPass;
        framebufferInfo.attachmentCount = static_cast<uint32_t>(attachments.size());
        framebufferInfo.pAttachments = attachments.data();
        framebufferInfo.width = swapChainExtent.width;
        framebufferInfo.height = swapChainExtent.height;
        framebufferInfo.layers = 1;
        if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &swapChainFramebuffers[i]) != VK_SUCCESS)
        {std::cerr << "failed to create framebuffer!" << std::endl; exit(-1);}}
    }
};

struct PipelineState : public BaseState {
    const VkDevice device;
    Micro micro;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorSetLayout;
    VkPipelineLayout pipelineLayout;
    VkPipeline graphicsPipeline;
    const char *name() {return "PipelineState";}
    PipelineState() :
        BaseState("PipelineState"),
        device(BindState::device), micro((Micro)BindState::micro++),
        descriptorPool(createDescriptorPool(BindState::device,BindState::frames)),
        descriptorSetLayout(createDescriptorSetLayout(BindState::device,micro)),
        pipelineLayout(createPipelineLayout(BindState::device,descriptorSetLayout)),
        graphicsPipeline(createGraphicsPipeline(BindState::device,BindState::renderPass,pipelineLayout,micro))
        {std::cout << "PipelineState" << std::endl;}
    ~PipelineState() {
        std::cout << "~PipelineState" << std::endl;
        vkDestroyPipeline(device, graphicsPipeline, nullptr);
        vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
        vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);
        vkDestroyDescriptorPool(device, descriptorPool, nullptr);
    }
    VkPipeline getGraphicsPipeline() {return graphicsPipeline;}
    VkPipelineLayout getPipelineLayout() {return pipelineLayout;}
    VkDescriptorPool getDescriptorPool() {return descriptorPool;}
    VkDescriptorSetLayout getDescriptorSetLayout() {return descriptorSetLayout;}
    void resize() {
    }
    void unsize() {
    }
    VkFence setup(void *ptr, int loc, int siz) {
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset() {
    }
    static VkDescriptorPool createDescriptorPool(VkDevice device, int frames) {
        VkDescriptorPool descriptorPool;
        std::array<VkDescriptorPoolSize, 2> poolSizes{};
        poolSizes[0].type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
        poolSizes[0].descriptorCount = static_cast<uint32_t>(frames);
        poolSizes[1].type = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        poolSizes[1].descriptorCount = static_cast<uint32_t>(frames);
        VkDescriptorPoolCreateInfo descriptorPoolInfo{};
        descriptorPoolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
        descriptorPoolInfo.flags = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT;
        descriptorPoolInfo.poolSizeCount = static_cast<uint32_t>(poolSizes.size());
        descriptorPoolInfo.pPoolSizes = poolSizes.data();
        descriptorPoolInfo.maxSets = static_cast<uint32_t>(frames);
        if (vkCreateDescriptorPool(device, &descriptorPoolInfo, nullptr, &descriptorPool) != VK_SUCCESS)
        {std::cerr << "failed to create descriptor pool!" << std::endl; exit(-1);}
        return descriptorPool;
    }
    static VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, Micro micro) {
        VkDescriptorSetLayout descriptorSetLayout;
        VkDescriptorSetLayoutBinding uboLayoutBinding{};
        uboLayoutBinding.binding = 0;
        uboLayoutBinding.descriptorCount = 1;
        uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
        uboLayoutBinding.pImmutableSamplers = nullptr;
        uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
        VkDescriptorSetLayoutBinding samplerLayoutBinding{};
        samplerLayoutBinding.binding = 1;
        samplerLayoutBinding.descriptorCount = 1;
        samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        samplerLayoutBinding.pImmutableSamplers = nullptr;
        samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
        //  TODO number and type of bindings depends on micro
        //  TODO add constant functions of micro to type.h
        //  TODO change array to vector
        std::array<VkDescriptorSetLayoutBinding, 2> bindings = {uboLayoutBinding, samplerLayoutBinding};
        VkDescriptorSetLayoutCreateInfo layoutInfo{};
        layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
        layoutInfo.bindingCount = static_cast<uint32_t>(bindings.size());
        layoutInfo.pBindings = bindings.data();
        if (vkCreateDescriptorSetLayout(device, &layoutInfo, nullptr, &descriptorSetLayout) != VK_SUCCESS)
        {std::cerr << "failed to create descriptor set layout!" << std::endl; exit(-1);}
        return descriptorSetLayout;
    }
    static VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout) {
        VkPipelineLayout pipelineLayout;
        VkPipelineLayoutCreateInfo pipelineLayoutInfo{};
        pipelineLayoutInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
        pipelineLayoutInfo.setLayoutCount = 1;
        pipelineLayoutInfo.pSetLayouts = &descriptorSetLayout;
        if (vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout) != VK_SUCCESS)
        {std::cerr << "failed to create pipeline layout!" << std::endl; exit(-1);}
        return pipelineLayout;
    }
    static std::vector<char> readFile(const std::string& filename) {
        std::ifstream file(filename, std::ios::ate | std::ios::binary);
        if (!file.is_open()) {std::cerr << "failed to open shader: " << filename << std::endl; exit(-1);}
        size_t fileSize = (size_t) file.tellg();
        std::vector<char> buffer(fileSize);
        file.seekg(0);
        file.read(buffer.data(), fileSize);
        file.close();
        return buffer;
    }
    static VkShaderModule createShaderModule(VkDevice device, const std::vector<char>& code) {
        VkShaderModuleCreateInfo createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
        createInfo.codeSize = code.size();
        createInfo.pCode = reinterpret_cast<const uint32_t*>(code.data());
        VkShaderModule shaderModule;
        if (vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule) != VK_SUCCESS)
        {std::cerr << "failed to create shader module!" << std::endl; exit(-1);}
        return shaderModule;
    }
    static VkPipeline createGraphicsPipeline(VkDevice device, VkRenderPass renderPass,
        VkPipelineLayout pipelineLayout, Micro micro) {
        VkPipeline graphicsPipeline;
        auto vertShaderCode = readFile(VertexFile__Micro__Str(micro));
        auto fragShaderCode = readFile(FragmentFile__Micro__Str(micro));
        VkShaderModule vertShaderModule = createShaderModule(device,vertShaderCode);
        VkShaderModule fragShaderModule = createShaderModule(device,fragShaderCode);
        VkPipelineShaderStageCreateInfo vertShaderStageInfo{};
        vertShaderStageInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
        vertShaderStageInfo.stage = VK_SHADER_STAGE_VERTEX_BIT;
        vertShaderStageInfo.module = vertShaderModule;
        vertShaderStageInfo.pName = "main";
        VkPipelineShaderStageCreateInfo fragShaderStageInfo{};
        fragShaderStageInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
        fragShaderStageInfo.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
        fragShaderStageInfo.module = fragShaderModule;
        fragShaderStageInfo.pName = "main";
        VkPipelineShaderStageCreateInfo shaderStages[] = {vertShaderStageInfo, fragShaderStageInfo};
        VkPipelineVertexInputStateCreateInfo vertexInputInfo{};
        vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
        std::vector<VkVertexInputBindingDescription> bindingDescriptions; {
            VkVertexInputBindingDescription bindingDescription{};
            bindingDescription.binding = 0;
            bindingDescription.stride = VertexStride__Micro__Int(micro);
            bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
            bindingDescriptions.push_back(bindingDescription);}
        std::vector<VkVertexInputAttributeDescription> attributeDescriptions;
        for (int i = 0; VertexFormat__Micro__Int__Int(micro)(i); i++) {
            VkVertexInputAttributeDescription attributeDescription{};
            attributeDescription.binding = 0;
            attributeDescription.location = i;
            switch (VertexFormat__Micro__Int__Int(micro)(i)) {
            default: {std::cerr << "invalid vertex format!" << std::endl; exit(-1);}
            case (109): attributeDescription.format = VK_FORMAT_R32G32B32A32_SFLOAT; break;
            case (107): attributeDescription.format = VK_FORMAT_R32G32B32A32_UINT; break;}
            attributeDescription.offset = VertexOffset__Micro__Int__Int(micro)(i);
            attributeDescriptions.push_back(attributeDescription);}
        vertexInputInfo.vertexBindingDescriptionCount = static_cast<uint32_t>(bindingDescriptions.size());
        vertexInputInfo.vertexAttributeDescriptionCount = static_cast<uint32_t>(attributeDescriptions.size());
        vertexInputInfo.pVertexBindingDescriptions = bindingDescriptions.data();
        vertexInputInfo.pVertexAttributeDescriptions = attributeDescriptions.data();
        VkPipelineInputAssemblyStateCreateInfo inputAssembly{};
        inputAssembly.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
        inputAssembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
        inputAssembly.primitiveRestartEnable = VK_FALSE;
        VkPipelineViewportStateCreateInfo viewportState{};
        viewportState.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
        viewportState.viewportCount = 1;
        viewportState.scissorCount = 1;
        VkPipelineRasterizationStateCreateInfo rasterizer{};
        rasterizer.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
        rasterizer.depthClampEnable = VK_FALSE;
        rasterizer.rasterizerDiscardEnable = VK_FALSE;
        rasterizer.polygonMode = VK_POLYGON_MODE_FILL;
        rasterizer.lineWidth = 1.0f;
        rasterizer.cullMode = VK_CULL_MODE_BACK_BIT;
        rasterizer.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
        rasterizer.depthBiasEnable = VK_FALSE;
        VkPipelineMultisampleStateCreateInfo multisampling{};
        multisampling.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
        multisampling.sampleShadingEnable = VK_FALSE;
        multisampling.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
        VkPipelineDepthStencilStateCreateInfo depthStencil{};
        depthStencil.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
        depthStencil.depthTestEnable = VK_TRUE;
        depthStencil.depthWriteEnable = VK_TRUE;
        depthStencil.depthCompareOp = VK_COMPARE_OP_LESS;
        depthStencil.depthBoundsTestEnable = VK_FALSE;
        depthStencil.stencilTestEnable = VK_FALSE;
        VkPipelineColorBlendAttachmentState colorBlendAttachment{};
        colorBlendAttachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
            VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
        colorBlendAttachment.blendEnable = VK_FALSE;
        VkPipelineColorBlendStateCreateInfo colorBlending{};
        colorBlending.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
        colorBlending.logicOpEnable = VK_FALSE;
        colorBlending.logicOp = VK_LOGIC_OP_COPY;
        colorBlending.attachmentCount = 1;
        colorBlending.pAttachments = &colorBlendAttachment;
        colorBlending.blendConstants[0] = 0.0f;
        colorBlending.blendConstants[1] = 0.0f;
        colorBlending.blendConstants[2] = 0.0f;
        colorBlending.blendConstants[3] = 0.0f;
        std::vector<VkDynamicState> dynamicStates = {VK_DYNAMIC_STATE_VIEWPORT,VK_DYNAMIC_STATE_SCISSOR};
        VkPipelineDynamicStateCreateInfo dynamicState{};
        dynamicState.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
        dynamicState.dynamicStateCount = static_cast<uint32_t>(dynamicStates.size());
        dynamicState.pDynamicStates = dynamicStates.data();
        VkGraphicsPipelineCreateInfo pipelineInfo{};
        pipelineInfo.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
        pipelineInfo.stageCount = 2;
        pipelineInfo.pStages = shaderStages;
        pipelineInfo.pVertexInputState = &vertexInputInfo;
        pipelineInfo.pInputAssemblyState = &inputAssembly;
        pipelineInfo.pViewportState = &viewportState;
        pipelineInfo.pRasterizationState = &rasterizer;
        pipelineInfo.pMultisampleState = &multisampling;
        pipelineInfo.pDepthStencilState = &depthStencil;
        pipelineInfo.pColorBlendState = &colorBlending;
        pipelineInfo.pDynamicState = &dynamicState;
        pipelineInfo.layout = pipelineLayout;
        pipelineInfo.renderPass = renderPass;
        pipelineInfo.subpass = 0;
        pipelineInfo.basePipelineHandle = VK_NULL_HANDLE;
        if (vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &graphicsPipeline) != VK_SUCCESS)
        {std::cerr << "failed to create graphics pipeline!" << std::endl; exit(-1);}
        vkDestroyShaderModule(device, fragShaderModule, nullptr);
        vkDestroyShaderModule(device, vertShaderModule, nullptr);
        return graphicsPipeline;
    }
};

struct UniformState : public ItemState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceMemoryProperties memProperties;
    ChangeState *change;
    VkBuffer buffer;
    VkDeviceMemory memory;
    void* mapped;
    UniformState() :
        ItemState("UniformState",BindState::self),
        device(BindState::device), physical(BindState::physical),
        memProperties(BindState::memProperties),
        change(BindState::change)
        {std::cout << "UniformState" << std::endl;}
    ~UniformState() {push(0); baseres(); std::cout << "~UniformState" << std::endl;}
    VkBuffer getBuffer() {return buffer;}
    int getRange() {return size.size;}
    void resize() {
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, buffer, memory);
        vkMapMemory(device, memory, 0, bufferSize, 0, &mapped);
    }
    void unsize() {
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz) {
        if (loc < 0 || siz < 0 || loc+siz > size.size)
        {std::cerr << "invalid uniform size!" << std::endl; exit(-1);}
        memcpy(mapped, (void*)((char*)ptr+loc), siz);
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset() {
    }
};

struct BufferState : public ItemState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const VkPhysicalDeviceMemoryProperties memProperties;
    const VkBufferUsageFlags flags;
    VkBuffer buffer;
    VkDeviceMemory bufferMemory;
    VkCommandBuffer commandBuffer;
    VkFence fence;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    BufferState() :
        ItemState("BufferState",BindState::self),
        device(BindState::device), physical(BindState::physical),
        graphics(BindState::graphics), commandPool(BindState::commandPool),
        memProperties(BindState::memProperties), flags(BindState::flags)
        {std::cout << "BufferState" << std::endl;}
    ~BufferState() {push(0); baseres(); std::cout << "~BufferState" << std::endl;}
    VkBuffer getBuffer() {return buffer;}
    int getRange() {return size.size;}
    void resize() {
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | flags,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, buffer, bufferMemory);
        commandBuffer = createCommandBuffer(device,commandPool);
        fence = createFence(device);
    }
    void unsize() {
        vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
        if (fence != VK_NULL_HANDLE) vkDestroyFence(device, fence, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeMemory(device, bufferMemory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz) {
        if (loc < 0 || siz < 0 || loc+siz > size.size)
        {std::cerr << "invalid buffer size!" << std::endl; exit(-1);}
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, stagingBuffer, stagingBufferMemory);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, &data);
        memcpy((void*)((char*)data+loc),ptr,siz);
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetFences(device, 1, &fence);
        copyBuffer(device, graphics, stagingBuffer, buffer, bufferSize, commandBuffer, fence);
        return fence;
    }
    void upset() {
        vkUnmapMemory(device, stagingBufferMemory);
        vkFreeMemory(device, stagingBufferMemory, nullptr);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
    }
    static void copyBuffer(VkDevice device, VkQueue graphics, VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size,
        VkCommandBuffer commandBuffer, VkFence fence) {
        VkCommandBufferBeginInfo beginInfo{};
        beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        beginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
        vkBeginCommandBuffer(commandBuffer, &beginInfo);
        VkBufferCopy copyRegion{};
        copyRegion.size = size;
        vkCmdCopyBuffer(commandBuffer, srcBuffer, dstBuffer, 1, &copyRegion);
        vkEndCommandBuffer(commandBuffer);
        VkSubmitInfo submitInfo{};
        submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        submitInfo.commandBufferCount = 1;
        submitInfo.pCommandBuffers = &commandBuffer;
        vkQueueSubmit(graphics, 1, &submitInfo, fence);
        if (fence == VK_NULL_HANDLE) vkQueueWaitIdle(graphics);
    }
};

struct TextureState : public ItemState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceProperties properties;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const VkPhysicalDeviceMemoryProperties memProperties;
    ChangeState *change;
    VkImage textureImage;
    VkDeviceMemory textureImageMemory;
    VkImageView textureImageView;
    VkSampler textureSampler;
    VkCommandBuffer beforeBuffer;
    VkCommandBuffer commandBuffer;
    VkCommandBuffer afterBuffer;
    VkSemaphore beforeSemaphore;
    VkSemaphore afterSemaphore;
    VkFence fence;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    TextureState() :
        ItemState("TextureState",BindState::self),
        device(BindState::device), physical(BindState::physical),
        properties(BindState::properties), graphics(BindState::graphics), commandPool(BindState::commandPool),
        memProperties(BindState::memProperties), change(BindState::change)
        {std::cout << "TextureState" << std::endl;}
    ~TextureState() {push(0); baseres(); std::cout << "~TextureState" << std::endl;}
    VkImageView getTextureImageView() {return textureImageView;}
    VkSampler getTextureSampler() {return textureSampler;}
    void resize() {
        int texWidth = size.extent.width;
        int texHeight = size.extent.height;
        createImage(device, physical, texWidth, texHeight, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_TILING_OPTIMAL,
        VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
        memProperties, /*output*/ textureImage, textureImageMemory);
        textureImageView = createImageView(device, textureImage, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_ASPECT_COLOR_BIT);
        textureSampler = createTextureSampler(device,properties);
        beforeBuffer = createCommandBuffer(device,commandPool);
        commandBuffer = createCommandBuffer(device,commandPool);
        afterBuffer = createCommandBuffer(device,commandPool);
        beforeSemaphore = createSemaphore(device);
        afterSemaphore = createSemaphore(device);
        fence = createFence(device);
    }
    void unsize() {
        vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
        if (afterSemaphore != VK_NULL_HANDLE) vkDestroySemaphore(device, afterSemaphore, nullptr);
        if (beforeSemaphore != VK_NULL_HANDLE) vkDestroySemaphore(device, beforeSemaphore, nullptr);
        if (fence != VK_NULL_HANDLE) vkDestroyFence(device, fence, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &afterBuffer);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeCommandBuffers(device, commandPool, 1, &beforeBuffer);
        vkDestroySampler(device, textureSampler, nullptr);
        vkDestroyImageView(device, textureImageView, nullptr);
        vkDestroyImage(device, textureImage, nullptr);
        vkFreeMemory(device, textureImageMemory, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz) {
        int texWidth = size.extent.width;
        int texHeight = size.extent.height;
        VkDeviceSize imageSize = texWidth * texHeight * 4; // TODO think of way to write to portions of texture
        createBuffer(device, physical, imageSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, memProperties,
            stagingBuffer, stagingBufferMemory);
        void* data;
        vkMapMemory(device, stagingBufferMemory, 0, imageSize, 0, &data);
        memcpy(data, ptr, static_cast<size_t>(imageSize));
        vkResetCommandBuffer(beforeBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetCommandBuffer(afterBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetFences(device, 1, &fence);
        copyTextureImage(device, graphics, memProperties, textureImage, texWidth, texHeight,
            beforeSemaphore, afterSemaphore, fence,
            stagingBuffer, beforeBuffer, commandBuffer, afterBuffer);
        return fence;
    }
    void upset() {
        vkUnmapMemory(device, stagingBufferMemory);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
        vkFreeMemory(device, stagingBufferMemory, nullptr);
    }
    static VkSampler createTextureSampler(VkDevice device, VkPhysicalDeviceProperties properties) {
        VkSampler textureSampler;
        VkSamplerCreateInfo samplerInfo{};
        samplerInfo.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
        samplerInfo.magFilter = VK_FILTER_LINEAR;
        samplerInfo.minFilter = VK_FILTER_LINEAR;
        samplerInfo.addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT;
        samplerInfo.addressModeV = VK_SAMPLER_ADDRESS_MODE_REPEAT;
        samplerInfo.addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT;
        samplerInfo.anisotropyEnable = VK_TRUE;
        samplerInfo.maxAnisotropy = properties.limits.maxSamplerAnisotropy;
        samplerInfo.borderColor = VK_BORDER_COLOR_INT_OPAQUE_BLACK;
        samplerInfo.unnormalizedCoordinates = VK_FALSE;
        samplerInfo.compareEnable = VK_FALSE;
        samplerInfo.compareOp = VK_COMPARE_OP_ALWAYS;
        samplerInfo.mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR;
        if (vkCreateSampler(device, &samplerInfo, nullptr, &textureSampler) != VK_SUCCESS)
        {std::cerr << "failed to create texture sampler!" << std::endl; exit(-1);}
        return textureSampler;
    }
    static void transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image,
        VkSemaphore semaphoreIn, VkSemaphore semaphoreOut, VkFence fenceOut,
        VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout) {
        VkCommandBufferBeginInfo beginInfo{};
        beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        beginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
        vkBeginCommandBuffer(commandBuffer, &beginInfo);
        VkImageMemoryBarrier barrier{};
        barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
        barrier.oldLayout = oldLayout;
        barrier.newLayout = newLayout;
        barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
        barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
        barrier.image = image;
        barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        barrier.subresourceRange.baseMipLevel = 0;
        barrier.subresourceRange.levelCount = 1;
        barrier.subresourceRange.baseArrayLayer = 0;
        barrier.subresourceRange.layerCount = 1;
        VkPipelineStageFlags sourceStage;
        VkPipelineStageFlags destinationStage;
        if (oldLayout == VK_IMAGE_LAYOUT_UNDEFINED && newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
            barrier.srcAccessMask = 0;
            barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
            sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
            destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        } else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL && newLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) {
            barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
            barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
            sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
            destinationStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
        } else {std::cerr << "unsupported layout transition!" << std::endl; exit(-1);}
        vkCmdPipelineBarrier(commandBuffer, sourceStage, destinationStage,
            0, 0, nullptr, 0, nullptr, 1, &barrier);
        vkEndCommandBuffer(commandBuffer);
        VkSubmitInfo submitInfo{};
        submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore waitSemaphores[] = {semaphoreIn};
        VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_ALL_COMMANDS_BIT};
        if (semaphoreIn != VK_NULL_HANDLE) {
        submitInfo.waitSemaphoreCount = 1;
        submitInfo.pWaitSemaphores = waitSemaphores;
        submitInfo.pWaitDstStageMask = waitStages;}
        submitInfo.commandBufferCount = 1;
        submitInfo.pCommandBuffers = &commandBuffer;
        VkSemaphore signalSemaphores[] = {semaphoreOut};
        if (semaphoreOut != VK_NULL_HANDLE) {
        submitInfo.signalSemaphoreCount = 1;
        submitInfo.pSignalSemaphores = signalSemaphores;}
        vkQueueSubmit(graphics, 1, &submitInfo, fenceOut);
        if (semaphoreOut == VK_NULL_HANDLE) vkQueueWaitIdle(graphics);
    }
    static void copyTextureImage(VkDevice device, VkQueue graphics,
        VkPhysicalDeviceMemoryProperties memProperties, VkImage textureImage, int texWidth, int texHeight,
        VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore, VkFence fence, VkBuffer stagingBuffer,
        VkCommandBuffer beforeBuffer, VkCommandBuffer commandBuffer, VkCommandBuffer afterBuffer) {
        transitionImageLayout(device, graphics, beforeBuffer, textureImage,
            VK_NULL_HANDLE, beforeSemaphore, VK_NULL_HANDLE,
            VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
        VkCommandBufferBeginInfo beginInfo{};
        beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        beginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
        vkBeginCommandBuffer(commandBuffer, &beginInfo);
        VkBufferImageCopy region{};
        region.bufferOffset = 0;
        region.bufferRowLength = 0;
        region.bufferImageHeight = 0;
        region.imageSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        region.imageSubresource.mipLevel = 0;
        region.imageSubresource.baseArrayLayer = 0;
        region.imageSubresource.layerCount = 1;
        region.imageOffset = {0, 0, 0};
        region.imageExtent = {static_cast<uint32_t>(texWidth), static_cast<uint32_t>(texHeight), 1};
        vkCmdCopyBufferToImage(commandBuffer, stagingBuffer, textureImage, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);
        vkEndCommandBuffer(commandBuffer);
        VkSubmitInfo submitInfo{};
        submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore waitSemaphores[] = {beforeSemaphore};
        VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_ALL_COMMANDS_BIT};
        if (beforeSemaphore != VK_NULL_HANDLE) {
        submitInfo.waitSemaphoreCount = 1;
        submitInfo.pWaitSemaphores = waitSemaphores;
        submitInfo.pWaitDstStageMask = waitStages;}
        submitInfo.commandBufferCount = 1;
        submitInfo.pCommandBuffers = &commandBuffer;
        VkSemaphore signalSemaphores[] = {afterSemaphore};
        if (afterSemaphore != VK_NULL_HANDLE) {
        submitInfo.signalSemaphoreCount = 1;
        submitInfo.pSignalSemaphores = signalSemaphores;}
        vkQueueSubmit(graphics, 1, &submitInfo, VK_NULL_HANDLE);
        transitionImageLayout(device, graphics, afterBuffer, textureImage,
            afterSemaphore, VK_NULL_HANDLE, fence,
            VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
    }
};

struct DrawState : public BaseState {
    const VkDevice device;
    const VkRenderPass renderPass;
    const VkQueue graphics;
    const VkQueue present;
    const VkCommandPool commandPool;
    const int frames;
    ChangeState *change;
    VkCommandBuffer commandBuffer;
    VkSemaphore beforeSemaphore;
    VkSemaphore afterSemaphore;
    VkFence fence;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorLayout;
    VkDescriptorSet descriptorSet;
    InitState<BaseState *, ConstState<BaseState *>, BindEnums+Memorys> bufptr;
    InitState<int, ConstState<int>, BindEnums+Memorys> bufidx; int bufsiz;
    DrawState() :
        BaseState("DrawState"),
        device(BindState::device),
        renderPass(BindState::renderPass),
        graphics(BindState::graphics),
        present(BindState::present),
        commandPool(BindState::commandPool),
        frames(BindState::frames),
        change(BindState::change),
        bufptr(ConstState<BaseState *>((BaseState*)0)),
        bufidx(ConstState<int>(0)), bufsiz(0)
        {std::cout << "DrawState " << debug << std::endl;}
    ~DrawState() {push(0); baseres(); std::cout << "~DrawState " << debug << std::endl;}
    bool bind(BindState **ary, int siz) {
        // choose latest of each in BindBase state
        if (bufsiz != 0) {std::cerr << "invalid bind state!" << std::endl; exit(-1);}
        for (int i = 0; i < siz; i++) {
        bufidx[i] = ary[i]->index();
        bufptr[bufidx[i]] = ary[i]->buffer();
        if (!bufptr[bufidx[i]]->take()) break;
        bufsiz++;}
        return (bufsiz == siz);
    }
    void free() {
        for (int i = 0; i < bufsiz; i++)
        bufptr[bufidx[i]]->give();
        bufsiz = 0;
    }
    BaseState *get(BindEnum typ, Memory mem) {
        int index = BindState::check(typ,mem);
        if (!bufptr[index]) {std::cerr << "invalid bind!" << typ << " " << mem << std::endl; exit(-1);}
        return bufptr[index];
    }
    void resize() {
        descriptorPool = get(PipelineBind,Memorys)->getDescriptorPool();
        descriptorLayout = get(PipelineBind,Memorys)->getDescriptorSetLayout(); free();
        descriptorSet = createDescriptorSet(device,descriptorPool,descriptorLayout,frames);
        commandBuffer = createCommandBuffer(device,commandPool);
        beforeSemaphore = createSemaphore(device);
        afterSemaphore = createSemaphore(device);
        fence = createFence(device);
    }
    void unsize() {
        vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
        if (afterSemaphore != VK_NULL_HANDLE) vkDestroySemaphore(device, afterSemaphore, nullptr);
        if (beforeSemaphore != VK_NULL_HANDLE) vkDestroySemaphore(device, beforeSemaphore, nullptr);
        if (fence != VK_NULL_HANDLE) vkDestroyFence(device, fence, nullptr);
        vkFreeDescriptorSets(device,descriptorPool,1,&descriptorSet);
    }
    VkFence setup(void *ptr, int loc, int siz) {
        if (size == SizeState(MicroTest)) {
            VkExtent2D swapChainExtent = get(SwapBind,Memorys)->getSwapChainExtent();
            uint32_t imageIndex;
            VkResult result = vkAcquireNextImageKHR(device, get(SwapBind,Memorys)->getSwapChain(),
                UINT64_MAX, beforeSemaphore, VK_NULL_HANDLE, &imageIndex);
            if (result == VK_ERROR_OUT_OF_DATE_KHR) change->async(ResizeAsync);
            else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            {std::cerr << "failed to acquire swap chain image!" << std::endl; exit(-1);}
            vkResetFences(device, 1, &fence);
            vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
            for (int i = 0; MemoryBinding__Micro__Int__Memory(size.micro)(i) != Memorys; i++)
            switch (MemoryBinding__Micro__Int__Memory(size.micro)(i)) {
            default: {std::cerr << "unsupported update memory!" << std::endl; exit(-1);}
            break; case (Matrixz): updateUniformDescriptor(device,
                get(BindEnums,Matrixz)->getBuffer(),i,
                get(BindEnums,Matrixz)->getRange(),descriptorSet);
            break; case (Texturez): updateTextureDescriptor(device,
                get(BindEnums,Texturez)->getTextureImageView(),
                get(BindEnums,Texturez)->getTextureSampler(),i,descriptorSet);}
            recordCommandBuffer(commandBuffer,renderPass,descriptorSet,swapChainExtent,size.micro,siz,
                get(SwapBind,Memorys)->getSwapChainFramebuffer(imageIndex),
                get(PipelineBind,Memorys)->getGraphicsPipeline(), get(PipelineBind,Memorys)->getPipelineLayout(),
                get(BindEnums,Vertexz)->getBuffer(), get(IndexBind,Memorys)->getBuffer());
            if (!drawFrame(commandBuffer,graphics,present,get(SwapBind,Memorys)->getSwapChain(),imageIndex,
                ptr,loc,siz,size.micro,beforeSemaphore,afterSemaphore,fence)) change->async(ResizeAsync);
            return fence;
        }
        return VK_NULL_HANDLE;
    }
    void upset() {
        free();
    }
    static VkDescriptorSet createDescriptorSet(VkDevice device, VkDescriptorPool descriptorPool,
        VkDescriptorSetLayout descriptorSetLayout, int frames) {
        VkDescriptorSet descriptorSet;
        std::vector<VkDescriptorSetLayout> layouts(frames, descriptorSetLayout);
        VkDescriptorSetAllocateInfo allocInfo{};
        allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
        allocInfo.descriptorPool = descriptorPool;
        allocInfo.descriptorSetCount = static_cast<uint32_t>(1);
        allocInfo.pSetLayouts = layouts.data();
        if (vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet) != VK_SUCCESS)
        {std::cerr << "failed to allocate descriptor sets!" << std::endl; exit(-1);}
        return descriptorSet;
    }
    static void updateStorageDescriptor(VkDevice device, VkBuffer buffer,
        int index, int size, VkDescriptorSet descriptorSet) {
        VkDescriptorBufferInfo bufferInfo{};
        bufferInfo.buffer = buffer;
        bufferInfo.offset = 0;
        bufferInfo.range = size;
        VkWriteDescriptorSet descriptorWrite{};
        descriptorWrite.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        descriptorWrite.dstSet = descriptorSet;
        descriptorWrite.dstBinding = index;
        descriptorWrite.dstArrayElement = 0;
        descriptorWrite.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        descriptorWrite.descriptorCount = 1;
        descriptorWrite.pBufferInfo = &bufferInfo;
        VkWriteDescriptorSet descriptorWrites [] = {descriptorWrite};
        vkUpdateDescriptorSets(device, 1, descriptorWrites, 0, nullptr);\
    }
    static void updateUniformDescriptor(VkDevice device, VkBuffer buffer,
        int index, int size, VkDescriptorSet descriptorSet) {
        VkDescriptorBufferInfo bufferInfo{};
        bufferInfo.buffer = buffer;
        bufferInfo.offset = 0;
        bufferInfo.range = size;
        VkWriteDescriptorSet descriptorWrite{};
        descriptorWrite.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        descriptorWrite.dstSet = descriptorSet;
        descriptorWrite.dstBinding = index;
        descriptorWrite.dstArrayElement = 0;
        descriptorWrite.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
        descriptorWrite.descriptorCount = 1;
        descriptorWrite.pBufferInfo = &bufferInfo;
        VkWriteDescriptorSet descriptorWrites [] = {descriptorWrite};
        vkUpdateDescriptorSets(device, 1, descriptorWrites, 0, nullptr);
    }
    static void updateTextureDescriptor(VkDevice device,
        VkImageView textureImageView, VkSampler textureSampler,
        int index, VkDescriptorSet descriptorSet) {
        VkDescriptorImageInfo imageInfo{};
        imageInfo.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        imageInfo.imageView = textureImageView;
        imageInfo.sampler = textureSampler;
        VkWriteDescriptorSet descriptorWrite{};
        descriptorWrite.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        descriptorWrite.dstSet = descriptorSet;
        descriptorWrite.dstBinding = index;
        descriptorWrite.dstArrayElement = 0;
        descriptorWrite.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        descriptorWrite.descriptorCount = 1;
        descriptorWrite.pImageInfo = &imageInfo;
        VkWriteDescriptorSet descriptorWrites [] = {descriptorWrite};
        vkUpdateDescriptorSets(device, 1, descriptorWrites, 0, nullptr);
    }
    static void recordCommandBuffer(VkCommandBuffer commandBuffer, VkRenderPass renderPass,
        VkDescriptorSet descriptorSet, VkExtent2D swapChainExtent, Micro micro, uint32_t indices,
        VkFramebuffer framebuffer, VkPipeline graphicsPipeline, VkPipelineLayout pipelineLayout,
        VkBuffer vertexBuffer, VkBuffer indexBuffer) {
        VkCommandBufferBeginInfo beginInfo{};
        beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        if (vkBeginCommandBuffer(commandBuffer, &beginInfo) != VK_SUCCESS)
        {std::cerr << "failed to begin recording command buffer!" << std::endl; exit(-1);}
        VkRenderPassBeginInfo renderPassInfo{};
        renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        renderPassInfo.renderPass = renderPass;
        renderPassInfo.framebuffer = framebuffer;
        renderPassInfo.renderArea.offset = {0, 0};
        renderPassInfo.renderArea.extent = swapChainExtent;
        std::array<VkClearValue, 2> clearValues{};
        clearValues[0].color = {{0.0f, 0.0f, 0.0f, 1.0f}};
        clearValues[1].depthStencil = {1.0f, 0};
        renderPassInfo.clearValueCount = static_cast<uint32_t>(clearValues.size());
        renderPassInfo.pClearValues = clearValues.data();
        vkCmdBeginRenderPass(commandBuffer, &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, graphicsPipeline);
        VkViewport viewport{};
        viewport.x = 0.0f;
        viewport.y = 0.0f;
        viewport.width = (float) swapChainExtent.width;
        viewport.height = (float) swapChainExtent.height;
        viewport.minDepth = 0.0f;
        viewport.maxDepth = 1.0f;
        vkCmdSetViewport(commandBuffer, 0, 1, &viewport);
        VkRect2D scissor{};
        scissor.offset = {0, 0};
        scissor.extent = swapChainExtent;
        vkCmdSetScissor(commandBuffer, 0, 1, &scissor);
        VkBuffer buffers[] = {vertexBuffer};
        VkDeviceSize offsets[] = {0};
        vkCmdBindVertexBuffers(commandBuffer, 0, 1, buffers, offsets); // TODO depends on micro
        vkCmdBindIndexBuffer(commandBuffer, indexBuffer, 0, VK_INDEX_TYPE_UINT16); // TODO depends on micro
        vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
        vkCmdDrawIndexed(commandBuffer, indices, 1, 0, 0, 0);
        vkCmdEndRenderPass(commandBuffer);
        if (vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
        {std::cerr << "failed to record command buffer!" << std::endl; exit(-1);}
    }
    static bool drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, VkQueue present,
        VkSwapchainKHR swapChain, uint32_t imageIndex, void *ptr, int loc, int siz, Micro micro,
        VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore, VkFence fence) {
        VkSubmitInfo submitInfo{};
        submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore waitSemaphores[] = {beforeSemaphore};
        VkCommandBuffer commandBuffers[] = {commandBuffer};
        VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
        submitInfo.waitSemaphoreCount = 1;
        submitInfo.pWaitSemaphores = waitSemaphores;
        submitInfo.pWaitDstStageMask = waitStages;
        submitInfo.commandBufferCount = 1;
        submitInfo.pCommandBuffers = &commandBuffer;
        VkSemaphore signalSemaphores[] = {afterSemaphore};
        submitInfo.signalSemaphoreCount = 1;
        submitInfo.pSignalSemaphores = signalSemaphores;
        if (vkQueueSubmit(graphics, 1, &submitInfo, fence) != VK_SUCCESS)
        {std::cerr << "failed to submit draw command buffer!" << std::endl; exit(-1);}
        VkPresentInfoKHR presentInfo{};
        presentInfo.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
        presentInfo.waitSemaphoreCount = 1;
        presentInfo.pWaitSemaphores = signalSemaphores;
        VkSwapchainKHR swapChains[] = {swapChain};
        uint32_t imageIndices[] = {imageIndex};
        presentInfo.swapchainCount = 1;
        presentInfo.pSwapchains = swapChains;
        presentInfo.pImageIndices = imageIndices;
        VkResult result = vkQueuePresentKHR(present, &presentInfo);
        if (result != VK_ERROR_OUT_OF_DATE_KHR && result != VK_SUBOPTIMAL_KHR && result != VK_SUCCESS)
        {std::cerr << "failed to present swap chain image!" << std::endl; exit(-1);}
        return (result == VK_SUCCESS);
    }
};

struct MainState {
    static const int frames = 2;
    ChangeState changeState;
    WindowState windowState;
    VulkanState vulkanState;
    PhysicalState physicalState;
    LogicalState logicalState;
    ThreadState threadState;
    ArrayState<SwapState,1> swapState;
    ArrayState<PipelineState,Micros> pipelineState;
    ArrayState<UniformState,frames> matrixState;
    ArrayState<BufferState,frames> vertexState;
    ArrayState<BufferState,frames> indexState;
    ArrayState<TextureState,frames> textureState;
    ArrayState<DrawState,frames> drawState;
    SizeState sizeState;
    MainState() :
        changeState(this),
        windowState(&changeState),
        vulkanState(windowState.window),
        physicalState(vulkanState.instance,vulkanState.surface),
        logicalState(physicalState.device,physicalState.graphicsFamily,
            physicalState.presentFamily,physicalState.surfaceFormat,
            vulkanState.validationLayers,physicalState.deviceExtensions),
        threadState(logicalState.device,&changeState),
        swapState("SwapBind",SwapBind,Memorys,
            windowState.window,vulkanState.surface,physicalState.device,logicalState.device,
            physicalState.surfaceFormat,physicalState.presentMode,
            physicalState.graphicsFamily,physicalState.presentFamily,logicalState.imageFormat,
            logicalState.depthFormat,logicalState.renderPass,physicalState.memProperties),
        pipelineState("PipelineBind",PipelineBind,Memorys,logicalState.commandPool,frames),
        matrixState("Matrixz",BindEnums,Matrixz,&changeState),
        vertexState("Vertexz",BindEnums,Vertexz,
            logicalState.graphics,logicalState.present,VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
        indexState("IndexBind",IndexBind,Memorys,VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
        textureState("Texturez",BindEnums,Texturez,physicalState.properties),
        drawState("DrawBind",DrawBind,Memorys),
        sizeState(findCapabilities(windowState.window,vulkanState.surface,physicalState.device)) {
        vulkanTest(); // TODO call changeState.loop()
    }
    static VkSurfaceCapabilitiesKHR findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device) {
        VkSurfaceCapabilitiesKHR capabilities;
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &capabilities);
        capabilities.currentExtent = chooseSwapExtent(window,capabilities);
        return capabilities;
    }
    static VkExtent2D chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities) {
        if (capabilities.currentExtent.width != std::numeric_limits<uint32_t>::max()) {
        return capabilities.currentExtent;}
        else {int width, height;
        glfwGetFramebufferSize(window, &width, &height);
        VkExtent2D actualExtent = {
            static_cast<uint32_t>(width),
            static_cast<uint32_t>(height)
        };
        actualExtent.width = std::clamp(actualExtent.width, capabilities.minImageExtent.width, capabilities.maxImageExtent.width);
        actualExtent.height = std::clamp(actualExtent.height, capabilities.minImageExtent.height, capabilities.maxImageExtent.height);
        return actualExtent;}
    }
    static void vulkanDone(int pass, Center *center) {
        if (center) switch (center->mem) {
        default: {std::cerr << "unsupported mem type! " << center->mem << std::endl; exit(-1);}
        break; case (Indexz): allocInt32(&center->ind,0); allocCenter(&center,0);
        break; case (Vertexz): for (int i = 0; i < center->siz; i++) freeVertex(&center->vtx[i]);
        allocVertex(&center->vtx,0); allocCenter(&center,0);
        break; case (Matrixz): for (int i = 0; i < center->siz; i++) freeMatrix(&center->mat[i]);
        allocMatrix(&center->mat,0); allocCenter(&center,0);
        break;case (Texturez): for (int i = 0; i < center->siz; i++) freeTexture(&center->tex[i]);
        allocTexture(&center->tex,0); allocCenter(&center,0);
        }
    }
    static void updateUniformBuffer(VkExtent2D swapChainExtent, glm::mat4 &model, glm::mat4 &view, glm::mat4 &proj) {
        static auto startTime = std::chrono::high_resolution_clock::now();
        auto currentTime = std::chrono::high_resolution_clock::now();
        float time = std::chrono::duration<float, std::chrono::seconds::period>(currentTime - startTime).count();
        model = glm::rotate(glm::mat4(1.0f), time * glm::radians(90.0f), glm::vec3(0.0f, 0.0f, 1.0f));
        view = glm::lookAt(glm::vec3(2.0f, 2.0f, 2.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));
        proj = glm::perspective(glm::radians(45.0f), swapChainExtent.width / (float) swapChainExtent.height, 0.1f, 10.0f);
        proj[1][1] *= -1;
    }
    void vulkanTest() {
        const std::vector<Vertex> vertices = {
            {{-0.5f, -0.5f, 0.0f, 0.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
            {{0.5f, -0.5f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
            {{0.5f, 0.5f, 0.0f, 0.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
            {{-0.5f, 0.5f, 0.0f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},

            {{-0.5f, -0.5f, -0.5f, 0.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
            {{0.5f, -0.5f, -0.5f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
            {{0.5f, 0.5f, -0.5f, 0.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
            {{-0.5f, 0.5f, -0.5f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        };
        const std::vector<uint16_t> indices = {
            0, 1, 2, 2, 3, 0,
            4, 5, 6, 6, 7, 4,
        };
        static const int NUM_FRAMES_IN_FLIGHT = 2;
        glm::mat4 model[NUM_FRAMES_IN_FLIGHT];
        glm::mat4 view[NUM_FRAMES_IN_FLIGHT];
        glm::mat4 proj[NUM_FRAMES_IN_FLIGHT];
        int currentUniform = 0;

        changeState.wrapDone = vulkanDone;

        if (!threadState.push(swapState.preview(0),sizeState,0))
        {std::cerr << "cannot push swap!" << std::endl; exit(-1);}
        swapState.advance(0);

        if (!threadState.push(pipelineState.preview(MicroTest),SizeState(MicroTest),0))
        {std::cerr << "cannot push pipeline!" << std::endl; exit(-1);}
        pipelineState.advance(MicroTest);

        BindState *single[] = {&pipelineState};
        for (int i = 0; i < frames; i++)
        while (!drawState.preview(i)->bind(single, 1) ||
        !threadState.push(drawState.preview(i),SizeState(MicroTest),0))
        {drawState.preview(i)->free(); glfwWaitEventsTimeout(0.001);}

        Center *vtx = 0; allocCenter(&vtx,1);
        vtx->mem = Vertexz; vtx->siz = vertices.size(); allocVertex(&vtx->vtx,vtx->siz);
        for (int i = 0; i < vtx->siz; i++)
        memcpy(&vtx->vtx[i],&vertices[i],sizeof(Vertex));
        changeState.copy(vtx);

        Center *ind = 0; allocCenter(&ind,1);
        int isiz = indices.size()*sizeof(uint16_t);
        ind->mem = Indexz; ind->siz = isiz/sizeof(int32_t); allocInt32(&ind->ind,ind->siz);
        memcpy(ind->ind,indices.data(),isiz);
        changeState.copy(ind);

        Center *tex = 0; allocCenter(&tex,1);
        tex->mem = Texturez; tex->siz = 1; allocTexture(&tex->tex,tex->siz);
        fmtxStbi(&tex->tex[0].dat,&tex->tex[0].wid,&tex->tex[0].hei,&tex->tex[0].cha,"texture.jpg");
        changeState.copy(tex);

        int count = 0;
        while (!glfwWindowShouldClose(windowState.window) && count++ < 1000) {

        if (changeState.read(RegisterMask) & (1<<ResizeAsync)) {
        changeState.wotc(RegisterMask,(1<<ResizeAsync));
        sizeState = SizeState(findCapabilities(windowState.window,vulkanState.surface,physicalState.device));
        while (!threadState.push(swapState.preview(0),sizeState,0)) glfwWaitEventsTimeout(0.001);
        swapState.advance(0);}

        updateUniformBuffer(sizeState.capabilities.currentExtent,model[currentUniform],view[currentUniform],proj[currentUniform]);
        Center *mat = 0; allocCenter(&mat,1);
        mat->mem = Matrixz; mat->siz = 3; allocMatrix(&mat->mat,mat->siz);
        memcpy(&mat->mat[0],&model[currentUniform],sizeof(Matrix));
        memcpy(&mat->mat[1],&view[currentUniform],sizeof(Matrix));
        memcpy(&mat->mat[2],&proj[currentUniform],sizeof(Matrix));
        if (changeState.copy(mat)) currentUniform = (currentUniform + 1) % NUM_FRAMES_IN_FLIGHT;

        BindState *bind[] = {
            &matrixState,
            &textureState,
            &vertexState,
            &indexState,
            &pipelineState,
            &swapState};
        if (drawState.derived()->check() &&
            drawState.derived()->bind(bind,sizeof(bind)/sizeof(bind[0])) &&
            threadState.push(drawState.derived(),0,0,static_cast<uint32_t>(indices.size()),0))
            drawState.advance();
        else {drawState.derived()->free(); glfwWaitEventsTimeout(0.001);}}

        threadState.push();
    }
};

void ChangeState::async(Async bit) {
    wots(RegisterMask,(1<<bit));
    // TODO call vulkanSafe and/or planeSafe
}
void ChangeState::loop() {
    // TODO poll glfw, poll ptasm, copy clear, stall main
}
extern "C" {
int datxVoids(void *dat);
void *datxVoidz(int num, void *dat);
};
int ChangeState::copy(Center *center) {
    int retval = 0;
    switch (center->mem) {
    default: {std::cerr << "cannot copy center!" << std::endl; exit(-1);}
    break; case (Indexz): {
    int iloc = center->idx*sizeof(int32_t); int isiz = center->siz*sizeof(int32_t);
    if (main->threadState.push(main->indexState.preview(),center->ind,iloc,isiz,SizeState(isiz),center)) retval++;}
    break; case (Vertexz): {
    int vloc = center->idx*sizeof(Vertex); int vsiz = center->siz*sizeof(Vertex);
    if (main->threadState.push(main->vertexState.preview(),center->vtx,vloc,vsiz,SizeState(vsiz),center)) retval++;}
    break; case (Matrixz): {
    int uloc = center->idx*sizeof(Matrix); int usiz = center->siz*sizeof(Matrix);
    if (main->threadState.push(main->matrixState.preview(),center->mat,uloc,usiz,SizeState(usiz),center)) retval++;}
    break; case (Texturez): {
    VkExtent2D texExtent; texExtent.width = center->tex[0].wid; texExtent.height = center->tex[0].hei;
    if (main->threadState.push(main->textureState.preview(),
    datxVoidz(0,center->tex[0].dat),0,datxVoids(center->tex[0].dat),SizeState(texExtent),center)) retval++;}
    }
    return retval;
}
void ChangeState::done(int pass, Center *ptr) {
    wrapDone(pass,ptr);
}

int main() {
    MainState app;
    return 0;
}
