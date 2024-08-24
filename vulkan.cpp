#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <vector>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include <limits>
#include <optional>
#include <deque>
#include <map>
#include <set>
#include <functional>
#include <pthread.h>
#include <semaphore.h>
#include <sys/time.h>

extern "C" {
#include "face.h"
#include "type.h"
#include "plane.h"
}

struct InitState;
struct OpenState;
struct PhysicalState;
struct DeviceState;
struct ThreadState;
struct QueueState;
struct TempState;
struct MouseState {
    double left, base, angle;
    bool moved, sized;
};
struct WindowState {
    int left, base, width, height;
    bool moved, sized;
};
struct SizeState {
    int width, height;
};
struct RatioState {
    int left, base, width, height;
};
struct MainState {
    bool manipReact[Reacts]; // little language changed; user filter
    bool manipEnact[Enacts]; // set and forget; work on filtered user
    bool manipAction[Actions]; // micro code response to little language
    bool registerDone[Enacts]; // record whether work remains
    bool registerOpen; // whether main loop running
    enum Plan registerPlan; // which builtin test to run
    int registerPoll; // how long to what without wake
    enum Interp mouseRead, mouseWrite; // which copy of mouse
    enum Interp windowRead, windowWrite; // which copy of window
    MouseState mouseClick, mouseMove, mouseCopy; // pierce, current, processed
    WindowState windowClick, windowMove, windowCopy; // pierce, current, processed
    SizeState swapMove, swapCopy; // current, processed
    RatioState windowRatio; // physical to virtual
    std::deque<MouseState> queryMove; MouseState queryCopy; // synchronous changes
    std::deque<Center*> deferMove; Center* deferCopy; // calls to dma
    std::deque<int> linePress; // synchronous characters
    int charPress; // async character
    int mouseIndex; // which plane to manipulate
    int paramFollow; // which matrix for window
    int paramModify; // which matrix for manipulate
    enum Micro paramDisplay; // which shader for diplay
    enum Micro paramBright; // which shader for index
    enum Micro paramDetect; // which shader for pierce
    int paramBase; // which facet to start on
    int paramLimit; // which facet to finish before
    int littleIndex; // which state user is in
    int littleSize; // how many states user can be in
    Little *littleState; // user changes to React and Action
    int argc; char **argv;
    InitState *initState;
    OpenState* openState;
    PhysicalState* physicalState;
    DeviceState* logicalState;
    ThreadState *threadState;
    TempState *tempState;
    QueueState* queueState;
    const int windowDelta = 10; // how far arrow keys move window edges
    const int MAX_FRAMES_INFLIGHT = 2; // how many swap indices
    const int MAX_BUFFERS_AVAILABLE = 3; // how many buffers per memory
    const int MAX_FENCES_INFLIGHT = 3; // how many fences to wait for
    const int MAX_FRAMEBUFFER_SWAPS = 2; // how many swap buffers
    const int MAX_FRAMEBUFFER_RESIZE = 100; // window change before new swap buffer
    const std::vector<const char*> extensions = {VK_KHR_SWAPCHAIN_EXTENSION_NAME};
#ifdef NDEBUG
    const std::vector<const char*> layers;
#else
    const std::vector<const char*> layers = {"VK_LAYER_KHRONOS_validation"};
#endif
} mainState = {0};

void windowMoved(GLFWwindow* window, int xpos, int ypos)
{
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    // std::cerr << "windowMoved " << xpos << "/" << ypos << std::endl;
    if (mainState->manipReact[Relate]) {
        mainState->mouseMove.left -= xpos-mainState->windowMove.left;
        mainState->mouseMove.base -= ypos-mainState->windowMove.base;
        mainState->mouseMove.moved = true;}
    mainState->windowMove.left = xpos; mainState->windowMove.base = ypos; mainState->windowMove.moved = false;
    if (mainState->manipReact[Enque]) planeSafe(Threads,Phases,WindowLeft);
}
void mouseMoved(GLFWwindow* window, double xpos, double ypos) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    // std::cerr << "mouseMoved " << xpos << "/" << ypos << std::endl;
    mainState->mouseMove.left = xpos; mainState->mouseMove.base = ypos; mainState->mouseMove.moved = false;
    // TODO if Drag, set winodowMove moved and/or sized = true;
    if (mainState->manipReact[Relate]) {
        mainState->mouseMove.left += mainState->windowMove.left;
        mainState->mouseMove.base += mainState->windowMove.base;}
    if (mainState->manipReact[Enque]) planeSafe(Threads,Phases,CursorLeft);
}
void windowSized(GLFWwindow* window, int width, int height)
{
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    // std::cerr << "windowSized " << width << "/" << height << std::endl;
    mainState->windowMove.width = width; mainState->windowMove.height = height; mainState->windowMove.sized = false;
    if (mainState->manipReact[Enque]) planeSafe(Threads,Phases,WindowWidth);
}
void mouseAngled(GLFWwindow *window, double amount) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    mainState->mouseMove.angle += amount; mainState->mouseMove.sized = false;
    if (mainState->manipReact[Enque]) planeSafe(Threads,Phases,CursorAngle);
}
void windowRefreshed(GLFWwindow* window)
{
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    // std::cerr << "windowRefresh" << std::endl;
}
void manipReact(struct MainState *mainState, int pat) {
    for (int i = mainState->littleIndex; i < mainState->littleSize; i++)
    if (mainState->littleState[i].pat == pat || mainState->littleState[i].pat == 0) {
    for (int j = 0; j < Reacts; j++) mainState->manipReact[(React)j] = ((mainState->littleState[i].val&(1<<j)) != 0);
    for (int j = 0; j < Actions; j++) mainState->manipAction[(Action)j] = ((mainState->littleState[i].num&(1<<j)) != 0);
    mainState->littleIndex = mainState->littleState[i].nxt; break;}
}
void keyPressed(GLFWwindow* window, int key, int scancode, int action, int mods) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    int north, east, south, west, delta;
    north = east = south = west = 0; delta = mainState->windowDelta;
    if (action != GLFW_PRESS || mods != 0) return;
    std::cerr << "keyPressed " << key << std::endl;
    if (key == GLFW_KEY_DOWN || key == GLFW_KEY_LEFT) delta = -delta;
    if (mainState->manipReact[Arrow] && mainState->manipReact[North]) north = delta;
    if (mainState->manipReact[Arrow] && mainState->manipReact[South]) south = delta;
    if (mainState->manipReact[Arrow] && mainState->manipReact[East]) east = delta;
    if (mainState->manipReact[Arrow] && mainState->manipReact[West]) west = delta;
    if (south || west) {
        mainState->windowMove.left += west; mainState->windowMove.base += south; mainState->windowMove.moved = true;}
    if (mainState->manipReact[Relate] && (south || west)) {
        mainState->mouseMove.left -= west; mainState->mouseMove.base -= south; mainState->mouseMove.moved = true;}
    if (north != south || east != west) {
        mainState->windowMove.width += east-west; mainState->windowMove.height += north-south; mainState->windowMove.sized = true;}
    if (mainState->manipReact[Enline]) mainState->linePress.push_back(key);
    if (mainState->manipReact[Enchar]) mainState->charPress = key;
    if (mainState->manipReact[Enque]) planeSafe(Threads,Phases,CursorPress);
    if (mainState->manipReact[Enstate]) manipReact(mainState,key);
}
void mouseClicked(GLFWwindow* window, int button, int action, int mods) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    std::cerr << "mouseClicked" << std::endl;
    if (action != GLFW_PRESS) return;
    mainState->windowClick = mainState->windowMove;
    mainState->mouseClick = mainState->mouseMove;
    mainState->queryMove.push_back(mainState->mouseMove);
    if (mainState->manipReact[Enque]) planeSafe(Threads,Phases,CursorClick);
    if (mainState->manipReact[Enstate]) manipReact(mainState,-1);
}
VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData) {
    // std::cerr << "validation layer: " << pCallbackData->pMessage << std::endl;
    return VK_FALSE;
}

struct InitState {
    VkInstance instance;
    bool enable;
    VkDebugUtilsMessengerEXT debug;
    InitState(const std::vector<const char*> layers) {
        this->enable = layers.size();
        glfwInit();
        VkDebugUtilsMessengerCreateInfoEXT info = {};
        info.sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
        info.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
        info.messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
        info.pfnUserCallback = debugCallback;
        instance = [](bool enable, VkDebugUtilsMessengerCreateInfoEXT create, const std::vector<const char*> layers) {
            if (layers.size() && ![](const std::vector<const char*> layers) {
                uint32_t count;
                vkEnumerateInstanceLayerProperties(&count, nullptr);
                std::vector<VkLayerProperties> available(count);
                vkEnumerateInstanceLayerProperties(&count, available.data());
                for (const char* name : layers) {
                bool found = false;
                for (const auto& properties : available) {
                if (strcmp(name, properties.layerName) == 0) {found = true; break;}}
                if (!found) return false;}
                return true;
            } (layers)) throw std::runtime_error("validation layers requested, but not available!");
            VkApplicationInfo application{};
            application.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
            application.pApplicationName = "Hello Triangle";
            application.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
            application.pEngineName = "No Engine";
            application.engineVersion = VK_MAKE_VERSION(1, 0, 0);
            application.apiVersion = VK_API_VERSION_1_0; 
            VkInstanceCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
            info.pApplicationInfo = &application;
            std::vector<const char*> extensions = [](bool enable) {
                uint32_t count = 0;
                const char** required = glfwGetRequiredInstanceExtensions(&count);
                std::vector<const char*> extensions(required, required + count);
                if (enable) extensions.push_back(VK_EXT_DEBUG_UTILS_EXTENSION_NAME);
                return extensions;
            } (enable);
            info.enabledExtensionCount = static_cast<int>(extensions.size());
            info.ppEnabledExtensionNames = extensions.data();
            if (enable) {
                info.enabledLayerCount = static_cast<int>(layers.size());
                info.ppEnabledLayerNames = layers.data();
                info.pNext = (VkDebugUtilsMessengerCreateInfoEXT*) &create;}
            else {
                info.enabledLayerCount = 0;
                info.pNext = nullptr;}
            VkInstance instance;
            if (vkCreateInstance(&info, nullptr, &instance) != VK_SUCCESS) throw std::runtime_error("failed to create instance!");
            return instance;
        } (enable,info,layers);
        if (enable) debug = [](VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info) {
            VkDebugUtilsMessengerEXT debug;
            if ([](VkInstance instance, const VkDebugUtilsMessengerCreateInfoEXT* pCreateInfo,
                const VkAllocationCallbacks* pAllocator, VkDebugUtilsMessengerEXT* pDebugMessenger) {
                auto func = (PFN_vkCreateDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
                if (func != nullptr) return func(instance, pCreateInfo, pAllocator, pDebugMessenger);
                else return VK_ERROR_EXTENSION_NOT_PRESENT;}(instance, &info, nullptr, &debug) != VK_SUCCESS)
                throw std::runtime_error("failed to set up debug messenger!");
            return debug;
        } (instance,info);
    }
    ~InitState() {
        if (enable) [](VkInstance instance, VkDebugUtilsMessengerEXT debugMessenger, const VkAllocationCallbacks* pAllocator) {
            auto func = (PFN_vkDestroyDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
            if (func != nullptr) func(instance, debugMessenger, pAllocator);}(instance, debug, nullptr);
        vkDestroyInstance(instance, nullptr);
        glfwTerminate();
    }
};

struct OpenState {
    VkInstance instance;
    GLFWwindow* window;
    VkSurfaceKHR surface;
    OpenState(VkInstance instance, int width, int height, void *ptr) {
        this->instance = instance;
        window = [](int32_t width, int32_t height) {
            glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
            return glfwCreateWindow(width, height, "Vulkan", nullptr, nullptr);
        } (width,height);
        glfwSetWindowUserPointer(window, ptr);
        // glfwSetWindowAttrib(window, GLFW_DECORATED, GLFW_FALSE);
        glfwSetKeyCallback(window, keyPressed);
        glfwSetMouseButtonCallback(window, mouseClicked);
        glfwSetCursorPosCallback(window, mouseMoved);
        glfwSetWindowPosCallback(window, windowMoved);
        glfwSetWindowSizeCallback(window, windowSized);
        glfwSetWindowRefreshCallback(window, windowRefreshed);
        if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
            throw std::runtime_error("failed to create window surface!");
    }
    ~OpenState() {
        vkDestroySurfaceKHR(instance, surface, nullptr);
        glfwDestroyWindow(window);
    }
};

struct PhysicalState {
    VkPhysicalDevice physical;
    int graphicid;
    int presentid;
    int minimum;
    VkSurfaceFormatKHR format;
    VkPresentModeKHR mode;
    VkFormat image;
    PhysicalState(VkInstance instance, VkSurfaceKHR surface, std::vector<const char*> extensions, int inflight) {
        std::optional<int> graphic;
        std::optional<int> present;
        std::optional<int> compute;
        std::vector<VkSurfaceFormatKHR> formats;
        std::vector<VkPresentModeKHR> modes;
        physical = [&graphic,&present,&formats,&modes](
            VkInstance instance, VkSurfaceKHR surface, std::vector<const char*> extensions) {
            VkPhysicalDevice physical = VK_NULL_HANDLE;
            uint32_t count = 0;
            vkEnumeratePhysicalDevices(instance, &count, nullptr);
            if (count == 0)
                throw std::runtime_error("failed to find GPUs with Vulkan support!");
            std::vector<VkPhysicalDevice> physicals(count);
            vkEnumeratePhysicalDevices(instance, &count, physicals.data());
            for (const auto& physicalz : physicals) {
                std::optional<int> graphicz;
                std::optional<int> presentz;
                [&graphicz,&presentz](VkPhysicalDevice physicalz, VkSurfaceKHR surface) {
                    uint32_t count = 0;
                    vkGetPhysicalDeviceQueueFamilyProperties(physicalz, &count, nullptr);
                    std::vector<VkQueueFamilyProperties> families(count);
                    vkGetPhysicalDeviceQueueFamilyProperties(physicalz, &count, families.data());
                    int i = 0;
                    for (const auto& family : families) {
                        if (family.queueFlags & VK_QUEUE_GRAPHICS_BIT) graphicz = i;
                        VkBool32 support = false;
                        vkGetPhysicalDeviceSurfaceSupportKHR(physicalz, i, surface, &support);
                        if (support) presentz = i;
                        if (graphicz.has_value() && presentz.has_value()) break;
                        i++;}
                } (physicalz,surface);
                std::vector<VkSurfaceFormatKHR> formatz;
                [&formatz](VkPhysicalDevice physicalz, VkSurfaceKHR surface) {
                    uint32_t count;
                    vkGetPhysicalDeviceSurfaceFormatsKHR(physicalz, surface, &count, nullptr);
                    if (count != 0) {
                        formatz.resize(count);
                        vkGetPhysicalDeviceSurfaceFormatsKHR(physicalz, surface, &count, formatz.data());}
                } (physicalz,surface);
                std::vector<VkPresentModeKHR> modez;
                [&modez](VkPhysicalDevice physicalz, VkSurfaceKHR surface) {
                    uint32_t count;
                    vkGetPhysicalDeviceSurfacePresentModesKHR(physicalz, surface, &count, nullptr);
                    if (count != 0) {
                        modez.resize(count);
                        vkGetPhysicalDeviceSurfacePresentModesKHR(physicalz, surface, &count, modez.data());}
                } (physicalz,surface);
                if ([](VkPhysicalDevice physicalz, const std::vector<const char*> extensions) {
                        uint32_t count;
                        vkEnumerateDeviceExtensionProperties(physicalz, nullptr, &count, nullptr);
                        std::vector<VkExtensionProperties> availableExtensions(count);
                        vkEnumerateDeviceExtensionProperties(physicalz, nullptr, &count, availableExtensions.data());
                        std::set<std::string> requiredExtensions(extensions.begin(), extensions.end());
                        for (const auto& extension : availableExtensions) requiredExtensions.erase(extension.extensionName);
                        return requiredExtensions.empty();
                    } (physicalz,extensions) &&
                    !formatz.empty() &&
                    !modez.empty() &&
                    graphicz.has_value() &&
                    presentz.has_value()) {
                    graphic = graphicz;
                    present = presentz;
                    formats = formatz;
                    physical = physicalz;
                    break;}}
            if (physical == VK_NULL_HANDLE)
                throw std::runtime_error("failed to find a suitable GPU!");
            return physical;
        } (instance,surface,extensions);
        graphicid = graphic.value();
        presentid = present.value();
        VkSurfaceCapabilitiesKHR capabilities;
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical, surface, &capabilities);
        minimum = capabilities.minImageCount+inflight;
        if (capabilities.maxImageCount > 0 && minimum > capabilities.maxImageCount)
            minimum = capabilities.maxImageCount;
        format = [](std::vector<VkSurfaceFormatKHR> formats) {
            for (const auto& format : formats) {
                if (format.format == VK_FORMAT_B8G8R8A8_SRGB && format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
                    return format;}}
            return formats[0];
        } (formats);
        mode = [](const std::vector<VkPresentModeKHR>& modes) {
            for (const auto& mode : modes) {
                if (mode == VK_PRESENT_MODE_MAILBOX_KHR) {
                    return mode;
                }
            }
            return VK_PRESENT_MODE_FIFO_KHR;
        } (modes);
        image = format.format;
    }
    ~PhysicalState() {
    }
};

struct DeviceState {
    VkDevice device;
    VkRenderPass render;
    VkQueue graphic;
    VkQueue present;
    VkCommandPool pool;
    VkDescriptorPool dpool;
    int graphicid;
    int presentid;
    DeviceState(VkPhysicalDevice physical, int graphicid, int presentid, VkFormat image,
        std::vector<const char*> layers, std::vector<const char*> extensions, int total) {
        this->graphicid = graphicid;
        this->presentid = presentid;
        device = [](VkPhysicalDevice physical, int graphicid, int presentid,
            const std::vector<const char*> layers, const std::vector<const char*> extensions, bool enable) {
            VkDevice device;
            std::vector<VkDeviceQueueCreateInfo> infos;
            std::vector<float> prioritys;
            prioritys.resize(1);
            for (int i = 0; i < 1; i++) prioritys[i] = 1.0f;
            if (1) {
                VkDeviceQueueCreateInfo info{};
                info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
                info.queueFamilyIndex = graphicid;
                info.queueCount = 1;
                info.pQueuePriorities = prioritys.data();
                infos.push_back(info);}
            if (presentid != graphicid) {
                VkDeviceQueueCreateInfo info{};
                info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
                info.queueFamilyIndex = presentid;
                info.queueCount = 1;
                info.pQueuePriorities = prioritys.data();
                infos.push_back(info);}
            VkPhysicalDeviceFeatures features{};
            VkDeviceCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
            info.queueCreateInfoCount = static_cast<int>(infos.size());
            info.pQueueCreateInfos = infos.data();
            info.pEnabledFeatures = &features;
            info.enabledExtensionCount = static_cast<int>(extensions.size());
            info.ppEnabledExtensionNames = extensions.data();
            if (enable) {
                info.enabledLayerCount = static_cast<int>(layers.size());
                info.ppEnabledLayerNames = layers.data();}
            else {
                info.enabledLayerCount = 0;}
            if (vkCreateDevice(physical, &info, nullptr, &device) != VK_SUCCESS)
                throw std::runtime_error("failed to create logical device!");
            return device;
        } (physical,graphicid,presentid,layers,extensions,layers.size());
        render = [](VkDevice device, VkFormat image) {
            VkAttachmentDescription attachment{};
            attachment.format = image;
            attachment.samples = VK_SAMPLE_COUNT_1_BIT;
            attachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
            attachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
            attachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
            attachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
            attachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
            attachment.finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
            VkAttachmentReference reference{};
            reference.attachment = 0;
            reference.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
            VkSubpassDescription subpass{};
            subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
            subpass.colorAttachmentCount = 1;
            subpass.pColorAttachments = &reference;
            VkSubpassDependency dependency{};
            dependency.srcSubpass = VK_SUBPASS_EXTERNAL;
            dependency.dstSubpass = 0;
            dependency.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
            dependency.srcAccessMask = 0;
            dependency.dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
            dependency.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
            VkRenderPassCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
            info.attachmentCount = 1;
            info.pAttachments = &attachment;
            info.subpassCount = 1;
            info.pSubpasses = &subpass;
            info.dependencyCount = 1;
            info.pDependencies = &dependency;
            VkRenderPass render;
            if (vkCreateRenderPass(device, &info, nullptr, &render) != VK_SUCCESS)
                throw std::runtime_error("failed to create render pass!");
            return render;
        } (device,image);
        vkGetDeviceQueue(device, graphicid, 0, &graphic);
        vkGetDeviceQueue(device, presentid, 0, &present);
        pool = [](VkDevice device, int graphicid) {
            VkCommandPoolCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
            info.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
            info.queueFamilyIndex = graphicid;
            VkCommandPool pool;
            if (vkCreateCommandPool(device, &info, nullptr, &pool) != VK_SUCCESS)
                throw std::runtime_error("failed to create graphic command pool!");
            return pool;
        } (device,graphicid);
        dpool = [](VkDevice device, int uniforms, int stores) {
            VkDescriptorPoolSize uniform{};
            uniform.type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
            uniform.descriptorCount = static_cast<int>(uniforms);
            VkDescriptorPoolSize store{};
            store.type = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
            store.descriptorCount = static_cast<int>(stores);
            VkDescriptorPoolSize size[] = {uniform,store};
            VkDescriptorPoolCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
            info.flags = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT;
            info.poolSizeCount = 2;
            info.pPoolSizes = size;
            info.maxSets = static_cast<int>(uniforms+stores);
            VkDescriptorPool pool;
            if (vkCreateDescriptorPool(device, &info, nullptr, &pool) != VK_SUCCESS)
                throw std::runtime_error("failed to create descriptor pool!");
            return pool;
        } (device, total, total);
    }
    ~DeviceState() {
        vkDestroyDescriptorPool(device, dpool, nullptr);
        vkDestroyCommandPool(device, pool, nullptr);
        vkDestroyRenderPass(device, render, nullptr);
        vkDestroyDevice(device, nullptr);
    }
};

struct BufferState {
    VkPhysicalDevice physical;
    VkDevice device;
    VkQueue graphic;
    VkCommandPool pool;
    VkDeviceSize size;
    WrapTag tag;
    VkBuffer staging;
    VkDeviceMemory wasted;
    VkBuffer buffer;
    VkDeviceMemory memory;
    void *mapped;
    VkCommandBuffer command;
    VkFence fence;
    int loc; int siz; const void *ptr; std::function<void()> dat;
BufferState(MainState *state, int size, WrapTag tag) {
    PhysicalState* physical = state->physicalState;
    DeviceState* logical = state->logicalState;
    this->physical = physical->physical;
    this->device = logical->device;
    this->graphic = logical->graphic;
    this->pool = logical->pool;
    this->size = size;
    this->tag = tag;}
void init() {
    // this called in separate thread on newly constructed
    VkMemoryRequirements requirements;
    VkPhysicalDeviceMemoryProperties properties;
    vkGetPhysicalDeviceMemoryProperties(physical, &properties);
    if (tag == FetchBuf || tag == StoreBuf || tag == QueryBuf) {
    staging = [](VkDevice device, VkDeviceSize size, VkBufferUsageFlags usage) {
        VkBufferCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
        info.size = size;
        info.usage = usage;
        info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
        VkBuffer buffer;
        if (vkCreateBuffer(device, &info, nullptr, &buffer) != VK_SUCCESS)
            throw std::runtime_error("failed to create buffer!");
        return buffer;
    } (device,size,(tag == QueryBuf ?
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT|VK_BUFFER_USAGE_TRANSFER_DST_BIT :
        VK_BUFFER_USAGE_TRANSFER_SRC_BIT));
    vkGetBufferMemoryRequirements(device, staging, &requirements);
    wasted = [](VkDevice device, VkMemoryRequirements requirements, uint32_t type) {
        VkMemoryAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        info.allocationSize = requirements.size;
        info.memoryTypeIndex = type;
        VkDeviceMemory memory;
        if (vkAllocateMemory(device, &info, nullptr, &memory) != VK_SUCCESS)
            throw std::runtime_error("failed to allocate buffer memory!");
        return memory;
    } (device,requirements,[](uint32_t type, VkPhysicalDeviceMemoryProperties properties,
        VkMemoryPropertyFlags flags)->uint32_t {
        for (int i = 0; i < properties.memoryTypeCount; i++) if ((type & (1 << i)) &&
            (properties.memoryTypes[i].propertyFlags & flags) == flags) return i;
        throw std::runtime_error("failed to find suitable wasted type!");
        return properties.memoryTypeCount;
    } (requirements.memoryTypeBits,properties,VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT));
    vkBindBufferMemory(device, staging, wasted, 0);}
    buffer = [](VkDevice device, VkDeviceSize size, VkBufferUsageFlags usage) {
        VkBufferCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
        info.size = size;
        info.usage = usage;
        info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
        VkBuffer buffer;
        if (vkCreateBuffer(device, &info, nullptr, &buffer) != VK_SUCCESS)
            throw std::runtime_error("failed to create buffer!");
        return buffer;
    } (device,size,tag==ChangeBuf?
        VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT:(tag==StoreBuf?
        VK_BUFFER_USAGE_TRANSFER_DST_BIT|VK_BUFFER_USAGE_STORAGE_BUFFER_BIT:(tag==QueryBuf?
        VK_BUFFER_USAGE_TRANSFER_DST_BIT|VK_BUFFER_USAGE_TRANSFER_SRC_BIT|VK_BUFFER_USAGE_STORAGE_BUFFER_BIT:
        VK_BUFFER_USAGE_TRANSFER_DST_BIT|VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)));
    vkGetBufferMemoryRequirements(device, buffer, &requirements);
    memory = [](VkDevice device, VkMemoryRequirements requirements, uint32_t type) {
        VkMemoryAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        info.allocationSize = requirements.size;
        info.memoryTypeIndex = type;
        VkDeviceMemory memory;
        if (vkAllocateMemory(device, &info, nullptr, &memory) != VK_SUCCESS)
            throw std::runtime_error("failed to allocate buffer memory!");
        return memory;
    } (device,requirements,[](uint32_t type, VkPhysicalDeviceMemoryProperties properties,
        VkMemoryPropertyFlags flags)->uint32_t {
        for (int i = 0; i < properties.memoryTypeCount; i++) if ((type & (1 << i)) &&
            (properties.memoryTypes[i].propertyFlags & flags) == flags) return i;
        throw std::runtime_error("failed to find suitable memory type!");
        return properties.memoryTypeCount;
    } (requirements.memoryTypeBits,properties,tag==ChangeBuf?
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT:
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
    vkBindBufferMemory(device, buffer, memory, 0);
    if (tag == FetchBuf || tag == ChangeBuf || tag == StoreBuf || tag == QueryBuf) {
    vkMapMemory(device, tag==ChangeBuf?memory:wasted, 0, size, 0, &mapped);}
    command = [](VkDevice device, VkCommandPool command) {
        VkCommandBuffer commandBuffer;
        VkCommandBufferAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
        info.commandPool = command;
        info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
        info.commandBufferCount = (int)1;
        if (vkAllocateCommandBuffers(device, &info, &commandBuffer) != VK_SUCCESS)
            throw std::runtime_error("failed to allocate command buffers!");
        return commandBuffer;}(device,pool);
    fence = [](VkDevice device) {
        VkFence fence;
        VkFenceCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
        info.flags = VK_FENCE_CREATE_SIGNALED_BIT;
        if (vkCreateFence(device, &info, nullptr, &fence) != VK_SUCCESS)
            throw std::runtime_error("failed to create fence!");
        return fence;}(device);}
~BufferState() {
    vkDestroyFence(device,fence,0);
    vkFreeCommandBuffers(device, pool, 1, &command);
    vkDestroyBuffer(device, buffer, nullptr);
    vkFreeMemory(device, memory, nullptr);
    if (tag == FetchBuf || tag == StoreBuf || tag == QueryBuf) {
    vkDestroyBuffer(device, staging, nullptr);
    vkFreeMemory(device, wasted, nullptr);}}
VkFence setup() {
    // this called in separate thread to get fence
    VkResult result;
    if (tag != FetchBuf && tag != ChangeBuf && tag != StoreBuf && tag != QueryBuf) return VK_NULL_HANDLE;
    if (ptr) {memcpy((char*)mapped+loc,ptr,siz); dat(); ptr = 0;}
    if (tag == ChangeBuf) return VK_NULL_HANDLE;
    vkResetCommandBuffer(command, /*VkCommandBufferResetFlagBits*/ 0);
    vkResetFences(device, 1, &fence);
    VkCommandBufferBeginInfo begin{};
    begin.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    begin.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    vkBeginCommandBuffer(command, &begin);
    VkBufferCopy copy{};
    copy.size = size;
    vkCmdCopyBuffer(command, staging, buffer, 1, &copy);
    vkEndCommandBuffer(command);
    VkSubmitInfo submit{};
    submit.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.commandBufferCount = 1;
    submit.pCommandBuffers = &command;
    result = vkQueueSubmit(graphic, 1, &submit, fence);
    return fence;}
VkFence getup() {
    // call this to get computations from gpu
    VkResult result;
    if (tag != QueryBuf) return VK_NULL_HANDLE;
    vkResetCommandBuffer(command, /*VkCommandBufferResetFlagBits*/ 0);
    vkResetFences(device, 1, &fence);
    VkCommandBufferBeginInfo begin{};
    begin.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    begin.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    vkBeginCommandBuffer(command, &begin);
    VkBufferCopy copy{};
    copy.size = size;
    // TODO call vkCmdCopyImageToBuffer if bound to SwapBuf
    vkCmdCopyBuffer(command, buffer, staging, 1, &copy);
    vkEndCommandBuffer(command);
    VkSubmitInfo submit{};
    submit.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.commandBufferCount = 1;
    submit.pCommandBuffers = &command;
    result = vkQueueSubmit(graphic, 1, &submit, fence);
    return fence;}
void *bind() {
    return mapped;}
void bind(int loc, int siz, const void *ptr, std::function<void()> dat) {
    this->loc = loc; this->siz = siz; this->ptr = ptr; this->dat = dat;}
bool bind(int layout, VkCommandBuffer command, VkDescriptorSet descriptor) {
    if (tag == FetchBuf) {
    [](VkBuffer buffer, VkCommandBuffer command){
        VkBuffer buffers[] = {buffer};
        VkDeviceSize offsets[] = {0};
        vkCmdBindVertexBuffers(command, 0, 1, buffers, offsets);
    }(buffer,command);}
    if (tag == ChangeBuf) {
    [](VkDevice device, VkBuffer buffer, VkDescriptorSet descriptor, int layout, int size) {
        VkDescriptorBufferInfo info{};
        info.buffer = buffer;
        info.offset = 0;
        info.range = size;
        VkWriteDescriptorSet update{};
        update.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        update.dstSet = descriptor;
        update.dstBinding = layout;
        update.dstArrayElement = 0;
        update.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
        update.descriptorCount = 1;
        update.pBufferInfo = &info;
        vkUpdateDescriptorSets(device, 1, &update, 0, nullptr);
    }(device,buffer,descriptor,layout,size);}
    if (tag == StoreBuf || tag == QueryBuf) {
    [](VkDevice device, VkBuffer buffer, VkDescriptorSet descriptor, int layout, int size) {
        VkDescriptorBufferInfo info{};
        info.buffer = buffer;
        info.offset = 0;
        info.range = size;
        VkWriteDescriptorSet update{};
        update.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        update.dstSet = descriptor;
        update.dstBinding = layout;
        update.dstArrayElement = 0;
        update.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        update.descriptorCount = 1;
        update.pBufferInfo = &info;
        vkUpdateDescriptorSets(device, 1, &update, 0, nullptr);
    }(device,buffer,descriptor,layout,size);}
    return (tag == ChangeBuf || tag == StoreBuf || tag == QueryBuf);}
};

struct SwapState {
    GLFWwindow *window;
    VkPhysicalDevice physical;
    VkDevice device;
    VkSurfaceKHR surface;
    VkFormat image;
    VkSurfaceFormatKHR format;
    VkPresentModeKHR mode;
    VkRenderPass pass;
    int minimum;
    int graphicid;
    int presentid;
    VkExtent2D extent;
    VkSwapchainKHR swap;
    std::vector<VkImage> images;
    std::vector<VkImageView> views;
    std::vector<VkFramebuffer> framebuffers;
    uint32_t count;
SwapState(MainState *state, int size, WrapTag tag) {
    int width, height;
    width = (size >> 16)*state->windowRatio.left/state->windowRatio.width;
    if (state->windowRatio.left%state->windowRatio.width) width += 1;
    height = (size & 0xffff)*state->windowRatio.base/state->windowRatio.height;
    if (state->windowRatio.base/state->windowRatio.height) height += 1;
    extent.width = width; extent.height = height;
    std::cerr << "SwapState " << width << "," << height << " " << this << std::endl;
    [this](OpenState *open, PhysicalState *physical, DeviceState *logical){
    this->device = logical->device;
    [this](GLFWwindow* window, VkPhysicalDevice physical, VkDevice device, VkSurfaceKHR surface,
    VkFormat image, VkSurfaceFormatKHR format, VkPresentModeKHR mode, VkRenderPass pass,
    int minimum, int graphicid, int presentid){
    this->window = window;
    this->physical = physical;
    this->surface = surface;
    this->image = image;
    this->format = format;
    this->mode = mode;
    this->pass = pass;
    this->minimum = minimum;
    this->graphicid = graphicid;
    this->presentid = presentid;
    }(open->window,physical->physical,logical->device,
    open->surface,physical->image,physical->format,physical->mode,
    logical->render,physical->minimum,physical->graphicid,physical->presentid);
    }(state->openState,state->physicalState,state->logicalState);}
void init() {
    VkSurfaceCapabilitiesKHR capabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical, surface, &capabilities);
    swap = [](VkDevice device, VkSurfaceKHR surface, VkSurfaceFormatKHR format, VkPresentModeKHR mode, VkExtent2D extent,
        VkSurfaceCapabilitiesKHR capabilities, int minimum, uint32_t graphicid, uint32_t presentid) {
        VkSwapchainKHR swap;
        VkSwapchainCreateInfoKHR info{};
        info.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
        info.surface = surface;
        info.minImageCount = minimum;
        info.imageFormat = format.format;
        info.imageColorSpace = format.colorSpace;
        info.imageExtent = extent;
        info.imageArrayLayers = 1;
        info.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
        if (graphicid != presentid) {
            uint32_t indices[2] = {graphicid,presentid};
            info.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
            info.queueFamilyIndexCount = 2;
            info.pQueueFamilyIndices = indices;
        } else
            info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
        info.preTransform = capabilities.currentTransform;
        info.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
        info.presentMode = mode;
        info.clipped = VK_TRUE;
        if (vkCreateSwapchainKHR(device, &info, nullptr, &swap) != VK_SUCCESS)
            throw std::runtime_error("failed to create swap chain!");
        return swap;}(device,surface,format,mode,extent,capabilities,minimum,graphicid,presentid);
    [this](uint32_t &count) {vkGetSwapchainImagesKHR(device, swap, &count, nullptr);}(count);
    images.resize(count);
    views.resize(count);
    framebuffers.resize(count);
    [this](uint32_t count) {vkGetSwapchainImagesKHR(device, swap, &count, images.data());}(count);
    for (int i = 0; i < count; i++) views[i] =
        [](VkDevice device, VkFormat format, VkImage image) {
        VkImageView view;
        VkImageViewCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        info.image = image;
        info.viewType = VK_IMAGE_VIEW_TYPE_2D;
        info.format = format;
        info.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
        info.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
        info.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
        info.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
        info.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        info.subresourceRange.baseMipLevel = 0;
        info.subresourceRange.levelCount = 1;
        info.subresourceRange.baseArrayLayer = 0;
        info.subresourceRange.layerCount = 1;
        if (vkCreateImageView(device, &info, nullptr, &view) != VK_SUCCESS)
            throw std::runtime_error("failed to create image views!");
        return view;}(device,image,images[i]);
    for (int i = 0; i < count; i++) framebuffers[i] =
        [](VkDevice device, VkExtent2D extent, VkImageView view, VkRenderPass pass) {
        VkFramebuffer swapFramebuffer;
        VkImageView attachments[] = {view};
        VkFramebufferCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        info.renderPass = pass;
        info.attachmentCount = 1;
        info.pAttachments = attachments;
        info.width = extent.width;
        info.height = extent.height;
        info.layers = 1;
        if (vkCreateFramebuffer(device, &info, nullptr, &swapFramebuffer) != VK_SUCCESS)
            throw std::runtime_error("failed to create framebuffer!");
        return swapFramebuffer;}(device,extent,views[i],pass);
}
~SwapState() {
    for (int i = 0; i < count; i++) vkDestroyFramebuffer(device, framebuffers[i], nullptr);
    for (int i = 0; i < count; i++) vkDestroyImageView(device, views[i], nullptr);
    vkDestroySwapchainKHR(device, swap, nullptr);}
};

// TODO map identField to one of VK_FORMAT_R32G32_SFLOAT or VK_FORMAT_R32_UINT
struct VertexState {
    int stride;
    std::vector<VkFormat> format;
    std::vector<int> offset;
    VertexState() {
    stride = sizeof(Vertex);
    format.push_back(VK_FORMAT_R32G32_SFLOAT); // TODO perhaps autogenerate function in type.gen
    format.push_back(VK_FORMAT_R32_UINT);
    offset.push_back(offsetof(Vertex,vec));
    offset.push_back(offsetof(Vertex,ref));}
};

struct PipelineState {
    VkDevice device;
    VkPipelineLayout layout;
    VkPipeline pipeline;
    VkDescriptorSetLayout dlayout;
    VkDescriptorSet descriptor;
PipelineState(VkDevice device, VkRenderPass render, VkDescriptorPool dpool, Micro micro) {
    const char *vertex = VertexG__Micro__Str(micro);
    const char *fragment = FragmentG__Micro__Str(micro);
    this->device = device;
    dlayout = [](VkDevice device, Micro micro) {
        std::vector<VkDescriptorSetLayoutBinding> bindings;
        int count = 0; int i = 0; Memory mem = Memorys;
        while ((mem = BindQ__Micro__Int__Memory(micro)(i++)) != Memorys) {
        WrapTag tag = TypeQ__Memory__WrapTag(mem);
        if (tag == ChangeBuf || tag == StoreBuf || tag == QueryBuf) {
        VkDescriptorSetLayoutBinding binding{};
        binding.binding = count++;
        binding.descriptorCount = 1;
        binding.descriptorType = (tag == ChangeBuf ?
            VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER : VK_DESCRIPTOR_TYPE_STORAGE_BUFFER);
        binding.pImmutableSamplers = nullptr;
        binding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
        bindings.push_back(binding);}}
        VkDescriptorSetLayoutCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
        info.bindingCount = bindings.size();
        info.pBindings = bindings.data();
        VkDescriptorSetLayout descriptor;
        if (vkCreateDescriptorSetLayout(device, &info, nullptr, &descriptor) != VK_SUCCESS)
            throw std::runtime_error("failed to create descriptor set layout!");
        return descriptor;
    } (device,micro);
    descriptor = [](VkDevice device, VkDescriptorSetLayout layout, VkDescriptorPool pool) {
        VkDescriptorSetAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
        info.descriptorPool = pool;
        info.descriptorSetCount = static_cast<int>(1);
        info.pSetLayouts = &layout;
        VkDescriptorSet descriptor;
        if (vkAllocateDescriptorSets(device, &info, &descriptor) != VK_SUCCESS)
            throw std::runtime_error("failed to allocate descriptor sets!");
        return descriptor;}(device,dlayout,dpool);
    layout = [](VkDevice device, VkDescriptorSetLayout descriptor) {
        VkPipelineLayoutCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
        info.setLayoutCount = 1;
        info.pSetLayouts = &descriptor;
        VkPipelineLayout layout;
        if (vkCreatePipelineLayout(device, &info, nullptr, &layout) != VK_SUCCESS)
            throw std::runtime_error("failed to create pipeline layout!");
        return layout;
    } (device,dlayout);
    std::vector<VkPipelineShaderStageCreateInfo> stages(Component__Micro__MicroOut(micro) == Discard ? 1 : 2);
    std::vector<VkShaderModule> modules(Component__Micro__MicroOut(micro) == Discard ? 1 : 2);
    VkShaderModule vmodule = [](VkDevice device, const std::vector<char>& code) {
        VkShaderModuleCreateInfo createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
        createInfo.codeSize = code.size();
        createInfo.pCode = reinterpret_cast<const uint32_t*>(code.data());
        VkShaderModule shaderModule;
        if (vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule) != VK_SUCCESS)
            throw std::runtime_error("failed to create shader module!");
        return shaderModule;} (device,[](const std::string& filename) {
        std::ifstream file(filename, std::ios::ate | std::ios::binary);
        if (!file.is_open()) throw std::runtime_error("failed to open file!");
        size_t fileSize = (size_t) file.tellg();
        std::vector<char> buffer(fileSize);
        file.seekg(0); file.read(buffer.data(), fileSize); file.close();
        return buffer;}(vertex));
    modules[0] = vmodule;
    VkPipelineShaderStageCreateInfo vinfo = [](VkShaderModule vmodule) {
        VkPipelineShaderStageCreateInfo vinfo{};
        vinfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
        vinfo.stage = VK_SHADER_STAGE_VERTEX_BIT;
        vinfo.module = vmodule;
        vinfo.pName = "main";
        return vinfo;}(vmodule);
    stages[0] = vinfo;
    if (Component__Micro__MicroOut(micro) != Discard) {
    VkShaderModule fmodule = [](VkDevice device, const std::vector<char>& code) {
        VkShaderModuleCreateInfo createInfo{};
        createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
        createInfo.codeSize = code.size();
        createInfo.pCode = reinterpret_cast<const uint32_t*>(code.data());
        VkShaderModule shaderModule;
        if (vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule) != VK_SUCCESS)
            throw std::runtime_error("failed to create shader module!");
        return shaderModule;} (device,[](const std::string& filename) {
        std::ifstream file(filename, std::ios::ate | std::ios::binary);
        if (!file.is_open()) throw std::runtime_error("failed to open file!");
        size_t fileSize = (size_t) file.tellg();
        std::vector<char> buffer(fileSize);
        file.seekg(0); file.read(buffer.data(), fileSize); file.close();
        return buffer;}(fragment));
    modules[1] = fmodule;
    VkPipelineShaderStageCreateInfo finfo = [](VkShaderModule fmodule) {
        VkPipelineShaderStageCreateInfo finfo{};
        finfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
        finfo.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
        finfo.module = fmodule;
        finfo.pName = "main";
        return finfo;}(fmodule);
    stages[1] = finfo;}
    std::vector<VkVertexInputBindingDescription> descriptions;
    std::vector<VkVertexInputAttributeDescription> attributes;
    if (Component__Micro__MicroIn(micro) == Practice)
    [&descriptions,&attributes](VertexState field) {
        int binding = 0; int location = 0;
        VkVertexInputBindingDescription description;
        description.binding = binding;
        description.stride = field.stride;
        description.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
        for (int j = 0; j < field.offset.size(); j++) {
        VkVertexInputAttributeDescription attribute;
        attribute.binding = binding;
        attribute.location = location++;
        attribute.format = field.format[j];
        attribute.offset = field.offset[j];
        attributes.push_back(attribute);}
        descriptions.push_back(description);}(VertexState());
    VkPipelineVertexInputStateCreateInfo input = [&descriptions,&attributes] () {
        VkPipelineVertexInputStateCreateInfo input{};
        input.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
        input.vertexBindingDescriptionCount = static_cast<int>(descriptions.size());
        input.vertexAttributeDescriptionCount = static_cast<int>(attributes.size());
        input.pVertexBindingDescriptions = descriptions.data();
        input.pVertexAttributeDescriptions = attributes.data();
        return input;}();
    VkPipelineInputAssemblyStateCreateInfo assembly = [] () {
        VkPipelineInputAssemblyStateCreateInfo assembly{};
        assembly.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
        assembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
        assembly.primitiveRestartEnable = VK_FALSE;
        return assembly;}();
    VkPipelineViewportStateCreateInfo viewport = [] () {
        VkPipelineViewportStateCreateInfo viewport{};
        viewport.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
        viewport.viewportCount = 1;
        viewport.scissorCount = 1;
        return viewport;}();
    VkPipelineRasterizationStateCreateInfo rasterize = [micro] () {
        VkPipelineRasterizationStateCreateInfo rasterize{};
        rasterize.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
        rasterize.depthClampEnable = VK_FALSE;
        rasterize.rasterizerDiscardEnable = (Component__Micro__MicroOut(micro) == Discard ? VK_TRUE : VK_FALSE);
        rasterize.polygonMode = VK_POLYGON_MODE_FILL;
        rasterize.lineWidth = 1.0f;
        rasterize.cullMode = VK_CULL_MODE_NONE;
        rasterize.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
        rasterize.depthBiasEnable = VK_FALSE;
        return rasterize;}();
    VkPipelineMultisampleStateCreateInfo multisample = [] () {
        VkPipelineMultisampleStateCreateInfo multisample{};
        multisample.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
        multisample.sampleShadingEnable = VK_FALSE;
        multisample.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
        return multisample;}();
    VkPipelineColorBlendAttachmentState attachment = [] () {
        VkPipelineColorBlendAttachmentState attachment{};
        attachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
            VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
        attachment.blendEnable = VK_FALSE;
        return attachment;}();
    VkPipelineColorBlendStateCreateInfo blend = [&attachment] () {
        VkPipelineColorBlendStateCreateInfo blend{};
        blend.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
        blend.logicOpEnable = VK_FALSE;
        blend.logicOp = VK_LOGIC_OP_COPY;
        blend.attachmentCount = 1;
        blend.pAttachments = &attachment;
        blend.blendConstants[0] = 0.0f;
        blend.blendConstants[1] = 0.0f;
        blend.blendConstants[2] = 0.0f;
        blend.blendConstants[3] = 0.0f;
        return blend;}();
    std::vector<VkDynamicState> states = {VK_DYNAMIC_STATE_VIEWPORT,VK_DYNAMIC_STATE_SCISSOR};
    VkPipelineDynamicStateCreateInfo state = [&states] () {
        VkPipelineDynamicStateCreateInfo state{};
        state.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
        state.dynamicStateCount = static_cast<int>(states.size());
        state.pDynamicStates = states.data();
        return state;}();
    VkGraphicsPipelineCreateInfo info{};
    info.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    info.stageCount = stages.size();
    info.pStages = stages.data();
    info.pVertexInputState = &input;
    info.pInputAssemblyState = &assembly;
    info.pViewportState = &viewport;
    info.pRasterizationState = &rasterize;
    info.pMultisampleState = &multisample;
    info.pColorBlendState = &blend;
    info.pDynamicState = &state;
    info.layout = layout;
    info.renderPass = render;
    info.subpass = 0;
    info.basePipelineHandle = VK_NULL_HANDLE;
    if (vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &info, nullptr, &pipeline) != VK_SUCCESS)
        throw std::runtime_error("failed to create graphic pipeline!");
    for (auto i = modules.begin(); i != modules.end(); i++)
    vkDestroyShaderModule(device, *i, nullptr);}
~PipelineState() {
    vkDestroyPipeline(device, pipeline, nullptr);
    vkDestroyPipelineLayout(device, layout, nullptr);
    vkDestroyDescriptorSetLayout(device, dlayout, nullptr);}
};

struct DrawState {
    VkDevice device;
    VkQueue graphic;
    VkQueue present;
    VkRenderPass render;
    VkCommandPool pool;
    VkSemaphore available;
    VkSemaphore signal;
    VkCommandBuffer command;
    VkFence fence;
    uint32_t index;
    PipelineState* pipeline;
DrawState(MainState *state, int size, WrapTag tag) {
    DeviceState* logical = state->logicalState;
    this->device = logical->device;
    this->graphic = logical->graphic;
    this->present = logical->present;
    this->render = logical->render;
    this->pool = logical->pool;
    this->pipeline = new PipelineState(device,render,logical->dpool,(Micro)size);}
void init() {
    // this called in separate thread on newly constructed
    available = [](VkDevice device) {
        VkSemaphore semaphore;
        VkSemaphoreCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
        if (vkCreateSemaphore(device, &info, nullptr, &semaphore) != VK_SUCCESS)
            throw std::runtime_error("failed to create semaphore!");
        return semaphore;}(device);
    signal = [](VkDevice device) {
        VkSemaphore semaphore;
        VkSemaphoreCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
        if (vkCreateSemaphore(device, &info, nullptr, &semaphore) != VK_SUCCESS)
            throw std::runtime_error("failed to create semaphore!");
        return semaphore;}(device);
    command = [](VkDevice device, VkCommandPool pool) {
        VkCommandBuffer command;
        VkCommandBufferAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
        info.commandPool = pool;
        info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
        info.commandBufferCount = (int)1;
        if (vkAllocateCommandBuffers(device, &info, &command) != VK_SUCCESS)
            throw std::runtime_error("failed to allocate command buffers!");
        return command;}(device,pool);
    fence = [](VkDevice device) {
        VkFence fence;
        VkFenceCreateInfo fenceInfo{};
        fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
        fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;
        if (vkCreateFence(device, &fenceInfo, nullptr, &fence) != VK_SUCCESS)
            throw std::runtime_error("failed to create fence!");
        return fence;}(device);}
~DrawState() {
    vkDestroyFence(device,fence,0);
    vkFreeCommandBuffers(device, pool, 1, &command);
    vkDestroySemaphore(device, available, nullptr);
    vkDestroySemaphore(device, signal, nullptr);
    delete pipeline;}
VkFence setup(const std::vector<BufferState*> &buffer, SwapState* swap, VkExtent2D extent, int base, int limit) {
    VkResult result = vkAcquireNextImageKHR(device, swap->swap, UINT64_MAX, available, VK_NULL_HANDLE, &index);
    if (result == VK_ERROR_OUT_OF_DATE_KHR) std::cerr << "out of date" << std::endl;
    else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
        throw std::runtime_error("failed to acquire swap chain image!");
    vkResetFences(device, 1, &fence);
    vkResetCommandBuffer(command, /*VkCommandBufferResetFlagBits*/ 0);
    [](VkCommandBuffer command){
        VkCommandBufferBeginInfo info{};
        info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        if (vkBeginCommandBuffer(command, &info) != VK_SUCCESS)
            throw std::runtime_error("failed to begin recording command buffer!");
    }(command);
    [](VkRenderPass render, VkFramebuffer framebuffer, VkExtent2D extent, VkCommandBuffer command) {
        VkRenderPassBeginInfo info{};
        info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        info.renderPass = render;
        info.framebuffer = framebuffer;
        info.renderArea.offset = {0, 0};
        info.renderArea.extent = extent;
        VkClearValue clearColor = {{{0.0f, 0.0f, 0.0f, 1.0f}}};
        info.clearValueCount = 1;
        info.pClearValues = &clearColor;
        vkCmdBeginRenderPass(command, &info, VK_SUBPASS_CONTENTS_INLINE);
    } (render,swap->framebuffers[index],extent,command);
    vkCmdBindPipeline(command, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->pipeline);
    [](VkExtent2D extent, VkCommandBuffer command){
        VkViewport info{};
        info.x = 0.0f; info.y = 0.0f;
        info.width = (float) extent.width;
        info.height = (float) extent.height;
        info.minDepth = 0.0f; info.maxDepth = 1.0f;
        vkCmdSetViewport(command, 0, 1, &info);}(extent,command);
    [](VkExtent2D extent, VkCommandBuffer command){
        VkRect2D scissor{};
        scissor.offset = {0, 0};
        scissor.extent = extent;
        vkCmdSetScissor(command, 0, 1, &scissor);}(extent,command);
    int count = 0; for (int i = 0; i < buffer.size(); i++) {
        if (buffer[i]->bind(count,command,pipeline->descriptor)) count++;}
    vkCmdBindDescriptorSets(command, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->layout, 0, 1,
        &pipeline->descriptor, 0, nullptr);
    vkCmdDraw(command, limit-base, (limit-base)/3, base, base/3);
    vkCmdEndRenderPass(command);
    if (vkEndCommandBuffer(command) != VK_SUCCESS)
        throw std::runtime_error("failed to record command buffer!");
    [](VkQueue graphic, VkCommandBuffer command, VkFence fence, VkSemaphore available, VkSemaphore signal){
        VkSubmitInfo info{};
        info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore availables[] = {available};
        VkSemaphore signals[] = {signal};
        VkPipelineStageFlags stages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
        info.waitSemaphoreCount = 1;
        info.pWaitSemaphores = availables;
        info.pWaitDstStageMask = stages;
        info.commandBufferCount = 1;
        info.pCommandBuffers = &command;
        info.signalSemaphoreCount = 1;
        info.pSignalSemaphores = signals;
        if (vkQueueSubmit(graphic, 1, &info, fence) != VK_SUCCESS)
            throw std::runtime_error("failed to submit draw command buffer!");
    }(graphic,command,fence,available,signal);
    [](VkSwapchainKHR swap, VkQueue present, uint32_t index, VkSemaphore signal){
        VkPresentInfoKHR info{};
        info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
        VkSemaphore signals[] = {signal};
        info.waitSemaphoreCount = 1;
        info.pWaitSemaphores = signals;
        VkSwapchainKHR swaps[] = {swap};
        info.swapchainCount = 1;
        info.pSwapchains = swaps;
        info.pImageIndices = &index;
        VkResult result = vkQueuePresentKHR(present, &info);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) std::cerr << "out of date" << std::endl;
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            throw std::runtime_error("device lost on wait for fence!");
    }(swap->swap,present,index,signal);
    return fence;}
};

struct SafeState {
    sem_t semaphore;
    SafeState() {
        if (sem_init(&semaphore, 0, 1) != 0) throw std::runtime_error("failed to create semaphore!");
    }
    ~SafeState() {
        if (sem_destroy(&semaphore) != 0) {
        std::cerr << "cannot destroy semaphore!" << std::endl; std::terminate();}
    }
    void wait() {
        if (sem_wait(&semaphore) != 0) throw std::runtime_error("cannot wait for semaphore!");
    }
    void post() {
        if (sem_post(&semaphore) != 0) throw std::runtime_error("cannot post to semaphore!");
    }
    bool trywait() {
        int tryval = sem_trywait(&semaphore);
        if (tryval != 0 && errno != EAGAIN) throw std::runtime_error("cannot trywait for semaphore!");
        return (tryval == 0);
    }
    int get() {
        int sval;
        if (sem_getvalue(&semaphore,&sval) != 0) throw std::runtime_error("cannot get semaphore!");
        return sval;
    }
};

void vulkanSafe();
struct ThreadState {
    VkDevice device;
    SafeState protect;
    SafeState semaphore;
    pthread_t thread;
    bool finish;
    std::deque<std::function<VkFence()>> setup;
    std::deque<VkFence> fence; // fence to wait for
    std::deque<int> order; // pushed or pending sequence number
    std::set<int> lookup; // whether sequence number unfinished
    std::deque<std::function<VkFence()>> presetup;
    std::deque<int> preord; // sequence number for push from separate thread
    std::deque<std::function<bool()>> when; // whether to push from separate thread
    int limit; int seqnum; int prog; bool pend;
    ThreadState(VkDevice device, int limit) {
        this->device = device; finish = false;
        this->limit = limit; seqnum = 0; prog = 0; pend = false;
        if (pthread_create(&thread,0,separate,this) != 0) throw std::runtime_error("failed to create thread!");
    }
    ~ThreadState() {
    // this destructed to wait for device idle
        protect.wait(); finish = true;
        protect.post(); semaphore.post();
        if (pthread_join(thread,0) != 0) {std::cerr << "failed to join thread!" << std::endl; std::terminate();}
    }
    static void *separate(void *ptr) {
        struct ThreadState *arg = (ThreadState*)ptr;
        arg->protect.wait();
        while (1) {
            if (arg->finish) {
                arg->protect.post();
                break;}
            while (!arg->when.empty() && arg->clear(arg->when.front())) {
                int ord = arg->preord.front(); arg->order.push_back(ord);
                arg->setup.push_back(arg->presetup.front()); arg->presetup.pop_front();
                arg->when.pop_front(); arg->preord.pop_front();}
            while (!arg->setup.empty()) {
                std::function<VkFence()> setup = arg->setup.front();
                VkFence fence;
                arg->protect.post();
                fence = setup();
                arg->protect.wait();
                arg->fence.push_back(fence);
                arg->setup.pop_front();}
            if (arg->fence.empty()) {
                arg->protect.post();
                arg->semaphore.wait();
                arg->protect.wait();}
            else {
                VkResult result = VK_SUCCESS; if (arg->fence.front() != VK_NULL_HANDLE) {
                VkFence fence = arg->fence.front();
                arg->protect.post();
                result = vkWaitForFences(arg->device,1,&fence,VK_FALSE,NANOSECONDS);
                arg->protect.wait();}
                if (result != VK_SUCCESS && result != VK_TIMEOUT) throw std::runtime_error("cannot wait for fence!");
                if (result == VK_SUCCESS) {int next = arg->order.front();
                arg->lookup.erase(next); arg->order.pop_front(); arg->fence.pop_front();
                if (arg->pend) {vulkanSafe(); arg->pend = false;}
                arg->prog += 1;}}}
        vkDeviceWaitIdle(arg->device);
        return 0;
    }
    int mark() {
        int ret;
        protect.wait();
        ret = prog;
        protect.post();
        return ret;
    }
    bool mark(int given) {
        bool ret;
        protect.wait();
        pend = (prog == given); // whether to wake later
        ret = !pend; // whether to tight loop
        protect.post();
        return ret;
    }
    bool clear(int given) {
    // return whether given sequence number is completed
        protect.wait();
        if (lookup.find(given) == lookup.end()) {
        protect.post();
        return true;}
        protect.post();
        return false;
    }
    bool clear(std::function<bool()> given) {
    // check given status from separate thread
        if (protect.get() != 0) throw std::runtime_error("cannot clear function!");
        protect.post();
        if (given()) {protect.wait(); return true;}
        protect.wait(); return false;
    }
    bool push() {
        bool ret;
        protect.wait();
        ret = (order.size() < limit);
        protect.post();
        return ret;
    }
    std::function<bool()> push(std::function<VkFence()> given, std::function<bool()> last) {
    // return query of whether fence is done for given function started in indicated sequence
        protect.wait();
        int sval = semaphore.get();
        if (sval == 0) semaphore.post();
        int ord = seqnum++; lookup.insert(ord);
        preord.push_back(ord); when.push_back(last); presetup.push_back(given);
        std::function<bool()> done = [this,ord](){return this->clear(ord);};
        if (fence.size()+setup.size() != order.size()) throw std::runtime_error("cannot push seqnum!");
        if (order.size()+preord.size() != lookup.size()) throw std::runtime_error("cannot insert seqnum!");
        protect.post();
        return done;
    }
    std::function<bool()> push(std::function<void()> given, std::function<bool()> last) {
        return push((std::function<VkFence()>)[given](){given(); return VK_NULL_HANDLE;},last);
    }
};

struct TempState {
    int seqnum;
    std::map<int,std::deque<std::function<bool()>>> done;
    std::set<int> pend;
    SafeState safe;
    TempState() {
        seqnum = 0;
    }
    void temq() {
        // maintain main state lambdas
        // separate thread lambdas must protect themselves
        bool doto = true;
        safe.wait();
        while (doto) {std::deque<int> todo; doto = false;
        for (auto i = done.begin(); i != done.end(); i++) {
        while (!(*i).second.empty() && (*i).second.front()()) (*i).second.pop_front();
        if ((*i).second.empty()) todo.push_back((*i).first);}
        for (auto i = todo.begin(); i != todo.end(); i++) {doto = true; done.erase(*i);}}
        safe.post();
    }
    bool temq(int tag) {
        // thread safe check of indicated lambda
        safe.wait();
        if (pend.find(tag) != pend.end()) {
        safe.post(); return false;}
        if (done.find(tag) == done.end()) {
        safe.post(); return true;}
        safe.post(); return false;
    }
    int temp() {
        // return new identifier
        int tag;
        safe.wait();
        tag = seqnum++;
        pend.insert(tag);
        safe.post();
        return tag;
    }
    void tmpq(int tag) {
        safe.wait();
        if (pend.find(tag) == pend.end()) throw std::runtime_error("temq already called in temq!");
        pend.erase(tag);
        safe.post();
    }
    std::function<bool()> temp(int tag) {
        // return lambda for identifier
        return [this,tag](){return temq(tag);};
    }
    void temp(int tag, std::function<bool()> fnc) {
        // append condition
        safe.wait();
        if (pend.find(tag) == pend.end()) throw std::runtime_error("disabled identifier!");
        done[tag].push_back(fnc);
        safe.post();
    }
    std::function<bool()> temp(bool &vld, int &tag) {
        if (vld) return temp(tag);
        vld = true;
        tag = temp();
        return temp(tag);
    }
    void temp(bool &vld, int &tag, std::function<bool()> given) {
        if (!vld) {
        vld = true;
        tag = temp();}
        temp(tag,given);
    }
    void temq(bool &vld, int tag) {
        if (!vld) throw std::runtime_error("temq call not valid!");
        vld = false;
        tmpq(tag);
    }
};

template<class Buffer> struct WrapState {
    std::deque<Buffer*> pool;
    std::deque<Buffer*> running; std::deque<std::function<bool()>> toready;
    Buffer* ready; std::deque<std::function<bool()>> toinuse;
    std::deque<Buffer*> inuse; std::deque<std::function<bool()>> topool;
    std::set<void*> lookup;
    int size; void *copy;
    int count; int limit;
    bool seqvld; int seqtag;
    bool sepvld; int septag;
    bool setvld; int settag;
    std::pair<Buffer*,bool> setbuf;
    WrapTag tag; MainState *info;
    ThreadState *thread;
    TempState *temp;
    WrapState(MainState *info, int limit, WrapTag tag) {
        ready = 0;
        size = 0;
        copy = 0;
        count = 0;
        this->limit = limit;
        seqvld = false;
        setvld = false;
        setbuf = std::make_pair((Buffer*)0,false);
        this->tag = tag;
        this->info = info;
        thread = info->threadState;
        temp = info->tempState;
    }
    ~WrapState() {
        while (!pool.empty()) {delete pool.front(); pool.pop_front();}
        while (!running.empty()) {delete running.front(); running.pop_front();}
        if (ready) delete ready;
        while (!inuse.empty()) {delete inuse.front(); inuse.pop_front();}
    }
    bool clr(int siz) {
    // advance queues with done fronts
        while (!toinuse.empty() && toinuse.front()()) {
            toinuse.pop_front();}
        while (!toready.empty() && toready.front()()) {
            if (ready && !toinuse.empty()) {
                while (toinuse.size() > 1) {inuse.push_back(0); topool.push_back(toinuse.front()); toinuse.pop_front();}
                inuse.push_back(ready); topool.push_back(toinuse.front()); toinuse.pop_front(); ready = 0;}
            if (!toinuse.empty()) throw std::runtime_error("cannot clr ready!");
            if (ready) pool.push_back(ready);
            ready = running.front();
            if (tag == SwapBuf) std::cerr << "ready " << ready << std::endl;
            running.pop_front(); toready.pop_front();}
        while (!topool.empty() && topool.front()()) {
            if (inuse.front()) pool.push_back(inuse.front());
            inuse.pop_front(); topool.pop_front();}
    // change queue item size
        if (siz != size) {size = siz;
            while (!pool.empty()) {delete pool.front(); pool.pop_front(); count--;}
            // TODO do this when copy needed in separate thread
            if (tag != DrawBuf && tag != SwapBuf) copy = realloc(copy,size);}
    // return whether queues are not full
        if (!thread->push()) return false;
        if (!pool.empty()) return true;
        if (count < limit) return true;
        return false;
    }
    WrapTag typ() {
        return tag;
    }
    bool clr() {
        return clr(size);
    }
    std::function<bool()> seq() {
    // produce marker to wait for in separate thread
        return temp->temp(seqvld,seqtag);
    }
    void seq(std::function<bool()> given) {
    // add function to wait for in separate thread
        temp->temp(seqvld,seqtag,given);
    }
    std::function<bool()> sep() {
    // produce lambda that will depend on fence
        return temp->temp(sepvld,septag);
    }
    void sep(std::function<bool()> given) {
    // make lambda depend on fence
        temp->temp(sepvld,septag,given);
    }
    std::function<bool()> tmp() {
    // produce lambda for when unreserved
        return temp->temp(setvld,settag);
    }
    void tmp(std::function<bool()> given) {
    // add function to postpone unreserve
        temp->temp(setvld,settag,given);
    }
    std::pair<Buffer *, bool> buf() {
    // produce next buffer to modify
        if (setbuf.first) return setbuf; else clr();
        if (pool.empty()) {if (count == limit) ERROR();
        pool.push_back(new Buffer(info,size,tag)); setbuf.second = true; count++;}
        else setbuf.second = false;
        setbuf.first = pool.front(); pool.pop_front();
        return setbuf;
    }
    std::function<bool()> set(std::function<bool()> done) {
        sep(done); // postpone those depending on this setup
        tmp(done); // postpone buffer ready until after setup
        temp->temq(sepvld,septag); // subsequent seq depend on subsequent set
        temp->temq(seqvld,seqtag); // subsequent set depend on subsequent seq
        setbuf.second = false; // subsequent set need not push init
        return done;
    }
    std::function<bool()> set(std::function<void(Buffer*buf)> setup) {
        Buffer *ptr = buf().first;
        return set((std::function<bool()>)(buf().second?
        thread->push((std::function<void()>)[setup,ptr](){ptr->init(); setup(ptr);},seq()):
        thread->push((std::function<void()>)[setup,ptr](){setup(ptr);},seq())));
    }
    std::function<bool()> set(std::function<VkFence(Buffer*)> setup) {
        Buffer *ptr = buf().first;
        return set((std::function<bool()>)(buf().second?
        thread->push((std::function<VkFence()>)[setup,ptr](){ptr->init(); return setup(ptr);},seq()):
        thread->push((std::function<VkFence()>)[setup,ptr](){return setup(ptr);},seq())));
    }
    std::function<bool()> set() {
        buf().first->bind(0,size,copy,[](){});
        return set((std::function<VkFence(Buffer*)>)[](Buffer*buf){return buf->setup();});
    }
    std::function<bool()> set(int loc, int siz, const void *ptr, std::function<void()> dat) {
        int size = (loc+siz > this->size ? loc+siz : this->size); clr(size);
        set((std::function<void(Buffer*)>)[this,loc,ptr,siz](Buffer*buf){
        memcpy((void*)((char*)copy+loc),ptr,siz);});
        if (tag == QueryBuf) return [](){return true;};
        if (buf().second) return set();
        buf().first->bind(loc,siz,ptr,dat);
        return set((std::function<VkFence(Buffer*)>)[](Buffer*buf){return buf->setup();});
    }
    void put() {
        if (tag == SwapBuf) std::cerr << "put " << buf().first << std::endl;
        running.push_back(buf().first); toready.push_back(tmp());
        temp->temq(setvld,settag); setbuf.first = 0;
    }
    bool get() {
    // return whether first enque after resize is ready
        clr();
        if (ready) return true;
        return false;
    }
    Buffer *get(std::function<bool()> done) {
    // get buffer and reserve it until given returns true
        clr();
        toinuse.push_back(done);
        return ready;
    }
    Buffer *get(int *siz, void *tag) {
    // get buffer reserved until returned tag is given back
        clr();
        if (ready == 0) return 0;
        *siz = size; lookup.insert(tag);
        toinuse.push_back([this,tag](){return (lookup.find(tag) != lookup.end());});
        return ready;
    }
    void get(void *tag) {
    // give back tag to unreserve buffer
        lookup.erase(tag);
    }
};

struct QueueState {
    WrapState<SwapState>* swapQueue;
    WrapState<BufferState>* bufferQueue[Memorys];
    WrapState<DrawState>* drawQueue[Micros];
    QueueState() {
        swapQueue = new WrapState<SwapState>(&mainState,mainState.MAX_FRAMEBUFFER_SWAPS,SwapBuf);
        for (int i = 0; i < Memorys; i++) {
        WrapTag tag = TypeQ__Memory__WrapTag((Memory)i); bufferQueue[i] = (tag == WrapTags ? 0 :
        new WrapState<BufferState>(&mainState,mainState.MAX_FRAMEBUFFER_SWAPS,tag));}
        for (int i = 0; i < Micros; i++) {
        Memory mem = BindQ__Micro__Int__Memory(MicroPRPC)((Micro)0); drawQueue[i] = (mem == Memorys ? 0 :
        new WrapState<DrawState>(&mainState,mainState.MAX_FRAMES_INFLIGHT,DrawBuf));}}
    ~QueueState() {
        for (int i = 0; i < Micros; i++) if (drawQueue[i]) delete drawQueue[i];
        for (int i = 0; i < Memorys; i++) if (bufferQueue[i]) delete bufferQueue[i];
        if (swapQueue) delete swapQueue;}
    std::vector<WrapState<BufferState>*> bindBuffer(Micro shader) {
        std::vector<WrapState<BufferState>*> bind;
        int i = 0; Memory mem = Memorys;
        while ((mem = BindQ__Micro__Int__Memory(shader)(i++)) != Memorys){
        bind.push_back(bufferQueue[mem]);}
        return bind;}
    std::vector<WrapState<BufferState>*> queryBuffer(Micro shader) {
        std::vector<WrapState<BufferState>*> query;
        int i = 0; Memory mem = Memorys;
        while((mem = QueryQ__Micro__Int__Memory(shader)(i++)) != Memorys)
        query.push_back(bufferQueue[mem]);
        return query;}
};

bool vulkanSet(Memory mem, int loc, int siz, void *ptr, std::function<void()> dat)
{
    WrapState<BufferState>* bufferQueue = mainState.queueState->bufferQueue[mem];
    if (!bufferQueue->clr()) return true;
    bufferQueue->set(loc,siz,ptr,dat); bufferQueue->put();
    return false;
}
bool vulkanDraw(enum Micro shader, int base, int limit)
{
    QueueState *queue = mainState.queueState;
    std::vector<BufferState*> buffer; SwapState *swap; DrawState *draw;
    WrapState<SwapState> *swapQueue = queue->swapQueue;
    std::vector<WrapState<BufferState>*> bindBuffer = queue->bindBuffer(shader);
    std::vector<WrapState<BufferState>*> queryBuffer = queue->queryBuffer(shader);
    WrapState<DrawState> *drawQueue = queue->drawQueue[shader];
    VkExtent2D extent; extent.width = mainState.windowCopy.width; extent.height = mainState.windowCopy.height;
    for (auto i = bindBuffer.begin(); i != bindBuffer.end(); i++) if (!(*i)->get()) return true;
    // buffers are available to get
    if (!queue->swapQueue->get()) return true;
    // swap buffer is available to get
    if (!drawQueue->clr(shader)) return true;
    // draw buffer is ready to set
    for (auto i = bindBuffer.begin(); i != bindBuffer.end(); i++) {
    buffer.push_back((*i)->get(drawQueue->tmp()));}
    swap = queue->swapQueue->get(drawQueue->tmp());
    std::function<bool()> done = drawQueue->set((std::function<VkFence(DrawState*)>)
    [buffer,swap,extent,base,limit](DrawState*draw){
    return draw->setup(buffer,swap,extent,base,limit);});
    for (auto i = queryBuffer.begin(); i != queryBuffer.end(); i++) {
    (*i)->seq(drawQueue->sep()); // wait to read image until after draw
    // kick off read of image buffer after finishing draw
    (*i)->set((std::function<VkFence(BufferState*buf)>)[](BufferState*buf){return buf->getup();});
    (*i)->put();}
    drawQueue->put();
    return false;
}
bool vulkanSwap()
{
    WrapState<SwapState>* swapQueue = mainState.queueState->swapQueue;
    if (!swapQueue->clr((mainState.swapCopy.width<<16)|(mainState.swapCopy.height))) return true;
    swapQueue->set((std::function<void(SwapState*)>)[](SwapState*buf){}); swapQueue->put();
    return false;
}
bool vulkanFollow()
{
    int siz = 16*sizeof(float); float *mat = (float*)malloc(siz); 
    return vulkanSet(Matrixz,mainState.paramFollow*siz,siz,planeWindow(mat),[mat](){free(mat);});
}
bool vulkanModify()
{
    int siz = 16*sizeof(float); float *mat = (float*)malloc(siz);
#ifdef PLANRA
    planraMatrix(mat);
#else
    planeMatrix(mat);
#endif
    return vulkanSet(Matrixz,mainState.paramModify*siz,siz,mat,[mat](){free(mat);});
}
bool vulkanDirect()
{
    // TODO write to Uniform
    return false;
}
bool vulkanDisplay()
{
    return vulkanDraw(mainState.paramDisplay,mainState.paramBase,mainState.paramLimit);
}
bool vulkanBright()
{
    return vulkanDraw(mainState.paramBright,mainState.paramBase,mainState.paramLimit);
}
bool vulkanDetect()
{
    return vulkanDraw(mainState.paramDetect,mainState.paramBase,mainState.paramLimit);
}
bool vulkanQuery()
{
    WrapState<BufferState> *ptr = mainState.queueState->bufferQueue[Piercez];
    TempState *tmp = mainState.tempState;
    if (!ptr->get()) return true;
    int tag = tmp->temp();
    planeReady(tag,(Pierce*)ptr->get(tmp->temp(tag))->bind());
    return false;
}
void vulkanDone(int tag)
{
    mainState.tempState->tmpq(tag);
}
bool vulkanDefer()
{
    struct Center *center = mainState.deferCopy;
    int siz; void *ptr; struct WrapState<BufferState> *buf;
    switch (center->mem) {default: throw std::runtime_error("unsupported mem!");
    break; case (Vertexz): siz = sizeof(center->vtx[0]); ptr = center->vtx;
    break; case (Matrixz): siz = sizeof(center->mat[0]); ptr = center->mat;}
    return vulkanSet(center->mem,center->idx*siz,siz*center->siz,ptr,[center](){planeDone(center);});
}
bool vulkanEnact(enum Enact hint, bool cond, bool tight)
{ // do work, and return if there is more work to do
    bool fail; int mark;
    if (mainState.manipEnact[hint] && cond) mainState.registerDone[hint] = true;
    if (!mainState.registerDone[hint]) return false;
    mark = mainState.threadState->mark(); // remember wakes after this
    switch (hint) {default: throw std::runtime_error("failed to call function!");
    break; case (Extent): fail = vulkanSwap();
    break; case (Follow): fail = vulkanFollow();
    break; case (Modify): fail = vulkanModify();
    break; case (Direct): fail = vulkanDirect();
    break; case (Display): fail = vulkanDisplay();
    break; case (Bright): fail = vulkanBright();
    break; case (Detect): fail = vulkanDetect();
    break; case (Query): fail = vulkanQuery();
    break; case (Defer): fail = vulkanDefer();}
    mainState.registerDone[hint] = fail; // will wake or already woke
    if (fail && mainState.threadState->mark(mark)) return true; // already woke
    return tight; // wait for wake unless there is already work to do
}
bool vulkanChange()
{ // do work, and return if there is more work to do
    bool moved = (mainState.windowMove.left != mainState.windowCopy.left || mainState.windowMove.base != mainState.windowCopy.base);
    bool sized = (mainState.windowMove.width != mainState.windowCopy.width || mainState.windowMove.height != mainState.windowCopy.height);
    bool moused = (mainState.mouseMove.left != mainState.mouseCopy.left || mainState.mouseMove.base != mainState.mouseCopy.base);
    bool queryd = !mainState.queryMove.empty();
    bool deferd = !mainState.deferMove.empty();
    bool drawed = ((mainState.manipEnact[Follow] && moved) || (mainState.manipEnact[Follow] && sized) ||
        (mainState.manipEnact[Modify] && moused) || (mainState.manipEnact[Direct] && moused));
    bool swaped = ((mainState.windowMove.width > mainState.swapMove.width) || (mainState.windowMove.height > mainState.swapMove.height));
    bool tight = false;
    if (moved && mainState.windowMove.moved) glfwSetWindowPos(mainState.openState->window,mainState.windowMove.left,mainState.windowMove.base);
    if (sized && mainState.windowMove.sized) glfwSetWindowSize(mainState.openState->window,mainState.windowMove.width,mainState.windowMove.height);
    if (moused && mainState.mouseMove.moved) glfwSetCursorPos(mainState.openState->window,mainState.mouseMove.left,mainState.mouseMove.base);
    if (swaped) {std::cerr << "vulkanChange swaped " << mainState.swapMove.width << "," << mainState.swapMove.height << std::endl;
        mainState.swapMove.width = mainState.windowMove.width+mainState.MAX_FRAMEBUFFER_RESIZE;
        mainState.swapMove.height = mainState.windowMove.height+mainState.MAX_FRAMEBUFFER_RESIZE;
    	mainState.swapCopy.width = mainState.swapMove.width+mainState.MAX_FRAMEBUFFER_RESIZE;
        mainState.swapCopy.height = mainState.swapMove.height+mainState.MAX_FRAMEBUFFER_RESIZE;}
    if (moved || sized) {mainState.windowMove.moved = mainState.windowMove.sized = false; mainState.windowCopy = mainState.windowMove;}
    if (moused) {mainState.mouseCopy.moved = mainState.mouseCopy.sized = false; mainState.mouseCopy = mainState.mouseMove;}
    if (queryd && !mainState.registerDone[Query]) {mainState.queryCopy = mainState.queryMove.front(); mainState.queryMove.pop_front();}
    if (deferd && !mainState.registerDone[Defer]) {mainState.deferCopy = mainState.deferMove.front(); mainState.deferMove.pop_front();}
    mainState.tempState->temq();
    tight = vulkanEnact(Extent,swaped,tight);
    tight = vulkanEnact(Follow,(moved||sized),tight);
    tight = vulkanEnact(Modify,moused,tight);
    tight = vulkanEnact(Direct,moused,tight);
    tight = vulkanEnact(Display,drawed,tight);
    tight = vulkanEnact(Bright,drawed,tight);
    tight = vulkanEnact(Detect,drawed,tight);
    tight = vulkanEnact(Query,queryd,tight);
    tight = vulkanEnact(Defer,deferd,tight);
    tight = tight || !mainState.queryMove.empty() || !mainState.deferMove.empty();
    return tight; // tight loop if any work remains
}

void vulkanInitial(enum Phase phase)
{
    switch (phase) {default: throw std::runtime_error("unsupported phase!");
    break; case (Init):
    std::cerr << "Initial,Init" << std::endl;
    break; case (Start):
    std::cerr << "Initial,Start" << std::endl;
    mainState.initState = new InitState(mainState.layers);
    break; case (Stop):
    std::cerr << "Initial,Stop" << std::endl;
    delete mainState.initState; mainState.initState = 0;}
}
void vulkanWindow(enum Phase phase)
{
    switch (phase) {default: throw std::runtime_error("unsupported phase!");
    break; case (Init):
    std::cerr << "Window,Init" << std::endl;
    mainState.windowMove.width = /*mainState.windowCopy.width =*/ 800;
    mainState.windowMove.height = /*mainState.windowCopy.height =*/ 800;
    break; case (Start):
    std::cerr << "Window,Start" << std::endl;
    mainState.openState = new OpenState(mainState.initState->instance,800,800,(void*)&mainState);
    break; case (Stop):
    std::cerr << "Window,Stop " << std::endl;
    delete mainState.openState; mainState.openState = 0;}
}
void vulkanGraphics(enum Phase phase)
{
    switch (phase) {default: throw std::runtime_error("unsupported phase!");
    break; case (Init):
    std::cerr << "Graphics,Init" << std::endl;
    {int32_t width, height, left, base, workx, worky, sizx, sizy; double posx, posy;
    GLFWwindow *window = mainState.openState->window;
    GLFWmonitor* monitor = glfwGetPrimaryMonitor();
    glfwGetMonitorWorkarea(monitor,&left,&base,&workx,&worky);
    glfwGetFramebufferSize(window,&sizx,&sizy);
    // glfwGetWindowSize(window,&width,&height);
    width = mainState.windowMove.width; height = mainState.windowMove.height;
    glfwGetCursorPos(window,&posx,&posy);
    left += (workx-width)/2; base += (worky-height)/2;
    posx -= (workx-width)/2; posy -= (worky-height)/2;
    glfwSetWindowPos(window,left,base); // TODO calculate this before create window
    if (mainState.manipReact[Relate]) {
    mainState.mouseClick.left = mainState.mouseMove.left = /*mainState.mouseCopy.left =*/ left+posx;
    mainState.mouseClick.base = mainState.mouseMove.base = /*mainState.mouseCopy.base =*/ base+posy;} else {
    mainState.mouseClick.left = mainState.mouseMove.left = /*mainState.mouseCopy.left =*/ posx;
    mainState.mouseClick.base = mainState.mouseMove.base = /*mainState.mouseCopy.base =*/ posy;}
    mainState.windowClick.width = mainState.windowMove.width = /*mainState.windowCopy.width =*/ width;
    mainState.windowClick.height = mainState.windowMove.height = /*mainState.windowCopy.height =*/ height;
    mainState.windowClick.left = mainState.windowMove.left = /*mainState.windowCopy.left =*/ left;
    mainState.windowClick.base = mainState.windowMove.base = /*mainState.windowCopy.base =*/ base;
    mainState.windowRatio.left = sizx; mainState.windowRatio.width = width;
    mainState.windowRatio.base = sizy; mainState.windowRatio.height = height;}
    break; case (Start):
    std::cerr << "Graphics,Start" << std::endl;
    mainState.physicalState = new PhysicalState(
    mainState.initState->instance,mainState.openState->surface,mainState.extensions,
    mainState.MAX_FRAMES_INFLIGHT);
    mainState.logicalState = [](PhysicalState *physical){
    return new DeviceState(physical->physical,physical->graphicid,physical->presentid,
    physical->image,mainState.layers,mainState.extensions,mainState.MAX_BUFFERS_AVAILABLE*Memorys);
    }(mainState.physicalState);
    mainState.threadState = new ThreadState(mainState.logicalState->device,
    mainState.MAX_FENCES_INFLIGHT);
    mainState.tempState = new TempState();
    mainState.queueState = new QueueState();
    break; case (Stop):
    std::cerr << "Graphics,Stop " << std::endl;
    delete mainState.threadState; mainState.threadState = 0;
    delete mainState.queueState; mainState.queueState = 0;
    delete mainState.tempState; mainState.tempState = 0;
    delete mainState.logicalState; mainState.logicalState = 0;
    delete mainState.physicalState; mainState.physicalState = 0;}
}
void vulkanProcess(enum Phase phase)
{
    switch (phase) {default: throw std::runtime_error("unsupported phase!");
    break; case (Init):
    std::cerr << "Process,Init" << std::endl;
    switch (mainState.registerPlan) {default: throw std::runtime_error("unsupported plan!");
    break; case(Regress): case (Bringup): {struct Center *center = 0; allocCenter(&center,1);
    center->mem = Configurez; center->siz = 6; center->idx = 0; center->slf = 0;
    allocConfigure(&center->cfg,6); allocInt(&center->val,6); center->cfg[0] = ManipEnact;
    center->cfg[1] = ParamDisplay; center->cfg[2] = ParamLimit; center->cfg[3] = RegisterDone;
    center->cfg[4] = WindowRead; center->cfg[5] = WindowWrite;
    center->val[0] = (1<<Display)|(1<<Follow)|(1<<Extent)|(1<<Defer);
    center->val[1] = MicroPRPC; center->val[2] = 6; center->val[3] = (1<<Display);
    center->val[4] = Effect; center->val[5] = Infect;
    planeCopy(&center); freeCenter(center); allocCenter(&center,0);}
    {int len = 0; char *str; char *tmp;
    struct Center *center = 0; allocCenter(&center,1);
    center->mem = Vertexz; center->siz = 6; center->idx = 0; center->slf = 0;
    allocVertex(&center->vtx,6);
    asprintf(&str,"Vertex(");
    asprintf(&str,"%svec[0]:Old(0.5)vec[1]:Old(-0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
    asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
    len = 0; hideVertex(&center->vtx[0],str,&len); free(str);
    asprintf(&str,"Vertex(");
    asprintf(&str,"%svec[0]:Old(0.5)vec[1]:Old(0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
    asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
    len = 0; hideVertex(&center->vtx[1],str,&len); free(str);
    asprintf(&str,"Vertex(");
    asprintf(&str,"%svec[0]:Old(-0.5)vec[1]:Old(-0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
    asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
    len = 0; hideVertex(&center->vtx[2],str,&len); free(str);
    asprintf(&str,"Vertex(");
    asprintf(&str,"%svec[0]:Old(-0.5)vec[1]:Old(0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
    asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
    len = 0; hideVertex(&center->vtx[3],str,&len); free(str);
    asprintf(&str,"Vertex(");
    asprintf(&str,"%svec[0]:Old(-0.5)vec[1]:Old(-0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
    asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
    len = 0; hideVertex(&center->vtx[4],str,&len); free(str);
    asprintf(&str,"Vertex(");
    asprintf(&str,"%svec[0]:Old(0.5)vec[1]:Old(0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
    asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
    len = 0; hideVertex(&center->vtx[5],str,&len); free(str);
    planeCopy(&center); freeCenter(center); allocCenter(&center,0);}}
    break; case (Start):
    std::cerr << "Process,Start" << std::endl;
    mainState.registerOpen = true;
    break; case (Stop):
    std::cerr << "Process,Stop" << std::endl;
    mainState.registerOpen = false;}
}

void vulkanInit()
{
    for (int arg = 0; arg < mainState.argc; arg++) planePutstr(mainState.argv[arg]);
}
void planraBoot()
{
    struct Center *center = 0; allocCenter(&center,1);
    center->mem = Configurez; center->siz = 4; center->idx = 0; center->slf = 0;
    allocConfigure(&center->cfg,4); allocInt(&center->val,4);
    center->cfg[0] = RegisterPlan; center->cfg[1] = RegisterPoll;
    center->cfg[2] = RegisterInit; center->cfg[3] = RegisterOpen;
    center->val[0] = PLAN; center->val[1] = 100;
    center->val[2] = center->val[3] = (1<<Initial)|(1<<Window)|(1<<Graphics)|(1<<Process);
    planeCopy(&center); freeCenter(center); allocCenter(&center,0);
}
int vulkanLoop()
{ // do work if any, and return if there is more work to do
    if (!mainState.registerOpen) return 0;
    glfwPollEvents(); // in case vulkanBlock not called
    return vulkanChange();
}
int vulkanBlock()
{ // wait for work, and return if there might be work to do
    if (!mainState.registerOpen) return 0;
    glfwWaitEventsTimeout(0.0001*mainState.registerPoll);
    return 1;
}
void vulkanPhase(enum Thread thread, enum Phase phase)
{
    switch (thread) {default: throw std::runtime_error("unsupported thread!");
    break; case (Initial): vulkanInitial(phase);
    break; case (Window): vulkanWindow(phase);
    break; case (Graphics): vulkanGraphics(phase);
    break; case (Process): vulkanProcess(phase);}
}
void vulkanSafe()
{
    glfwPostEmptyEvent();
}
void vulkanCopy(struct Center **given)
{
    struct Center *center = *given;
    switch (center->mem) {default: mainState.deferMove.push_back(center); *given = 0;
    break; case (Littlez): if (center->idx+center->siz > mainState.littleSize) {
    mainState.littleSize = center->idx+center->siz; mainState.littleState =
    (struct Little *)realloc(mainState.littleState,mainState.littleSize*sizeof(center->pvn[0]));}
    memcpy(mainState.littleState,center->pvn+center->idx,center->siz*sizeof(center->pvn[0]));
    planeDone(center); *given = 0; return;
    break; case (Configurez):
    for (int i = 0; i < center->siz; i++)
    switch (center->cfg[i]) {default:
    break; case (CursorIndex): mainState.mouseIndex = center->val[i];
    break; case (CursorRead): mainState.mouseRead = (Interp)center->val[i];
    break; case (CursorWrite): mainState.mouseWrite = (Interp)center->val[i];
    break; case (CursorLeft): switch (mainState.mouseWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.mouseClick.left = center->val[i]; break;
    case (Infect): mainState.mouseMove.left = center->val[i]; mainState.mouseMove.moved = true; break;
    case (Effect): mainState.mouseCopy.left = center->val[i]; break;}
    break; case (CursorBase): switch (mainState.mouseWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.mouseClick.base = center->val[i]; break;
    case (Infect): mainState.mouseMove.base = center->val[i]; mainState.mouseMove.moved = true; break;
    case (Effect): mainState.mouseCopy.base = center->val[i]; break;}
    break; case (CursorAngle): switch (mainState.mouseWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.mouseClick.angle = center->val[i]; break;
    case (Infect): mainState.mouseMove.angle = center->val[i]; mainState.mouseMove.sized = true; break;
    case (Effect): mainState.mouseCopy.angle = center->val[i]; break;}
    break; case (CursorPress): if (center->val[i] == 0)
    mainState.linePress.clear(); else mainState.linePress.push_front(center->val[i]);
    break; case (WindowRead): mainState.windowRead = (Interp)center->val[i];
    break; case (WindowWrite): mainState.windowWrite = (Interp)center->val[i];
    break; case (WindowLeft): switch (mainState.windowWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.windowClick.left = center->val[i]; break;
    case (Infect): mainState.windowMove.left = center->val[i]; mainState.windowMove.moved = true; break;
    case (Effect): mainState.windowCopy.left = center->val[i]; break;}
    break; case (WindowBase): switch (mainState.windowWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.windowClick.base = center->val[i]; break;
    case (Infect): mainState.windowMove.base = center->val[i]; mainState.windowMove.moved = true; break;
    case (Effect): mainState.windowCopy.base = center->val[i]; break;}
    break; case (WindowWidth): switch (mainState.windowWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.windowClick.width = center->val[i]; break;
    case (Infect): mainState.windowMove.width = center->val[i]; mainState.windowMove.sized = true; break;
    case (Effect): mainState.windowCopy.width = center->val[i]; break;}
    break; case (WindowHeight): switch (mainState.windowWrite) {default: throw std::runtime_error("cannot get info!");
    case (Affect): mainState.windowClick.height = center->val[i]; break;
    case (Infect): mainState.windowMove.height = center->val[i]; mainState.windowMove.sized = true; break;
    case (Effect): mainState.windowCopy.height = center->val[i]; break;}
    break; case (RegisterPlan): mainState.registerPlan = (Plan)center->val[i];
    break; case (RegisterPoll): mainState.registerPoll = center->val[i];
    break; case (RegisterDone): for (int j = 0; j < Enacts; j++)
    mainState.registerDone[(Enact)j] = (mainState.registerDone[(Enact)j] || ((center->val[i]&(1<<j)) != 0));
    break; case (ManipReact): for (int j = 0; j < Reacts; j++)
    mainState.manipReact[(React)j] = ((center->val[i]&(1<<j)) != 0);
    break; case (ManipEnact): for (int j = 0; j < Enacts; j++)
    mainState.manipEnact[(Enact)j] = ((center->val[i]&(1<<j)) != 0);
    break; case (ManipAction): for (int j = 0; j < Actions; j++)
    mainState.manipAction[(Action)j] = ((center->val[i]&(1<<j)) != 0);
    break; case (ParamFollow): mainState.paramFollow = center->val[i];
    break; case (ParamModify): mainState.paramModify = center->val[i];
    break; case (ParamDisplay): mainState.paramDisplay = (Micro)center->val[i];
    break; case (ParamBright): mainState.paramBright = (Micro)center->val[i];
    break; case (ParamDetect): mainState.paramDetect = (Micro)center->val[i];
    break; case (ParamBase): mainState.paramBase = center->val[i];
    break; case (ParamLimit): mainState.paramLimit = center->val[i];
    break; case (LittleIndex): mainState.littleIndex = center->val[i];}
    planeDone(center); *given = 0;}
}
int vulkanInfo(enum Configure query)
{
    switch (query) {default: throw std::runtime_error("cannot get info!");
    break; case (CursorLeft): switch (mainState.mouseRead) {default: throw std::runtime_error("cannot get info!");
       case (Affect): return mainState.mouseClick.left;
    case (Infect): return mainState.mouseMove.left;
    case (Effect): return mainState.mouseCopy.left;}
    break; case (CursorBase): switch (mainState.mouseRead) {default: throw std::runtime_error("cannot get info!");
    case (Affect): return mainState.mouseClick.base;
    case (Infect): return mainState.mouseMove.base;
    case (Effect): return mainState.mouseCopy.base;}
    break; case (CursorAngle): switch (mainState.mouseRead) {default: throw std::runtime_error("cannot get info!");
    case (Affect): return mainState.mouseClick.angle;
    case (Infect): return mainState.mouseMove.angle;
    case (Effect): return mainState.mouseCopy.angle;}
    break; case (CursorPress): {if (mainState.linePress.empty()) return 0;
    int key = mainState.linePress.front(); mainState.linePress.pop_front(); return key;}
    break; case (WindowLeft): switch (mainState.windowRead) {default: throw std::runtime_error("cannot get info!");
    case (Affect): return mainState.windowClick.left;
    case (Infect): return mainState.windowMove.left;
    case (Effect): return mainState.windowCopy.left;}
    break; case (WindowBase): switch (mainState.windowRead) {default: throw std::runtime_error("cannot get info!");
    case (Affect): return mainState.windowClick.base;
    case (Infect): return mainState.windowMove.base;
    case (Effect): return mainState.windowCopy.base;}
    break; case (WindowWidth): switch (mainState.windowRead) {default: throw std::runtime_error("cannot get info!");
    case (Affect): return mainState.windowClick.width;
    case (Infect): return mainState.windowMove.width;
    case (Effect): return mainState.windowCopy.width;}
    break; case (WindowHeight): switch (mainState.windowRead) {default: throw std::runtime_error("cannot get info!");
    case (Affect): return mainState.windowClick.height;
    case (Infect): return mainState.windowMove.height;
    case (Effect): return mainState.windowCopy.height;}
    break; case (MonitorWidth): return mainState.windowRatio.width;
    break; case (MonitorHeight): return mainState.windowRatio.height;
    break; case (PhysicalWidth): return mainState.windowRatio.left;
    break; case (PhysicalHeight): return mainState.windowRatio.base;
    break; case (RegisterPlan): return mainState.registerPlan;
    break; case (RegisterDone): {int val = 0; for (int j = 0; j < Enacts; j++)
    if (mainState.registerDone[(Enact)j]) val |= (1<<j); return val;}
    break; case (ManipReact): {int val = 0; for (int j = 0; j < Reacts; j++)
    if (mainState.manipReact[(React)j]) val |= (1<<j); return val;}
    break; case (ManipEnact): {int val = 0; for (int j = 0; j < Enacts; j++)
    if (mainState.manipEnact[(Enact)j]) val |= (1<<j); return val;}
    break; case (ManipAction): {int val = 0; for (int j = 0; j < Actions; j++)
    if (mainState.manipAction[(Action)j]) val |= (1<<j); return val;}}
    return 0;
}
void planraWake(enum Configure hint);
int main(int argc, char **argv)
{
    mainState.argc = argc;
    mainState.argv = argv;
    try {
#ifdef PLANRA
       planeInit(vulkanInit,planraBoot,planeMain,vulkanLoop,vulkanBlock,planraWake,vulkanPhase,vulkanSafe,vulkanCopy,vulkanInfo,vulkanDone);
#else
       planeInit(vulkanInit,planeBoot,planeMain,vulkanLoop,vulkanBlock,planeWake,vulkanPhase,vulkanSafe,vulkanCopy,vulkanInfo,vulkanDone);
#endif
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return -1;
    }
    return 0;
}

int planraDone = 0;
int planraOnce = 0;
struct timeval planraTime;
float planraLast = 0.0;
void planraWake(enum Configure hint)
{
    if (!planraOnce) {
        planraOnce = 1;
        gettimeofday(&planraTime, NULL);
    }
    if (planraDone) return;
    struct timeval stop; gettimeofday(&stop, NULL);
    float time = (stop.tv_sec - planraTime.tv_sec) + (stop.tv_usec - planraTime.tv_usec) / (double)MICROSECONDS;
    if (time > 4.0 && vulkanInfo(RegisterPlan) == Regress) planraDone = 1;
    if (hint == CursorPress) {
        int key1 = vulkanInfo(CursorPress);
        int key2 = vulkanInfo(CursorPress);
        if (key1 == 256 && key2 == 257) planraDone = 1;
        if (key1 == 256 && key2 == 0) {
        struct Center *center = 0; allocCenter(&center,1);
        allocConfigure(&center->cfg,1); allocInt(&center->val,1);
        center->cfg[0] = CursorPress; center->val[0] = 256;
        center->mem = Configurez; center->idx = 0; center->siz = 1; center->slf = 1;
        planeCopy(&center); freeCenter(center); allocCenter(&center,0);}
    }
    if (planraDone) {
        planeSafe(Process,Stop,Configures);
        planeSafe(Graphics,Stop,Configures);
        planeSafe(Window,Stop,Configures);
        planeSafe(Initial,Stop,Configures);
        return;
    }
    if (time - planraLast > 0.01) {planraLast = time;
        struct Center *center = 0; allocCenter(&center,1); center->mem = Configurez;
        allocConfigure(&center->cfg,5); allocInt(&center->val,5);
        center->cfg[0] = RegisterDone; // TODO remove unnecessary
        center->cfg[1] = WindowLeft; center->cfg[2] = WindowBase;
        center->cfg[3] = WindowWidth; center->cfg[4] = WindowHeight;
        center->val[0] = (1<<Display);
        center->val[1] = vulkanInfo(WindowLeft) - 1;
        center->val[2] = vulkanInfo(WindowBase) - 1;
        center->val[3] = vulkanInfo(WindowWidth) + 2;
        center->val[4] = vulkanInfo(WindowHeight) + 2;
        center->idx = 0; center->siz = 5; center->slf = 1;
        planeCopy(&center); freeCenter(center); allocCenter(&center,0);
    }
}
