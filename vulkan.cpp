#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#define GLM_FORCE_RADIANS
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <algorithm>
#include <chrono>
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

extern "C" {
    #include "type.h"
    #include "plane.h"
}

#define NANOSECONDS (10^9)

#ifdef PLANRA
// TODO move to planer.lua
struct Input: public Vertex {
    Input(float *pos) {
        for (int i = 0; i < 2; i++) this->vec[i] = pos[i];
        for (int i = 2; i < 4; i++) this->vec[i] = 0.0;
        for (int i = 0; i < 4; i++) this->ref[i] = 0;
        char *str = 0; showVertex(this,&str);
        std::cout << str << std::endl;
        free(str);
    }
};
std::vector<Input> vertices;
extern "C" {
    // TODO link with plane.c
    vftype callSafe;
    uftype callDma;
    wftype callDraw;
    void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw) {
        callSafe = safe;
        callDma = dma;
        callDraw = draw;
        init();
        main(Window,Start);
        main(Graphics,Start);
        main(Process,Start);
        main(Process,Stop);
        main(Graphics,Stop);
        main(Window,Stop);
    }
    void planeAddarg(const char *str) {}
    int planeInfo(enum Configure cfg) {return 0;}
    void planeSafe(enum Proc proc, enum Wait wait, enum Configure hint) {
        callSafe();
    }
    void planeMain() {
        callDma(0);
        callDraw(Practice,0,vertices.size());
    }
    void planeReady(struct Pierce *pierce) {}
    void vulkanInit();
    void vulkanDma(struct Center *center);
    void vulkanSafe();
    void vulkanMain(enum Proc proc, enum Wait wait);
    int vulkanInfo(enum Configure query);
    void vulkanDraw(enum Micro shader, int base, int limit);
}
#endif

// TODO add type.h enum for these builtin cursors
GLFWcursor *moveCursor(bool e, bool t, bool r, bool b, bool l) {
    int dim = 11;
    int hot = dim/2;
    unsigned char pixels[dim * dim * 4];
    memset(pixels, 0x00, sizeof(pixels));

    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        if (k == 0 || k == dim-1) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == 0 || j == dim-1) pixels[k*dim*4+j*4+i] = 0xff;
        if (k == hot-2 && j >= hot-1 && j <= hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot-2 && k >= hot-1 && k <= hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (k == hot+2 && j >= hot-1 && j <= hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot+2 && k >= hot-1 && k <= hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (e && k >= hot-1 && k <= hot+1 && j >= hot-1 && j <= hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (t && k > hot+2 && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (r && j > hot+2 && k == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (b && k < hot-2 && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (l && j < hot-2 && k == hot) pixels[k*dim*4+j*4+i] = 0xff;
    }

    GLFWimage image;
    image.width = dim;
    image.height = dim;
    image.pixels = pixels;

    return glfwCreateCursor(&image, hot, hot);
}
GLFWcursor *rotateCursor(bool e) {
    int dim = 11;
    int hot = dim/2;
    unsigned char pixels[dim * dim * 4];
    memset(pixels, 0x00, sizeof(pixels));

    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int diffx = j-hot;
        int diffy = k-hot;
        int exact = hot*hot;
        int square = diffx*diffx + diffy*diffy;
        bool center = k >= hot-1 && k <= hot+1 && j >= hot-1 && j <= hot+1;
        if (square < exact+5 && !center) pixels[k*dim*4+j*4+i] = 0xff;
        if (e && center) pixels[k*dim*4+j*4+i] = 0xff;
    }

    GLFWimage image;
    image.width = dim;
    image.height = dim;
    image.pixels = pixels;

    return glfwCreateCursor(&image, hot, hot);
}
GLFWcursor *translateCursor(bool e) {
    int dim = 11;
    int hot = dim/2;
    unsigned char pixels[dim * dim * 4];
    memset(pixels, 0x00, sizeof(pixels));

    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int diffx = (j>hot?j-hot:hot-j);
        int diffy = (k>hot?k-hot:hot-k);
        int sum = diffx + diffy;
        bool center = k >= hot-1 && k <= hot+1 && j >= hot-1 && j <= hot+1;
        if (!center && sum < hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (e && center) pixels[k*dim*4+j*4+i] = 0xff;
    }

    GLFWimage image;
    image.width = dim;
    image.height = dim;
    image.pixels = pixels;

    return glfwCreateCursor(&image, hot, hot);
}
GLFWcursor *refineCursor() {
    int dim = 11;
    int hot = dim/2;
    unsigned char pixels[dim * dim * 4];
    memset(pixels, 0x00, sizeof(pixels));

    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int diffx = j-hot;
        int diffy = k-hot;
        if (diffx == diffy) pixels[k*dim*4+j*4+i] = 0xff;
        if (diffx == -diffy) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (k == hot) pixels[k*dim*4+j*4+i] = 0xff;
    }

    GLFWimage image;
    image.width = dim;
    image.height = dim;
    image.pixels = pixels;

    return glfwCreateCursor(&image, hot, hot);
}
GLFWcursor *sculptCursor(bool e) {
    int dim = 11;
    int hot = dim/2;
    unsigned char pixels[dim * dim * 4];
    memset(pixels, 0x00, sizeof(pixels));

    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        bool center = k >= hot-2 && k <= hot+2 && j >= hot-2 && j <= hot+2;
        if ((e || !center) && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if ((e || !center) && k == hot) pixels[k*dim*4+j*4+i] = 0xff;
    }

    GLFWimage image;
    image.width = dim;
    image.height = dim;
    image.pixels = pixels;

    return glfwCreateCursor(&image, hot, hot);
}

struct InitState;
struct OpenState;
struct PhysicalState;
struct DeviceState;
struct SwapState;
struct ThreadState;
struct QueueState;
struct MainState {
    bool callOnce;
    bool callDma;
    bool callDraw;
    bool framebufferResized;
    bool escapePressed;
    bool enterPressed;
    bool otherPressed;
    bool windowMoving;
    double mouseLastx;
    double mouseLasty;
    int windowLastx;
    int windowLasty;
    int argc;
    char **argv;
    InitState *initState;
    OpenState* openState;
    PhysicalState* physicalState;
    DeviceState* logicalState;
    SwapState *swapState;
    ThreadState *threadState;
    QueueState* queueState;
    const uint32_t WIDTH = 800;
    const uint32_t HEIGHT = 600;
    const int MAX_FRAMES_IN_FLIGHT = 2;
    const int MAX_BUFFERS_AVAILABLE = 7; // TODO collective limit for BufferQueue
    const std::vector<const char*> extensions = {
        VK_KHR_SWAPCHAIN_EXTENSION_NAME
    };
    const std::vector<const char*> computions = {
        // TODO compute extensions
    };
    const std::vector<const char*> layers = {
        "VK_LAYER_KHRONOS_validation"
    };
    #ifdef NDEBUG
    const bool enable = false;
    #else
    const bool enable = true;
    #endif
} mainState = {
    .callOnce = true,
    .callDma = true,
    .callDraw = true,
    .framebufferResized = false,
    .escapePressed = false,
    .enterPressed = false,
    .otherPressed = false,
    .windowMoving = false,
    .mouseLastx = 0.0,
    .mouseLasty = 0.0,
    .windowLastx = 0,
    .windowLasty = 0,
    .argc = 0,
    .argv = 0,
    .initState = 0,
    .openState = 0,
    .physicalState = 0,
    .logicalState = 0,
    .swapState = 0,
    .threadState = 0,
    .queueState = 0,
};

void framebufferResized(GLFWwindow* window, int width, int height) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    mainState->framebufferResized = true;
}
void keyPressed(GLFWwindow* window, int key, int scancode, int action, int mods) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    if (action != GLFW_PRESS || mods != 0) {
        return;
    }
    if (key == GLFW_KEY_ENTER) {
        mainState->enterPressed = true;
    } else {
        mainState->enterPressed = false;
    }
    if (key == GLFW_KEY_ESCAPE) {
        mainState->escapePressed = true;
        mainState->otherPressed = false;
    } else if (mainState->otherPressed) {
        mainState->escapePressed = false;
        mainState->otherPressed = false;
    } else {
        mainState->otherPressed = true;
    }
}
void mouseClicked(GLFWwindow* window, int button, int action, int mods) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    if (action != GLFW_PRESS) {
        return;
    }
    mainState->windowMoving = !mainState->windowMoving;
    if (mainState->windowMoving) {
        glfwGetCursorPos(window,&mainState->mouseLastx,&mainState->mouseLasty);
        glfwGetWindowPos(window,&mainState->windowLastx,&mainState->windowLasty);
    }
}
void mouseMoved(GLFWwindow* window, double xpos, double ypos) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    double mouseNextx, mouseNexty;
    int windowNextx, windowNexty;
    glfwGetCursorPos(window,&mouseNextx,&mouseNexty);
    if (mainState->windowMoving) {
        windowNextx = mainState->windowLastx + (mouseNextx - mainState->mouseLastx);
        windowNexty = mainState->windowLasty + (mouseNexty - mainState->mouseLasty);
        glfwSetWindowPos(window,windowNextx,windowNexty);
        mainState->windowLastx = windowNextx; mainState->windowLasty = windowNexty;
    }
}
VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData) {
    std::cerr << "validation layer: " << pCallbackData->pMessage << std::endl;
    return VK_FALSE;
}

struct InitState {
    VkInstance instance;
    bool enable;
    VkDebugUtilsMessengerEXT debug;
    InitState(bool enable, const std::vector<const char*> layers) {
        this->enable = enable;
        glfwInit();
        VkDebugUtilsMessengerCreateInfoEXT info = {};
        info.sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
        info.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
        info.messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
            VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
        info.pfnUserCallback = debugCallback;
        instance = [](bool enable, VkDebugUtilsMessengerCreateInfoEXT create, const std::vector<const char*> layers) {
            if (enable && ![](const std::vector<const char*> layers) {
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
            info.enabledExtensionCount = static_cast<uint32_t>(extensions.size());
            info.ppEnabledExtensionNames = extensions.data();
            if (enable) {
                info.enabledLayerCount = static_cast<uint32_t>(layers.size());
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
    GLFWcursor* moveCursor[2][2][2][2][2];
    GLFWcursor* rotateCursor[2];
    GLFWcursor* translateCursor[2];
    GLFWcursor* refineCursor;
    GLFWcursor* sculptCursor[2];
    GLFWcursor* standardCursor;
    VkSurfaceKHR surface;
    OpenState(VkInstance instance, uint32_t WIDTH, uint32_t HEIGHT, void *mainState) {
        this->instance = instance;
        window = [](const uint32_t WIDTH, const uint32_t HEIGHT) {
            glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
            return glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
        } (WIDTH,HEIGHT);
        glfwSetWindowUserPointer(window, mainState);
        glfwSetWindowAttrib(window, GLFW_DECORATED, GLFW_FALSE);
        glfwSetFramebufferSizeCallback(window, framebufferResized);
        glfwSetKeyCallback(window, keyPressed);
        glfwSetMouseButtonCallback(window, mouseClicked);
        glfwSetCursorPosCallback(window, mouseMoved);
        for (int t = 0; t < 2; t++) for (int b = 0; b < 2; b++)
        for (int l = 0; l < 2; l++) for (int r = 0; r < 2; r++)
        for (int e = 0; e < 2; e++) moveCursor[e][t][r][b][l] = ::moveCursor(e,t,r,b,l);
        for (int e = 0; e < 2; e++) rotateCursor[e] = ::rotateCursor(e);
        for (int e = 0; e < 2; e++) translateCursor[e] = ::translateCursor(e);
        refineCursor = ::refineCursor();
        for (int e = 0; e < 2; e++) sculptCursor[e] = ::sculptCursor(e);
        standardCursor = glfwCreateStandardCursor(GLFW_ARROW_CURSOR);
        glfwSetCursor(window,moveCursor[true][true][true][true][true]);
        if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
            throw std::runtime_error("failed to create window surface!");
    }
    ~OpenState() {
        vkDestroySurfaceKHR(instance, surface, nullptr);
        for (int t = 0; t < 2; t++) for (int b = 0; b < 2; b++)
        for (int l = 0; l < 2; l++) for (int r = 0; r < 2; r++)
        for (int e = 0; e < 2; e++) glfwDestroyCursor(moveCursor[e][t][r][b][l]);
        for (int e = 0; e < 2; e++) glfwDestroyCursor(rotateCursor[e]);
        for (int e = 0; e < 2; e++) glfwDestroyCursor(translateCursor[e]);
        glfwDestroyCursor(refineCursor);
        for (int e = 0; e < 2; e++) glfwDestroyCursor(sculptCursor[e]);
        glfwDestroyCursor(standardCursor);
        glfwDestroyWindow(window);
    }
};

struct PhysicalState {
    VkPhysicalDevice physical;
    uint32_t graphicid;
    uint32_t presentid;
    uint32_t minimum;
    VkSurfaceFormatKHR format;
    VkPresentModeKHR mode;
    VkFormat image;
    PhysicalState(VkInstance instance, VkSurfaceKHR surface, std::vector<const char*> extensions) {
        std::optional<uint32_t> graphic;
        std::optional<uint32_t> present;
        std::optional<uint32_t> compute;
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
                std::optional<uint32_t> graphicz;
                std::optional<uint32_t> presentz;
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
                        uint32_t extensionCount;
                        vkEnumerateDeviceExtensionProperties(physicalz, nullptr, &extensionCount, nullptr);
                        std::vector<VkExtensionProperties> availableExtensions(extensionCount);
                        vkEnumerateDeviceExtensionProperties(physicalz, nullptr, &extensionCount, availableExtensions.data());
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
        minimum = capabilities.minImageCount + 1;
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
    uint32_t graphicid;
    uint32_t presentid;
    DeviceState(VkPhysicalDevice physical, uint32_t graphicid, uint32_t presentid, VkFormat image,
        std::vector<const char*> layers, std::vector<const char*> extensions, bool enable, int MAX_BUFFERS_AVAILABLE) {
        this->graphicid = graphicid;
        this->presentid = presentid;
        device = [](VkPhysicalDevice physical, uint32_t graphicid, uint32_t presentid,
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
            info.queueCreateInfoCount = static_cast<uint32_t>(infos.size());
            info.pQueueCreateInfos = infos.data();
            info.pEnabledFeatures = &features;
            info.enabledExtensionCount = static_cast<uint32_t>(extensions.size());
            info.ppEnabledExtensionNames = extensions.data();
            if (enable) {
                info.enabledLayerCount = static_cast<uint32_t>(layers.size());
                info.ppEnabledLayerNames = layers.data();}
            else {
                info.enabledLayerCount = 0;}
            if (vkCreateDevice(physical, &info, nullptr, &device) != VK_SUCCESS)
                throw std::runtime_error("failed to create logical device!");
            return device;
        } (physical,graphicid,presentid,layers,extensions,enable);
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
        pool = [](VkDevice device, uint32_t graphicid) {
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
            uniform.descriptorCount = static_cast<uint32_t>(uniforms);
            VkDescriptorPoolSize store{};
            store.type = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
            store.descriptorCount = static_cast<uint32_t>(stores);
            VkDescriptorPoolSize size[] = {uniform,store};
            VkDescriptorPoolCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
            info.flags = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT;
            info.poolSizeCount = 2;
            info.pPoolSizes = size;
            info.maxSets = static_cast<uint32_t>(uniforms+stores);
            VkDescriptorPool pool;
            if (vkCreateDescriptorPool(device, &info, nullptr, &pool) != VK_SUCCESS)
                throw std::runtime_error("failed to create descriptor pool!");
            return pool;
        } (device, MAX_BUFFERS_AVAILABLE, MAX_BUFFERS_AVAILABLE);
    }
    ~DeviceState() {
        vkDestroyDescriptorPool(device, dpool, nullptr);
        vkDestroyCommandPool(device, pool, nullptr);
        vkDestroyRenderPass(device, render, nullptr);
        vkDestroyDevice(device, nullptr);
    }
};

struct SwapState {
    VkDevice device;
    VkExtent2D extent;
    VkSwapchainKHR swap;
    uint32_t count;
    std::vector<VkImage> images;
    std::vector<VkImageView> views;
    std::vector<VkFramebuffer> framebuffers;
    SwapState(GLFWwindow* window, VkPhysicalDevice physical, VkDevice device, VkSurfaceKHR surface,
        VkFormat image, VkSurfaceFormatKHR format, VkPresentModeKHR mode, VkRenderPass pass,
        uint32_t minimum, uint32_t graphicid, uint32_t presentid) {
    // this reconstructed after device idle upon resize window
        this->device = device;
        int width = 0, height = 0;
        glfwGetFramebufferSize(window, &width, &height);
        while (width == 0 || height == 0) {
            glfwWaitEvents();
            glfwGetFramebufferSize(window, &width, &height);
        }
        VkSurfaceCapabilitiesKHR capabilities;
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical, surface, &capabilities);
        extent = [](GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities) {
            int width, height;
            if (capabilities.currentExtent.width != std::numeric_limits<uint32_t>::max()) return capabilities.currentExtent;
            glfwGetFramebufferSize(window, &width, &height);
            VkExtent2D actualExtent = {static_cast<uint32_t>(width),static_cast<uint32_t>(height)};
            actualExtent.width = std::clamp(actualExtent.width, capabilities.minImageExtent.width, capabilities.maxImageExtent.width);
            actualExtent.height = std::clamp(actualExtent.height, capabilities.minImageExtent.height, capabilities.maxImageExtent.height);
            return actualExtent;}(window,capabilities);
        swap = [](VkDevice device, VkSurfaceKHR surface, VkSurfaceFormatKHR format, VkPresentModeKHR mode, VkExtent2D extent,
            VkSurfaceCapabilitiesKHR capabilities, uint32_t minimum, uint32_t graphicid, uint32_t presentid) {
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
            return swap;}(device,surface,format,mode, extent,capabilities,minimum,graphicid,presentid);
        vkGetSwapchainImagesKHR(device, swap, &count, nullptr);
        images.resize(count);
        views.resize(count);
        framebuffers.resize(count);
        vkGetSwapchainImagesKHR(device, swap, &count, images.data());
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
        vkDestroySwapchainKHR(device, swap, nullptr);
    }
};

struct ThreadState {
    VkDevice device;
    sem_t protect;
    sem_t semaphore;
    pthread_t thread;
    bool finish;
    std::deque<std::function<VkFence()>> setup;
    std::deque<std::function<void()>> stage;
    std::deque<VkFence> fence;
    std::deque<int> order;
    std::set<int> lookup;
    int seqnum;
    ThreadState(VkDevice device) {
        this->device = device;
        finish = false;
        seqnum = 0;
        if (sem_init(&protect, 0, 1) != 0 ||
            sem_init(&semaphore, 0, 0) != 0 ||
            pthread_create(&thread,0,separate,this) != 0) throw std::runtime_error("failed to create thread!");
    }
    ~ThreadState() {
    // this destructed to wait for device idle
        if (sem_wait(&protect) != 0) {
            std::cerr << "cannot wait for protect!" << std::endl;
            std::terminate();}
        finish = true;
        if (sem_post(&protect) != 0) {
            std::cerr << "cannot post to protect!" << std::endl;
            std::terminate();}
        if (sem_post(&semaphore) != 0 ||
            pthread_join(thread,0) != 0 ||
            sem_destroy(&semaphore) != 0 ||
            sem_destroy(&protect) != 0) {
            std::cerr << "failed to join thread!" << std::endl;
            std::terminate();}
    }
    static void *separate(void *ptr) {
        struct ThreadState *arg = (ThreadState*)ptr;
        if (sem_wait(&arg->protect) != 0) throw std::runtime_error("cannot wait for protect!");
        while (1) {
            if (arg->finish) {
                if (sem_post(&arg->protect) != 0) throw std::runtime_error("cannot post to protect!");
                break;}
            while (!arg->setup.empty()) {
                arg->fence.push_back(arg->setup.front()()); arg->setup.pop_front();}
            if (arg->fence.empty()) {
                if (sem_post(&arg->protect) != 0) throw std::runtime_error("cannot post to protect!");
                if (sem_wait(&arg->semaphore) != 0) throw std::runtime_error("cannot wait for semaphore!");
                if (sem_wait(&arg->protect) != 0) throw std::runtime_error("cannot wait for protect!");}
            else {
                if (sem_post(&arg->protect) != 0) throw std::runtime_error("cannot post to protect!");
                VkResult result = vkWaitForFences(arg->device,1,&arg->fence.front(),VK_FALSE,NANOSECONDS);
                if (sem_wait(&arg->protect) != 0) throw std::runtime_error("cannot wait for protect!");
                if (result != VK_SUCCESS && result != VK_TIMEOUT) throw std::runtime_error("cannot wait for fence!");
                if (result == VK_SUCCESS) {arg->stage.front()(); arg->lookup.erase(arg->order.front());
                arg->order.pop_front(); arg->fence.pop_front(); arg->stage.pop_front();}
                /*planeSafe(...);*/}}
        vkDeviceWaitIdle(arg->device);
        return 0;
    }
    bool clear(int given) {
        if (sem_wait(&protect) != 0) throw std::runtime_error("cannot wait for protect!");
        bool done = (lookup.find(given) == lookup.end());
        if (sem_post(&protect) != 0) throw std::runtime_error("cannot post to protect!");
        return done;
    }
    std::function<bool()> push(std::function<VkFence()> given, std::function<void()> argen) {
    // return function that returns whether fence returned by given function in separate thread is done
        if (sem_wait(&protect) != 0) throw std::runtime_error("cannot wait for protect!");
        if (fence.empty() && sem_post(&semaphore) != 0) throw std::runtime_error("cannot post to semaphore!");
        setup.push_back(given); stage.push_back(argen);
        int local = seqnum++; order.push_back(local); lookup.insert(local);
        if (fence.size()+setup.size() != order.size()) throw std::runtime_error("cannot push seqnum!");
        if (order.size() != lookup.size()) throw std::runtime_error("cannot insert seqnum!");
        std::function<bool()> done = [this,local](){return this->clear(local);};
        if (sem_post(&protect) != 0) throw std::runtime_error("cannot post to protect!");
        return done;
    }
};

enum BufferTag {TestBuf,FetchBuf,ChangeBuf,StoreBuf,DrawBuf};
template<class Buffer> struct BufferQueue {
    std::deque<Buffer*> pool;
    std::deque<Buffer*> running; std::deque<std::function<bool()>> toready;
    Buffer* ready; std::deque<std::function<bool()>> toinuse;
    std::deque<Buffer*> inuse; std::deque<std::function<bool()>> topool;
    std::map<int,std::function<bool()>> temp;
    std::deque<void*> data; std::deque<std::function<bool()>> done;
    int size; void *copy; int seqnum; int count; int limit; BufferTag tag;
    MainState *info;
    BufferQueue(MainState *info, int limit, BufferTag tag) {
        ready = 0;
        size = 0;
        copy = 0;
        seqnum = 0;
        count = 0;
        this->limit = limit;
        this->tag = tag;
        this->info = info;
    }
    ~BufferQueue() {
        while (!pool.empty()) {delete pool.front(); pool.pop_front();}
        while (!running.empty()) {delete running.front(); running.pop_front();}
        if (ready) delete ready;
        while (!inuse.empty()) {delete inuse.front(); inuse.pop_front();}
    }
    void clr() {
    // advance queues with done fronts
        if (ready && !toinuse.empty() && !toready.empty() && toready.front()()) {
            while (toinuse.size() > 1) {
                inuse.push_back(0); topool.push_back(toinuse.front()); toinuse.pop_front();}
            inuse.push_back(ready); topool.push_back(toinuse.front()); toinuse.pop_front(); ready = 0;}
        while (!toready.empty() && toready.front()()) {
            if (ready) pool.push_back(ready);
            ready = running.front(); running.pop_front(); toready.pop_front();}
        while (!topool.empty() && topool.front()()) {
            if (inuse.front()) pool.push_back(inuse.front());
            inuse.pop_front(); topool.pop_front();}
        while (!data.empty() && done.front()()) {
            free(data.front()); data.pop_front(); done.pop_front();}
    }
    int tmp() {
    // return identifier for done function
        temp[seqnum] = [](){return false;};
        return seqnum++;
    }
    std::function<bool()> tmp(int tmp) {
    // return done function from identifier
        return [this,tmp](){
            if (this->temp.find(tmp) == this->temp.end()) return true;
            if (!this->temp[tmp]()) return false;
            this->temp.erase(tmp); return true;};
    }
    void tmp(int tmp, std::function<bool()> done) {
    // bind done function to identified done function
        temp[tmp] = done;
    }
    bool set() {
    // return whether queues are not full
        clr();
        if (!pool.empty()) return true;
        if (count < limit) return true;
        return false;
    }
    bool set(int siz) {
    // change queue item size; return whether pool is new
        if (siz != size) {
            while (!pool.empty()) {delete pool.front(); pool.pop_front();}
            copy = realloc(copy,size = siz);}
        if (!set()) return false;
        bool first = false; if (pool.empty()) {
            Buffer *ptr = new Buffer(info,size,tag); pool.push_back(ptr); first = true; count++;}
        return first;
    }
    std::function<bool()> set(bool first, std::function<VkFence(Buffer*)> setup) {
    // enque function to return fence in separate thread
        if (pool.empty()) return [](){return true;};
        Buffer *ptr = pool.front(); pool.pop_front();
        std::function<bool()> done;
        if (first) done = info->threadState->push([setup,ptr](){ptr->init(); return setup(ptr);},[](){});
        else done = info->threadState->push([setup,ptr](){return setup(ptr);},[](){});
        running.push_back(ptr); toready.push_back(done);
        return done;
    }
    std::function<bool()> set(int size, std::function<VkFence(Buffer*)> setup) {
    // change size and enque function to return fence in separate thread
        return set(set(size),setup);
    }
    std::function<bool()> set(std::deque<void*> &queue,std::deque<std::function<bool()>> &inuse,
        int loc, int siz, const void *ptr) {
    // enque data using given queues
        int size = (loc+siz > this->size ? loc+siz : this->size);
        bool first = set(size);
        memcpy((void*)((char*)copy+loc),ptr,siz);
        if (first) {loc = 0; siz = size; ptr = copy;}
        void *mem = malloc(siz); memcpy(mem,ptr,siz);
        int temp = tmp(); queue.push_back(mem); inuse.push_back(tmp(temp));
        std::function<bool()> done = set(first,[loc,siz,mem](Buffer*buf){return buf->setup(loc,siz,mem);});
        tmp(temp,done);
        return done;
    }
    std::function<bool()> set(int loc, int siz, const void *ptr) {
    // enque data
        return set(data,done,loc,siz,ptr);
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
    void dbg() {
        clr();
        if (!toinuse.empty() && toinuse.front()) std::cout << "debug toinuse:" << toinuse.front()() << std::endl;
        else if (!toinuse.empty()) std::cout << "debug ptr:" << &toinuse.front() << std::endl;
        else if (!topool.empty()) std::cout << "debug topool:" << topool.front()() << std::endl;
        else std::cout << "debug count:" << count << std::endl;
    }
};

struct BufferState;
struct DrawState;
struct FieldState {
    int stride;
    std::vector<VkFormat> format;
    std::vector<uint32_t> offset;
};
struct QueueState {
    BufferQueue<BufferState>* bufferQueue[Memorys];
    BufferQueue<DrawState>* drawQueue[Micros];
    std::vector<BufferQueue<BufferState>*> bindBuffer[Micros];
    std::vector<BufferTag> typeBuffer[Micros];
    std::vector<FieldState*> fieldBuffer[Micros];
    QueueState() {
    for (int i = 0; i < Memorys; i++) bufferQueue[i] = 0;
    for (int i = 0; i < Micros; i++) drawQueue[i] = 0;
    bufferQueue[Vertexz] = new BufferQueue<BufferState>(&mainState,mainState.MAX_BUFFERS_AVAILABLE,FetchBuf);
    bufferQueue[Matrixz] = new BufferQueue<BufferState>(&mainState,mainState.MAX_BUFFERS_AVAILABLE,ChangeBuf);
    bindBuffer[Practice].push_back(bufferQueue[Vertexz]);
    bindBuffer[Practice].push_back(bufferQueue[Matrixz]);
    typeBuffer[Practice].push_back(FetchBuf);
    typeBuffer[Practice].push_back(ChangeBuf);
    FieldState *field = new FieldState();
    field->stride = sizeof(Vertex);
    field->format.push_back(VK_FORMAT_R32G32_SFLOAT);
    field->format.push_back(VK_FORMAT_R32_UINT);
    field->offset.push_back(offsetof(Vertex,vec));
    field->offset.push_back(offsetof(Vertex,ref));
    fieldBuffer[Practice].push_back(field);
    fieldBuffer[Practice].push_back(0);
    drawQueue[Practice] = new BufferQueue<DrawState>(&mainState,mainState.MAX_FRAMES_IN_FLIGHT,DrawBuf);}
    ~QueueState() {
    for (int i = 0; i < Micros; i++) if (drawQueue[i]) delete drawQueue[i];
    for (int i = 0; i < Micros; i++)
    for (auto j = fieldBuffer[i].begin(); j != fieldBuffer[i].end(); j++) if (*j) delete *j;
    for (int i = 0; i < Memorys; i++) if (bufferQueue[i]) delete bufferQueue[i];}
};

struct PipelineState {
    VkDevice device;
    VkPipelineLayout layout;
    VkPipeline pipeline;
    VkDescriptorSetLayout dlayout;
    VkDescriptorSet descriptor;
    PipelineState(VkDevice device, VkRenderPass render, VkDescriptorPool dpool,
    Micro micro, QueueState *queue, const char *vertex, const char *fragment) {
    this->device = device;
    dlayout = [](VkDevice device, std::vector<BufferTag> &type) {
        std::vector<VkDescriptorSetLayoutBinding> bindings;
        int count = 0; for (auto i = type.begin(); i != type.end(); i++) if ((*i) == ChangeBuf || (*i) == StoreBuf) {
        VkDescriptorSetLayoutBinding binding{};
        binding.binding = count++;
        binding.descriptorCount = 1;
        binding.descriptorType = ((*i) == ChangeBuf ? VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER : VK_DESCRIPTOR_TYPE_STORAGE_BUFFER);
        binding.pImmutableSamplers = nullptr;
        binding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
        bindings.push_back(binding);}
        VkDescriptorSetLayoutCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
        info.bindingCount = bindings.size();
        info.pBindings = bindings.data();
        VkDescriptorSetLayout descriptor;
        if (vkCreateDescriptorSetLayout(device, &info, nullptr, &descriptor) != VK_SUCCESS)
            throw std::runtime_error("failed to create descriptor set layout!");
        return descriptor;
    } (device,queue->typeBuffer[micro]);
    descriptor = [](VkDevice device, VkDescriptorSetLayout layout, VkDescriptorPool pool) {
        VkDescriptorSetAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
        info.descriptorPool = pool;
        info.descriptorSetCount = static_cast<uint32_t>(1);
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
    VkPipelineShaderStageCreateInfo vinfo = [](VkShaderModule vmodule) {
        VkPipelineShaderStageCreateInfo vinfo{};
        vinfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
        vinfo.stage = VK_SHADER_STAGE_VERTEX_BIT;
        vinfo.module = vmodule;
        vinfo.pName = "main";
        return vinfo;}(vmodule);
    VkPipelineShaderStageCreateInfo finfo = [](VkShaderModule fmodule) {
        VkPipelineShaderStageCreateInfo finfo{};
        finfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
        finfo.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
        finfo.module = fmodule;
        finfo.pName = "main";
        return finfo;}(fmodule);
    VkPipelineShaderStageCreateInfo stages[] = {vinfo, finfo};
    std::vector<VkVertexInputBindingDescription> descriptions;
    std::vector<VkVertexInputAttributeDescription> attributes;
    [&descriptions,&attributes](std::vector<FieldState*> &field) {
        int count = 0; int location = 0;
        for (auto i = field.begin(); i != field.end(); i++) if (*i) {
        int binding = count++;
        VkVertexInputBindingDescription description;
        description.binding = binding;
        description.stride = (*i)->stride;
        description.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
        for (int j = 0; j < (*i)->offset.size(); j++) {
        VkVertexInputAttributeDescription attribute;
        attribute.binding = binding;
        attribute.location = location++;
        attribute.format = (*i)->format[j];
        attribute.offset = (*i)->offset[j];
        attributes.push_back(attribute);}
        descriptions.push_back(description);}}(queue->fieldBuffer[micro]);
    VkPipelineVertexInputStateCreateInfo input = [&descriptions,&attributes] () {
        VkPipelineVertexInputStateCreateInfo input{};
        input.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
        input.vertexBindingDescriptionCount = static_cast<uint32_t>(descriptions.size());
        input.vertexAttributeDescriptionCount = static_cast<uint32_t>(attributes.size());
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
    VkPipelineRasterizationStateCreateInfo rasterize = [] () {
        VkPipelineRasterizationStateCreateInfo rasterize{};
        rasterize.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
        rasterize.depthClampEnable = VK_FALSE;
        rasterize.rasterizerDiscardEnable = VK_FALSE;
        rasterize.polygonMode = VK_POLYGON_MODE_FILL;
        rasterize.lineWidth = 1.0f;
        rasterize.cullMode = VK_CULL_MODE_BACK_BIT;
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
        state.dynamicStateCount = static_cast<uint32_t>(states.size());
        state.pDynamicStates = states.data();
        return state;}();
    VkGraphicsPipelineCreateInfo info{};
    info.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    info.stageCount = 2;
    info.pStages = stages;
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
    vkDestroyShaderModule(device, fmodule, nullptr);
    vkDestroyShaderModule(device, vmodule, nullptr);}
    ~PipelineState() {
    vkDestroyPipeline(device, pipeline, nullptr);
    vkDestroyPipelineLayout(device, layout, nullptr);
    vkDestroyDescriptorSetLayout(device, dlayout, nullptr);}
};

struct BufferState {
    VkPhysicalDevice physical;
    VkDevice device;
    VkQueue graphic;
    VkCommandPool pool;
    VkDeviceSize size;
    BufferTag tag;
    VkBuffer staging;
    VkDeviceMemory wasted;
    VkBuffer buffer;
    VkDeviceMemory memory;
    void *mapped;
    VkCommandBuffer command;
    VkFence fence;
BufferState(MainState *state, int size, BufferTag tag) {
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
    if (tag == FetchBuf || tag == StoreBuf) {
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
    } (device,size,VK_BUFFER_USAGE_TRANSFER_SRC_BIT);
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
    } (device,requirements,[](uint32_t type, VkPhysicalDeviceMemoryProperties properties, VkMemoryPropertyFlags flags) {
        for (uint32_t i = 0; i < properties.memoryTypeCount; i++) if ((type & (1 << i)) &&
            (properties.memoryTypes[i].propertyFlags & flags) == flags) return i;
        throw std::runtime_error("failed to find suitable wasted type!");
        return properties.memoryTypeCount;
    } (requirements.memoryTypeBits,properties,VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT));
    vkBindBufferMemory(device, staging, wasted, 0);}
    if (tag == FetchBuf || tag == ChangeBuf || tag == StoreBuf) {
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
    VK_BUFFER_USAGE_TRANSFER_DST_BIT|VK_BUFFER_USAGE_STORAGE_BUFFER_BIT:
        VK_BUFFER_USAGE_TRANSFER_DST_BIT|VK_BUFFER_USAGE_VERTEX_BUFFER_BIT));
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
    } (device,requirements,[](uint32_t type, VkPhysicalDeviceMemoryProperties properties, VkMemoryPropertyFlags flags) {
        for (uint32_t i = 0; i < properties.memoryTypeCount; i++) if ((type & (1 << i)) &&
            (properties.memoryTypes[i].propertyFlags & flags) == flags) return i;
        throw std::runtime_error("failed to find suitable memory type!");
        return properties.memoryTypeCount;
    } (requirements.memoryTypeBits,properties,tag==ChangeBuf?
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT:
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT));
    vkBindBufferMemory(device, buffer, memory, 0);
    vkMapMemory(device, tag==ChangeBuf?memory:wasted, 0, size, 0, &mapped);}
    command = [](VkDevice device, VkCommandPool command) {
        VkCommandBuffer commandBuffer;
        VkCommandBufferAllocateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
        info.commandPool = command;
        info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
        info.commandBufferCount = (uint32_t)1;
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
    if (tag == FetchBuf || tag == ChangeBuf || tag == StoreBuf) {
    vkDestroyBuffer(device, buffer, nullptr);
    vkFreeMemory(device, memory, nullptr);}
    if (tag == FetchBuf || tag == StoreBuf) {
    vkDestroyBuffer(device, staging, nullptr);
    vkFreeMemory(device, wasted, nullptr);}}
VkFence setup(int loc, int siz, const void *ptr) {
    // this called in separate thread to get fence
    VkResult result;
    if (tag == FetchBuf || tag == ChangeBuf || tag == StoreBuf) {
        memcpy((char*)mapped+loc,ptr,siz);}
    vkResetCommandBuffer(command, /*VkCommandBufferResetFlagBits*/ 0);
    vkResetFences(device, 1, &fence);
    VkCommandBufferBeginInfo begin{};
    begin.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    begin.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    vkBeginCommandBuffer(command, &begin);
    if (tag == FetchBuf || tag == StoreBuf) {
    VkBufferCopy copy{};
    copy.size = size;
    vkCmdCopyBuffer(command, staging, buffer, 1, &copy);}
    vkEndCommandBuffer(command);
    VkSubmitInfo submit{};
    submit.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.commandBufferCount = 1;
    submit.pCommandBuffers = &command;
    result = vkQueueSubmit(graphic, 1, &submit, fence);
    return fence;}
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
    if (tag == StoreBuf) {
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
    return (tag == ChangeBuf || tag == StoreBuf);}
};

struct DrawState {
    VkDevice device;
    VkQueue graphic;
    VkQueue present;
    VkRenderPass render;
    VkCommandPool pool;
    VkSemaphore available;
    VkSemaphore finished;
    VkCommandBuffer command;
    VkFence fence;
    MainState *state;
    PipelineState* pipeline;
    // interpret size as type of pipeline
DrawState(MainState *state, int size, BufferTag tag) {
    DeviceState* logical = state->logicalState;
    this->device = logical->device;
    this->graphic = logical->graphic;
    this->present = logical->present;
    this->render = logical->render;
    this->pool = logical->pool;
    this->state = state;
    this->pipeline = new PipelineState(device,render,logical->dpool,(Micro)size,state->queueState,"vertexPracticeG","fragmentPracticeG");}
void init() {
    // this called in separate thread on newly constructed
    available = [](VkDevice device) {
        VkSemaphore semaphore;
        VkSemaphoreCreateInfo info{};
        info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
        if (vkCreateSemaphore(device, &info, nullptr, &semaphore) != VK_SUCCESS)
            throw std::runtime_error("failed to create semaphore!");
        return semaphore;}(device);
    finished = [](VkDevice device) {
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
        info.commandBufferCount = (uint32_t)1;
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
    vkDestroySemaphore(device, finished, nullptr);
    vkDestroySemaphore(device, available, nullptr);
    delete pipeline;}
VkFence setup(const std::vector<BufferState*> &buffer, uint32_t base, uint32_t limit, bool *framebufferResized) {
    uint32_t index;
    VkResult result = vkAcquireNextImageKHR(device, state->swapState->swap, UINT64_MAX, available, VK_NULL_HANDLE, &index);
    if (result == VK_ERROR_OUT_OF_DATE_KHR) *framebufferResized = true;
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
    } (render,state->swapState->framebuffers[index],state->swapState->extent,command);
    vkCmdBindPipeline(command, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->pipeline);
    [](VkExtent2D extent, VkCommandBuffer command){
        VkViewport info{};
        info.x = 0.0f; info.y = 0.0f;
        info.width = (float) extent.width;
        info.height = (float) extent.height;
        info.minDepth = 0.0f; info.maxDepth = 1.0f;
        vkCmdSetViewport(command, 0, 1, &info);}(state->swapState->extent,command);
    [](VkExtent2D extent, VkCommandBuffer command){
        VkRect2D scissor{};
        scissor.offset = {0, 0};
        scissor.extent = extent;
        vkCmdSetScissor(command, 0, 1, &scissor);}(state->swapState->extent,command);
    int count = 0; for (int i = 0; i < buffer.size(); i++)
        if (buffer[i]->bind(count,command,pipeline->descriptor)) count++;
    vkCmdBindDescriptorSets(command, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->layout, 0, 1,
        &pipeline->descriptor, 0, nullptr);
    vkCmdDraw(command, limit-base, (limit-base)/3, base, base/3);
    vkCmdEndRenderPass(command);
    if (vkEndCommandBuffer(command) != VK_SUCCESS)
        throw std::runtime_error("failed to record command buffer!");
    [](VkQueue graphic, VkCommandBuffer command, VkFence fence, VkSemaphore available, VkSemaphore finished){
        VkSubmitInfo info{};
        info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore availables[] = {available};
        VkPipelineStageFlags stages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
        info.waitSemaphoreCount = 1;
        info.pWaitSemaphores = availables;
        info.pWaitDstStageMask = stages;
        info.commandBufferCount = 1;
        info.pCommandBuffers = &command;
        VkSemaphore finisheds[] = {finished};
        info.signalSemaphoreCount = 1;
        info.pSignalSemaphores = finisheds;
        if (vkQueueSubmit(graphic, 1, &info, fence) != VK_SUCCESS)
            throw std::runtime_error("failed to submit draw command buffer!");
    }(graphic,command,fence,available,finished);
    [](VkSwapchainKHR swap, VkQueue present, uint32_t index, VkSemaphore finished, bool *resized){
        VkPresentInfoKHR info{};
        info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
        info.waitSemaphoreCount = 1;
        VkSemaphore finisheds[] = {finished};
        info.pWaitSemaphores = finisheds;
        VkSwapchainKHR swaps[] = {swap};
        info.swapchainCount = 1;
        info.pSwapchains = swaps;
        info.pImageIndices = &index;
        VkResult result = vkQueuePresentKHR(present, &info);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) *resized = true;
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            throw std::runtime_error("device lost on wait for fence!");
    }(state->swapState->swap,present,index,finished,framebufferResized);
    return fence;}
};

int vulkanInfo(enum Configure query) {
    return 0; // TODO
}
void vulkanSafe() {
    glfwPostEmptyEvent();
}
void vulkanInit() {
    for (int arg = 0; arg < mainState.argc; arg++) planeAddarg(mainState.argv[arg]);
    mainState.initState = new InitState(mainState.enable,mainState.layers);
}
void vulkanMain(enum Proc proc, enum Wait wait) {
    switch(wait) {
    case (Start):
    switch (proc) {
    case (Window):
    mainState.openState = new OpenState(
    mainState.initState->instance,mainState.WIDTH,mainState.HEIGHT,(void*)&mainState);
    mainState.physicalState = new PhysicalState(
    mainState.initState->instance,mainState.openState->surface,mainState.extensions);
    break;
    case (Graphics):
    mainState.logicalState = [](PhysicalState *physical){
    return new DeviceState(physical->physical,physical->graphicid,physical->presentid,
    physical->image,mainState.layers,mainState.extensions,mainState.enable,
    mainState.MAX_BUFFERS_AVAILABLE*Memorys);
    }(mainState.physicalState);
    mainState.queueState = new QueueState();
    break;
    case (Process):
    while (!mainState.escapePressed || !mainState.enterPressed) {
    glfwWaitEventsTimeout(0.01); // TODO increase to 1 second and call planeSafe from test thread
    if (mainState.framebufferResized) {
        mainState.framebufferResized = false;
        if (mainState.threadState) delete mainState.threadState;
        if (mainState.swapState) delete mainState.swapState;
        mainState.swapState = 0; mainState.threadState = 0;}
    if (!mainState.swapState && mainState.openState && mainState.physicalState && mainState.logicalState) {
        mainState.swapState = [](OpenState *open, PhysicalState *physical, DeviceState *device){
        return new SwapState(open->window,physical->physical,device->device,
        open->surface,physical->image,physical->format,physical->mode,
        device->render,physical->minimum,physical->graphicid,physical->presentid);
        }(mainState.openState,mainState.physicalState,mainState.logicalState);}
    if (!mainState.threadState && mainState.logicalState) {
        mainState.threadState = new ThreadState(mainState.logicalState->device);}
    if (mainState.threadState) {
        planeMain();}}
    break;
    default:
    break;}
    break;
    case (Stop):
    switch (proc) {
    case (Process):
    delete mainState.threadState; mainState.threadState = 0;
    delete mainState.swapState; mainState.swapState = 0;
    break;
    case (Graphics):
    delete mainState.queueState; mainState.queueState = 0;
    delete mainState.logicalState; mainState.logicalState = 0;
    if (!mainState.openState) {delete mainState.physicalState; mainState.physicalState = 0;}
    break;
    case (Window):
    delete mainState.physicalState; mainState.physicalState = 0;
    delete mainState.openState; mainState.openState = 0;
    break;
    default:
    break;}
    break;
    default:
    throw std::runtime_error("no case in switch!");}  
}
auto startTime = std::chrono::high_resolution_clock::now();
void vulkanDma(struct Center *center) {
    // TODO switch on center tag to choose buffer queue
        auto currentTime = std::chrono::high_resolution_clock::now();
        float time = std::chrono::duration<float, std::chrono::seconds::period>(currentTime - startTime).count();
        VkExtent2D extent = mainState.swapState->extent;
    if (mainState.callOnce) {
        mainState.queueState->bufferQueue[Vertexz]->set(0,sizeof(vertices[0])*vertices.size(),vertices.data());
    glm::mat4 tmp[3]; // model view proj
    tmp[0] = glm::rotate(glm::mat4(1.0f), time * glm::radians(90.0f), glm::vec3(0.0f, 0.0f, 1.0f));
    tmp[1] = glm::lookAt(glm::vec3(2.0f, 2.0f, 2.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));
    tmp[2] = glm::perspective(glm::radians(45.0f), extent.width / (float) extent.height, 0.1f, 10.0f); tmp[2][1][1] *= -1;
        mainState.queueState->bufferQueue[Matrixz]->set(0,3*sizeof(tmp[0]),tmp);
        mainState.callOnce = false;
    }
    if (mainState.callDma) {
    glm::mat4 tmp; // model
    tmp = glm::rotate(glm::mat4(1.0f), time * glm::radians(90.0f), glm::vec3(0.0f, 0.0f, 1.0f));
        mainState.queueState->bufferQueue[Matrixz]->set(0,sizeof(tmp),&tmp);
        mainState.callDma = false;
    }
}
void vulkanDraw(enum Micro shader, int base, int limit) {
    if (mainState.callDraw) {
        std::vector<BufferState*> buffer;
        std::vector<BufferQueue<BufferState>*> *bindBuffer = mainState.queueState->bindBuffer+shader;
        BufferQueue<DrawState> *draw = mainState.queueState->drawQueue[shader];
        for (auto i = bindBuffer->begin(); i != bindBuffer->end(); i++) if (!(*i)->get()) return;
        if (!draw->set()) return;
        int temp = draw->tmp();
        for (auto i = bindBuffer->begin(); i != bindBuffer->end(); i++) buffer.push_back((*i)->get(draw->tmp(temp)));
        std::function<bool()> done = draw->set(shader,[buffer,base,limit](DrawState*draw){
        return draw->setup(buffer,base,limit,&mainState.framebufferResized);});
        draw->tmp(temp,done);
        mainState.callDma = true;
    }
}

int main(int argc, char **argv) {
#ifdef PLANRA
    {float pos[] = {-0.5f, -0.5f}; vertices.push_back(Input(pos));}
    {float pos[] = {0.5f, -0.5f}; vertices.push_back(Input(pos));}
    {float pos[] = {0.5f, 0.5f}; vertices.push_back(Input(pos));}
    {float pos[] = {-0.5f, 0.5f}; vertices.push_back(Input(pos));}
#endif
    mainState.argc = argc;
    mainState.argv = argv;
    try {
        planeInit(vulkanInit,vulkanDma,vulkanSafe,vulkanMain,vulkanInfo,vulkanDraw);
        delete mainState.initState;
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
