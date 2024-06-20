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
    #include "type.h"
    #include "plane.h"
}

struct MouseState {
    double left, base, angle;
};
struct WindowState {
    int left, base, width, height;
};
struct InitState;
struct OpenState;
struct PhysicalState;
struct DeviceState;
struct SwapState;
struct GlfwState;
struct ThreadState;
struct QueueState;
struct MainState {
    bool escapeEnter;
    std::deque<int> keyPressed;
    bool manipReact[Reacts];
    bool manipAction[Actions];
    struct MouseState mouseClick, mouseMove, mouseCopy;
    struct WindowState windowClick, windowMove, windowCopy;
    int paramFollow;
    int paramModify;
    int paramIndex;
    enum Micro paramDisplay;
    enum Micro paramBrighten;
    enum Micro paramDetect;
    int paramBase;
    int paramLimit;
    int changedIndex;
    int changedSize;
    struct Little *changedState;
    int argc;
    char **argv;
    InitState *initState;
    OpenState* openState;
    GlfwState *glfwState;
    PhysicalState* physicalState;
    DeviceState* logicalState;
    SwapState *swapState;
    ThreadState *threadState;
    QueueState* queueState;
    int registerDone;
    const int MAX_FRAMES_IN_FLIGHT = 2;
    const int MAX_BUFFERS_AVAILABLE = 3;
    const int MAX_FENCES_IN_FLIGHT = 3;
    const std::vector<const char*> extensions = {VK_KHR_SWAPCHAIN_EXTENSION_NAME};
    const std::vector<const char*> layers = {"VK_LAYER_KHRONOS_validation"};
    #ifdef NDEBUG
    const bool enable = false;
    #else
    const bool enable = true;
    #endif
} mainState = {
    .escapeEnter = false,
    .manipReact = {0},
    .manipAction = {0},
    .mouseClick = {0.0,0.0,0.0},
    .mouseMove = {0.0,0.0,0.0},
    .mouseCopy = {0.0,0.0,0.0},
    .windowClick = {0,0,800,800},
    .windowMove = {0,0,800,800},
    .windowCopy = {0,0,800,800},
    .paramFollow = 0,
    .paramModify = 0,
    .paramIndex = 0,
    .paramDisplay = MicroPRPC, // just display
    .paramBrighten = MicroPRRC, // fill Indexz
    .paramDetect = MicroPRCP, // retreive Piercez
    .paramBase = 0,
    .paramLimit = 0,
    .changedIndex = 0,
    .changedSize = 0,
    .changedState = 0,
    .argc = 0,
    .argv = 0,
    .initState = 0,
    .openState = 0,
    .glfwState = 0,
    .physicalState = 0,
    .logicalState = 0,
    .swapState = 0,
    .threadState = 0,
    .queueState = 0,
    .registerDone = 0,
};

GLFWcursor *moveCursor(bool e, bool t, bool r, bool b, bool l) {
    int dim = 13;
    int hot = dim/2;
    int box = 1;
    unsigned char pixels[dim * dim * 4];
    memset(pixels, 0x00, sizeof(pixels));

    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        // top and bottom
        if (k == 0 || k == dim-1) pixels[k*dim*4+j*4+i] = 0xff;
        // left and right
        if (j == 0 || j == dim-1) pixels[k*dim*4+j*4+i] = 0xff;
        // close box
        if (k == hot-(box+1) && j >= hot-box && j <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot-(box+1) && k >= hot-box && k <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        if (k == hot+(box+1) && j >= hot-box && j <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot+(box+1) && k >= hot-box && k <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        // open box
        if (e && k >= hot-box && k <= hot+box && j >= hot-box && j <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        // cross marks
        if (t && k < hot-box && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (r && j > hot+box && k == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (b && k > hot+box && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (l && j < hot-box && k == hot) pixels[k*dim*4+j*4+i] = 0xff;
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

float debug_diff = 0.0;
struct timeval debug_start;
void debugStart() {gettimeofday(&debug_start, NULL);}
void debugStop(const char *str) {
    struct timeval stop; gettimeofday(&stop, NULL);
    float diff = (stop.tv_sec - debug_start.tv_sec) + (stop.tv_usec - debug_start.tv_usec) / (double)MICROSECONDS;
    if (diff > debug_diff) {debug_diff = diff; std::cerr << str << " " << diff << std::endl;}
}

void windowMoved(GLFWwindow* window, int xpos, int ypos)
{
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    mainState->windowMove.left = xpos; mainState->windowMove.base = ypos;
}
void windowSized(GLFWwindow* window, int width, int height)
{
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    mainState->windowMove.width = width; mainState->windowMove.height = height;
}
void windowRefreshed(GLFWwindow* window)
{
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
}
void manipReact(struct MainState *mainState, int pat) {
    for (int i = mainState->changedIndex; i < mainState->changedSize; i++)
    if (mainState->changedState[i].pat == pat || mainState->changedState[i].pat == 0) {
    for (int j = 0; j < Reacts; j++) mainState->manipReact[(React)j] = ((mainState->changedState[i].val&(1<<j)) != 0);
    for (int j = 0; j < Actions; j++) mainState->manipAction[(Action)j] = ((mainState->changedState[i].num&(1<<j)) != 0);
    mainState->changedIndex = mainState->changedState[i].nxt; break;}
}
void keyPressed(GLFWwindow* window, int key, int scancode, int action, int mods) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    if (action != GLFW_PRESS || mods != 0) return;
    if (mainState->manipReact[Pressed]) {mainState->keyPressed.push_back(key); planeSafe(Threads,Waits,CursorPress);}
    if (mainState->manipReact[Changed]) manipReact(mainState,key);
}
void mouseClicked(GLFWwindow* window, int button, int action, int mods) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    if (action != GLFW_PRESS) return;
    mainState->windowCopy = mainState->windowClick = mainState->windowMove;
    mainState->mouseCopy = mainState->mouseClick = mainState->mouseMove;
    if (mainState->manipReact[Clicked]) planeSafe(Threads,Waits,CursorClick);
    if (mainState->manipReact[Changed]) manipReact(mainState,-1);
}
void mouseMoved(GLFWwindow* window, double xpos, double ypos) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    mainState->mouseMove.left = mainState->windowMove.left+xpos;
    mainState->mouseMove.base = mainState->windowMove.base+ypos;
    if (mainState->manipReact[Moved]) planeSafe(Threads,Waits,CursorLeft);
}
void mouseAngled(GLFWwindow *window, double amount) {
    struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
    mainState->mouseMove.angle += amount;
    if (mainState->manipReact[Angled]) planeSafe(Threads,Waits,CursorAngle);
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
    GLFWmonitor* monitor;
    GLFWcursor* moveCursor[2][2][2][2][2];
    GLFWcursor* rotateCursor[2];
    GLFWcursor* refineCursor;
    GLFWcursor* sculptCursor[2];
    GLFWcursor* standardCursor;
    VkSurfaceKHR surface;
    OpenState(VkInstance instance, int width, int height, void *ptr) {
        struct MainState *mainState = (struct MainState *)ptr;
        int32_t left, base, workx, worky, sizx, sizy;
        double posx, posy;
        this->instance = instance;
        window = [](int width, int height) {
            glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
            return glfwCreateWindow(width, height-32, "Vulkan", nullptr, nullptr);
        } (width,height);
        monitor = glfwGetPrimaryMonitor();
	glfwGetCursorPos(window,&posx,&posy);
	glfwGetWindowPos(window,&left,&base);
	mainState->mouseClick.left = mainState->mouseMove.left = mainState->mouseCopy.left = left+posx;
	mainState->mouseClick.base = mainState->mouseMove.base = mainState->mouseCopy.base = base+posy;
        glfwGetMonitorWorkarea(monitor,&left,&base,&workx,&worky);
        left += (workx-width)/2; base += (worky-height)/2;
        glfwSetWindowPos(window,left,base);
        glfwSetWindowUserPointer(window, ptr);
        glfwSetWindowAttrib(window, GLFW_DECORATED, GLFW_FALSE);
        glfwSetKeyCallback(window, keyPressed);
        glfwSetMouseButtonCallback(window, mouseClicked);
        glfwSetCursorPosCallback(window, mouseMoved);
        glfwSetWindowPosCallback(window, windowMoved);
        glfwSetWindowSizeCallback(window, windowSized);
        glfwSetWindowRefreshCallback(window, windowRefreshed);
        for (int t = 0; t < 2; t++) for (int b = 0; b < 2; b++)
        for (int l = 0; l < 2; l++) for (int r = 0; r < 2; r++)
        for (int e = 0; e < 2; e++) moveCursor[e][t][r][b][l] = ::moveCursor(e,t,r,b,l);
        for (int e = 0; e < 2; e++) rotateCursor[e] = ::rotateCursor(e);
        refineCursor = ::refineCursor();
        for (int e = 0; e < 2; e++) sculptCursor[e] = ::sculptCursor(e);
        standardCursor = glfwCreateStandardCursor(GLFW_ARROW_CURSOR);
        setCursor();
        if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
            throw std::runtime_error("failed to create window surface!");
    }
    ~OpenState() {
        vkDestroySurfaceKHR(instance, surface, nullptr);
        for (int t = 0; t < 2; t++) for (int b = 0; b < 2; b++)
        for (int l = 0; l < 2; l++) for (int r = 0; r < 2; r++)
        for (int e = 0; e < 2; e++) glfwDestroyCursor(moveCursor[e][t][r][b][l]);
        for (int e = 0; e < 2; e++) glfwDestroyCursor(rotateCursor[e]);
        glfwDestroyCursor(refineCursor);
        for (int e = 0; e < 2; e++) glfwDestroyCursor(sculptCursor[e]);
        glfwDestroyCursor(standardCursor);
        glfwDestroyWindow(window);
    }
    void setCursor() {
        struct MainState *mainState = (struct MainState *)glfwGetWindowUserPointer(window);
        int e = (mainState->manipReact[Apply]?1:0);
        int t = (mainState->manipAction[North]?1:0);
        int r = (mainState->manipAction[East]?1:0);
        int b = (mainState->manipAction[South]?1:0);
        int l = (mainState->manipAction[West]?1:0);
        int m = (mainState->manipAction[Additive]?1:0);
        if (t||r||b||l) glfwSetCursor(window,moveCursor[e][t][r][b][l]);
        else if (mainState->manipAction[Manipulate]) glfwSetCursor(window,rotateCursor[e]);
        else if (mainState->manipAction[Refine]) glfwSetCursor(window,refineCursor);
        else if (mainState->manipAction[Subtractive]||m) glfwSetCursor(window,sculptCursor[m]);
        else glfwSetCursor(window,standardCursor);
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
    PhysicalState(VkInstance instance, VkSurfaceKHR surface, std::vector<const char*> extensions, uint32_t inflight) {
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
struct GlfwState {
    SafeState safe;
    SafeState block;
    GLFWwindow* window;
    bool resizeNeeded;
    GlfwState(GLFWwindow *window) {
        this->window = window;
    }
    void wait(std::function<void()> given) {
        safe.wait(); block.wait(); safe.post(); given(); block.post();
    }
    void call(std::function<void()> given, std::function<void()> wake) {
        safe.wait(); if (!block.trywait()) {wake(); block.wait();}
        given(); safe.post(); block.post();
    }
    void call(std::function<void()> given) {
        safe.wait(); given(); safe.post();
    }
    void call(WindowState copied, bool moved, bool sized) {
        call([this,copied,moved,sized](){
        if (sized) resizeNeeded = true;
        if (moved) glfwSetWindowPos(window,copied.left,copied.base);
        if (sized) glfwSetWindowSize(window,copied.width,copied.height);
        },[](){vulkanSafe();});
    }
    void call() {
        call([this](){resizeNeeded = true;},[](){vulkanSafe();});
    }
    bool check() {
        bool resizeNeeded = false;
        bool *ref = &resizeNeeded;
        call([this,ref](){
        *ref = this->resizeNeeded;
        this->resizeNeeded = false;});
        return resizeNeeded;
    }
};

struct ThreadState {
    VkDevice device;
    SafeState protect;
    SafeState semaphore;
    pthread_t thread;
    bool finish;
    bool repeat;
    std::deque<std::function<void()>> comp;
    std::deque<std::function<VkFence()>> setup;
    std::deque<int> which;
    std::deque<VkFence> fence;
    std::deque<int> order;
    std::set<int> lookup;
    std::deque<std::function<void()>> precom;
    std::deque<std::function<VkFence()>> preset;
    std::deque<int> what;
    std::deque<int> where;
    std::deque<std::function<bool()>> when;
    int limit; int seqnum;
    ThreadState(VkDevice device, bool repeat, int limit) {
        this->device = device;
        finish = false;
        this->repeat = repeat;
        this->limit = limit;
        seqnum = 0;
        if (pthread_create(&thread,0,separate,this) != 0) throw std::runtime_error("failed to create thread!");
    }
    ~ThreadState() {
    // this destructed to wait for device idle
        if (sem_wait(&protect.semaphore) != 0) {std::cerr << "cannot wait for protect!" << std::endl; std::terminate();}
        finish = true;
        if (sem_post(&protect.semaphore) != 0) {std::cerr << "cannot post to protect!" << std::endl; std::terminate();}
        if (sem_post(&semaphore.semaphore) != 0 ||
            pthread_join(thread,0) != 0) {std::cerr << "failed to join thread!" << std::endl; std::terminate();}
    }
    static void *separate(void *ptr) {
        struct ThreadState *arg = (ThreadState*)ptr;
        arg->protect.wait();
        while (1) {
            if (arg->finish) {
                arg->protect.post();
                break;}
            while (!arg->when.empty() && arg->when.front()()) {
                int temp = arg->what.front(); arg->order.push_back(temp); arg->lookup.insert(temp);
                int tmp = arg->where.front(); arg->which.push_back(tmp); switch (tmp) {
                case (0): arg->setup.push_back(arg->preset.front()); arg->preset.pop_front(); break;
                case (1): arg->comp.push_back(arg->precom.front()); arg->precom.pop_front(); break;
                default: throw std::runtime_error("unsupported separate lambda!"); break;}
                arg->when.pop_front(); arg->what.pop_front();}
            while (!arg->which.empty()) {switch (arg->which.front()) {
                case (0): arg->fence.push_back(arg->setup.front()()); arg->setup.pop_front(); break;
                case (1): arg->comp.front()(); arg->comp.pop_front();
                arg->lookup.erase(arg->order.front()); arg->order.pop_front(); break;
                default: throw std::runtime_error("unsupported separate lambda!"); break;}
                arg->which.pop_front();}
            if (arg->fence.empty()) {
                arg->protect.post();
                arg->semaphore.wait();
                arg->protect.wait();}
            else {
                arg->protect.post();
                VkResult result = VK_SUCCESS; if (arg->fence.front() != VK_NULL_HANDLE) {
                result = vkWaitForFences(arg->device,1,&arg->fence.front(),VK_FALSE,NANOSECONDS);}
                arg->protect.wait();
                if (result != VK_SUCCESS && result != VK_TIMEOUT) throw std::runtime_error("cannot wait for fence!");
                if (result == VK_SUCCESS) {int next = arg->order.front();
                arg->lookup.erase(next); arg->order.pop_front(); arg->fence.pop_front();
                if (arg->repeat) {planeSafe(Threads,Waits,RegisterDone); vulkanSafe();}}}}
        vkDeviceWaitIdle(arg->device);
        return 0;
    }
    bool clear(int given) {
        if (protect.trywait()) throw std::runtime_error("protect not held!");
        return (lookup.find(given) == lookup.end());
    }
    bool clear(std::function<bool()> given) {
        protect.wait();
        if (given()) {protect.post(); return true;}
        protect.post(); return false;
    }
    bool push() {
        bool ret;
        protect.wait();
        ret = (order.size() < limit);
        protect.post();
        return ret;
    }
    std::function<bool()> push(std::function<void(int)> given) {
        protect.wait();
        int sval = semaphore.get();
        if (sval == 0) semaphore.post();
        int temp = seqnum++; lookup.insert(temp); given(temp);
        std::function<bool()> done = [this,temp](){return this->clear(temp);};
        if (fence.size()+setup.size()+comp.size() != order.size()) throw std::runtime_error("cannot push seqnum!");
        if (order.size()+what.size() != lookup.size()) throw std::runtime_error("cannot insert seqnum!");
        protect.post();
        return done;}
    std::function<bool()> push(std::function<void()> given) {
    // return function that returns whether given function called in separate thread
        return push([this,given](int temp){order.push_back(temp); comp.push_back(given); which.push_back(1);});}
    std::function<bool()> push(std::function<void()> given, std::function<bool()> last) {
    // return query of whether given function called in indicated sequence
        if (clear(last)) return push(given);
        return push([this,given,last](int temp){what.push_back(temp); when.push_back(last);
        where.push_back(1); precom.push_back(given);});}
    std::function<bool()> push(std::function<VkFence()> given) {
    // return function that returns whether fence returned by given function in separate thread is done
        return push([this,given](int temp){order.push_back(temp); setup.push_back(given); which.push_back(0);});}
    std::function<bool()> push(std::function<VkFence()> given, std::function<bool()> last) {
    // return query of whether fence is done for given function started in indicated sequence
        if (clear(last)) return push(given);
        return push([this,given,last](int temp){what.push_back(temp); when.push_back(last);
        where.push_back(0); preset.push_back(given);});}
};

struct TempState {
    int seqnum;
    std::map<int,std::deque<std::function<bool()>>> done;
    TempState() {
        seqnum = 0;
    }
    int temp() {
    // return identifier for done function
        done[seqnum] = std::deque<std::function<bool()>>();
        return seqnum++;
    }
    std::function<bool()> temp(int tmp) {
    // return done function from identifier
        return [this,tmp](){
            if (done.find(tmp) == done.end()) return true;
            for (auto i = done[tmp].begin(); i != done[tmp].end(); i++) if (!(*i)()) return false;
            done.erase(tmp); return true;};
    }
    void temp(int tmp, std::function<bool()> fnc) {
    // bind done function to identified done function
        done[tmp].push_back(fnc);
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
};

enum WrapTag {FetchBuf,ChangeBuf,StoreBuf,QueryBuf,DrawBuf};
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
    WrapTag tag; MainState *info; TempState *temp;
    WrapState(MainState *info, TempState *temp, int limit, WrapTag tag) {
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
        this->temp = temp;
    }
    ~WrapState() {
        while (!pool.empty()) {delete pool.front(); pool.pop_front();}
        while (!running.empty()) {delete running.front(); running.pop_front();}
        if (ready) delete ready;
        while (!inuse.empty()) {delete inuse.front(); inuse.pop_front();}
    }
    bool clr() {
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
    // return whether queues are not full
        if (!info->threadState->push()) return false;
        if (!pool.empty()) return true;
        if (count < limit) return true;
        return false;
    }
    WrapTag typ() {
        return tag;
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
        std::function<bool()> wrap = [given,this](){
        info->threadState->protect.wait();
        bool ret = given();
        info->threadState->protect.post();
        return ret;};
        temp->temp(setvld,settag,wrap);
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
    void set(int siz) {
    // change queue item size
        clr(); if (siz != size) {
        while (!pool.empty()) {delete pool.front(); pool.pop_front();}
        copy = realloc(copy,size = siz);}
    }
    std::function<bool()> set(std::function<void(Buffer*buf)> setup) {
        Buffer *ptr = buf().first; std::function<bool()> done = (buf().second?
        info->threadState->push((std::function<void()>)[setup,ptr](){ptr->init(); setup(ptr);},seq()):
        info->threadState->push((std::function<void()>)[setup,ptr](){setup(ptr);},seq()));
        sep(done); tmp(done); sepvld = false; seqvld = false; setbuf.second = false;
        return done;
    }
    std::function<bool()> set(std::function<VkFence(Buffer*)> setup) {
        Buffer *ptr = buf().first; std::function<bool()> done = (buf().second?
        info->threadState->push((std::function<VkFence()>)[setup,ptr](){ptr->init(); return setup(ptr);},seq()):
        info->threadState->push((std::function<VkFence()>)[setup,ptr](){return setup(ptr);},seq()));
        sep(done); tmp(done); sepvld = false; seqvld = false; setbuf.second = false;
        return done;
    }
    std::function<bool()> set() {
        buf().first->bind(0,size,copy,[](){});
        return set((std::function<VkFence(Buffer*)>)[](Buffer*buf){return buf->setup();});
    }
    std::function<bool()> set(int loc, int siz, const void *ptr, std::function<void()> dat) {
        int size = (loc+siz > this->size ? loc+siz : this->size); set(size);
        info->threadState->push((std::function<void()>)[this,loc,ptr,siz](){memcpy((void*)((char*)copy+loc),ptr,siz);});
        if (tag == QueryBuf) return [](){return true;};
        if (buf().second) return set();
        buf().first->bind(loc,siz,ptr,dat);
        return set((std::function<VkFence(Buffer*)>)[](Buffer*buf){return buf->setup();});
    }
    void put() {
        running.push_back(buf().first); toready.push_back(tmp());
        setvld = false; setbuf.first = 0;
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

struct BufferState;
struct DrawState;
struct FieldState {
    int stride;
    std::vector<VkFormat> format;
    std::vector<uint32_t> offset;
};
struct QueueState {
    WrapState<BufferState>* bufferQueue[Memorys];
    WrapState<DrawState>* drawQueue[Micros];
    FieldState *fieldState;
    std::vector<WrapState<BufferState>*> bindBuffer[Micros];
    std::vector<WrapState<BufferState>*> queryBuffer[Micros];
    std::vector<FieldState*> fieldBuffer[Micros];
    const char *vertexName[Micros];
    const char *fragmentName[Micros];
    TempState tempState;
    QueueState() {
    for (int i = 0; i < Micros; i++) {vertexName[i] = "vertexMicrosG";
    switch (Component__Micro__MicroIn((Micro)i)) {
    case (Practice): vertexName[i] = "vertexPracticeG"; break;
    case (Concept): switch (Component__Micro__MicroMid((Micro)i)) {
    case (Raster): vertexName[i] = "vertexRasterG"; break;
    case (CoPoint): vertexName[i] = "vertexCopointG"; break;
    case (Coplane): vertexName[i] = "vertexCoplaneG"; break;} break;}}
    for (int i = 0; i < Micros; i++) {fragmentName[i] = "fragmentMicrosG";
    switch (Component__Micro__MicroOut((Micro)i)) {
    case (DisPlay): fragmentName[i] = "fragmentDisplayG"; break;
    case (Compute): fragmentName[i] = "fragmentComputeG"; break;
    case (PRepare): fragmentName[i] = "fragmentPrepareG"; break;}}
    for (int i = 0; i < Memorys; i++) bufferQueue[i] = 0;
    for (int i = 0; i < Micros; i++) drawQueue[i] = 0;
    fieldState = new FieldState();
    fieldState->stride = sizeof(Vertex);
    fieldState->format.push_back(VK_FORMAT_R32G32_SFLOAT);
    fieldState->format.push_back(VK_FORMAT_R32_UINT);
    fieldState->offset.push_back(offsetof(Vertex,vec));
    fieldState->offset.push_back(offsetof(Vertex,ref));
    bufferQueue[Vertexz] = new WrapState<BufferState>(&mainState,&tempState,mainState.MAX_BUFFERS_AVAILABLE,FetchBuf);
    bufferQueue[Matrixz] = new WrapState<BufferState>(&mainState,&tempState,mainState.MAX_BUFFERS_AVAILABLE,ChangeBuf);
    bufferQueue[Indexz] = new WrapState<BufferState>(&mainState,&tempState,mainState.MAX_BUFFERS_AVAILABLE,QueryBuf);
    bufferQueue[Piercez] = new WrapState<BufferState>(&mainState,&tempState,mainState.MAX_BUFFERS_AVAILABLE,QueryBuf);
    bindBuffer[MicroPRPC].push_back(bufferQueue[Vertexz]);
    bindBuffer[MicroPRPC].push_back(bufferQueue[Matrixz]);
    fieldBuffer[MicroPRPC].push_back(fieldState);
    fieldBuffer[MicroPRPC].push_back(0);
    drawQueue[MicroPRPC] = new WrapState<DrawState>(&mainState,&tempState,mainState.MAX_FRAMES_IN_FLIGHT,DrawBuf);
    bindBuffer[MicroPRRC].push_back(bufferQueue[Vertexz]);
    bindBuffer[MicroPRRC].push_back(bufferQueue[Matrixz]);
    queryBuffer[MicroPRRC].push_back(bufferQueue[Indexz]);
    fieldBuffer[MicroPRRC].push_back(fieldState);
    fieldBuffer[MicroPRRC].push_back(0);
    drawQueue[MicroPRRC] = new WrapState<DrawState>(&mainState,&tempState,mainState.MAX_FRAMES_IN_FLIGHT,DrawBuf);
    }
    ~QueueState() {
    for (int i = 0; i < Micros; i++) if (drawQueue[i]) delete drawQueue[i];
    if (fieldState) delete fieldState;
    for (int i = 0; i < Memorys; i++) if (bufferQueue[i]) delete bufferQueue[i];}
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
    int loc; int siz; const void *ptr;
    std::function<void()> dat;
    WindowState copied; bool moved, sized;
BufferState(MainState *state, int size, WrapTag tag) {
    PhysicalState* physical = state->physicalState;
    DeviceState* logical = state->logicalState;
    this->physical = physical->physical;
    this->device = logical->device;
    this->graphic = logical->graphic;
    this->pool = logical->pool;
    this->size = size;
    this->tag = tag;
    ptr = 0; moved = sized = false;}
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
    } (device,requirements,[](uint32_t type, VkPhysicalDeviceMemoryProperties properties, VkMemoryPropertyFlags flags) {
        for (uint32_t i = 0; i < properties.memoryTypeCount; i++) if ((type & (1 << i)) &&
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
    } (device,requirements,[](uint32_t type, VkPhysicalDeviceMemoryProperties properties, VkMemoryPropertyFlags flags) {
        for (uint32_t i = 0; i < properties.memoryTypeCount; i++) if ((type & (1 << i)) &&
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
    vkCmdCopyBuffer(command, buffer, staging, 1, &copy);
    vkEndCommandBuffer(command);
    VkSubmitInfo submit{};
    submit.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.commandBufferCount = 1;
    submit.pCommandBuffers = &command;
    result = vkQueueSubmit(graphic, 1, &submit, fence);
    return fence;}
void bind(int loc, int siz, const void *ptr, std::function<void()> dat) {
    this->loc = loc; this->siz = siz; this->ptr = ptr; this->dat = dat;}
void bind(WindowState copied) {
    this->moved = (this->copied.left != copied.left || this->copied.base != copied.base);
    this->sized = (this->copied.width != copied.width || this->copied.height != copied.height);
    this->copied = copied;}
void bind(std::function<void(WindowState copied, bool moved, bool sized)> bind) {
    if (moved || sized) bind(copied,moved,sized); moved = sized = false;}
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

struct PipelineState {
    VkDevice device;
    VkPipelineLayout layout;
    VkPipeline pipeline;
    VkDescriptorSetLayout dlayout;
    VkDescriptorSet descriptor;
PipelineState(VkDevice device, VkRenderPass render, VkDescriptorPool dpool, Micro micro, QueueState *queue) {
    this->device = device;
    const char *vertex = queue->vertexName[micro];
    const char *fragment = queue->fragmentName[micro];
    dlayout = [](VkDevice device, std::vector<WrapState<BufferState>*> &type) {
        std::vector<VkDescriptorSetLayoutBinding> bindings;
        int count = 0; for (auto i = type.begin(); i != type.end(); i++)
        if ((*i)->typ() == ChangeBuf || (*i)->typ() == StoreBuf || (*i)->typ() == QueryBuf) {
        VkDescriptorSetLayoutBinding binding{};
        binding.binding = count++;
        binding.descriptorCount = 1;
        binding.descriptorType = ((*i)->typ() == ChangeBuf ?
            VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER : VK_DESCRIPTOR_TYPE_STORAGE_BUFFER);
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
    } (device,queue->bindBuffer[micro]);
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
        state.dynamicStateCount = static_cast<uint32_t>(states.size());
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
    VkCommandBuffer command;
    VkFence fence;
    uint32_t index;
    std::function<SwapState*()> swap;
    GlfwState *glfw;
    PipelineState* pipeline;
    WindowState copied; bool moved, sized;
DrawState(MainState *state, int size, WrapTag tag) {
    DeviceState* logical = state->logicalState;
    this->device = logical->device;
    this->graphic = logical->graphic;
    this->present = logical->present;
    this->render = logical->render;
    this->pool = logical->pool;
    this->swap = [state](){return state->swapState;};
    this->glfw = state->glfwState;
    this->pipeline = new PipelineState(device,render,logical->dpool,(Micro)size,state->queueState);
    moved = sized = false;}
void init() {
    // this called in separate thread on newly constructed
    available = [](VkDevice device) {
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
    vkDestroySemaphore(device, available, nullptr);
    delete pipeline;}
// TODO use bind captured members instead of lambda captured parameters
VkFence setup(const std::vector<BufferState*> &buffer, uint32_t base, uint32_t limit) {
    VkResult result = vkAcquireNextImageKHR(device, swap()->swap, UINT64_MAX, available, VK_NULL_HANDLE, &index);
    if (result == VK_ERROR_OUT_OF_DATE_KHR) {glfw->call(); std::cerr << "out of date" << std::endl;}
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
    } (render,swap()->framebuffers[index],swap()->extent,command);
    vkCmdBindPipeline(command, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->pipeline);
    [](VkExtent2D extent, VkCommandBuffer command){
        VkViewport info{};
        info.x = 0.0f; info.y = 0.0f;
        info.width = (float) extent.width;
        info.height = (float) extent.height;
        info.minDepth = 0.0f; info.maxDepth = 1.0f;
        vkCmdSetViewport(command, 0, 1, &info);}(swap()->extent,command);
    [](VkExtent2D extent, VkCommandBuffer command){
        VkRect2D scissor{};
        scissor.offset = {0, 0};
        scissor.extent = extent;
        vkCmdSetScissor(command, 0, 1, &scissor);}(swap()->extent,command);
    int count = 0; for (int i = 0; i < buffer.size(); i++) {
        buffer[i]->bind([this](WindowState copied, bool moved, bool sized){bind(copied,moved,sized);});
        if (buffer[i]->bind(count,command,pipeline->descriptor)) count++;}
    vkCmdBindDescriptorSets(command, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->layout, 0, 1,
        &pipeline->descriptor, 0, nullptr);
    vkCmdDraw(command, limit-base, (limit-base)/3, base, base/3);
    vkCmdEndRenderPass(command);
    if (vkEndCommandBuffer(command) != VK_SUCCESS)
        throw std::runtime_error("failed to record command buffer!");
    [](VkQueue graphic, VkCommandBuffer command, VkFence fence, VkSemaphore available){
        VkSubmitInfo info{};
        info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore availables[] = {available};
        VkPipelineStageFlags stages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
        info.waitSemaphoreCount = 1;
        info.pWaitSemaphores = availables;
        info.pWaitDstStageMask = stages;
        info.commandBufferCount = 1;
        info.pCommandBuffers = &command;
        info.signalSemaphoreCount = 0;
        if (vkQueueSubmit(graphic, 1, &info, fence) != VK_SUCCESS)
            throw std::runtime_error("failed to submit draw command buffer!");
    }(graphic,command,fence,available);
    // TODO if moved and sized are false, use signal semaphore instead of fenced setup
    return fence;}
void setup() {
    if (moved || sized) glfw->call(copied,moved,sized);
    [](VkSwapchainKHR swap, VkQueue present, uint32_t index, GlfwState *glfw){
        VkPresentInfoKHR info{};
        info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
        info.waitSemaphoreCount = 0;
        VkSwapchainKHR swaps[] = {swap};
        info.swapchainCount = 1;
        info.pSwapchains = swaps;
        info.pImageIndices = &index;
        VkResult result = vkQueuePresentKHR(present, &info);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) glfw->call();
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            throw std::runtime_error("device lost on wait for fence!");
    }(swap()->swap,present,index,glfw);}
void bind(WindowState copied, bool moved, bool sized) {
    this->copied = copied; this->moved = moved; this->sized = sized;}
};

void vulkanExtent()
{
    // TODO pipeline this; create now for immediate use; delete and activate next when idle
    if (mainState.glfwState->check()) {
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
        mainState.threadState = new ThreadState(mainState.logicalState->device,
        mainState.manipReact[Repeat],mainState.MAX_FENCES_IN_FLIGHT);}
}
int vulkanInfo(enum Configure query)
{
    switch (query) {default: throw std::runtime_error("cannot get info!");
    break; case (OriginLeft): return mainState.mouseClick.left;
    break; case (OriginBase): return mainState.mouseClick.base;
    break; case (OriginAngle): return mainState.mouseClick.angle;
    break; case (CursorLeft): return mainState.mouseMove.left;
    break; case (CursorBase): return mainState.mouseMove.base;
    break; case (CursorAngle): return mainState.mouseMove.angle;
    break; case (CursorPress): {if (mainState.keyPressed.empty()) return 0;
        int key = mainState.keyPressed.front(); mainState.keyPressed.pop_front(); return key;}
    break; case (WindowLeft): return mainState.windowMove.left;
    break; case (WindowBase): return mainState.windowMove.base;
    break; case (WindowWidth): mainState.windowMove.height;
    break; case (WindowHeight): mainState.windowMove.width;
    break; case (MonitorWidth): {const GLFWvidmode *mode = // TODO add to mainState
        glfwGetVideoMode(mainState.openState->monitor);
        return mode->width;}
    break; case (MonitorHeight): {const GLFWvidmode *mode =
        glfwGetVideoMode(mainState.openState->monitor);
        return mode->height;}
    break; case (PhysicalWidth): {int xphys, yphys;
        glfwGetMonitorPhysicalSize(mainState.openState->monitor,&xphys,&yphys);
        return xphys;}
    break; case (PhysicalHeight): {int xphys, yphys;
        glfwGetMonitorPhysicalSize(mainState.openState->monitor,&xphys,&yphys);
        return yphys;}
    break; case (RegisterDone): return (mainState.registerDone ? mainState.registerDone-- : 0);
    break; case (RegisterOpen): return (!mainState.escapeEnter);
    break; case (ParamFollow): return mainState.paramFollow;
    break; case (ParamModify): return mainState.paramModify;
    break; case (ParamDisplay): return mainState.paramDisplay;
    break; case (ParamBrighten): return mainState.paramBrighten;
    break; case (ParamDetect): return mainState.paramDetect;
    break; case (ParamBase): return mainState.paramBase;
    break; case (ParamLimit): return mainState.paramLimit;
    break; case (ManipReact): {int mask = 0; for (int i = 0; i < Reacts; i++)
        if (mainState.manipReact[(React)i]) mask |= (1<<i); return mask;}
    break; case (ManipAction): {int mask = 0; for (int i = 0; i < Actions; i++)
        if (mainState.manipAction[(Action)i]) mask |= (1<<i); return mask;}
    }
    return 0;
}
void vulkanDma(struct Center *center)
{
    int siz; void *ptr; struct WrapState<BufferState> *buf;
    switch (center->mem) {default: throw std::runtime_error("unsupported mem!");
    break; case (Indexz): siz = sizeof(center->buf[0]); ptr = center->buf;
    break; case (Piercez): siz = sizeof(center->pie[0]); ptr = center->pie;
    break; case (Vertexz): siz = sizeof(center->vtx[0]); ptr = center->vtx;
    break; case (Matrixz): siz = sizeof(center->mat[0]); ptr = center->mat;
    break; case (Littlez): if (center->idx+center->siz > mainState.changedSize)
        mainState.changedSize = center->idx+center->siz; mainState.changedState =
        (struct Little *)realloc(mainState.changedState,mainState.changedSize*sizeof(center->pvn[0]));
        memcpy(mainState.changedState,center->pvn+center->idx,center->siz*sizeof(center->pvn[0])); return;
    break; case (Configurez): for (int i = 0; i < center->siz; i++)
    switch (center->cfg[i]) {default: throw std::runtime_error("unsupported cfg!");
    break; case (RegisterDone): mainState.registerDone = center->val[i];
    break; case (RegisterOpen): if (center->val[i] || !mainState.enable) mainState.escapeEnter = true;
    break; case (CursorPress): if (center->val[i] == 0)
        mainState.keyPressed.clear(); else mainState.keyPressed.push_front(center->val[i]);
    break; case (CursorIndex): mainState.paramIndex = center->val[i];
    break; case (ManipReact): if (mainState.manipReact[Repeat] != ((center->val[i]&(1<<Repeat)) != 0))
        mainState.glfwState->call(); for (int j = 0; j < Reacts; j++)
        mainState.manipReact[(React)j] = ((center->val[i]&(1<<j)) != 0); mainState.openState->setCursor();
    break; case (ManipAction): if (mainState.manipAction[Repeat] != ((center->val[i]&(1<<Repeat)) != 0))
        mainState.glfwState->call(); for (int j = 0; j < Actions; j++)
        mainState.manipAction[(Action)j] = ((center->val[i]&(1<<j)) != 0); mainState.openState->setCursor();
    break; case (ParamFollow): mainState.paramFollow = center->val[i];
    break; case (ParamModify): mainState.paramModify = center->val[i];
    break; case (ParamDisplay): mainState.paramDisplay = (Micro)center->val[i];
    break; case (ParamBrighten): mainState.paramBrighten = (Micro)center->val[i];
    break; case (ParamDetect): mainState.paramDetect = (Micro)center->val[i];
    break; case (ParamBase): mainState.paramBase = center->val[i];
    break; case (ParamLimit): mainState.paramLimit = center->val[i];
    break; case (LittleIndex): mainState.changedIndex = center->val[i];
    break; case (LittleSize): mainState.changedSize = center->val[i]; mainState.changedState =
        (struct Little *)realloc(mainState.changedState,mainState.changedSize*sizeof(center->pvn[0]));
    } planeDone(center); return;}
    vulkanExtent(); buf = mainState.queueState->bufferQueue[center->mem];
    buf->set(center->idx*siz,siz*center->siz,ptr,[center](){planeDone(center);}); buf->put();
}
void vulkanDraw(enum Micro shader, int base, int limit)
{
    QueueState *queue = mainState.queueState;
    TempState *temp = &queue->tempState;
    std::vector<BufferState*> buffer;
    std::vector<WrapState<BufferState>*> *bindBuffer = queue->bindBuffer+shader;
    std::vector<WrapState<BufferState>*> *queryBuffer = queue->queryBuffer+shader;
    WrapState<DrawState> *draw = queue->drawQueue[shader];
    vulkanExtent();
    for (auto i = bindBuffer->begin(); i != bindBuffer->end(); i++) if (!(*i)->get()) return;
    if (!draw->clr()) return;
    for (auto i = queryBuffer->begin(); i != queryBuffer->end(); i++) {
    draw->seq((*i)->sep()); buffer.push_back((*i)->buf().first);
    if (Component__Micro__MicroOn(shader) == CoPyon) {
    (*i)->set(); (*i)->put(); (*i)->seq(draw->sep());} else {
    (*i)->tmp(draw->tmp()); (*i)->set();}}
    for (auto i = bindBuffer->begin(); i != bindBuffer->end(); i++) {
    buffer.push_back((*i)->get(draw->tmp()));}
    draw->set(shader); mainState.registerDone++;
    std::function<bool()> done = draw->set((std::function<VkFence(DrawState*)>)[buffer,base,limit](DrawState*draw){
    return draw->setup(buffer,base,limit);}); draw->seq(done);
    draw->set((std::function<void(DrawState*)>)[](DrawState*draw){draw->setup();}); draw->put();
    for (auto i = queryBuffer->begin(); i != queryBuffer->end(); i++) {
    if (Component__Micro__MicroOn(shader) == CoPyon) {
    (*i)->set((std::function<VkFence(BufferState*buf)>)[](BufferState*buf){return buf->getup();});}}
}
void vulkanModify(int loc, int siz, float *mat, std::function<void()> dat)
{
    WrapState<BufferState>* bufferQueue = mainState.queueState->bufferQueue[Matrixz];
    vulkanExtent();
    if (!bufferQueue->clr()) return; // TODO set bit in register for error info
    bufferQueue->set(loc,siz,mat,dat); bufferQueue->put();
}
void vulkanFollow(int loc, int siz, float *mat, std::function<void()> dat)
{
    WrapState<BufferState>* bufferQueue = mainState.queueState->bufferQueue[Matrixz];
    if (!bufferQueue->clr()) return; // TODO set bit in register for error info
    if (mainState.manipReact[Apply] &&
        mainState.manipAction[North] && mainState.manipAction[East] &&
        mainState.manipAction[South] && mainState.manipAction[West]) {
        int xpos = mainState.mouseMove.left;
        int ypos = mainState.mouseMove.base;
        int xofs = xpos-mainState.mouseCopy.left;
        int yofs = ypos-mainState.mouseCopy.base;
        mainState.windowCopy.left += xofs; mainState.windowCopy.base += yofs;
        mainState.mouseCopy.left = xpos; mainState.mouseCopy.base = ypos;
        bufferQueue->buf().first->bind(mainState.windowCopy);}
    vulkanModify(loc,siz,mat,dat);
}
void vulkanDirect(float left, float base, float angle, int index)
{
    // TODO write CursorLeft CursorBase CursorIndex to Uniform
}
void vulkanChanged()
{
    int siz = 16*sizeof(float);
    float *mat = (float*)malloc(siz);
    if (mainState.manipReact[Follow]) vulkanFollow(mainState.paramFollow*siz,siz,planeWindow(mat),[mat](){free(mat);});
    #ifdef PLANRA
    if (mainState.manipReact[Modify]) vulkanModify(mainState.paramModify*siz,siz,planraMatrix(mat),[mat](){free(mat);});
    #else
    if (mainState.manipReact[Modify]) vulkanModify(mainState.paramModify*siz,siz,planeMatrix(mat),[mat](){free(mat);});
    #endif
    if (mainState.manipReact[Direct]) vulkanDirect(
        mainState.mouseMove.left,mainState.mouseMove.base,mainState.mouseMove.angle,mainState.paramIndex);
    debugStart();
    if (mainState.manipReact[Display]) vulkanDraw(mainState.paramDisplay,mainState.paramBase,mainState.paramLimit);
    if (mainState.manipReact[Brighten]) vulkanDraw(mainState.paramBrighten,mainState.paramBase,mainState.paramLimit);
    if (mainState.manipReact[Detect]) vulkanDraw(mainState.paramDetect,mainState.paramBase,mainState.paramLimit);
    debugStop("vulkanChanged");
}
struct Center *vulkanReady(enum Memory mem)
{
    // reserve buffer to return mapped in zero time
    struct Center *center = 0; allocCenter(&center,1); center->mem = mem;
    void *ptr = mainState.queueState->bufferQueue[mem]->get(&center->siz,center)->mapped;
    switch (mem) {default: throw std::runtime_error("unsupported ready memory!");
    break; case (Piercez): center->pie = (struct Pierce *)ptr; center->siz /= sizeof(struct Pierce);}
    return center;
}
void vulkanDone(struct Center *ptr)
{
    // release reserved buffer
    mainState.queueState->bufferQueue[Piercez]->get(ptr);
    ptr->siz = 0; allocCenter(&ptr,0);
}
void vulkanSafe()
{
    glfwPostEmptyEvent();
}
void vulkanInit()
{
    for (int arg = 0; arg < mainState.argc; arg++) planePutstr(mainState.argv[arg]);
    mainState.initState = new InitState(mainState.enable,mainState.layers);
}
void vulkanMain(enum Thread proc, enum Wait wait)
{
    switch(wait) {
    case (Start):
    switch (proc) {
    case (Window):
    mainState.openState = new OpenState(mainState.initState->instance,
    mainState.windowMove.width,mainState.windowMove.height,(void*)&mainState);
    mainState.glfwState = new GlfwState(mainState.openState->window);
    break;
    case (Graphics):
    mainState.physicalState = new PhysicalState(
    mainState.initState->instance,mainState.openState->surface,mainState.extensions,
    mainState.MAX_FRAMES_IN_FLIGHT);
    mainState.logicalState = [](PhysicalState *physical){
    return new DeviceState(physical->physical,physical->graphicid,physical->presentid,
    physical->image,mainState.layers,mainState.extensions,mainState.enable,
    mainState.MAX_BUFFERS_AVAILABLE*Memorys);
    }(mainState.physicalState);
    mainState.queueState = new QueueState();
    break;
    case (Process):
    vulkanExtent();
    while (!mainState.escapeEnter) {
    vulkanChanged();
    planeMain();
    if (mainState.manipReact[Poll]) mainState.glfwState->wait([](){glfwPollEvents();});
    else mainState.glfwState->wait([](){glfwWaitEventsTimeout(1.0);});}
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

int main(int argc, char **argv)
{
    mainState.argc = argc;
    mainState.argv = argv;
    try {
#ifdef PLANRA
        planeInit(vulkanInit,vulkanSafe,vulkanMain,vulkanDma,vulkanDraw,vulkanReady,vulkanDone,planraWake,vulkanInfo,planraBoot);
#else
        planeInit(vulkanInit,vulkanSafe,vulkanMain,vulkanDma,vulkanDraw,vulkanReady,vulkanDone,planrWake,vulkanInfo,planrBoot);
#endif
        delete mainState.initState;
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
