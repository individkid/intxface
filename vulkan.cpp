#define GLFW_INCLUDE_VULKAN
#define _GLFW_X11
#define _GLFW_WAYLAND
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
#include <stdio.h>
extern "C" {
#include "proto.h"
#include "face.h"
#include "type.h"
#include "plane.h"
#include "fmtx.h"
};
#include "stlx.h"

// TODO declare glfw callbacks


struct WindowState {
    const uint32_t WIDTH = 800;
    const uint32_t HEIGHT = 600;
    GLFWwindow* const window;
    WindowState() :
        window(createWindow(WIDTH,HEIGHT))
        {std::cout << "WindowState" << std::endl;}
    ~WindowState() {std::cout << "~WindowState" << std::endl;
        glfwDestroyWindow(window);
        glfwTerminate();
    }
    static GLFWwindow* createWindow(uint32_t WIDTH, uint32_t HEIGHT);
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
    ~VulkanState() {std::cout << "~VulkanState" << std::endl;
        vkDestroySurfaceKHR(instance, surface, nullptr);
        auto func = (PFN_vkDestroyDebugUtilsMessengerEXT)
        vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
        if (func != nullptr) func(instance, debug, nullptr);
        vkDestroyInstance(instance, nullptr);
    }
    static VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
        VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData);
    static VkDebugUtilsMessengerCreateInfoEXT createInfo(const char **validationLayers);
    static VkInstance createInstance(VkDebugUtilsMessengerCreateInfoEXT info, const char **validationLayers);
    static VkDebugUtilsMessengerEXT createDebug(VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info,
        const char **validationLayers);
    static VkSurfaceKHR createSurface(VkInstance instance, GLFWwindow* window);
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
    static bool foundIndices(VkSurfaceKHR surface, VkPhysicalDevice device);
    static bool foundDetails(VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkPhysicalDevice createDevice(VkInstance instance, VkSurfaceKHR surface, const char **deviceExtensions);
    static uint32_t findGraphicsFamily(VkSurfaceKHR surface, VkPhysicalDevice device);
    static uint32_t findPresentFamily(VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkPhysicalDeviceProperties findProperties(VkPhysicalDevice device);
    static VkSurfaceFormatKHR chooseSwapSurfaceFormat(VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkPresentModeKHR chooseSwapPresentMode(VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkPhysicalDeviceMemoryProperties findMemoryProperties(VkPhysicalDevice device);
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
    ~LogicalState() {std::cout << "~LogicalState" << std::endl;
        vkDestroyRenderPass(device, renderPass, nullptr);
        vkDestroyCommandPool(device, commandPool, nullptr);
        vkDestroyDevice(device, nullptr);
    }
    static VkDevice createDevice(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily,
        const char **validationLayers, const char **deviceExtensions);
    static VkQueue createQueue(VkDevice device, uint32_t family);
    static VkCommandPool createCommandPool(VkDevice device, uint32_t family);
    static VkFormat findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size,
        VkImageTiling tiling, VkFormatFeatureFlags features);
    static VkRenderPass createRenderPass(VkDevice device, VkFormat imageFormat, VkFormat depthFormat);
};

struct BaseState;
struct StackState {
    const char *name;
    static const int frames = 2;
    virtual BaseState *buffer() = 0; // no block beween push and advance
    virtual BaseState *prebuf() = 0; // current available for read while next is written
    virtual BaseState *prebuf(int i) = 0;
    virtual void advance() = 0;
    virtual void advance(int i) = 0;
    virtual Bind buftyp() = 0;
    virtual int bufsiz() = 0;
    static StackState* self;
    static int debug;
    static int micro;
    static ChangeState<Configure,Configures> *copy;
    static GLFWwindow* window;
    static VkSurfaceKHR surface;
    static VkPhysicalDevice physical;
    static VkSurfaceFormatKHR surfaceFormat;
    static VkPresentModeKHR presentMode;
    static uint32_t graphicsFamily;
    static uint32_t presentFamily;
    static VkPhysicalDeviceProperties properties;
    static VkPhysicalDeviceMemoryProperties memProperties;
    static VkDevice device;
    static VkCommandPool commandPool;
    static VkRenderPass renderPass;
    static VkFormat imageFormat;
    static VkFormat depthFormat;
    static VkQueue graphics;
    static VkQueue present;
    static VkBufferUsageFlags flags;
    StackState(const char *name,
        ChangeState<Configure,Configures> *copy,
        GLFWwindow* window,
        VkSurfaceKHR surface,
        VkPhysicalDevice physical,
        VkSurfaceFormatKHR surfaceFormat,
        VkPresentModeKHR presentMode,
        uint32_t graphicsFamily,
        uint32_t presentFamily,
        VkPhysicalDeviceProperties properties,
        VkPhysicalDeviceMemoryProperties memProperties,
        VkDevice device,
        VkCommandPool commandPool,
        VkRenderPass renderPass,
        VkFormat imageFormat,
        VkFormat depthFormat,
        VkQueue graphics,
        VkQueue present) :
    name(name) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::copy = copy;
        StackState::window = window;
        StackState::surface = surface;
        StackState::physical = physical;
        StackState::surfaceFormat = surfaceFormat;
        StackState::presentMode = presentMode;
        StackState::graphicsFamily = graphicsFamily;
        StackState::presentFamily = presentFamily;
        StackState::properties = properties;
        StackState::memProperties = memProperties;
        StackState::device = device;
        StackState::commandPool = commandPool;
        StackState::renderPass = renderPass;
        StackState::imageFormat = imageFormat;
        StackState::depthFormat = depthFormat;
        StackState::graphics = graphics;
        StackState::present = present;
    }
    StackState(const char *name, VkBufferUsageFlags flags) : name(name) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::flags = flags;        
    }
    StackState(const char *name) : name(name) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;        
    }
};
StackState* StackState::self;
int StackState::debug;
int StackState::micro;
ChangeState<Configure,Configures> *StackState::copy;
GLFWwindow* StackState::window;
VkSurfaceKHR StackState::surface;
VkPhysicalDevice StackState::physical;
VkSurfaceFormatKHR StackState::surfaceFormat;
VkPresentModeKHR StackState::presentMode;
uint32_t StackState::graphicsFamily;
uint32_t StackState::presentFamily;
VkPhysicalDeviceProperties StackState::properties;
VkPhysicalDeviceMemoryProperties StackState::memProperties;
VkDevice StackState::device;
VkCommandPool StackState::commandPool;
VkRenderPass StackState::renderPass;
VkFormat StackState::imageFormat;
VkFormat StackState::depthFormat;
VkQueue StackState::graphics;
VkQueue StackState::present;
VkBufferUsageFlags StackState::flags;

template <class State, Bind Type, int Size> struct ArrayState : public StackState {
    SafeState safe;
    int idx;
    State state[Size];
    ArrayState(const char *name,
        ChangeState<Configure,Configures> *copy,
        GLFWwindow* window,
        VkSurfaceKHR surface,
        VkPhysicalDevice physical,
        VkSurfaceFormatKHR surfaceFormat,
        VkPresentModeKHR presentMode,
        uint32_t graphicsFamily,
        uint32_t presentFamily,
        VkPhysicalDeviceProperties properties,
        VkPhysicalDeviceMemoryProperties memProperties,
        VkDevice device,
        VkCommandPool commandPool,
        VkRenderPass renderPass,
        VkFormat imageFormat,
        VkFormat depthFormat,
        VkQueue graphics,
        VkQueue present) :
    StackState(name,
        copy,
        window,
        surface,
        physical,
        surfaceFormat,
        presentMode,
        graphicsFamily,
        presentFamily,
        properties,
        memProperties,
        device,
        commandPool,
        renderPass,
        imageFormat,
        depthFormat,
        graphics,
        present),
        safe(1), idx(0) {}
    ArrayState(const char *name, VkBufferUsageFlags flags) :
        StackState(name,flags), safe(1), idx(0) {}
    ArrayState(const char *name) :
        StackState(name), safe(1), idx(0) {}
    BaseState *buffer() override {safe.wait(); BaseState *ptr = &state[idx]; safe.post(); return ptr;}
    BaseState *prebuf() override {safe.wait(); BaseState *ptr = &state[(idx+1)%Size]; safe.post(); return ptr;}
    BaseState *prebuf(int i) override {
        if (i < 0 || i >= Size) {std::cerr << "cannot buffer! " << i << " " << Size << std::endl; exit(-1);}
        safe.wait(); State *ptr = &state[i]; safe.post(); return ptr;}
    void advance() override {safe.wait(); idx = (idx+1)%Size; safe.post();}
    void advance(int i) override {
        if (i < 0 || i >= Size) {std::cerr << "cannot advance!" << std::endl; exit(-1);}
        safe.wait(); idx = i; safe.post();}
    Bind buftyp() override {return Type;}
    int bufsiz() override {return sizeof(State);}
};
template<class State, class Init, int Size> struct InitState {
    State state[Size];
    InitState(Init func) {for (int i = 0; i < Size; i++) state[i] = func();}
    InitState(State init) {for (int i = 0; i < Size; i++) state[i] = init;}
    State &operator[](int i) {return state[i];}
};
template<class State> struct ConstState {
    State value;
    ConstState(State value) : value(value) {}
    State operator()() {return value;}
};

enum SizeEnum {
    IntSize,
    ExtentSize,
    MicroSize,
    SwapSize,
};
struct SizeState {
    SizeEnum tag;
    int base, size;
    VkExtent2D extent;
    Micro micro;
    VkSurfaceCapabilitiesKHR capabilities;
    SizeState() {
        tag = IntSize;
        size = 0;
    }
    SizeState(int base, int size) {
        tag = IntSize;
        this->base = base;
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
        base == other.base && size == other.size) return true;
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

enum ArgEnum {
    BothArg,
    LockArg,
    SizeArg,
};
struct Arg {
    ArgEnum tag; void *ptr; int loc; int siz; SizeState max;
};
enum BaseEnum {
    InitBase, // avoid binding to uninitialized
    FillBase, // ready for update
    FreeBase, // ready for use
    BothBase, // check for both change
    SizeBase, // check for size change
    LockBase, // check for data change
    NextBase, // waiting for fence done
    BaseEnums
};
struct BindState;
struct BaseState {
    int debugCount;
    const char *debugChar;
    SafeState safe;
    BaseEnum state;
    int rlock, wlock;
    BindState *lock;
    StackState *item;
    // following indirectly protected by state/lock that are directly protected by safe
    // only one ThreadState acts on BaseState with reserved state/lock
    SizeState size, todo;
    void *ptr; int loc; int siz;
    char debug[64];
    BaseState() : safe(1), state(InitBase),
        rlock(0), wlock(0), lock(0), item(0), size(0,0), todo(0,0), debug{0} {debugChar="none";}
    BaseState(const char *name) : safe(1), state(InitBase),
        rlock(0), wlock(0), lock(0), item(0), size(0,0), todo(0,0), debug{0} {debugChar="none";
        sprintf(debug,"%s%d",name,StackState::debug++);
    }
    BaseState(const char *name, StackState *ptr) : safe(1), state(InitBase),
        rlock(0), wlock(0), lock(0), item(ptr), size(0,0), todo(0,0), debug{0} {
        sprintf(debug,"%s%s%d",item->name,name,StackState::debug++);
    }
    bool push(Arg arg, SmartState log) {
        switch (arg.tag) {default: {std::cerr << "invalid push arg!" << std::endl; exit(-1);}
        break; case (BothArg): return push(arg.ptr,arg.loc,arg.siz,arg.max,log);
        break; case (LockArg): return push(arg.ptr,arg.loc,arg.siz,log);
        break; case (SizeArg): return push(arg.max,log);}
        return false;
    }
    bool push(void *ptr, int loc, int siz, SizeState max, SmartState log) { // called in main thread
        safe.wait();
        if (state != InitBase && state != FillBase && state != FreeBase) {safe.post(); return false;}
        if (rlock || wlock) {safe.post(); return false;}
        this->ptr = ptr; this->loc = loc; this->siz = siz; todo = max;
        state = BothBase;
        safe.post();
        return true;
    }
    bool push(SizeState max, SmartState log) { // called in main thread
        safe.wait();
        if (state != InitBase && state != FillBase && state != FreeBase) {safe.post(); return false;}
        if (rlock || wlock) {safe.post(); return false;}
        todo = max;
        state = SizeBase;
        safe.post();
        return true;
    }
    bool push(void *ptr, int loc, int siz, SmartState log) { // called in main thread
        safe.wait();
        if (state != FillBase && state != FreeBase) {safe.post(); return false;}
        if (rlock || wlock) {safe.post(); return false;}
        this->ptr = ptr; this->loc = loc; this->siz = siz;
        state = LockBase;
        safe.post();
        return true;
    }
    void push(SmartState log) { // called in main thread
        safe.wait();
        if (state != BothBase && state != SizeBase && state != LockBase)
        {std::cerr << "invalid push state!" << std::endl; exit(-1);}
        state = FreeBase;
        safe.post();
    }
    VkFence sizeup(SmartState log) { // called in separate thread
        safe.wait();
        if (state != BothBase) {std::cerr << "sizeup invalid state! " << state << std::endl; exit(-1);}
        if (rlock || wlock) {std::cerr << "sizeup invalid lock!" << std::endl; exit(-1);}
        if (size == todo); else {
        if (size == SizeState(0,0)); else {safe.post(); unsize(log); safe.wait();}
        if ((size = todo) == SizeState(0,0)); else {
        safe.post(); resize(log); safe.wait();}}
        state = NextBase;
        safe.post();
        return setup(ptr,loc,siz,log);
    }
    void baseres(SmartState log) { // called in separate thread
        safe.wait();
        if (state != SizeBase) {std::cerr << "baseres invalid state! " << state << "(" << SizeBase << ")" << " " << debug << std::endl; exit(-1);}
        if (rlock || wlock) {std::cerr << "baseres invalid lock!" << std::endl; exit(-1);}
        if (size == todo); else {
        if (size == SizeState(0,0)); else {safe.post(); unsize(log); safe.wait();}
        if ((size = todo) == SizeState(0,0)); else {
        safe.post(); resize(log); safe.wait();}}
        state = NextBase; // TODO think of way to use FillBase, or remove it
        safe.post();
    }
    VkFence basesup(SmartState log) { // called in separate thread
        safe.wait();
        if (state != LockBase)
        {std::cerr << "setup invalid state! " << debug << " " << state << std::endl; exit(-1);}
        if (rlock || wlock) {std::cerr << "setup invalid lock!" << std::endl; exit(-1);}
        state = NextBase;
        safe.post();
        return setup(ptr,loc,siz,log);
    }
    void baseups(SmartState log) { // called in separate thread
        safe.wait();
        if (state != NextBase) {std::cerr << "upset invalid state!" << std::endl; exit(-1);}
        if (rlock || wlock) {std::cerr << "upset invalid lock!" << std::endl; exit(-1);}
        if (item) log << "baseups " << debug << " " << item->name << std::endl;
        else log << "baseups " << debug << std::endl;
        if (item) item->advance();
        safe.post(); upset(log); safe.wait();
        state = FreeBase;
        lock = 0;
        safe.post();
    }
    virtual void unsize(SmartState log) { // consumes time
        std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual void resize(SmartState log) { // consumes time
        std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual VkFence setup(void *ptr, int loc, int siz, SmartState log) { // consumes time
        std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual void upset(SmartState log) { // consumes time
        std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual bool check(BaseEnum state) {
        return false;
    }
    BaseEnum check() {
        safe.wait();
        BaseEnum ret = state;
        safe.post();
        return ret;
    }
    void set(BindState *ptr) {
        safe.wait();
        if (state != BothBase && state != SizeBase && state != LockBase)
        {std::cerr << "invalid set state!" << std::endl; exit(-1);}
        if (lock != 0) {std::cerr << "invalid set lock! " << debug << std::endl; exit(-1);}
        lock = ptr;
        safe.post();
    }
    virtual BindState *get() {
        if (lock == 0) {std::cerr << "lock is zero!" << std::endl; *(int*)0 = 0; exit(-1);}
        return lock;
    }
    virtual VkSemaphore getSemaphore() {std::cerr << "BaseState::getSemaphore" << std::endl; exit(-1);}
    virtual void setSemaphore(VkSemaphore s) {std::cerr << "BaseState::setSemaphore" << std::endl; exit(-1);}
    virtual VkSwapchainKHR getSwapChain() {std::cerr << "BaseState::swapChain" << std::endl; exit(-1);}
    virtual uint32_t getImageIndex() {std::cerr << "BaseState::getImageIndex" << std::endl; exit(-1);}
    virtual VkFramebuffer getFramebuffer(int i) {std::cerr << "BaseState::framebuffer" << std::endl; exit(-1);}
    virtual VkPipeline getPipeline() {std::cerr << "BaseState::pipeline" << std::endl; exit(-1);}
    virtual VkPipelineLayout getPipelineLayout() {std::cerr << "BaseState::pipelineLayout" << std::endl; exit(-1);}
    virtual VkBuffer getBuffer() {std::cerr << "BaseState::buffer" << std::endl; exit(-1);}
    virtual int getRange() {std::cerr << "BaseState::size" << std::endl; exit(-1);}
    virtual VkImageView getTextureImageView() {std::cerr << "BaseState::textureImageView" << std::endl; exit(-1);}
    virtual VkSampler getTextureSampler() {std::cerr << "BaseState::textureSampler" << std::endl; exit(-1);}
    virtual VkDescriptorPool getDescriptorPool() {std::cerr << "BaseState::getDescriptorPool" << std::endl; exit(-1);}
    virtual VkDescriptorSetLayout getDescriptorSetLayout() {std::cerr << "BaseState::getDescriptorSetLayout" << std::endl; exit(-1);}
    virtual VkExtent2D getSwapChainExtent() {std::cerr << "BaseState::getSwapChainExtent" << std::endl; exit(-1);}
    static uint32_t findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags,
        VkPhysicalDeviceMemoryProperties memProperties);
    static VkCommandBuffer createCommandBuffer(VkDevice device, VkCommandPool pool);
    static VkFence createFence(VkDevice device);
    static VkSemaphore createSemaphore(VkDevice device);
    static VkImageView createImageView(VkDevice device, VkImage image, VkFormat format, VkImageAspectFlags aspectFlags);
    static void createBuffer(VkDevice device, VkPhysicalDevice physical, VkDeviceSize size, VkBufferUsageFlags usage,
        VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties,
        VkBuffer& buffer, VkDeviceMemory& bufferMemory);
    static void createImage(VkDevice device, VkPhysicalDevice physical,
        uint32_t width, uint32_t height, VkFormat format, VkImageTiling tiling, VkImageUsageFlags usage,
        VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties,
        VkImage& image, VkDeviceMemory& imageMemory);
};

struct BindState : public BaseState {
    BaseState *bind[Binds];
    int rsav[Binds];
    int wsav[Binds];
    int lock; bool excl;
    BindState() : BaseState("BindState"), lock(0), excl(false) {
        std::cout << "BindState " << debug << std::endl;
        for (int i = 0; i < Binds; i++) {bind[i] = 0; rsav[i] = wsav[i] = 0;}}
    ~BindState()
        {std::cout << "~BindState " << debug << std::endl;}
    BindState *get() override {
        safe.wait();
        if (excl) {safe.post(); return 0;}
        excl = true;
        safe.post();
        return this;
    }
    void set() {
        safe.wait();
        if (lock != 0) {std::cerr << "invalid bind set!" << std::endl; exit(-1);}
        excl = false;
        safe.post();
    }
    BaseState *get(Bind i) { // called by pushee
        safe.wait();
        if (bind[i] == 0) {std::cerr << "invalid get bind!" << std::endl; exit(-1);}
        BaseState *ptr = bind[i];
        safe.post();
        return ptr;
    }
    bool rinc(Bind i, BaseState *buf, SmartState log) {
        safe.wait();
        if (!excl) {std::cerr << "invalid excl rinc!" << std::endl; exit(-1);}
        buf->safe.wait();
        if (buf->state != FreeBase && buf->state != FillBase) {
        log << "rinc state fail " << buf->state << std::endl;
        buf->safe.post(); if (lock == 0) excl = false; safe.post(); return false;}
        if (buf->state == FillBase && !buf->check(buf->state)) {
        log << "rinc check fail" << std::endl;
        buf->safe.post(); if (lock == 0) excl = false; safe.post(); return false;}
        if (buf->wlock) {
        log << "rinc wlock fail" << std::endl;
        buf->safe.post(); if (lock == 0) excl = false; safe.post(); return false;}
        log << "rinc " << debug << " " << buf->debug << " " << buf->rlock << std::endl;
        if (bind[i] == 0) lock += 1;
        bind[i] = buf; buf->rlock += 1; rsav[i] += 1;
        buf->safe.post(); safe.post(); return true;
    }
    bool winc(Bind i, BaseState *buf, SmartState log) {
        safe.wait();
        if (!excl) {std::cerr << "invalid excl winc!" << std::endl; exit(-1);}
        buf->safe.wait();
        if (buf->state != FreeBase && buf->state != FillBase) {
        buf->safe.post(); if (lock == 0) excl = false; safe.post(); return false;}
        if (buf->state == FillBase && !buf->check(buf->state)) {
        buf->safe.post(); if (lock == 0) excl = false; safe.post(); return false;}
        if (buf->rlock || buf->wlock) {
        buf->safe.post(); if (lock == 0) excl = false; safe.post(); return false;}
        log << "winc " << buf->debug << buf->wlock << std::endl;
        if (bind[i] == 0) lock += 1;
        bind[i] = buf; buf->wlock += 1; wsav[i] += 1;
        buf->safe.post(); safe.post(); return true;
    }
    void rdec(Bind i, SmartState log) {
        safe.wait();
        if (!excl) {std::cerr << "invalid excl rdec!" << std::endl; exit(-1);}
        if (lock <= 0 || bind[i] == 0) {std::cerr << "rdec lock bind! " << i << std::endl; exit(-1);}
        bind[i]->safe.wait();
        if (bind[i]->rlock <= 0) {log << "invalid rdec rlock! " << debug << " " << bind[i]->debug << std::endl; slog.clr(); exit(-1);}
        bind[i]->rlock -= 1;
        log << "rdec " << debug << " " << bind[i]->debug << " " << bind[i]->rlock << std::endl;
        bind[i]->safe.post();
        if (rsav[i] <= 0) {std::cerr << "invalid rdec sav!" << std::endl; exit(-1);}
        rsav[i] -= 1;
        if (rsav[i] == 0 && wsav[i] == 0) {bind[i] = 0; lock -= 1;}
        if (lock == 0) excl = false;
        safe.post();
    }
    void wdec(Bind i, SmartState log) {
        safe.wait();
        if (!excl) {std::cerr << "invalid excl wdec!" << std::endl; exit(-1);}
        if (lock <= 0 || bind[i] == 0) {std::cerr << "wdec lock bind!" << std::endl; exit(-1);}
        bind[i]->safe.wait();
        if (bind[i]->wlock <= 0) {std::cerr << "invalid wdec wlock!" << std::endl; exit(-1);}
        bind[i]->wlock -= 1;
        bind[i]->safe.post();
        if (wsav[i] <= 0) {std::cerr << "invalid wdec sav!" << std::endl; exit(-1);}
        wsav[i] -= 1;
        if (rsav[i] == 0 && wsav[i] == 0) {bind[i] = 0; lock -= 1;}
        if (lock == 0) excl = false;
        safe.post();
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
    std::vector<VkFramebuffer> framebuffers;
    SwapState() :
        BaseState("SwapState"),
        window(StackState::window),
        surface(StackState::surface),
        physical(StackState::physical),
        device(StackState::device),
        surfaceFormat(StackState::surfaceFormat),
        presentMode(StackState::presentMode),
        graphicsFamily(StackState::graphicsFamily),
        presentFamily(StackState::presentFamily),
        imageFormat(StackState::imageFormat),
        depthFormat(StackState::depthFormat),
        renderPass(StackState::renderPass),
        memProperties(StackState::memProperties)
        {std::cout << "SwapState " << debug << std::endl;}
    ~SwapState() {SmartState log; push(SizeState(0,0),log); baseres(log); std::cout << "~SwapState " << debug << std::endl;}
    VkSwapchainKHR getSwapChain() override {return swapChain;}
    VkFramebuffer getFramebuffer(int i) override {return framebuffers[i];}
    VkExtent2D getSwapChainExtent() override {return size.capabilities.currentExtent;}
    void resize(SmartState log) override {
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
        createFramebuffers(device,getSwapChainExtent(),renderPass,swapChainImageViews,depthImageView,framebuffers);
    }
    void unsize(SmartState log) override {
        int width = 0, height = 0;
        glfwGetFramebufferSize(window, &width, &height);
        while (width == 0 || height == 0) {
            glfwGetFramebufferSize(window, &width, &height);
            glfwWaitEvents();}
        vkDeviceWaitIdle(device);
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthImageMemory, nullptr);
        for (auto framebuffer : framebuffers)
            vkDestroyFramebuffer(device, framebuffer, nullptr);
        for (auto imageView : swapChainImageViews)
            vkDestroyImageView(device, imageView, nullptr);
        vkDestroySwapchainKHR(device, swapChain, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
    }
    bool check(BaseEnum state) override {
        return (state == FillBase);
    }
    static VkSwapchainKHR createSwapChain(VkSurfaceKHR surface, VkDevice device, VkExtent2D swapChainExtent,
        VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode,
        VkSurfaceCapabilitiesKHR capabilities, uint32_t graphicsFamily, uint32_t presentFamily);
    static void createSwapChainImages(VkDevice device, VkSwapchainKHR swapChain, std::vector<VkImage> &swapChainImages);
    static void createFramebuffers(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
        std::vector<VkImageView> swapChainImageViews, VkImageView depthImageView,
        std::vector<VkFramebuffer> &framebuffers);
};

struct PipeState : public BaseState {
    const VkDevice device;
    Micro micro;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorSetLayout;
    VkPipelineLayout pipelineLayout;
    VkPipeline pipeline;
    const char *name() {return "PipeState";}
    PipeState() :
        BaseState("PipeState"),
        device(StackState::device), micro((Micro)StackState::micro++),
        descriptorPool(createDescriptorPool(StackState::device,StackState::frames)),
        descriptorSetLayout(createDescriptorSetLayout(StackState::device,micro)),
        pipelineLayout(createPipelineLayout(StackState::device,descriptorSetLayout)),
        pipeline(createGraphicsPipeline(StackState::device,StackState::renderPass,pipelineLayout,micro))
        {std::cout << "PipeState " << debug << std::endl;}
    ~PipeState() {std::cout << "~PipeState " << debug << std::endl;
        vkDestroyPipeline(device, pipeline, nullptr);
        vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
        vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);
        vkDestroyDescriptorPool(device, descriptorPool, nullptr);
    }
    VkPipeline getPipeline() override {return pipeline;}
    VkPipelineLayout getPipelineLayout() override {return pipelineLayout;}
    VkDescriptorPool getDescriptorPool() override {return descriptorPool;}
    VkDescriptorSetLayout getDescriptorSetLayout() override {return descriptorSetLayout;}
    void resize(SmartState log) override {
    }
    void unsize(SmartState log) override {
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(SmartState log) override {
    }
    bool check(BaseEnum) override {
        return (state == FillBase);
    }
    static VkDescriptorPool createDescriptorPool(VkDevice device, int frames);
    static VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, Micro micro);
    static VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout);
    static std::vector<char> readFile(const std::string& filename);
    static VkPipeline createGraphicsPipeline(VkDevice device, VkRenderPass renderPass,
        VkPipelineLayout pipelineLayout, Micro micro);
    static VkShaderModule createShaderModule(VkDevice device, const std::vector<char>& code);
};

struct UniformState : public BaseState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceMemoryProperties memProperties;
    VkBuffer buffer;
    VkDeviceMemory memory;
    void* mapped;
    UniformState() :
        BaseState("UniformState",StackState::self),
        device(StackState::device), physical(StackState::physical),
        memProperties(StackState::memProperties)
        {std::cout << "UniformState " << debug << std::endl;}
    ~UniformState() {SmartState log; push(SizeState(0,0),log); baseres(log); std::cout << "~UniformState " << debug << std::endl;}
    VkBuffer getBuffer() override {return buffer;}
    int getRange() override {return size.size;}
    void resize(SmartState log) override {
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, buffer, memory);
        vkMapMemory(device, memory, 0, bufferSize, 0, &mapped);
    }
    void unsize(SmartState log) override {
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        loc = loc - size.base;
        if (loc < 0 || siz < 0 || loc+siz > size.size)
        {std::cerr << "invalid uniform size!" << std::endl; exit(-1);}
        log << "memcpy " << debug << " " << ptr << " " << loc << " " << siz << std::endl;
        memcpy((void*)((char*)mapped+loc), ptr, siz);
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
    }
};

struct BufferState : public BaseState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const VkPhysicalDeviceMemoryProperties memProperties;
    const VkBufferUsageFlags flags;
    VkBuffer buffer;
    VkDeviceMemory bufferMemory;
    VkCommandBuffer commandBuffer;
    VkFence fence; VkSemaphore before, after; bool atomic;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    BufferState() :
        BaseState("BufferState",StackState::self),
        device(StackState::device), physical(StackState::physical),
        graphics(StackState::graphics), commandPool(StackState::commandPool),
        memProperties(StackState::memProperties), flags(StackState::flags),
        before(VK_NULL_HANDLE), atomic(false)
        {std::cout << "BufferState " << debug << std::endl;}
    ~BufferState() {SmartState log; push(SizeState(0,0),log); baseres(log); std::cout << "~BufferState " << debug << std::endl;}
    VkBuffer getBuffer() override {return buffer;}
    int getRange() override {return size.size;}
    VkSemaphore getSemaphore() override {atomic = true; return after;}
    void setSemaphore(VkSemaphore sem) {before = sem;}
    void resize(SmartState log) override {
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | flags,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, buffer, bufferMemory);
        commandBuffer = createCommandBuffer(device,commandPool);
        fence = createFence(device);
        after = createSemaphore(device);
    }
    void unsize(SmartState log) override {
        vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
        vkDestroySemaphore(device, after, nullptr);
        vkDestroyFence(device, fence, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeMemory(device, bufferMemory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        loc = loc - size.base;
        if (loc < 0 || siz < 0 || loc+siz > size.size)
        {std::cerr << "invalid buffer size! " << debug << " " << loc << " " << siz << " " << size.size << std::endl; exit(-1);}
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, stagingBuffer, stagingBufferMemory);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, &data);
        memcpy((void*)((char*)data+loc),ptr,siz);
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetFences(device, 1, &fence);
        copyBuffer(device, graphics, stagingBuffer, buffer, bufferSize, commandBuffer, fence,
        before, (atomic?after:VK_NULL_HANDLE)); before = VK_NULL_HANDLE;
        if (atomic) {atomic = false; return VK_NULL_HANDLE;}
        return fence;
    }
    void upset(SmartState log) override {
        vkUnmapMemory(device, stagingBufferMemory);
        vkFreeMemory(device, stagingBufferMemory, nullptr);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
    }
    static void copyBuffer(VkDevice device, VkQueue graphics, VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size,
        VkCommandBuffer commandBuffer, VkFence fence, VkSemaphore before, VkSemaphore after);
};

struct TextureState : public BaseState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceProperties properties;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const VkPhysicalDeviceMemoryProperties memProperties;
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
        BaseState("TextureState",StackState::self),
        device(StackState::device), physical(StackState::physical),
        properties(StackState::properties), graphics(StackState::graphics), commandPool(StackState::commandPool),
        memProperties(StackState::memProperties)
        {std::cout << "TextureState " << debug << std::endl;}
    ~TextureState() {SmartState log; push(SizeState(0,0),log); baseres(log); std::cout << "~TextureState " << debug << std::endl;}
    VkImageView getTextureImageView() override {return textureImageView;}
    VkSampler getTextureSampler() override {return textureSampler;}
    void resize(SmartState log) override {
        log << "resize " << debug << std::endl;
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
    void unsize(SmartState log) override {
        log << "unsize " << debug << std::endl;
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
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
        if (loc != 0) {std::cerr << "unsupported texture loc!" << std::endl; exit(-1);}
        int texWidth = size.extent.width;
        int texHeight = size.extent.height;
        VkDeviceSize imageSize = texWidth * texHeight * 4;
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
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
        vkUnmapMemory(device, stagingBufferMemory);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
        vkFreeMemory(device, stagingBufferMemory, nullptr);
    }
    static VkSampler createTextureSampler(VkDevice device, VkPhysicalDeviceProperties properties);
    static void transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image,
        VkSemaphore semaphoreIn, VkSemaphore semaphoreOut, VkFence fenceOut,
        VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout);
    static void copyTextureImage(VkDevice device, VkQueue graphics,
        VkPhysicalDeviceMemoryProperties memProperties, VkImage textureImage, int texWidth, int texHeight,
        VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore, VkFence fence, VkBuffer stagingBuffer,
        VkCommandBuffer beforeBuffer, VkCommandBuffer commandBuffer, VkCommandBuffer afterBuffer);
};

struct ResultState : public BaseState {
    ResultState() :
        BaseState("ResultState")
        {std::cout << "ResultState " << debug << std::endl;}
    ~ResultState() {std::cout << "~ResultState " << debug << std::endl;}
    void unsize(SmartState log) override {
    }
    void resize(SmartState log) override {
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        // TODO copy buffer to mapped buffer
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        // TODO copy from maped buffer to pointer from setup
    }
};

struct AcquireState : public BaseState {
    const VkDevice device;
    ChangeState<Configure,Configures> *copy;
    bool atomic; VkSemaphore after;
    uint32_t imageIndex;
    AcquireState() :
        BaseState("AcquireState"),
        device(StackState::device),
        copy(StackState::copy),
        atomic(false) {
        after = createSemaphore(device);
        std::cout << "AcquireState " << debug << std::endl;}
    ~AcquireState() {
        vkDestroySemaphore(device, after, nullptr);
        std::cout << "~AcquireState " << debug << std::endl;}
    VkSemaphore getSemaphore() override {atomic = true; return after;}
    uint32_t getImageIndex() override {return imageIndex;}
    void resize(SmartState log) override {std::cerr << "invalid acquire resize!" << std::endl; exit(-1);}
    void unsize(SmartState log) override {std::cerr << "invalid acquire unsize!" << std::endl; exit(-1);}
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        BindState *bind = get();
        VkExtent2D frameExtent = bind->get(SwapBnd)->getSwapChainExtent();
        VkResult result = vkAcquireNextImageKHR(device,
        bind->get(SwapBnd)->getSwapChain(), UINT64_MAX, (atomic?after:VK_NULL_HANDLE), VK_NULL_HANDLE, &imageIndex);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) copy->wots(RegisterMask,1<<ResizeAsync);
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
        {std::cerr << "failed to acquire swap chain image!" << std::endl; exit(-1);}
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        get()->rdec(SwapBnd,log);
        atomic = false;
    }
};

struct PresentState : public BaseState {
    const VkQueue present;
    ChangeState<Configure,Configures> *copy;
    PresentState() :
        BaseState("PresentState"),
        present(StackState::present),
        copy(StackState::copy)
        {std::cout << "PresentState " << debug << std::endl;}
    ~PresentState() {SmartState log("present"); push(SizeState(0,0),log); baseres(log); log.clr();
        std::cout << "~PresentState " << debug << std::endl;}
    void resize(SmartState log) override {std::cerr << "invalid present resize!" << std::endl; exit(-1);}
    void unsize(SmartState log) override {std::cerr << "invalid present unsize!" << std::endl; exit(-1);}
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        BindState *bind = get();
        if (!presentFrame(present,bind->get(SwapBnd)->getSwapChain(),
        bind->get(AcquireBnd)->getImageIndex(),bind->get(DrawBnd)->getSemaphore()))
        copy->wots(RegisterMask,1<<ResizeAsync);
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        get()->rdec(SwapBnd,log);
        get()->rdec(AcquireBnd,log);
        get()->rdec(DrawBnd,log);
    }
    static bool presentFrame(VkQueue present, VkSwapchainKHR swapChain, uint32_t imageIndex, VkSemaphore before);
};

struct DrawState : public BaseState {
    const VkDevice device;
    const VkRenderPass renderPass;
    const VkQueue graphics;
    const VkQueue present;
    const VkCommandPool commandPool;
    const int frames;
    ChangeState<Configure,Configures> *copy;
    VkSemaphore acquire;
    VkSemaphore before;
    VkSemaphore after;
    VkFence fence;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorLayout;
    VkDescriptorSet descriptorSet;
    VkCommandBuffer commandBuffer;
    InitState<BaseState *, ConstState<BaseState *>, Binds> bufptr;
    InitState<int, ConstState<int>, Binds> bufidx; int bufsiz;
    BaseEnum type;
    DrawState() :
        BaseState("DrawState"),
        device(StackState::device),
        renderPass(StackState::renderPass),
        graphics(StackState::graphics),
        present(StackState::present),
        commandPool(StackState::commandPool),
        frames(StackState::frames),
        copy(StackState::copy),
        before(VK_NULL_HANDLE),
        bufptr(ConstState<BaseState *>((BaseState*)0)),
        bufidx(ConstState<int>(0)), bufsiz(0), type(FreeBase)
        {std::cout << "DrawState " << debug << std::endl;}
    ~DrawState() {SmartState log; push(SizeState(0,0),log); baseres(log);
        std::cout << "~DrawState " << debug << std::endl;}
    VkSemaphore getSemaphore() override {return after;}
    void setSemaphore(VkSemaphore sem) {before = sem;}
    BaseState *get(Bind typ) {
        return BaseState::get()->get(typ);
    }
    void resize(SmartState log) override {
        type = check();
        descriptorPool = /*get()->*/get(PipelineBnd)->getDescriptorPool();
        descriptorLayout = /*get()->*/get(PipelineBnd)->getDescriptorSetLayout();
        descriptorSet = createDescriptorSet(device,descriptorPool,descriptorLayout,frames);
        commandBuffer = createCommandBuffer(device,commandPool);
        acquire = createSemaphore(device);
        after = createSemaphore(device);
        fence = createFence(device);
    }
    void unsize(SmartState log) override {
        vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
        vkDestroySemaphore(device, acquire, nullptr);
        vkDestroySemaphore(device, after, nullptr);
        vkDestroyFence(device, fence, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeDescriptorSets(device,descriptorPool,1,&descriptorSet);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        type = check();
        if (ptr != 0 || loc != 0) {std::cerr << "unsupported draw loc!" << std::endl; exit(-1);}
        if (size == SizeState(MicroTest) && siz > 0) {
            VkExtent2D swapChainExtent = get(SwapBnd)->getSwapChainExtent();
            uint32_t imageIndex;
            // TODO depending on size.micro, disable acquire
            VkResult result = vkAcquireNextImageKHR(device, get(SwapBnd)->getSwapChain(),
                UINT64_MAX, acquire, VK_NULL_HANDLE, &imageIndex);
            if (result == VK_ERROR_OUT_OF_DATE_KHR) copy->wots(RegisterMask,1<<ResizeAsync);
            else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            {std::cerr << "failed to acquire swap chain image!" << std::endl; exit(-1);}
            vkResetFences(device, 1, &fence);
            vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
            BaseState *swapPtr = 0;
            BaseState *pipelinePtr = 0;
            BaseState *fetchPtr = 0;
            BaseState *indexPtr = 0;
            for (int i = 0; true; i++) {
            Bind bnd = MicroBind__Micro__BindLoc__Int__Bind(size.micro)(MiddleLoc)(i);
            BindTyp typ = BindType__Bind__BindTyp(bnd);
            if (bnd == Binds) break;
            switch (typ) {
            default: {std::cerr << "invalid bind type!" << std::endl; exit(-1);}
            break; case (SwapTyp): swapPtr = get(bnd);
            break; case (PipelineTyp): pipelinePtr = get(bnd);
            break; case (FetchTyp): fetchPtr = get(bnd);
            break; case (IndexTyp): indexPtr = get(bnd);
            break; case (UniformTyp):
                if (get(bnd)->getBuffer() == VK_NULL_HANDLE) {std::cerr << "invalid uniform buffer! " << get(bnd)->debug << std::endl; exit(-1);}
                updateUniformDescriptor(device,get(bnd)->getBuffer(),i,get(bnd)->getRange(),descriptorSet);
            break; case (TextureTyp):
                if (get(bnd)->getTextureImageView() == VK_NULL_HANDLE) {std::cerr << "invalid texture view! " << get(bnd)->debug << std::endl; exit(-1);}
                if (get(bnd)->getTextureSampler() == VK_NULL_HANDLE) {std::cerr << "invalid texture sampler!" << std::endl; exit(-1);}
                updateTextureDescriptor(device,get(bnd)->getTextureImageView(),get(bnd)->getTextureSampler(),i,descriptorSet);}}
            if (swapPtr && pipelinePtr && fetchPtr && indexPtr) {
                recordCommandBuffer(commandBuffer,renderPass,descriptorSet,swapChainExtent,size.micro,siz,
                swapPtr->getFramebuffer(imageIndex),pipelinePtr->getPipeline(),pipelinePtr->getPipelineLayout(),
                fetchPtr->getBuffer(),indexPtr->getBuffer());
            if (!drawFrame(commandBuffer,graphics,present,swapPtr->getSwapChain(),imageIndex,ptr,loc,siz,size.micro,acquire,after,fence,before))
                copy->wots(RegisterMask,1<<ResizeAsync); before = VK_NULL_HANDLE;}
            else {std::cerr << "invalid bind set! " << swapPtr << " " << pipelinePtr << " " << fetchPtr << " " << indexPtr << std::endl; exit(-1);}
            return fence;
        }
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        if (type == SizeBase) for (int i = 0; true; i++) {
        Bind bnd = MicroBind__Micro__BindLoc__Int__Bind(size.micro)(ResizeLoc)(i);
        if (bnd == Binds) break; log << "ResizeLoc" << std::endl; BaseState::get()->rdec(bnd,log);}
        else for (int i = 0; true; i++) {
        Bind bnd = MicroBind__Micro__BindLoc__Int__Bind(size.micro)(MiddleLoc)(i);
        if (bnd == Binds) break; log << "MiddleLoc " << i << " " << bnd << std::endl; BaseState::get()->rdec(bnd,log);}
    }
    static VkDescriptorSet createDescriptorSet(VkDevice device, VkDescriptorPool descriptorPool,
        VkDescriptorSetLayout descriptorSetLayout, int frames);
    static void updateStorageDescriptor(VkDevice device, VkBuffer buffer,
        int index, int size, VkDescriptorSet descriptorSet);
    static void updateUniformDescriptor(VkDevice device, VkBuffer buffer,
        int index, int size, VkDescriptorSet descriptorSet);
    static void updateTextureDescriptor(VkDevice device,
        VkImageView textureImageView, VkSampler textureSampler,
        int index, VkDescriptorSet descriptorSet);
    static void recordCommandBuffer(VkCommandBuffer commandBuffer, VkRenderPass renderPass,
        VkDescriptorSet descriptorSet, VkExtent2D renderArea, Micro micro, uint32_t indices,
        VkFramebuffer framebuffer, VkPipeline graphicsPipeline, VkPipelineLayout pipelineLayout,
        VkBuffer vertexBuffer, VkBuffer indexBuffer);
    static bool drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, VkQueue present,
        VkSwapchainKHR swapChain, uint32_t imageIndex, void *ptr, int loc, int siz, Micro micro,
        VkSemaphore acquire, VkSemaphore after, VkFence fence, VkSemaphore before);
};

struct PushState {
    BaseState *base;
    VkFence fence;
    Center *ptr;
    int sub;
    void (*fnc)(Center*,int);
    SmartState log;
};
struct ThreadState : public DoneState {
    const VkDevice device;
    ChangeState<Configure,Configures> *copy;
    SafeState safe; SafeState wake;
    std::deque<PushState> before;
    std::deque<PushState> after;
    bool goon;
    ThreadState(VkDevice device, ChangeState<Configure,Configures> *copy) :
        device(device), copy(copy),
        safe(1), wake(0), goon(true) {
        strcpy(debug,"ThreadState"); std::cout << debug << std::endl;
    }
    ~ThreadState() {std::cout << "~ThreadState" << std::endl;}
    void push(BaseState *base, Center *ptr, int sub, void (*fnc)(Center*,int), SmartState log) {
        PushState push = {base,VK_NULL_HANDLE,ptr,sub,fnc,log};
        safe.wait();
        before.push_back(push);
        safe.post();
        wake.wake();
    }
    bool stage() {
        while (1) {while (1) {
        safe.wait();
        if (before.empty()) {safe.post(); break;}
        PushState push = before.front(); before.pop_front();
        safe.post();
        if (push.base) switch (push.base->check()) {
        default: {std::cerr << "stage push tag! " << push.base->debug << std::endl; exit(-1);}
        break; case(SizeBase): push.fence = VK_NULL_HANDLE; push.base->baseres(push.log);
        break; case(LockBase): push.fence = push.base->basesup(push.log);
        break; case(BothBase): push.fence = push.base->sizeup(push.log);}
        after.push_back(push);}
        if (!after.empty()) break;
        safe.wait();
        bool empty = before.empty();
        bool done = (empty && !goon);
        safe.post();
        if (done) return false;
        if (empty) wake.wait();}
        return true;
    }
    void call() override {
        while (stage()) {
        if (after.empty()) {std::cerr << "separate empty after!" << std::endl; exit(-1);}
        PushState push = after.front(); after.pop_front();
        if (push.fence != VK_NULL_HANDLE) {
        VkResult result = vkWaitForFences(device,1,&push.fence,VK_FALSE,NANOSECONDS);
        if (result != VK_SUCCESS) {std::cerr << "cannot wait for fence!" << std::endl; exit(-1);}}
        if (push.fnc) {push.fnc(push.ptr,push.sub);}
        copy->wots(RegisterMask,1<<FenceAsync);
        if (push.base) push.base->baseups(push.log);}
        vkDeviceWaitIdle(device);
    }
    void done() override {
        safe.wait();
        goon = false;
        safe.post();
        wake.wake();
    }
    void heap() override {}
};

extern "C" {
int datxVoids(void *dat);
void *datxVoidz(int num, void *dat);
};
struct EnumState {
    Bind key;
    StackState *val;
};
enum ReqEnum {
    DerReq, // push arg to current, set bind, push to thread, and advance
    PDerReq, // push arg to next, set bind, and push to thread
    IDerReq, // push arg to indexed, set bind, push to thread, and advance to index
    RDeeReq, // increment read lock in current, and add to bind
    IRDeeReq, // increment read lock in indexed, and add to bind
    WDeeReq, // increment write lock in current, and add to bind
    PNowReq, // call function now if pass
    FNowReq, // call function now if fail
    PEnqReq, // call function when free if pass
    FEnqReq, // call function when free if fail
    GoonReq, // retry if fail
    NoopReq, // do nothing
};
struct Req {
    ReqEnum tag; Bind bnd; int idx; Arg arg; Center *ptr; int sub; void (*fnc)(Center*,int);
};
struct CopyState : public ChangeState<Configure,Configures> {
    ThreadState *thread; StackState *stack[Binds];
    CopyState(ThreadState *thread, EnumState *stack) :
        thread(thread), stack{0} {
        for (EnumState *i = stack; i->key != Binds; i++) this->stack[i->key] = i->val;
        std::cout << "CopyState" << std::endl;}
    ~CopyState() {std::cout << "~CopyState" << std::endl;}
    void push(Req *req, int num, SmartState log) {
        bool goon = true; while (goon) {goon = false;
        BaseState *buf[num] = {}; bool need = false; BindState *bind = 0;
        for (int i = 0; i < num; i++) switch (req[i].tag) {default:
            break; case(DerReq): buf[i] = stack[req[i].bnd]->buffer();
            break; case(PDerReq): buf[i] = stack[req[i].bnd]->prebuf();
            break; case(IDerReq): buf[i] = stack[req[i].bnd]->prebuf(req[i].idx);
            break; case(RDeeReq): case(IRDeeReq): case(WDeeReq): buf[i] = stack[req[i].bnd]->buffer(); need = true;}
        if (need) bind = stack[BindBnd]->buffer()->get();
        int lim = num; if (need && bind == 0) lim = 0;
        for (int i = 0; i < num && i < lim; i++) switch (req[i].tag) {default:
            break; case(DerReq): case(PDerReq): case(IDerReq):
            if (!buf[i]->push(req[i].arg,log)) lim = i;
            break; case(RDeeReq): if (!bind->rinc(req[i].bnd,buf[i],log)) lim = i;
            break; case(IRDeeReq): if (!bind->rinc(req[i].bnd,buf[i],log)) lim = i;
            break; case(WDeeReq): if (!bind->winc(req[i].bnd,buf[i],log)) lim = i;}
        if (lim < num) for (int i = 0; i < lim; i++) switch (req[i].tag) {default:
            break; case(DerReq): case(PDerReq): case(IDerReq): buf[i]->push(log);
            break; case(RDeeReq): case(IRDeeReq): bind->rdec(req[i].bnd,log);
            break; case(WDeeReq): bind->wdec(req[i].bnd,log);
            break; case(FNowReq): req[i].fnc(req[i].ptr,req[i].sub);
            break; case(FEnqReq): thread->push(0,req[i].ptr,req[i].sub,req[i].fnc,log);
            break; case(GoonReq): goon = true;}
        Center *ptr = 0; int sub = 0; void (*fnc)(Center*,int) = 0; BaseState *last = 0;
        if (lim == num) for (int i = 0; i < num; i++) switch (req[i].tag) {default:
            break; case(DerReq): stack[req[i].bnd]->advance();
            if (last) buf[i]->setSemaphore(last->getSemaphore()); last = buf[i];
            if (bind) buf[i]->set(bind); thread->push(buf[i],ptr,sub,fnc,log); ptr = 0; sub = 0; fnc = 0;
            break; case(PDerReq): 
            if (last) buf[i]->setSemaphore(last->getSemaphore()); last = buf[i];
            if (bind) buf[i]->set(bind); thread->push(buf[i],ptr,sub,fnc,log); ptr = 0; sub = 0; fnc = 0;
            break; case(IDerReq): stack[req[i].bnd]->advance(req[i].idx);
            if (last) buf[i]->setSemaphore(last->getSemaphore()); last = buf[i];
            if (bind) buf[i]->set(bind); thread->push(buf[i],ptr,sub,fnc,log); ptr = 0; sub = 0; fnc = 0;
            break; case(PNowReq): req[i].fnc(req[i].ptr,req[i].sub);
            break; case(PEnqReq): ptr = req[i].ptr; sub = req[i].sub; fnc = req[i].fnc;}
        if (lim == num && bind) stack[BindBnd]->advance();}
    }
    void push(Req req, SmartState log) {
        push(&req,1,log);
    }
    void push(HeapState<Req> req, SmartState log) {
        push(req.data(), req.size(), log);
    }
    void push(Micro mic, BindLoc *loc, int num, int idx, int siz, Center *ptr, int sub,
        void (*pass)(Center*,int), void (*fail)(Center*,int), bool goon, SmartState log) {
        HeapState<Req> req;
        if (pass) req<<Req{PNowReq,Binds,0,{},ptr,sub,pass};
        if (fail) req<<Req{FNowReq,Binds,0,{},ptr,sub,fail};
        if (goon) req<<Req{GoonReq,Binds};
        for (int j = 0; j < num; j++) {
        for (int i = 0; true; i++) {
        Bind bnd = MicroBind__Micro__BindLoc__Int__Bind(mic)(loc[j])(i);
        BindTyp typ = BindType__Bind__BindTyp(bnd);
        if (bnd == Binds) break;
        if (typ == PipelineTyp) req<<Req{IRDeeReq,bnd,mic}; else req<<Req{RDeeReq,bnd};}
        req<<Req{DerReq,DrawBnd,0,{(loc[j] == ResizeLoc ? SizeArg : BothArg),0,idx,siz,SizeState(mic)}};}
        push(req,log);
    }
    void push(Center *center, int sub, void (*pass)(Center*,int), void (*fail)(Center*,int), SmartState log) {
        Bind bnd = MemoryBind__Memory__Bind(center->mem);
        if (bnd == Binds) {std::cerr << "cannot map memory!" << std::endl; exit(-1);}
        int mod = stack[bnd]->bufsiz();
        int idx = center->idx*mod; int siz = center->siz*mod; SizeState max(0,center->siz*mod);
        void *ptr = 0; switch (center->mem) {
        default: {std::cerr << "cannot copy center!" << std::endl; exit(-1);}
        break; case (Indexz): ptr = (void*)center->ind;
        break; case (Bringupz): ptr = (void*)center->ver;
        break; case (Texturez):
        ptr = datxVoidz(0,center->tex[0].dat); mod = datxVoids(center->tex[0].dat);
        idx = center->idx*mod; siz = center->siz*mod;
        max = SizeState(VkExtent2D{(uint32_t)center->tex[0].wid,(uint32_t)center->tex[0].hei});
        break; case (Uniformz): ptr = (void*)center->uni;
        break; case (Matrixz): ptr = (void*)center->mat;
        break; case (Trianglez): ptr = (void*)center->tri;
        break; case (Numericz): ptr = (void*)center->num;
        break; case (Vertexz): ptr = (void*)center->vtx;
        break; case (Basisz): ptr = (void*)center->bas;
        break; case (Piercez): ptr = (void*)center->pie;
        break; case (Drawz): for (int i = 0; i < center->siz; i++)
        push(center->drw[i].mic,center->drw[i].loc,center->drw[i].num,
        center->drw[i].idx,center->drw[i].siz,
        center,sub,planePass,planeFail,false,log);
        break; case (Configurez): // TODO alias Uniform* Configure to Uniformz fields
        for (int i = 0; i < center->siz; i++)
        write(center->cfg[i],center->val[i]);
        if (pass) thread->push(0,center,0,pass,log);
        return;}
        /*if (base>idx) {
        ptr = (void*)((char*)ptr+base-idx);
        siz -= base-idx; idx = 0;}
        else {idx = idx-base;}
        if (idx+siz>size) {siz = size-idx;}*/ // TODO
        Arg arg = {BothArg,ptr,idx,siz,max};
        HeapState<Req> req;
        if (pass) req<<Req{PEnqReq,Binds,0,{},center,sub,pass};
        if (fail) req<<Req{FEnqReq,Binds,0,{},center,sub,fail};
        req<<Req{PDerReq,bnd,0,arg};
        push(req,log);
    }
};

extern "C" {
float *planeWindow(float *mat);
float *matrc(float *u, int r, int c, int n);
}
struct TestState : public DoneState {
    SafeState safe, wake; bool goon; CopyState *copy; SizeState *size;
    TestState(CopyState *copy, SizeState *size) :
        safe(1), wake(0), goon(true), copy(copy), size(size) {
        strcpy(debug,"TestState"); std::cout << debug << std::endl;
    }
    ~TestState() {std::cout << "~TestState" << std::endl;}
    void call() override;
    void done() override {
        safe.wait();
        goon = false;
        safe.post();
        wake.wake();
    }
    void heap() override {}
    static void testUpdate(VkExtent2D swapChainExtent, glm::mat4 &model, glm::mat4 &view, glm::mat4 &proj, glm::mat4 &debug);
};
void vulkanWake(Center *ptr, int sub);
void vulkanWait(Center *ptr, int sub);
void vulkanPass(Center *ptr, int sub);
void vulkanForce(Center *ptr, int sub);
void TestState::call() {
    slog.onof(0,10000,123,5);
    const std::vector<Vertex> vertices = {
        {{-0.5f, -0.5f, 0.0f, 0.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, -0.5f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, 0.5f, 0.0f, 0.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{-0.5f, 0.5f, 0.0f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        //
        {{-0.5f, -0.5f, -0.5f, 0.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, -0.5f, -0.5f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, 0.5f, -0.5f, 0.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{-0.5f, 0.5f, -0.5f, 0.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
    };
    const std::vector<uint16_t> indices = {
        0, 1, 2, 2, 3, 0,
        4, 5, 6, 6, 7, 4,
    };
    //
    int xsiz = 800; int ysiz = 600;
    copy->write(WindowLeft,-xsiz/2); copy->write(WindowBase,-ysiz/2);
    copy->write(WindowWidth,xsiz); copy->write(WindowHeight,ysiz);
    copy->write(FocalLength,10); copy->write(FocalDepth,10);
    //
    copy->push(HeapState<Req>()<<
    Req{FNowReq,Binds,0,{},0,0,vulkanForce}<<
    Req{PDerReq,SwapBnd,0,{SizeArg,0,0,0,*size}},SmartState());
    //
    copy->push(HeapState<Req>()<<
    Req{FNowReq,Binds,0,{},0,0,vulkanForce}<<
    Req{IDerReq,PipelineBnd,MicroTest,{SizeArg,0,0,0,SizeState(MicroTest)}},SmartState());
    //
    BindLoc res[] = {ResizeLoc};
    for (int i = 0; i < StackState::frames; i++)
    copy->push(MicroTest,res,sizeof(res)/sizeof(res[0]),0,0,0,0,0,vulkanWait,true,SmartState());
    //
    Center *vtx = 0; allocCenter(&vtx,1);
    vtx->mem = Bringupz; vtx->siz = vertices.size(); allocVertex(&vtx->ver,vtx->siz);
    for (int i = 0; i < vtx->siz; i++) memcpy(&vtx->ver[i],&vertices[i],sizeof(Vertex));
    copy->push(vtx,0,vulkanPass,vulkanForce,SmartState());
    //
    Center *ind = 0; allocCenter(&ind,1);
    int isiz = indices.size()*sizeof(uint16_t);
    ind->mem = Indexz; ind->siz = isiz/sizeof(int32_t); allocInt32(&ind->ind,ind->siz);
    memcpy(ind->ind,indices.data(),isiz);
    copy->push(ind,0,vulkanPass,vulkanForce,SmartState());
    //
    Center *tex = 0; allocCenter(&tex,1);
    tex->mem = Texturez; tex->siz = 1; allocTexture(&tex->tex,tex->siz);
    fmtxStbi(&tex->tex[0].dat,&tex->tex[0].wid,&tex->tex[0].hei,&tex->tex[0].cha,"texture.jpg");
    copy->push(tex,0,vulkanPass,vulkanForce,SmartState());
    //
    bool temp; while (safe.wait(), temp = goon, safe.post(), temp) {
    //
    glm::mat4 model, view, proj, debug;
    testUpdate(size->capabilities.currentExtent,model,view,proj,debug);
    Center *mat = 0; allocCenter(&mat,1);
    mat->mem = Matrixz; mat->siz = 4; allocMatrix(&mat->mat,mat->siz);
    memcpy(&mat->mat[0],&model,sizeof(Matrix));
    memcpy(&mat->mat[1],&view,sizeof(Matrix));
    memcpy(&mat->mat[2],&proj,sizeof(Matrix));
    memcpy(&mat->mat[3],&debug,sizeof(Matrix));
    copy->push(mat,0,vulkanPass,vulkanPass,SmartState());
    //
    // BindLoc loc[] = {BeforeLoc,MiddleLoc,AfterLoc};
    BindLoc loc[] = {MiddleLoc};
    copy->push(MicroTest,loc,sizeof(loc)/sizeof(loc[0]),0,static_cast<uint32_t>(indices.size()),0,0,
    vulkanWake,vulkanWake,false,SmartState());}
}

struct ForkState : public DoneState {
    Thread thd; int idx; mftype cfnc; mftype dfnc;
    ForkState (Thread thd, int idx, mftype call, mftype done) :
        thd(thd), idx(idx), cfnc(call), dfnc(done) {
        strcpy(debug,"ForkState"); std::cout << debug << std::endl;
    }
    void call() override {cfnc(thd,idx);}
    void done() override {dfnc(thd,idx);}
    void heap() override {delete this;}
};

struct MainState {
    WindowState windowState;
    VulkanState vulkanState;
    PhysicalState physicalState;
    LogicalState logicalState;
    SizeState sizeState;
    ArrayState<SwapState,SwapBnd,1> swapState;
    ArrayState<PipeState,PipelineBnd,Micros> pipelineState;
    ArrayState<BufferState,IndexBnd,StackState::frames> indexState;
    ArrayState<BufferState,BringupBnd,StackState::frames> bringupState;
    ArrayState<TextureState,TextureBnd,StackState::frames> textureState;
    ArrayState<UniformState,UniformBnd,StackState::frames> uniformState;
    ArrayState<UniformState,MatrixBnd,StackState::frames> matrixState;
    ArrayState<BufferState,TriangleBnd,StackState::frames> triangleState;
    ArrayState<BufferState,NumericBnd,StackState::frames> numericState;
    ArrayState<BufferState,VertexBnd,StackState::frames> vertexState;
    ArrayState<BufferState,BasisBnd,StackState::frames> basisState;
    ArrayState<BufferState,PierceBnd,StackState::frames> pierceState;
    ArrayState<ResultState,ResultBnd,StackState::frames> resultState;
    ArrayState<AcquireState,AcquireBnd,StackState::frames> acquireState;
    ArrayState<PresentState,PresentBnd,StackState::frames> presentState;
    ArrayState<DrawState,DrawBnd,StackState::frames> drawState;
    ArrayState<BindState,BindBnd,StackState::frames> bindState;
    EnumState enumState[Binds+1];
    ThreadState threadState;
    CopyState copyState;
    TestState testState;
    CallState callState;
    MainState() :
        vulkanState(windowState.window),
        physicalState(vulkanState.instance,vulkanState.surface),
        logicalState(physicalState.device,physicalState.graphicsFamily,
            physicalState.presentFamily,physicalState.surfaceFormat,
            vulkanState.validationLayers,physicalState.deviceExtensions),
        sizeState(findCapabilities(windowState.window,vulkanState.surface,physicalState.device)),
        swapState("SwapBnd",&copyState,
            windowState.window,vulkanState.surface,physicalState.device,
            physicalState.surfaceFormat,physicalState.presentMode,
            physicalState.graphicsFamily,physicalState.presentFamily,
            physicalState.properties,physicalState.memProperties,
            logicalState.device,logicalState.commandPool,logicalState.renderPass,
            logicalState.imageFormat,logicalState.depthFormat,
            logicalState.graphics,logicalState.present),
        pipelineState("PipelineBnd"),
        indexState("Indexz",VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
        bringupState("Bringupz",VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
        textureState("Texturez"),
        uniformState("Uniformz"),
        matrixState("Matrixz"),
        triangleState("Trianglez",VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
        numericState("Numericz"),
        vertexState("Vertexz"),
        basisState("Basisz"),
        pierceState("Piercez"),
        resultState("ResultBnd"),
        acquireState("AcquireBnd"),
        presentState("PresentBnd"),
        drawState("DrawBnd"),
        bindState("BindBnd"),
        enumState{
            {SwapBnd,&swapState},
            {PipelineBnd,&pipelineState},
            {IndexBnd,&indexState},
            {BringupBnd,&bringupState},
            {TextureBnd,&textureState},
            {UniformBnd,&uniformState},
            {MatrixBnd,&matrixState},
            {TriangleBnd,&triangleState},
            {NumericBnd,&numericState},
            {VertexBnd,&vertexState},
            {BasisBnd,&basisState},
            {PierceBnd,&pierceState},
            {ResultBnd,&resultState},
            {AcquireBnd,&acquireState},
            {PresentBnd,&presentState},
            {DrawBnd,&drawState},
            {BindBnd,&bindState},
            {Binds,0}},
        threadState(logicalState.device,&copyState),
        copyState(&threadState,enumState),
        testState(&copyState,&sizeState) {
        std::cout << "MainState" << std::endl;}
    ~MainState() {std::cout << "~MainState" << std::endl;}
    static VkSurfaceCapabilitiesKHR findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkExtent2D chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities);
};
MainState *mptr = 0;

// TODO define glfw callbacks

void vulkanWake(Center *ptr, int sub) {
    mptr->testState.wake.wait();
}
void vulkanWait(Center *ptr, int sub) {
    glfwWaitEventsTimeout(0.001);
}
void vulkanPass(Center *ptr, int sub) {
    freeCenter(ptr); allocCenter(&ptr,0);
}
void vulkanForce(Center *ptr, int sub) {
    std::cerr << "unexpected copy fail!" << std::endl; exit(-1);
}
void vulkanCopy(Center *ptr, int sub) {
    mptr->copyState.push(ptr,sub,planePass,planeFail,SmartState());
}
void vulkanCall(Configure cfg, xftype back) {
    mptr->copyState.call(cfg,back);
}
void vulkanFork(Thread thd, int idx, mftype fnc, mftype done) {
    mptr->callState.push(new ForkState(thd,idx,fnc,done));
}
int vulkanInfo(Configure cfg, int val, yftype fnc) {
    return mptr->copyState.info(cfg,val,fnc);
}
int vulkanJnfo(Configure cfg, int val, yftype fnc) {
    return mptr->copyState.jnfo(cfg,val,fnc);
}
int vulkanKnfo(Configure cfg, int val, yftype fnc) {
    return mptr->copyState.knfo(cfg,val,fnc);
}
std::vector<const char *> cmdl;
const char *vulkanCmnd(int arg) {
    if (arg < 0 || arg >= cmdl.size()) return 0;
    return cmdl[arg];
}
void vulkanBack(Configure cfg, int sav, int val) {
    if (cfg == RegisterOpen && (val & (1<<TestThd)) && !(sav & (1<<TestThd)))
    mptr->callState.push(&mptr->testState);
    if (cfg == RegisterOpen && !(val & (1<<TestThd)) && (sav & (1<<TestThd)))
    mptr->callState.stop(&mptr->testState);
    if (cfg == RegisterOpen && (val & (1<<TestThd)) && (sav & (1<<TestThd)))
    mptr->testState.wake.wake();
    if (cfg == RegisterOpen && (val & (1<<FenceThd)) && !(sav & (1<<FenceThd)))
    mptr->callState.push(&mptr->threadState);
    if (cfg == RegisterOpen && !(val & (1<<FenceThd)) && (sav & (1<<FenceThd)))
    mptr->callState.stop(&mptr->threadState);
}

int main(int argc, const char **argv) {
    for (int i = 1; i < argc; i++) cmdl.push_back(argv[i]);
    MainState main;
    mptr = &main;
    main.copyState.call(RegisterOpen,vulkanBack);
    planeInit(vulkanCopy,vulkanCall,vulkanFork,vulkanInfo,vulkanJnfo,vulkanKnfo,vulkanCmnd);
    while (!glfwWindowShouldClose(main.windowState.window) && main.copyState.read(RegisterOpen) != 0) {
    glfwWaitEventsTimeout(main.copyState.read(RegisterPoll)*0.001);
    planeLoop();}
    planeDone();
    return 0;
}

GLFWwindow* WindowState::createWindow(uint32_t WIDTH, uint32_t HEIGHT) {
    glfwInit();
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    return glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
}

VKAPI_ATTR VkBool32 VKAPI_CALL VulkanState::debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData) {
    std::cout << "validation layer: " << pCallbackData->pMessage << std::endl;
    return VK_FALSE;
}
VkDebugUtilsMessengerCreateInfoEXT VulkanState::createInfo(const char **validationLayers) {
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
VkInstance VulkanState::createInstance(VkDebugUtilsMessengerCreateInfoEXT info, const char **validationLayers) {
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
VkDebugUtilsMessengerEXT VulkanState::createDebug(VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info,
    const char **validationLayers) {
    VkDebugUtilsMessengerEXT debug{};
    if (!validationLayers) return debug;
    auto func = (PFN_vkCreateDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
    if (func == nullptr || func(instance, &info, nullptr, &debug) != VK_SUCCESS)
    {std::cerr << "failed to set up debug messenger!" << std::endl; exit(-1);}
    return debug;
}
VkSurfaceKHR VulkanState::createSurface(VkInstance instance, GLFWwindow* window) {
    VkSurfaceKHR surface;
    if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
    {std::cerr << "failed to create window surface!" << std::endl; exit(-1);}
    return surface;
}

bool PhysicalState::foundIndices(VkSurfaceKHR surface, VkPhysicalDevice device) {
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
bool PhysicalState::foundDetails(VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t surfaceFormats = 0; vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &surfaceFormats, nullptr);
    uint32_t presentModes = 0; vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &presentModes, nullptr);
    return (surfaceFormats > 0 && presentModes > 0);
}
VkPhysicalDevice PhysicalState::createDevice(VkInstance instance, VkSurfaceKHR surface, const char **deviceExtensions) {
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
uint32_t PhysicalState::findGraphicsFamily(VkSurfaceKHR surface, VkPhysicalDevice device) {
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
uint32_t PhysicalState::findPresentFamily(VkSurfaceKHR surface, VkPhysicalDevice device) {
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
VkPhysicalDeviceProperties PhysicalState::findProperties(VkPhysicalDevice device) {
    VkPhysicalDeviceProperties properties{};
    vkGetPhysicalDeviceProperties(device, &properties);
    return properties;
}
VkSurfaceFormatKHR PhysicalState::chooseSwapSurfaceFormat(VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t count = 0; vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, nullptr);
    std::vector<VkSurfaceFormatKHR> formats(count);
    if (count != 0) vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, formats.data());
    for (const auto& format : formats) {
    if (format.format == VK_FORMAT_B8G8R8A8_SRGB && format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
    return format;}}
    return formats[0];
}
VkPresentModeKHR PhysicalState::chooseSwapPresentMode(VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t count = 0; vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, nullptr);
    std::vector<VkPresentModeKHR> presentModes(count);
    if (count != 0) vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, presentModes.data());
    for (const auto& presentMode : presentModes) {
    if (presentMode == VK_PRESENT_MODE_MAILBOX_KHR) {
    return presentMode;}}
    return VK_PRESENT_MODE_FIFO_KHR;
}
VkPhysicalDeviceMemoryProperties PhysicalState::findMemoryProperties(VkPhysicalDevice device) {
    VkPhysicalDeviceMemoryProperties memProperties;
    vkGetPhysicalDeviceMemoryProperties(device, &memProperties);
    return memProperties;
}

VkDevice LogicalState::createDevice(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily,
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
VkQueue LogicalState::createQueue(VkDevice device, uint32_t family) {
    VkQueue queue;
    vkGetDeviceQueue(device, family, 0, &queue);
    return queue;
}
VkCommandPool LogicalState::createCommandPool(VkDevice device, uint32_t family) {
    VkCommandPool pool;
    VkCommandPoolCreateInfo commandPoolInfo{};
    commandPoolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
    commandPoolInfo.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    commandPoolInfo.queueFamilyIndex = family;
    if (vkCreateCommandPool(device, &commandPoolInfo, nullptr, &pool) != VK_SUCCESS)
    {std::cerr << "failed to create graphics command pool!" << std::endl; exit(-1);}
    return pool;
}
VkFormat LogicalState::findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size,
    VkImageTiling tiling, VkFormatFeatureFlags features) {
    for (int i = 0; i < size; i++) {
    VkFormatProperties props;
    vkGetPhysicalDeviceFormatProperties(physicalDevice, candidates[i], &props);
    if (tiling == VK_IMAGE_TILING_LINEAR && (props.linearTilingFeatures & features) == features) return candidates[i];
    else if (tiling == VK_IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures & features) == features) return candidates[i];}
    {std::cerr << "failed to find supported format!" << std::endl; exit(-1);}
}
VkRenderPass LogicalState::createRenderPass(VkDevice device, VkFormat imageFormat, VkFormat depthFormat) {
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

uint32_t BaseState::findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags,
    VkPhysicalDeviceMemoryProperties memProperties) {
    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
    if ((filter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & flags) == flags) return i;
    {std::cerr << "failed to find suitable memory type!" << std::endl; exit(-1);}
}
VkCommandBuffer BaseState::createCommandBuffer(VkDevice device, VkCommandPool pool) {
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
VkFence BaseState::createFence(VkDevice device) {
    VkFence fence;
    VkFenceCreateInfo fenceInfo{};
    fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;
    if (vkCreateFence(device, &fenceInfo, nullptr, &fence) != VK_SUCCESS)
    {std::cerr << "failed to create fence!" << std::endl; exit(-1);}
    return fence;
}
VkSemaphore BaseState::createSemaphore(VkDevice device) {
    VkSemaphore semaphore;
    VkSemaphoreCreateInfo semaphoreInfo{};
    semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    if (vkCreateSemaphore(device, &semaphoreInfo, nullptr, &semaphore) != VK_SUCCESS)
    {std::cerr << "failed to create semaphore!" << std::endl; exit(-1);}
    return semaphore;
}
void BaseState::createImage(VkDevice device, VkPhysicalDevice physical,
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
VkImageView BaseState::createImageView(VkDevice device, VkImage image, VkFormat format, VkImageAspectFlags aspectFlags) {
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
void BaseState::createBuffer(VkDevice device, VkPhysicalDevice physical, VkDeviceSize size, VkBufferUsageFlags usage,
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

VkSwapchainKHR SwapState::createSwapChain(VkSurfaceKHR surface, VkDevice device, VkExtent2D swapChainExtent,
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
void SwapState::createSwapChainImages(VkDevice device, VkSwapchainKHR swapChain, std::vector<VkImage> &swapChainImages) {
    uint32_t imageCount;
    vkGetSwapchainImagesKHR(device, swapChain, &imageCount, nullptr);
    swapChainImages.resize(imageCount);
    vkGetSwapchainImagesKHR(device, swapChain, &imageCount, swapChainImages.data());
}
void SwapState::createFramebuffers(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
    std::vector<VkImageView> swapChainImageViews, VkImageView depthImageView,
    std::vector<VkFramebuffer> &framebuffers) {
    framebuffers.resize(swapChainImageViews.size());
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
    if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &framebuffers[i]) != VK_SUCCESS)
    {std::cerr << "failed to create framebuffer!" << std::endl; exit(-1);}}
}

VkDescriptorPool PipeState::createDescriptorPool(VkDevice device, int frames) {
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
VkDescriptorSetLayout PipeState::createDescriptorSetLayout(VkDevice device, Micro micro) {
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
VkPipelineLayout PipeState::createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout) {
    VkPipelineLayout pipelineLayout;
    VkPipelineLayoutCreateInfo pipelineLayoutInfo{};
    pipelineLayoutInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pipelineLayoutInfo.setLayoutCount = 1;
    pipelineLayoutInfo.pSetLayouts = &descriptorSetLayout;
    if (vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout) != VK_SUCCESS)
    {std::cerr << "failed to create pipeline layout!" << std::endl; exit(-1);}
    return pipelineLayout;
}
std::vector<char> PipeState::readFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::ate | std::ios::binary);
    if (!file.is_open()) {std::cerr << "failed to open shader: " << filename << std::endl; exit(-1);}
    size_t fileSize = (size_t) file.tellg();
    std::vector<char> buffer(fileSize);
    file.seekg(0);
    file.read(buffer.data(), fileSize);
    file.close();
    return buffer;
}
VkShaderModule PipeState::createShaderModule(VkDevice device, const std::vector<char>& code) {
    VkShaderModuleCreateInfo createInfo{};
    createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    createInfo.codeSize = code.size();
    createInfo.pCode = reinterpret_cast<const uint32_t*>(code.data());
    VkShaderModule shaderModule;
    if (vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule) != VK_SUCCESS)
    {std::cerr << "failed to create shader module!" << std::endl; exit(-1);}
    return shaderModule;
}
VkPipeline PipeState::createGraphicsPipeline(VkDevice device, VkRenderPass renderPass,
    VkPipelineLayout pipelineLayout, Micro micro) {
    VkPipeline pipeline;
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
    for (int i = 0; VertexFormat__Micro__Int__Format(micro)(i) != Formats; i++) {
        VkVertexInputAttributeDescription attributeDescription{};
        attributeDescription.binding = 0;
        attributeDescription.location = i;
        switch (VertexFormat__Micro__Int__Format(micro)(i)) {
        default: {std::cerr << "invalid vertex format!" << std::endl; exit(-1);}
        case (VecFormat): attributeDescription.format = VK_FORMAT_R32G32B32A32_SFLOAT; break;
        case (UvecFormat): attributeDescription.format = VK_FORMAT_R32G32B32A32_UINT; break;}
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
    if (vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &pipeline) != VK_SUCCESS)
    {std::cerr << "failed to create graphics pipeline!" << std::endl; exit(-1);}
    vkDestroyShaderModule(device, fragShaderModule, nullptr);
    vkDestroyShaderModule(device, vertShaderModule, nullptr);
    return pipeline;
}

void BufferState::copyBuffer(VkDevice device, VkQueue graphics, VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size,
    VkCommandBuffer commandBuffer, VkFence fence, VkSemaphore before, VkSemaphore after) {
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
    VkSemaphore befores[] = {before};
    VkPipelineStageFlags stages[] = {VK_PIPELINE_STAGE_ALL_COMMANDS_BIT};
    if (before != VK_NULL_HANDLE) {
    submitInfo.waitSemaphoreCount = 1;
    submitInfo.pWaitSemaphores = befores;
    submitInfo.pWaitDstStageMask = stages;}
    VkSemaphore afters[] = {after};
    if (after != VK_NULL_HANDLE) {
    submitInfo.signalSemaphoreCount = 1;
    submitInfo.pSignalSemaphores = afters;}
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &commandBuffer;
    vkQueueSubmit(graphics, 1, &submitInfo, fence);
}

VkSampler TextureState::createTextureSampler(VkDevice device, VkPhysicalDeviceProperties properties) {
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
void TextureState::transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image,
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
void TextureState::copyTextureImage(VkDevice device, VkQueue graphics,
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
    transitionImageLayout(device, graphics, afterBuffer, textureImage, afterSemaphore, VK_NULL_HANDLE, fence,
    VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
}

bool PresentState::presentFrame(VkQueue present, VkSwapchainKHR swapChain, uint32_t imageIndex, VkSemaphore before) {
    VkPresentInfoKHR presentInfo{};
    presentInfo.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    VkSemaphore signalSemaphores[] = {before};
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

VkDescriptorSet DrawState::createDescriptorSet(VkDevice device, VkDescriptorPool descriptorPool,
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
void DrawState::updateStorageDescriptor(VkDevice device, VkBuffer buffer,
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
void DrawState::updateUniformDescriptor(VkDevice device, VkBuffer buffer,
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
void DrawState::updateTextureDescriptor(VkDevice device,
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
void DrawState::recordCommandBuffer(VkCommandBuffer commandBuffer, VkRenderPass renderPass,
    VkDescriptorSet descriptorSet, VkExtent2D renderArea, Micro micro, uint32_t indices,
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
    renderPassInfo.renderArea.extent = renderArea;
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
    viewport.width = (float) renderArea.width;
    viewport.height = (float) renderArea.height;
    viewport.minDepth = 0.0f;
    viewport.maxDepth = 1.0f;
    vkCmdSetViewport(commandBuffer, 0, 1, &viewport);
    VkRect2D scissor{};
    scissor.offset = {0, 0};
    scissor.extent = renderArea;
    vkCmdSetScissor(commandBuffer, 0, 1, &scissor);
    VkBuffer buffers[] = {vertexBuffer};
    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(commandBuffer, 0, 1, buffers, offsets); // TODO depends on micro
    vkCmdBindIndexBuffer(commandBuffer, indexBuffer, 0, VK_INDEX_TYPE_UINT16); // TODO depends on micro
    // HERE
    vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
    vkCmdDrawIndexed(commandBuffer, indices, 1, 0, 0, 0);
    vkCmdEndRenderPass(commandBuffer);
    if (vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
    {std::cerr << "failed to record command buffer!" << std::endl; exit(-1);}
}
bool DrawState::drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, VkQueue present,
    VkSwapchainKHR swapChain, uint32_t imageIndex, void *ptr, int loc, int siz, Micro micro,
    VkSemaphore acquire, VkSemaphore after, VkFence fence, VkSemaphore before) {
    // TODO depending on micro, disable acquire and present
    VkSubmitInfo submitInfo{};
    submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    VkSemaphore waitSemaphores[] = {acquire,before};
    VkPipelineStageFlags waitStages[] = {
    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    VK_PIPELINE_STAGE_ALL_COMMANDS_BIT};
    submitInfo.waitSemaphoreCount = (before == VK_NULL_HANDLE ? 1 : 2);
    submitInfo.pWaitSemaphores = waitSemaphores;
    submitInfo.pWaitDstStageMask = waitStages;
    VkCommandBuffer commandBuffers[] = {commandBuffer};
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &commandBuffer;
    VkSemaphore signalSemaphores[] = {after};
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

void TestState::testUpdate(VkExtent2D swapChainExtent, glm::mat4 &model, glm::mat4 &view, glm::mat4 &proj, glm::mat4 &debug) {
    static auto startTime = std::chrono::high_resolution_clock::now();
    auto currentTime = std::chrono::high_resolution_clock::now();
    float time = std::chrono::duration<float, std::chrono::seconds::period>(currentTime - startTime).count();
    model = glm::rotate(glm::mat4(1.0f), time * glm::radians(90.0f), glm::vec3(0.0f, 0.0f, 1.0f));
    view = glm::lookAt(glm::vec3(2.0f, 2.0f, 2.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));
    proj = glm::perspective(glm::radians(45.0f), swapChainExtent.width / (float) swapChainExtent.height, 0.1f, 10.0f);
    proj[1][1] *= -1;
    float mat[16]; planeWindow(mat); for (int r = 0; r < 4; r++) for (int c = 0; c < 4; c++) debug[r][c] = *matrc(mat,r,c,4);
}

VkSurfaceCapabilitiesKHR MainState::findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device) {
    VkSurfaceCapabilitiesKHR capabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &capabilities);
    capabilities.currentExtent = chooseSwapExtent(window,capabilities);
    return capabilities;
}
VkExtent2D MainState::chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities) {
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
