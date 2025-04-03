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
    WindowState() : // TODO add argument to switch between glfw and wayland
        window(createWindow(WIDTH,HEIGHT)) {
        std::cout << "WindowState" << std::endl;
    }
    ~WindowState() {
        glfwDestroyWindow(window);
        glfwTerminate();
        std::cout << "~WindowState" << std::endl;
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
        surface(createSurface(instance, window)) {
        std::cout << "VulkanState" << std::endl;
    }
    ~VulkanState() {
        vkDestroySurfaceKHR(instance, surface, nullptr);
        auto func = (PFN_vkDestroyDebugUtilsMessengerEXT)
        vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
        if (func != nullptr) func(instance, debug, nullptr);
        vkDestroyInstance(instance, nullptr);
        std::cout << "~VulkanState" << std::endl;
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
        memProperties(findMemoryProperties(device)) {
        std::cout << "PhysicalState" << std::endl;
    }
    ~PhysicalState() {
        std::cout << "~PhysicalState" << std::endl;
    }
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
        renderPass(createRenderPass(device,imageFormat,depthFormat)) {
        std::cout << "LogicalState" << std::endl;
    }
    ~LogicalState() {
        vkDestroyRenderPass(device, renderPass, nullptr);
        vkDestroyCommandPool(device, commandPool, nullptr);
        vkDestroyDevice(device, nullptr);
        std::cout << "~LogicalState" << std::endl;
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
    static const int frames = 2;
    virtual BaseState *buffer() = 0; // no block beween push and advance
    virtual BaseState *prebuf() = 0; // current available for read while next is written
    virtual BaseState *prebuf(int i) = 0;
    virtual void advance() = 0;
    virtual void advance(int i) = 0;
    virtual Bind buftyp() = 0;
    virtual int bufsiz() = 0;
    virtual const char *bufnam() = 0;
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
    StackState(
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
        VkQueue present) {
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
    StackState(VkBufferUsageFlags flags) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::flags = flags;        
    }
    StackState() {
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
    ArrayState(
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
    StackState(
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
        safe(1), idx(0) {
    }
    ArrayState(VkBufferUsageFlags flags) : StackState(flags), safe(1), idx(0) {
    }
    ArrayState() : safe(1), idx(0) {
    }
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
    const char *bufnam() override {
        switch (Type) {
        default: {std::cerr << "unnamed array type!" << std::endl; exit(-1);}
        case (SwapBnd): return "SwapBnd";
        case (PipelineBnd): return "PipelineBnd";
        case (IndexBnd): return "IndexBnd";
        case (BringupBnd): return "BringupBnd";
        case (ImageBnd): return "ImageBnd";
        case (BeforeBnd): return "BeforeBnd";
        case (AfterBnd): return "AfterBnd";
        case (TextureBnd): return "TextureBnd";
        case (UniformBnd): return "UniformBnd";
        case (MatrixBnd): return "MatrixBnd";
        case (TriangleBnd): return "TriangleBnd";
        case (NumericBnd): return "NumericBnd";
        case (VertexBnd): return "VertexBnd";
        case (BasisBnd): return "BasisBnd";
        case (PierceBnd): return "PierceBnd";
        case (PokeBnd): return "PokeBnd";
        case (PeekBnd): return "PeekBnd";
        case (AcquireBnd): return "AcquireBnd";
        case (PresentBnd): return "PresentBnd";
        case (DrawBnd): return "DrawBnd";
        case (BindBnd): return "BindBnd";}
        return 0;
    }
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

struct SizeState {
    Extent tag;
    int base, size;
    VkExtent2D extent;
    Micro micro;
    Bind bind;
    SizeState() {
        tag = InitExt;
    }
    SizeState(int base, int size) {
        tag = IntExt;
        this->base = base;
        this->size = size;
    }
    SizeState(VkExtent2D extent) {
        tag = ExtentExt;
        this->extent = extent;
    }
    SizeState(Micro micro) {
        tag = MicroExt;
        this->micro = micro;
    }
    SizeState(Bind bind) {
        tag = BindExt;
        this->bind = bind;
    }
    SizeState(Extent tag) {
        this->tag = tag;
    }
    bool operator==(const SizeState &other) const {
        if (tag == InitExt && other.tag == InitExt) return true;
        if (tag == IntExt && other.tag == IntExt &&
        base == other.base && size == other.size) return true;
        if (tag == ExtentExt && other.tag == ExtentExt &&
        extent.width == other.extent.width &&
        extent.height == other.extent.height) return true;
        if (tag == MicroExt && other.tag == MicroExt &&
        micro == other.micro) return true;
        if (tag == BindExt && other.tag == BindExt &&
        bind == other.bind) return true;
        return false;
    }
};
std::ostream& operator<<(std::ostream& os, const SizeState& size) {
    switch (size.tag) {default: os << "MicroSize()"; break;
    case (InitExt): os << "InitSize()"; break;
    case (IntExt): os << "IntSize(" << size.size << ")"; break;
    case (ExtentExt): os << "ExtentSize(" << size.extent.width << "," << size.extent.height << ")"; break;
    case (MicroExt): os << "MicroSize(" << size.micro << ")"; break;
    case (BindExt): os << "BindSize(" << size.bind << ")"; break;
    case (FalseExt): os << "FalseSize()"; break;}
    return os;
}

enum ReqEnum {
    BothReq,
    LockReq,
    SizeReq,
    ReqEnums
};
struct Req {
    ReqEnum tag; void *ptr; int loc; int siz; SizeState max; bool pre;
};
struct Rsp {
    Micro mic; Memory mem; Bind bnd; BindLoc loc;
};
enum BaseEnum {
    InitBase, // avoid binding to uninitialized
    FreeBase, // ready for use
    BothBase, // check for both change
    SizeBase, // check for size change
    LockBase, // check for data change
    NextBase, // waiting for fence done
    BaseEnums
};
struct BindState;
struct BaseState {
    StackState *item;
    BaseState *next; BaseState *last;
    SafeState safe;
    BaseEnum state;
    int rlock, wlock;
    // indirectly protected by state/lock that are directly protected by safe
    // only one ThreadState acts on BaseState with reserved state/lock
    BindState *lock;
    Rsp resp;
    Req rqst;
    SizeState size;
    char debug[64];
    BaseState(const char *name, StackState *ptr) :
        item(ptr), next(0), last(0), safe(1), state(InitBase), rlock(0), wlock(0),
        resp{Micros,Memorys,Binds,BindLocs}, rqst{ReqEnums,0,0,0},
        lock(0), debug{0} {
        sprintf(debug,"%s_%s_%d",name,item->bufnam(),StackState::debug++);
        std::cout << debug << std::endl;
    }
    ~BaseState() {
        std::cout << "~" << debug << std::endl;
    }
    bool push(int rdec, int wdec, Req req, SmartState log) {
        safe.wait();
        if (state != InitBase && state != FreeBase) {
        log << "push state fail " << debug << " " << state << std::endl;
        safe.post(); return false;}
        if (rlock-rdec || wlock-wdec) {
        log << "push lock fail " << debug << " " << state << std::endl;
        safe.post(); return false;}
        log << "push pass " << debug << std::endl;
        rqst = req;
        switch (req.tag) {default: {std::cerr << "invalid push req!" << std::endl; exit(-1);}
        break; case (BothReq): state = BothBase;
        break; case (LockReq): state = LockBase;
        break; case (SizeReq): state = SizeBase;}
        safe.post();
        return true;
    }
    bool push(Req req, SmartState log) {
        return push(0,0,req,log);
    }
    void push(SmartState log) {
        safe.wait();
        if (state != BothBase && state != SizeBase && state != LockBase)
            {std::cerr << "invalid push state!" << std::endl; exit(-1);}
        state = FreeBase;
        safe.post();
    }
    void push(Rsp rsp, BindState *ptr, SmartState log) {
        safe.wait();
        if (state != BothBase && state != SizeBase && state != LockBase)
            {std::cerr << "invalid set state!" << std::endl; exit(-1);}
        safe.post();
        if (lock != 0) {std::cerr << "invalid set lock! " <<
            debug << std::endl; exit(-1);}
        lock = ptr;
        resp = rsp;
    }
    void setre(SizeState siz, SmartState log) {
        push(Req{SizeReq,0,0,0,siz,false},log);
        baseres(log); baseups(log);
    }
    void reset(SmartState log) {
        push(Req{SizeReq,0,0,0,SizeState(InitExt),false},log);
        baseres(log);
    }
    VkFence sizeup(SmartState log) {
        safe.wait();
        if (state != BothBase) {std::cerr << "sizeup invalid state! " <<
            state << std::endl; exit(-1);}
        if (size == rqst.max); else {
        if (size == SizeState(InitExt)); else {
        safe.post();
        unsize(log);
        safe.wait();}
        if ((size = rqst.max) == SizeState(InitExt)); else {
        safe.post();
        resize(log);
        safe.wait();}}
        state = NextBase;
        safe.post();
        return setup(rqst.ptr,rqst.loc,rqst.siz,log);
    }
    void baseres(SmartState log) {
        safe.wait();
        if (state != SizeBase) {std::cerr << "baseres invalid state! " << state <<
            "(" << SizeBase << ")" << " " << debug << std::endl; exit(-1);}
        if (size == rqst.max); else {
        if (size == SizeState(InitExt)); else {
        safe.post();
        unsize(log);
        safe.wait();}
        if ((size = rqst.max) == SizeState(InitExt)); else {
        safe.post();
        resize(log);
        safe.wait();}}
        state = NextBase;
        safe.post();
    }
    VkFence basesup(SmartState log) {
        safe.wait();
        if (state != LockBase) {std::cerr << "basesup invalid state! " <<
            debug << " " << state << std::endl; exit(-1);}
        state = NextBase;
        safe.post();
        return setup(rqst.ptr,rqst.loc,rqst.siz,log);
    }
    void unlock(SmartState log);
    void baseups(SmartState log) {
        safe.wait();
        if (state != NextBase) {std::cerr << "upset invalid state!" <<
            std::endl; exit(-1);}
        safe.post();
        if (rqst.pre) log << "baseups " << debug << " " << item->debug << std::endl;
        else log << "baseups " << debug << std::endl;
        if (rqst.pre) item->advance();
        upset(log);
        unlock(log);
        resp = Rsp{Micros,Memorys,Binds,BindLocs};
        lock = 0;
        safe.wait();
        state = FreeBase;
        safe.post();
    }
    void incr(bool elock) {
        safe.wait();
        (elock ? wlock : rlock) += 1;
        safe.post();
    }
    void decr(bool elock) {
        safe.wait();
        if ((elock ? wlock : rlock) <= 0)
            {std::cerr << "invalid decr lock! " << std::endl; exit(-1);}
        (elock ? wlock : rlock) -= 1;
        safe.post();
    }
    bool get(bool elock, int psav, int rsav, int wsav) {
        safe.wait();
        switch (state) {
        default: {safe.post(); return false;}
        break; case (FreeBase):
        break; case (BothBase): case (SizeBase): case (LockBase):
        if (psav == 0) {safe.post(); return false;}}
        if (wlock < wsav || rlock < rsav)
            {std::cerr << "invalid get lock!" << std::endl; exit(-1);}
        if (wlock-wsav || (elock && rlock-rsav)) {safe.post(); return false;}
        safe.post();
        return true;
    }
    BaseEnum get() {
        safe.wait();
        BaseEnum ret = state;
        safe.post();
        return ret;
    }
    BindLoc loc() {
        return resp.loc;
    }
    BaseState *dee(Bind typ);
    virtual void unsize(SmartState log) {std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual void resize(SmartState log) {std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual VkFence setup(void *ptr, int loc, int siz, SmartState log) {std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual void upset(SmartState log) {std::cerr << "unsize not base!" << std::endl; exit(-1);}
    virtual bool get(BaseEnum state) {return false;}
    virtual BindState *getBind() {std::cerr << "BaseState::getBind" << std::endl; exit(-1);}
    virtual VkSemaphore getSemaphore() {std::cerr << "BaseState::getSemaphore" << std::endl; exit(-1);}
    virtual VkImage getImage() {std::cerr << "BaseState::getImage" << std::endl; exit(-1);}
    virtual VkSwapchainKHR getSwapChain() {std::cerr << "BaseState::swapChain" << std::endl; exit(-1);}
    virtual uint32_t getImageIndex() {std::cerr << "BaseState::getImageIndex" << std::endl; exit(-1);}
    virtual VkFramebuffer getFramebuffer() {std::cerr << "BaseState::framebuffer" << std::endl; exit(-1);}
    virtual VkFramebuffer getFramebuffer(int i) {std::cerr << "BaseState::framebuffer" << std::endl; exit(-1);}
    virtual VkPipeline getPipeline() {std::cerr << "BaseState::pipeline" << std::endl; exit(-1);}
    virtual VkPipelineLayout getPipelineLayout() {std::cerr << "BaseState::pipelineLayout" << std::endl; exit(-1);}
    virtual VkBuffer getBuffer() {std::cerr << "BaseState::buffer" << std::endl; exit(-1);}
    virtual VkDeviceMemory getMemory() {std::cerr << "BaseState::memory" << std::endl; exit(-1);}
    virtual int getRange() {std::cerr << "BaseState::size" << std::endl; exit(-1);}
    virtual VkImageView getImageView() {std::cerr << "BaseState::imageView" << std::endl; exit(-1);}
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
        VkBuffer& buffer, VkDeviceMemory& memory);
    static void createImage(VkDevice device, VkPhysicalDevice physical,
        uint32_t width, uint32_t height, VkFormat format, VkImageTiling tiling, VkImageUsageFlags usage,
        VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties,
        VkImage& image, VkDeviceMemory& imageMemory);
};

enum IterEnum {
    Dee,
    Die,
    Ded,
    Deps,
    Mee,
    Mie,
    Med,
    Meps,
    Bee,
    Bie,
    Bed,
    Beps,
};
struct Iter {
    Rsp rsp;
    IterEnum seq;
    int sub;
    Bind bnd;
    Iter(Rsp rsp) : rsp(rsp), seq(r2s(rsp)), sub(0) {incr(); init();}
    Iter(Draw drw, BindLoc loc) : rsp(d2r(drw,loc)), seq(d2s(drw)), sub(0) {incr(); init();}
    bool operator()() {return (bnd != Binds);}
    Iter &operator++() {sub++; init(); return *this;}
    bool isee() {
        return (seq == Dee || seq == Mee || seq == Bee);
    }
    bool isie() {
        return (seq == Die || seq == Mie || seq == Bie);
    }
    bool ised() {
        return (seq == Ded || seq == Med || seq == Bed);
    }
    bool iseps() {
        return (seq == Deps || seq == Meps || seq == Beps);
    }
    Rsp d2r(Draw drw, BindLoc loc) {
        Rsp rsp;
        switch (drw.adv) {default: {std::cerr << "invalid rsp adv!" << std::endl; exit(-1);}
        break; case(MicroAdv): rsp = Rsp{drw.drw,Memorys,Binds,loc};
        break; case(PrememAdv): rsp = Rsp{Micros,drw.mem,Binds,loc};
        break; case(SubmicAdv): case(BufmicAdv): case(BufnotAdv): case(PresizAdv):
        rsp = Rsp{Micros,Memorys,drw.bnd};}
        return rsp;
    }
    IterEnum d2s(Draw drw) {
        IterEnum seq;
        switch (drw.adv) {default: {std::cerr << "invalid seq adv!" << std::endl; exit(-1);}
        break; case(MicroAdv): seq = Deps;
        break; case(PrememAdv): seq = Meps;
        break; case(SubmicAdv): case(BufmicAdv): case(BufnotAdv): case(PresizAdv):
        seq = Beps;}
        return seq;
    }
    IterEnum r2s(Rsp rsp) {
        IterEnum seq;
        if (rsp.mic == Micros && rsp.mem == Memorys) seq = Beps;
        else if (rsp.mic == Micros) seq = Meps;
        else if (rsp.mem == Memorys) seq = Deps;
        else {std::cerr << "invalid iter rsp!" << std::endl; exit(-1);}
        return seq;
    }
    void incr() {
        if (seq == Deps) seq = Dee;
        else if (seq == Dee) seq = Die;
        else if (seq == Die) seq = Ded;
        else if (seq == Ded) seq = Deps;
        else if (seq == Meps) seq = Mee;
        else if (seq == Mee) seq = Mie;
        else if (seq == Mie) seq = Med;
        else if (seq == Med) seq = Meps;
        else if (seq == Beps) seq = Bee;
        else if (seq == Bee) seq = Bie;
        else if (seq == Bie) seq = Bed;
        else if (seq == Bed) seq = Beps;
    }
    void init() {
        bnd = Binds;
        while (seq != Deps && seq != Meps && seq != Beps && bnd == Binds) {
        auto f = Dependee__Micro__BindLoc__Int__Bind(Micros);
        if (seq == Dee) f = Dependee__Micro__BindLoc__Int__Bind(rsp.mic);
        else if (seq == Die) f = Dependie__Micro__BindLoc__Int__Bind(rsp.mic);
        else if (seq == Ded) f = Depended__Micro__BindLoc__Int__Bind(rsp.mic);
        else if (seq == Mee) f = Memoryee__Memory__BindLoc__Int__Bind(rsp.mem);
        else if (seq == Mie) f = Memoryie__Memory__BindLoc__Int__Bind(rsp.mem);
        else if (seq == Med) f = Memoryed__Memory__BindLoc__Int__Bind(rsp.mem);
        else if (seq == Bee) f = Bindee__Bind__BindLoc__Int__Bind(rsp.bnd);
        else if (seq == Bie) f = Bindie__Bind__BindLoc__Int__Bind(rsp.bnd);
        else if (seq == Bed) f = Binded__Bind__BindLoc__Int__Bind(rsp.bnd);
        if (f == 0) {incr(); sub = 0; continue;}
        auto g = f(rsp.loc);
        if (g == 0) {incr(); sub = 0; continue;}
        bnd = g(sub);
        if (bnd == Binds) {incr(); sub = 0; continue;}}
    }
};
struct BindState : public BaseState {
    BaseState *bind[Binds];
    int psav[Binds];
    int rsav[Binds];
    int wsav[Binds];
    int lock; bool excl;
    BaseState *last;
    BindState() :
        BaseState("BindState",StackState::self),
        lock(0), excl(false), last(0) {
        for (int i = 0; i < Binds; i++) {
        bind[i] = 0; psav[i] = rsav[i] = wsav[i] = 0;}
    }
    BindState *getBind() override {
        safe.wait();
        if (excl) {safe.post(); return 0;}
        excl = true;
        safe.post();
        return this;
    }
    BaseState *get(Bind i) {
        if (bind[i] == 0) {std::cerr << "invalid get bind!" << std::endl; exit(-1);}
        return bind[i];
    }
    bool push(Bind i, BaseState *buf, Req req, SmartState log) {
        if (!excl) {std::cerr << "invalid excl push!" << std::endl; exit(-1);}
        if (!buf->push(rsav[i],wsav[i],req,log)) return false;
        if (bind[i] == 0) lock += 1;
        if (bind[i] != 0 && bind[i] != buf)
        {std::cerr << "invalid rinc bind!" << std::endl; exit(-1);}
        bind[i] = buf;
        psav[i] += 1;
        return true;
    }
    void push(Bind i, SmartState log) {
        if (!excl) {std::cerr << "invalid excl unpush!" << std::endl; exit(-1);}
        if (psav[i] <= 0) {std::cerr << "invalid push sav!" << std::endl; exit(-1);}
        psav[i] -= 1;
        if (psav[i] == 0 && rsav[i] == 0 && wsav[i] == 0) {bind[i] = 0; lock -= 1;}
        if (lock == 0) {safe.wait(); excl = false; safe.post();}
    }
    void push(Rsp rsp, SmartState log) {
        for (Iter i(rsp); i(); ++i) {
        if (i.isee()) rdec(i.bnd,log);
        else if (i.isie()) rdec(i.bnd,log);
        else if (i.ised()) wdec(i.bnd,log);}
        push(rsp.bnd,log);
    }
    bool incr(Bind i, BaseState *buf, bool elock, SmartState log) {
        if (!excl) {std::cerr << "invalid incr excl!" << std::endl; exit(-1);}
        if (bind[i] != 0 && bind[i] != buf)
            {std::cerr << "invalid incr bind!" << std::endl; exit(-1);}
        if (!buf->get(elock,psav[i],rsav[i],wsav[i])) {
        if (lock == 0) {safe.wait(); excl = false; safe.post();}
        return false;}
        if (bind[i] == 0) lock += 1;
        bind[i] = buf;
        buf->incr(elock);
        (elock ? wsav[i] : rsav[i]) += 1;
        return true;
    }
    void decr(Bind i, bool elock, SmartState log) {
        if (!excl) {std::cerr << "invalid decr excl!" << std::endl; exit(-1);}
        if (lock <= 0 || bind[i] == 0)
            {std::cerr << "invalid decr bind! " << i << std::endl; exit(-1);}
        bind[i]->decr(elock);
        if ((elock ? wsav[i] : rsav[i]) <= 0)
            {std::cerr << "invalid rdec sav!" << std::endl; exit(-1);}
        (elock ? wsav[i] : rsav[i]) -= 1;
        if (psav[i] == 0 && rsav[i] == 0 && wsav[i] == 0) {bind[i] = 0; lock -= 1;}
        if (lock == 0) {safe.wait(); excl = false; safe.post();}
    }
    bool rinc(Bind i, BaseState *buf, SmartState log) {
        return incr(i,buf,false,log);
    }
    bool winc(Bind i, BaseState *buf, SmartState log) {
        return incr(i,buf,true,log);
    }
    void rdec(Bind i, SmartState log) {
        decr(i,false,log);
    }
    void wdec(Bind i, SmartState log) {
        decr(i,true,log);
    }
};
void BaseState::unlock(SmartState log) {
    if (lock) lock->push(resp,log);
}
BaseState *BaseState::dee(Bind typ) {
    if (lock == 0) {std::cerr << "invalid get lock! " << debug << std::endl; exit(-1);}
    return lock->get(typ);
}

struct Push {
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
    std::deque<Push> before;
    std::deque<Push> after;
    bool goon;
    ThreadState(VkDevice device, ChangeState<Configure,Configures> *copy) :
        device(device), copy(copy),
        safe(1), wake(0), goon(true) {
        strcpy(debug,"ThreadState");
        std::cout << debug << std::endl;
    }
    ~ThreadState() {
        std::cout << "~" << debug << std::endl;
    }
    void push(BaseState *base, Center *ptr, int sub, void (*fnc)(Center*,int), SmartState log) {
        Push push = {base,VK_NULL_HANDLE,ptr,sub,fnc,log};
        safe.wait();
        before.push_back(push);
        safe.post();
        wake.wake();
    }
    bool stage() {
        while (1) {while (1) {
        safe.wait();
        if (before.empty()) {safe.post(); break;}
        Push push = before.front(); before.pop_front();
        safe.post();
        if (push.base) push.log << "stage " << push.base->debug << std::endl;
        if (push.base) switch (push.base->get()) {
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
        Push push = after.front(); after.pop_front();
        if (push.fence != VK_NULL_HANDLE) {
        VkResult result = vkWaitForFences(device,1,&push.fence,VK_FALSE,NANOSECONDS);
        if (result != VK_SUCCESS) {std::cerr << "cannot wait for fence!" << std::endl; exit(-1);}}
        if (push.fnc) push.fnc(push.ptr,push.sub);
        if (push.base) push.base->baseups(push.log);
        copy->wots(RegisterMask,1<<FnceMsk);}
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
enum CmdEnum {
    DerCmd, // push req to current, set bind, push to thread, and advance
    PDerCmd, // push req to next, set bind, and push to thread that advances
    IDerCmd, // push req to indexed, set bind, push to thread, and advance to index
    RDeeCmd, // increment read lock in current, and add to bind
    IRDeeCmd, // increment read lock in indexed, and add to bind
    WDeeCmd, // increment write lock in current, and add to bind
    PNowCmd, // call function now if pass
    FNowCmd, // call function now if fail
    PEnqCmd, // call function when free if pass
    FEnqCmd, // call function when free if fail
    GoonCmd, // retry if fail
};
struct Cmd {
    CmdEnum tag; Rsp rsp; Req req; int idx; Center *ptr; int sub; void (*fnc)(Center*,int);
};
struct Fnc {
    bool pnow; void (*pass)(Center*,int);
    bool fnow; void (*fail)(Center*,int);
    bool goon;
};
struct CopyState : public ChangeState<Configure,Configures> {
    ThreadState *thread; StackState *stack[Binds];
    CopyState(ThreadState *thread, EnumState *stack) :
        thread(thread), stack{0} {
        std::cout << "CopyState" << std::endl;
        for (EnumState *i = stack; i->key != Binds; i++) this->stack[i->key] = i->val;
    }
    ~CopyState() {
        std::cout << "~CopyState" << std::endl;
    }
    void push(Cmd *cmd, int num, SmartState log) {
        bool goon = true; while (goon) {goon = false;
        BaseState *buf[num] = {}; BindState *bind = 0; int count = 0; bool prep[num] = {};
        for (int i = 0; i < num; i++) switch (cmd[i].tag) {default:
            break; case(DerCmd): buf[i] = stack[cmd[i].rsp.bnd]->buffer(); cmd[i].req.pre = false; count += 1;
            break; case(PDerCmd): buf[i] = stack[cmd[i].rsp.bnd]->prebuf(); cmd[i].req.pre = true; count += 1;
            break; case(IDerCmd): buf[i] = stack[cmd[i].rsp.bnd]->prebuf(cmd[i].idx); cmd[i].req.pre = false; count += 1;
            break; case(RDeeCmd): case(WDeeCmd): buf[i] = stack[cmd[i].rsp.bnd]->buffer(); count += 1;
            break; case(IRDeeCmd): buf[i] = stack[cmd[i].rsp.bnd]->prebuf(cmd[i].idx); count += 1;}
        if (count > 1) bind = stack[BindBnd]->buffer()->getBind();
        int lim = num; if (count > 1 && bind == 0) lim = -1;
        for (int i = 0; i < num && i < lim; i++) switch (cmd[i].tag) {default:
            break; case(DerCmd): case(PDerCmd): case(IDerCmd): if (bind) {
            if (!bind->push(cmd[i].rsp.bnd,buf[i],cmd[i].req,log)) lim = i;} else {
            if (!buf[i]->push(cmd[i].req,log)) lim = i;}
            break; case(RDeeCmd): if (!bind->rinc(cmd[i].rsp.bnd,buf[i],log)) lim = i;
            break; case(IRDeeCmd): if (!bind->rinc(cmd[i].rsp.bnd,buf[i],log)) lim = i;
            break; case(WDeeCmd): if (!bind->winc(cmd[i].rsp.bnd,buf[i],log)) lim = i;}
        if (lim < num) for (int i = 0; i < lim; i++) switch (cmd[i].tag) {default:
            break; case(DerCmd): case(PDerCmd): case(IDerCmd):
            if (bind) bind->push(cmd[i].rsp.bnd,log); buf[i]->push(log);
            break; case(RDeeCmd): case(IRDeeCmd): bind->rdec(cmd[i].rsp.bnd,log);
            break; case(WDeeCmd): bind->wdec(cmd[i].rsp.bnd,log);
            break; case(FNowCmd): cmd[i].fnc(cmd[i].ptr,cmd[i].sub);
            break; case(FEnqCmd): thread->push(0,cmd[i].ptr,cmd[i].sub,cmd[i].fnc,log);
            break; case(GoonCmd): goon = true;}
        Center *ptr = 0; int sub = 0; void (*fnc)(Center*,int) = 0; BaseState *last = 0;
        if (lim == num) for (int i = 0; i < num; i++) switch(cmd[i].tag) {default:
            break; case(DerCmd): case (PDerCmd): case (IDerCmd):
            if (last) last->next = buf[i]; buf[i]->last = last; buf[i]->next = 0; last = buf[i];}
        if (lim == num) for (int i = 0; i < num; i++) switch (cmd[i].tag) {default:
            break; case(DerCmd): stack[cmd[i].rsp.bnd]->advance();
            buf[i]->push(cmd[i].rsp,bind,log);
            thread->push(buf[i],ptr,sub,fnc,log); ptr = 0; sub = 0; fnc = 0;
            break; case(PDerCmd):
            buf[i]->push(cmd[i].rsp,bind,log);
            thread->push(buf[i],ptr,sub,fnc,log); ptr = 0; sub = 0; fnc = 0;
            break; case(IDerCmd): stack[cmd[i].rsp.bnd]->advance(cmd[i].idx);
            buf[i]->push(cmd[i].rsp,bind,log);
            thread->push(buf[i],ptr,sub,fnc,log); ptr = 0; sub = 0; fnc = 0;
            break; case(PNowCmd): cmd[i].fnc(cmd[i].ptr,cmd[i].sub);
            break; case(PEnqCmd): ptr = cmd[i].ptr; sub = cmd[i].sub; fnc = cmd[i].fnc;}
        if (lim == num && bind) stack[BindBnd]->advance();}
    }
    void push(Cmd req, SmartState log) {
        push(&req,1,log);
    }
    void push(HeapState<Cmd> req, SmartState log) {
        push(req.data(), req.size(), log);
    }
    void push(Draw drw, Center *ptr, int sub, Fnc fnc, SmartState log) {
        HeapState<Cmd> cmd; int count = 0; int limit = 0;
        switch (AdvConst__Advance__Constant(drw.adv)) {
        default: {std::cerr << "invalid push adv!" << std::endl; exit(-1);}
        break; case (MicroConst): limit = Limit__Micro__Int(drw.drw);
        break; case (MemoryConst): limit = Memoryit__Memory__Int(drw.mem);
        break; case (BindConst): limit = Bindit__Bind__Int(drw.bnd);}
        if (drw.siz != limit) {
            std::cerr << "limit check failed! " << drw.adv << " " << drw.siz << " " << limit << std::endl; exit(-1);}
        if (fnc.pass) cmd<<Cmd{(fnc.pnow?PNowCmd:PEnqCmd),Rsp{Micros,Memorys,Binds,BindLocs},Req{},0,ptr,sub,fnc.pass};
        if (fnc.fail) cmd<<Cmd{(fnc.fnow?FNowCmd:FEnqCmd),Rsp{Micros,Memorys,Binds,BindLocs},Req{},0,ptr,sub,fnc.fail};
        if (fnc.goon) cmd<<Cmd{GoonCmd,Rsp{Micros,Memorys,Binds,BindLocs}};
        for (int j = 0; true; j++) {
        BindLoc loc = BindLocs;
        switch (AdvConst__Advance__Constant(drw.adv)) {
        default: {std::cerr << "invalid push adv!" << std::endl; exit(-1);}
        break; case (MicroConst): loc = Location__Micro__Int__BindLoc(drw.drw)(j);
        break; case (MemoryConst): loc = Memoryat__Memory__Int__BindLoc(drw.mem)(j);
        break; case (BindConst): loc = Bindat__Bind__Int__BindLoc(drw.bnd)(j);}
        if (loc == BindLocs) break;
        for (Iter i(drw,loc); i(); ++i) {
        if (i.isee()) cmd<<Cmd{RDeeCmd,Rsp{Micros,Memorys,i.bnd,BindLocs}};
        else if (i.isie()) {if (count >= limit) {std::cerr << "invalid limit check!" << std::endl; exit(-1);}
        cmd<<Cmd{IRDeeCmd,Rsp{Micros,Memorys,i.bnd,BindLocs},Req{},drw.arg[count++]};}
        else if (i.ised()) cmd<<Cmd{WDeeCmd,Rsp{Micros,Memorys,i.bnd,BindLocs}};}}
        for (int j = 0; true; j++) {
        BindLoc loc; switch (AdvConst__Advance__Constant(drw.adv)) {
        default: {std::cerr << "invalid push adv!" << std::endl; exit(-1);}
        break; case (MicroConst): loc = Location__Micro__Int__BindLoc(drw.drw)(j);
        break; case (MemoryConst): loc = Memoryat__Memory__Int__BindLoc(drw.mem)(j);
        break; case (BindConst): loc = Bindat__Bind__Int__BindLoc(drw.bnd)(j);}
        if (loc == BindLocs) break;
        Bind bnd; switch (AdvConst__Advance__Constant(drw.adv)) {
        default: {std::cerr << "invalid push adv!" << std::endl; exit(-1);}
        break; case (MicroConst): bnd = Depender__Micro__BindLoc__Bind(drw.drw)(loc);
        break; case (MemoryConst): bnd = Memoryer__Memory__BindLoc__Bind(drw.mem)(loc);
        break; case (BindConst): bnd = Binder__Bind__BindLoc__Bind(drw.bnd)(loc);}
        CmdEnum tag; Rsp rsp; Req req; int idx = 0;
        switch (drw.adv) {default: {std::cerr << "invalid push adv!" << std::endl; exit(-1);}
        break; case (MicroAdv): if (count+1 >= limit) {
            std::cerr << "invalid MicroAdv limit! " << count << " " << limit << " " << bnd << std::endl; exit(-1);}
        tag = DerCmd; rsp = Rsp{drw.drw,Memorys,bnd,loc}; req = Req{LockReq,0,drw.arg[count++],drw.arg[count++]}; idx = 0;
        break; case (SubmicAdv): if (count >= limit) {std::cerr << "invalid SubmicAdv limit!" << std::endl; exit(-1);}
        tag = IDerCmd; rsp = Rsp{Micros,Memorys,bnd,loc}; req = Req{SizeReq,0,0,0,SizeState((Micro)drw.arg[count++])}; idx = drw.arg[count++];
        break; case (BufmicAdv): if (count >= limit) {
            std::cerr << "invalid BufmicAdv limit! " << count << " " << limit << " " << bnd << std::endl; exit(-1);}
        tag = DerCmd; rsp = Rsp{Micros,Memorys,bnd,loc}; req = Req{SizeReq,0,0,0,SizeState((Micro)drw.arg[count++])}; idx = 0;
        break; case (BufnotAdv): tag = DerCmd; rsp = Rsp{Micros,Memorys,bnd,loc}; req = Req{SizeReq,0,0,0,SizeState(FalseExt)}; idx = 0;
        break; case (PresizAdv): if (count+1 >= limit) {std::cerr << "invalid PresizAdv limit!" << std::endl; exit(-1);}
        tag = PDerCmd; rsp = Rsp{Micros,Memorys,bnd,loc}; req = Req{SizeReq,0,0,0,SizeState(drw.arg[count++],drw.arg[count++])}; idx = 0;
        break; case (PrememAdv): if (count+1 >= limit) {std::cerr << "invalid PrememAdv limit!" << std::endl; exit(-1);}
        tag = PDerCmd; rsp = Rsp{Micros,drw.mem,bnd,loc}; req = Req{SizeReq,0,0,0,SizeState(drw.arg[count++],drw.arg[count++])}; idx = 0;}
        cmd<<Cmd{tag,rsp,req,idx};}
        push(cmd,log);
    }
    void push(Center *center, int sub, Fnc fnc, SmartState log) {
        Bind bnd = MemoryBind__Memory__Bind(center->mem);
        if (bnd == Binds) {std::cerr << "cannot map memory!" << std::endl; exit(-1);}
        HeapState<Cmd> cmd;
        if (fnc.pass) cmd<<Cmd{(fnc.pnow?PNowCmd:PEnqCmd),Rsp{Micros,Memorys,Binds,BindLocs},Req{},0,center,sub,fnc.pass};
        if (fnc.fail) cmd<<Cmd{(fnc.fnow?FNowCmd:FEnqCmd),Rsp{Micros,Memorys,Binds,BindLocs},Req{},0,center,sub,fnc.fail};
        if (fnc.goon) cmd<<Cmd{GoonCmd,Rsp{Micros,Memorys,Binds,BindLocs}};
        int mod = stack[bnd]->bufsiz();
        int idx = center->idx*mod; int siz = center->siz*mod; SizeState max(0,center->siz*mod);
        void *ptr = 0; switch (center->mem) {
        default: {std::cerr << "cannot copy center!" << std::endl; exit(-1);}
        break; case (Indexz): ptr = (void*)center->ind;
        break; case (Bringupz): ptr = (void*)center->ver;
        break; case (Texturez): { // TODO convert to Memoryat; should work
        ptr = datxVoidz(0,center->tex[0].dat); mod = datxVoids(center->tex[0].dat);
        idx = center->idx*mod; siz = center->siz*mod;
        max = SizeState(VkExtent2D{(uint32_t)center->tex[0].wid,(uint32_t)center->tex[0].hei});
        cmd<<Cmd{PDerCmd,Rsp{Micros,Memorys,ImageBnd,ResizeLoc},Req{SizeReq,0,0,0,max}};
        cmd<<Cmd{PDerCmd,Rsp{Micros,Memorys,BeforeBnd,BeforeLoc},Req{BothReq,0,0,0,max}};
        cmd<<Cmd{PDerCmd,Rsp{Micros,Memorys,TextureBnd,MiddleLoc},Req{BothReq,ptr,idx,siz,max}};
        cmd<<Cmd{PDerCmd,Rsp{Micros,Memorys,AfterBnd,AfterLoc},{BothReq,0,0,0,max}};
        push(cmd,log); return;}
        break; case (Uniformz): ptr = (void*)center->uni;
        break; case (Matrixz): ptr = (void*)center->mat;
        break; case (Trianglez): ptr = (void*)center->tri;
        break; case (Numericz): ptr = (void*)center->num;
        break; case (Vertexz): ptr = (void*)center->vtx;
        break; case (Basisz): ptr = (void*)center->bas;
        break; case (Piercez): ptr = (void*)center->pie;
        break; case (Drawz): for (int i = 0; i < center->siz-1; i++)
        // TODO use Configure or Draw fields to decide between registered Fnc structs
        push(center->drw[i],0,i,Fnc{true,planePass,true,planeFail,false},log);
        push(center->drw[center->siz-1],center,sub,Fnc{true,planePass,true,planeFail,false},log);
        return;
        break; case (Configurez): // TODO alias Uniform* Configure to Uniformz fields
        for (int i = 0; i < center->siz; i++)
        write(center->cfg[i],center->val[i]);
        if (fnc.pass) thread->push(0,center,0,fnc.pass,log);
        return;}
        /*if (base>idx) {
        ptr = (void*)((char*)ptr+base-idx);
        siz -= base-idx; idx = 0;}
        else {idx = idx-base;}
        if (idx+siz>size) {siz = size-idx;}*/ // TODO
        cmd<<Cmd{PDerCmd,Rsp{Micros,Memorys,bnd,BindLocs},Req{BothReq,ptr,idx,siz,max}};
        push(cmd,log);
    }
};

extern "C" {
float *planeWindow(float *mat);
float *matrc(float *u, int r, int c, int n);
}
struct TestState : public DoneState {
    SafeState safe, wake; bool goon; CopyState *copy; StackState *swap; StackState *bind;
    TestState(CopyState *copy, StackState *swap, StackState *bind) :
        safe(1), wake(0), goon(true), copy(copy), swap(swap), bind(bind) {
        strcpy(debug,"TestState");
        std::cout << debug << std::endl;
    }
    ~TestState() {
        std::cout << "~" << debug << std::endl;
    }
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
    int args[] = {(int)MicroTest,(int)MicroTest};
    copy->write(WindowLeft,-xsiz/2); copy->write(WindowBase,-ysiz/2);
    copy->write(WindowWidth,xsiz); copy->write(WindowHeight,ysiz);
    copy->write(FocalLength,10); copy->write(FocalDepth,10);
    //
    copy->push(Draw{.adv=BufnotAdv,.bnd=SwapBnd},0,0,Fnc{true,0,true,vulkanForce,false},SmartState());
    //
    for (int i = 0; i < StackState::frames; i++)
    copy->push(Draw{.adv=BufmicAdv,.bnd=DrawBnd,.siz=2,.arg=args},0,0,Fnc{},SmartState());
    //
    for (int i = 0; i < StackState::frames; i++)
    copy->push(Draw{.adv=BufmicAdv,.bnd=AcquireBnd,.siz=1,.arg=args},0,0,Fnc{},SmartState());
    //
    for (int i = 0; i < StackState::frames; i++)
    copy->push(Draw{.adv=BufmicAdv,.bnd=PresentBnd,.siz=1,.arg=args},0,0,Fnc{},SmartState());
    //
    Center *vtx = 0; allocCenter(&vtx,1);
    vtx->mem = Bringupz; vtx->siz = vertices.size(); allocVertex(&vtx->ver,vtx->siz);
    for (int i = 0; i < vtx->siz; i++) memcpy(&vtx->ver[i],&vertices[i],sizeof(Vertex));
    copy->push(vtx,0,Fnc{false,vulkanPass,false,vulkanForce,false},SmartState());
    //
    Center *ind = 0; allocCenter(&ind,1);
    int isiz = indices.size()*sizeof(uint16_t);
    ind->mem = Indexz; ind->siz = isiz/sizeof(int32_t); allocInt32(&ind->ind,ind->siz);
    memcpy(ind->ind,indices.data(),isiz);
    copy->push(ind,0,Fnc{false,vulkanPass,false,vulkanForce,false},SmartState());
    //
    Center *tex = 0; allocCenter(&tex,1);
    tex->mem = Texturez; tex->siz = 1; allocTexture(&tex->tex,tex->siz);
    fmtxStbi(&tex->tex[0].dat,&tex->tex[0].wid,&tex->tex[0].hei,&tex->tex[0].cha,"texture.jpg");
    copy->push(tex,0,Fnc{false,vulkanPass,false,vulkanForce,false},SmartState());
    //
    int sizes[] = {
    /*PiplelineBnd array index*/(int)MicroTest,
    /*AcquireBnd idx,siz*/0,0,
    /*DrawBnd idx,siz*/0,static_cast<int>(indices.size()),
    /*PresentBnd idx,siz*/0,0};
    bool temp; while (safe.wait(), temp = goon, safe.post(), temp) {
    //
    SmartState log;
    glm::mat4 model, view, proj, debug;
    BindState *bptr = bind->buffer()->getBind();
    if (!bptr) {log << "bptr continue" << std::endl; vulkanWake(0,0); continue;}
    BaseState *sptr = swap->buffer();
    if (!bptr->rinc(SwapBnd,sptr,log)) {log << "rinc continue" << std::endl; vulkanWake(0,0); continue;}
    testUpdate(sptr->getSwapChainExtent(),model,view,proj,debug);
    bptr->rdec(SwapBnd,log);
    Center *mat = 0; allocCenter(&mat,1);
    mat->mem = Matrixz; mat->siz = 4; allocMatrix(&mat->mat,mat->siz);
    memcpy(&mat->mat[0],&model,sizeof(Matrix));
    memcpy(&mat->mat[1],&view,sizeof(Matrix));
    memcpy(&mat->mat[2],&proj,sizeof(Matrix));
    memcpy(&mat->mat[3],&debug,sizeof(Matrix));
    copy->push(mat,0,Fnc{false,vulkanPass,false,vulkanPass,false},log);
    //
    copy->push(Draw{.adv=MicroAdv,.drw=MicroTest,.siz=7,.arg=sizes},
    0,0,Fnc{true,vulkanWake,true,vulkanWake,false},log);}
}

struct ForkState : public DoneState {
    Thread thd; int idx; mftype cfnc; mftype dfnc;
    ForkState (Thread thd, int idx, mftype call, mftype done) :
        thd(thd), idx(idx), cfnc(call), dfnc(done) {
        strcpy(debug,"ForkState");
    }
    void call() override {cfnc(thd,idx);}
    void done() override {dfnc(thd,idx);}
    void heap() override {delete this;}
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
    VkSurfaceCapabilitiesKHR capabilities;
    SwapState() :
        BaseState("SwapState",StackState::self),
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
        memProperties(StackState::memProperties) {
    }
    ~SwapState() {
        reset(SmartState());
    }
    VkSwapchainKHR getSwapChain() override {return swapChain;}
    VkFramebuffer getFramebuffer(int i) override {return framebuffers[i];}
    VkExtent2D getSwapChainExtent() override {return capabilities.currentExtent;}
    void resize(SmartState log) override {
        capabilities = findCapabilities(window,surface,physical);
        swapChain = createSwapChain(surface,device,getSwapChainExtent(),surfaceFormat,presentMode,
            capabilities,graphicsFamily,presentFamily);
        createSwapChainImages(device,swapChain,swapChainImages);
        // TODO copy following to PierceState
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
        log << "setup " << debug << std::endl;
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
    }
    static VkSurfaceCapabilitiesKHR findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkExtent2D chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities);
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
    PipeState() :
        BaseState("PipeState",StackState::self),
        device(StackState::device), micro((Micro)StackState::micro++),
        descriptorPool(createDescriptorPool(StackState::device,StackState::frames)),
        descriptorSetLayout(createDescriptorSetLayout(StackState::device,micro)),
        pipelineLayout(createPipelineLayout(StackState::device,descriptorSetLayout)),
        pipeline(createGraphicsPipeline(StackState::device,StackState::renderPass,pipelineLayout,micro)) {
        std::cout << debug << std::endl;
        setre(SizeState(micro),SmartState());
    }
    ~PipeState() {
        vkDestroyPipeline(device, pipeline, nullptr);
        vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
        vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);
        vkDestroyDescriptorPool(device, descriptorPool, nullptr);
        std::cout << "~" << debug << std::endl;
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
        log << "setup " << debug << std::endl;
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
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
        memProperties(StackState::memProperties) {
    }
    ~UniformState() {
        reset(SmartState());
    }
    VkBuffer getBuffer() override {
        return buffer;
    }
    int getRange() override {
        return size.size;
    }
    void resize(SmartState log) override {
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
            memProperties, buffer, memory);
        vkMapMemory(device, memory, 0, bufferSize, 0, &mapped);
    }
    void unsize(SmartState log) override {
        vkUnmapMemory(device,memory);
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
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
    VkDeviceMemory memory;
    VkCommandBuffer commandBuffer;
    VkFence fence;
    VkSemaphore after;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    BufferState() :
        BaseState("BufferState",StackState::self),
        device(StackState::device), physical(StackState::physical),
        graphics(StackState::graphics), commandPool(StackState::commandPool),
        memProperties(StackState::memProperties), flags(StackState::flags) {
    }
    ~BufferState() {
        reset(SmartState());
    }
    VkBuffer getBuffer() override {
        return buffer;
    }
    VkDeviceMemory getMemory() override {
        return memory;
    }
    int getRange() override {
        return size.size;
    }
    VkSemaphore getSemaphore() override {
        return after;
    }
    void resize(SmartState log) override {
        VkDeviceSize bufferSize = size.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | flags,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, buffer, memory);
        commandBuffer = createCommandBuffer(device,commandPool);
        fence = createFence(device);
        after = createSemaphore(device);
    }
    void unsize(SmartState log) override {
        vkWaitForFences(device, 1, &fence, VK_TRUE, UINT64_MAX);
        vkDestroySemaphore(device, after, nullptr);
        vkDestroyFence(device, fence, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
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
        (last ? last->getSemaphore() : VK_NULL_HANDLE), (next ? after : VK_NULL_HANDLE));
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

struct ImageState : public BaseState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceMemoryProperties memProperties;
    VkImage image;
    VkDeviceMemory imageMemory;
    VkImageView imageView;
    VkImageView getImageView() override {return imageView;}
    VkImage getImage() override {return image;}
    ImageState() :
        BaseState("ImageState",StackState::self),
        device(StackState::device),
        physical(StackState::physical),
        memProperties(StackState::memProperties) {
    }
    ~ImageState() {
        reset(SmartState());
    }
    void resize(SmartState log) override {
        log << "resize " << debug << std::endl;
        int texWidth = size.extent.width;
        int texHeight = size.extent.height;
        createImage(device, physical, texWidth, texHeight, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_TILING_OPTIMAL,
            VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
            memProperties, /*output*/ image, imageMemory);
        imageView = createImageView(device, image, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_ASPECT_COLOR_BIT);
    }
    void unsize(SmartState log) override {
        log << "unsize " << debug << std::endl;
        vkDestroyImageView(device, imageView, nullptr);
        vkDestroyImage(device, image, nullptr);
        vkFreeMemory(device, imageMemory, nullptr);
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
    }
};

struct LayoutState : public BaseState {
    const VkDevice device;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    VkCommandBuffer buffer;
    VkSemaphore after;
    VkFence fence;
    LayoutState() :
        BaseState("LayoutState",StackState::self),
        device(StackState::device),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool) {
    }
    ~LayoutState() {
        reset(SmartState());
    }
    VkSemaphore getSemaphore() override {
        return after;
    }
    void resize(SmartState log) override {
        log << "resize " << debug << std::endl;
        buffer = createCommandBuffer(device,commandPool);
        after = createSemaphore(device);
        fence = createFence(device);
    }
    void unsize(SmartState log) override {
        log << "unsize " << debug << std::endl;
        if (after != VK_NULL_HANDLE) vkDestroySemaphore(device, after, nullptr);
        if (fence != VK_NULL_HANDLE) vkDestroyFence(device, fence, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &buffer);
    }
    VkFence setup(void *ptr, int idx, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
        vkResetCommandBuffer(buffer, /*VkCommandBufferResetFlagBits*/ 0);
        if (loc()==AfterLoc) vkResetFences(device, 1, &fence);
        transitionImageLayout(device, graphics, buffer, dee(ImageBnd)->getImage(),
            (loc()==AfterLoc?last->getSemaphore():VK_NULL_HANDLE), (next?after:VK_NULL_HANDLE),
            (loc()==AfterLoc?fence:VK_NULL_HANDLE), VK_FORMAT_R8G8B8A8_SRGB,
            (loc()==AfterLoc?VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:VK_IMAGE_LAYOUT_UNDEFINED),
            (loc()==AfterLoc?VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL));
        return (loc()==AfterLoc?fence:VK_NULL_HANDLE);
    }
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
    }
    static void transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image,
        VkSemaphore semaphoreIn, VkSemaphore semaphoreOut, VkFence fenceOut,
        VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout);
};

struct TextureState : public BaseState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceProperties properties;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const VkPhysicalDeviceMemoryProperties memProperties;
    VkSampler textureSampler;
    VkCommandBuffer commandBuffer;
    VkSemaphore after;
    VkFence fence;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    TextureState() :
        BaseState("TextureState",StackState::self),
        device(StackState::device),
        physical(StackState::physical),
        properties(StackState::properties),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool),
        memProperties(StackState::memProperties) {
    }
    ~TextureState() {
        reset(SmartState());
    }
    VkSampler getTextureSampler() override {return textureSampler;}
    VkSemaphore getSemaphore() override {return after;}
    void resize(SmartState log) override {
        log << "resize " << debug << std::endl;
        int texWidth = size.extent.width;
        int texHeight = size.extent.height;
        textureSampler = createTextureSampler(device,properties);
        commandBuffer = createCommandBuffer(device,commandPool);
        after = createSemaphore(device);
    }
    void unsize(SmartState log) override {
        log << "unsize " << debug << std::endl;
        if (after != VK_NULL_HANDLE) vkDestroySemaphore(device, after, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkDestroySampler(device, textureSampler, nullptr);
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
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        copyTextureImage(device, graphics, memProperties, dee(ImageBnd)->getImage(), texWidth, texHeight,
            last->getSemaphore(), after, stagingBuffer, commandBuffer);
        return fence;
    }
    void upset(SmartState log) override {
        log << "upset " << debug << std::endl;
        vkUnmapMemory(device, stagingBufferMemory);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
        vkFreeMemory(device, stagingBufferMemory, nullptr);
    }
    static VkSampler createTextureSampler(VkDevice device, VkPhysicalDeviceProperties properties);
    static void copyTextureImage(VkDevice device, VkQueue graphics,
        VkPhysicalDeviceMemoryProperties memProperties, VkImage textureImage, int texWidth, int texHeight,
        VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore,
        VkBuffer stagingBuffer, VkCommandBuffer commandBuffer);
};

struct ProbeState : public BaseState {
    const VkDevice device;
    ChangeState<Configure,Configures> *copy;
    ProbeState() :
        BaseState("ProbeState",StackState::self),
        device(StackState::device),
        copy(StackState::copy) {
    }
    ~ProbeState() {
        reset(SmartState());
    }
    VkSemaphore getSemaphore() override {
        return VK_NULL_HANDLE;
    }
    void resize(SmartState log) override {
        log << "resize " << debug << std::endl;
    }
    void unsize(SmartState log) override {
        log << "usize " << debug << std::endl;
    }
    VkFence setup(void *ptr, int idx, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
        if (loc() == BeforeLoc) {
        void *mapped = 0; int32_t value;
        VkDeviceMemory memory = dee(size.bind)->getMemory();
        mapMemory(device,memory,&mapped,size.bind,copy);
        value = copy->read(ProbePoke);
        memcpy(mapped,&value,sizeof(int32_t));
        unmapMemory(device,memory);}
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        log << "upset" << std::endl;
        if (loc() == AfterLoc) {
        void *mapped = 0; int32_t value;
        VkDeviceMemory memory = dee(size.bind)->getMemory();
        mapMemory(device,memory,&mapped,size.bind,copy);
        memcpy(&value,mapped,sizeof(int32_t));
        copy->write(ProbePeek,value);
        unmapMemory(device,memory);}
    }
    static void mapMemory(VkDevice device, VkDeviceMemory memory, void **mapped,
        Bind bnd, ChangeState<Configure,Configures> *copy) {
        int idx = copy->read(ProbeIndex);
        int fie = copy->read(ProbeField);
        int dim = copy->read(ProbeDimen);
        Format fmt = BindElement__Bind__Int__Format(bnd)(fie);
        int siz = FormatSize__Format__Int(fmt);
        int bas = BindOffset__Bind__Int__Int(bnd)(fie);
        int str = BindStride__Bind__Int(bnd);
        int ofs = idx*str+bas+dim*siz;
        vkMapMemory(device, memory, VkDeviceSize(ofs), VkDeviceSize(siz), 0, mapped);
    }
    static void unmapMemory(VkDevice device, VkDeviceMemory memory) {
        vkUnmapMemory(device,memory);
    }
};

struct AcquireState : public BaseState {
    const VkDevice device;
    ChangeState<Configure,Configures> *copy;
    VkSemaphore after;
    uint32_t imageIndex;
    VkFramebuffer framebuffer;
    AcquireState() :
        BaseState("AcquireState",StackState::self),
        device(StackState::device),
        copy(StackState::copy),
        after(VK_NULL_HANDLE) {}
    ~AcquireState() {
        reset(SmartState());
    }
    VkSemaphore getSemaphore() override {
        return after;
    }
    uint32_t getImageIndex() override {
        return imageIndex;
    }
    VkFramebuffer getFramebuffer() override {
        return framebuffer;
    }
    void resize(SmartState log) override {
        after = createSemaphore(device);
        log << "resize " << debug << std::endl;
    }
    void unsize(SmartState log) override {
        vkDestroySemaphore(device, after, nullptr);
        log << "usize " << debug << std::endl;
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
        VkExtent2D frameExtent = dee(SwapBnd)->getSwapChainExtent();
        VkResult result = vkAcquireNextImageKHR(device,
        dee(SwapBnd)->getSwapChain(), UINT64_MAX, after, VK_NULL_HANDLE, &imageIndex);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) copy->wots(RegisterMask,1<<SizeMsk);
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            {std::cerr << "failed to acquire swap chain image!" << std::endl; exit(-1);}
        framebuffer = dee(SwapBnd)->getFramebuffer(imageIndex);
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        log << "upset" << std::endl;
    }
};

struct PresentState : public BaseState {
    const VkQueue present;
    ChangeState<Configure,Configures> *copy;
    PresentState() :
        BaseState("PresentState",StackState::self),
        present(StackState::present),
        copy(StackState::copy) {
    }
    ~PresentState() {
        reset(SmartState());
    }
    void resize(SmartState log) override {
        log << "resize " << debug << std::endl;
    }
    void unsize(SmartState log) override {
        log << "usize " << debug << std::endl;
    }
    VkFence setup(void *ptr, int loc, int siz, SmartState log) override {
        log << "setup " << debug << std::endl;
        if (!presentFrame(present,dee(SwapBnd)->getSwapChain(),
        dee(AcquireBnd)->getImageIndex(),last->getSemaphore()))
        copy->wots(RegisterMask,1<<SizeMsk);
        return VK_NULL_HANDLE;
    }
    void upset(SmartState log) override {
        log << "upset" << std::endl;
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
    VkSemaphore after;
    VkFence fence;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorLayout;
    VkDescriptorSet descriptorSet;
    VkCommandBuffer commandBuffer;
    InitState<BaseState *, ConstState<BaseState *>, Binds> bufptr;
    InitState<int, ConstState<int>, Binds> bufidx; int bufsiz;
    DrawState() :
        BaseState("DrawState",StackState::self),
        device(StackState::device),
        renderPass(StackState::renderPass),
        graphics(StackState::graphics),
        present(StackState::present),
        commandPool(StackState::commandPool),
        frames(StackState::frames),
        copy(StackState::copy),
        bufptr(ConstState<BaseState *>((BaseState*)0)),
        bufidx(ConstState<int>(0)), bufsiz(0) {
    }
    ~DrawState() {
        reset(SmartState());
    }
    VkSemaphore getSemaphore() override {
        return after;
    }
    void resize(SmartState log) override {
        descriptorPool = dee(PipelineBnd)->getDescriptorPool();
        descriptorLayout = dee(PipelineBnd)->getDescriptorSetLayout();
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
        log << "setup " << debug << std::endl;
        if (ptr != 0 || loc != 0) {std::cerr << "unsupported draw loc!" <<
            std::endl; exit(-1);}
        VkExtent2D swapChainExtent = dee(SwapBnd)->getSwapChainExtent();
        vkResetFences(device, 1, &fence);
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        BaseState *pipelinePtr = 0;
        BaseState *fetchPtr = 0;
        BaseState *indexPtr = 0;
        BaseState *acquirePtr = 0;
        BaseState *piercePtr = 0;
        BaseState *matrixPtr = 0;
        BaseState *imagePtr = 0;
        BaseState *texturePtr = 0;
        int index = 0;
        int matrixIdx = 0;
        int textureIdx = 0;
        for (Iter i(Rsp{size.micro,Memorys,Binds,MiddleLoc}); i(); ++i)
        if (i.isee() || i.isie()) switch (i.bnd) {
        default: {std::cerr << "invalid bind check! " << debug << " " << i.bnd << std::endl; exit(-1);}
        break; case (PipelineBnd): pipelinePtr = dee(i.bnd);
        break; case (AcquireBnd): acquirePtr = dee(i.bnd);
        break; case (IndexBnd): indexPtr = dee(i.bnd);
        break; case (BringupBnd): fetchPtr = dee(i.bnd);
        break; case (ImageBnd): imagePtr = dee(i.bnd);
        break; case (TextureBnd): texturePtr = dee(i.bnd); textureIdx = index++;
        break; case (MatrixBnd): matrixPtr = dee(i.bnd); matrixIdx = index++;
        break; case (PierceBnd): piercePtr = dee(i.bnd);} // TODO use this instead of acquirePtr for MicroDebug
        /*if (trianglePtr) {
            updateStorageDescriptor(device,trianglePtr->getBuffer(),
                trianglePtr->getRange(),pierceIdx,descriptorSet);}*/ // TODO vertexPtr and basisPtr etc for MicroSculpt
        if (matrixPtr) {
            updateUniformDescriptor(device,matrixPtr->getBuffer(),
                matrixPtr->getRange(),matrixIdx,descriptorSet);}
        if (imagePtr && texturePtr) {
            updateTextureDescriptor(device,imagePtr->getImageView(),
                texturePtr->getTextureSampler(),textureIdx,descriptorSet);}
        if (pipelinePtr && fetchPtr && indexPtr && acquirePtr) {
            recordCommandBuffer(commandBuffer,renderPass,descriptorSet,swapChainExtent,size.micro,siz,
                acquirePtr->getFramebuffer(),pipelinePtr->getPipeline(),pipelinePtr->getPipelineLayout(),
                fetchPtr->getBuffer(),indexPtr->getBuffer());
            drawFrame(commandBuffer, graphics, ptr, loc, siz, size.micro,
                acquirePtr->getSemaphore(),after,fence,VK_NULL_HANDLE);}
        else {std::cerr << "invalid bind set! " <<
            acquirePtr << " " << pipelinePtr << " " << fetchPtr << " " << indexPtr << std::endl; exit(-1);}
        return fence;
    }
    void upset(SmartState log) override {
        log << "upset" << std::endl;
    }
    static VkDescriptorSet createDescriptorSet(VkDevice device, VkDescriptorPool descriptorPool,
        VkDescriptorSetLayout descriptorSetLayout, int frames);
    static void updateStorageDescriptor(VkDevice device, VkBuffer buffer,
        int size, int index, VkDescriptorSet descriptorSet);
    static void updateUniformDescriptor(VkDevice device, VkBuffer buffer,
        int size, int index, VkDescriptorSet descriptorSet);
    static void updateTextureDescriptor(VkDevice device,
        VkImageView textureImageView, VkSampler textureSampler,
        int index, VkDescriptorSet descriptorSet);
    static void recordCommandBuffer(VkCommandBuffer commandBuffer, VkRenderPass renderPass,
        VkDescriptorSet descriptorSet, VkExtent2D renderArea, Micro micro, uint32_t indices,
        VkFramebuffer framebuffer, VkPipeline graphicsPipeline, VkPipelineLayout pipelineLayout,
        VkBuffer vertexBuffer, VkBuffer indexBuffer);
    static void drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, void *ptr, int loc, int siz, Micro micro,
        VkSemaphore acquire, VkSemaphore after, VkFence fence, VkSemaphore before);
};

struct MainState {
    WindowState windowState;
    VulkanState vulkanState;
    PhysicalState physicalState;
    LogicalState logicalState;
    ArrayState<SwapState,SwapBnd,1> swapState;
    // ArrayState<PipeState,PipelineBnd,Micros> pipelineState;
    ArrayState<PipeState,PipelineBnd,1> pipelineState;
    ArrayState<BufferState,IndexBnd,StackState::frames> indexState;
    ArrayState<BufferState,BringupBnd,StackState::frames> bringupState;
    ArrayState<ImageState,ImageBnd,StackState::frames> imageState;
    ArrayState<LayoutState,BeforeBnd,StackState::frames> beforeState;
    ArrayState<LayoutState,AfterBnd,StackState::frames> afterState;
    ArrayState<TextureState,TextureBnd,StackState::frames> textureState;
    ArrayState<UniformState,UniformBnd,StackState::frames> uniformState;
    ArrayState<UniformState,MatrixBnd,StackState::frames> matrixState;
    ArrayState<BufferState,TriangleBnd,StackState::frames> triangleState;
    ArrayState<BufferState,NumericBnd,StackState::frames> numericState;
    ArrayState<BufferState,VertexBnd,StackState::frames> vertexState;
    ArrayState<BufferState,BasisBnd,StackState::frames> basisState;
    ArrayState<BufferState,PierceBnd,StackState::frames> pierceState; // TODO change to PierceState that has getFramebuffer
    ArrayState<ProbeState,PokeBnd,StackState::frames> pokeState;
    ArrayState<ProbeState,PeekBnd,StackState::frames> peekState;
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
        swapState(&copyState,
            windowState.window,vulkanState.surface,physicalState.device,
            physicalState.surfaceFormat,physicalState.presentMode,
            physicalState.graphicsFamily,physicalState.presentFamily,
            physicalState.properties,physicalState.memProperties,
            logicalState.device,logicalState.commandPool,logicalState.renderPass,
            logicalState.imageFormat,logicalState.depthFormat,
            logicalState.graphics,logicalState.present),
        indexState(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
        bringupState(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
        triangleState(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
        enumState{
            {SwapBnd,&swapState},
            {PipelineBnd,&pipelineState},
            {IndexBnd,&indexState},
            {BringupBnd,&bringupState},
            {ImageBnd,&imageState},
            {BeforeBnd,&beforeState},
            {AfterBnd,&afterState},
            {TextureBnd,&textureState},
            {UniformBnd,&uniformState},
            {MatrixBnd,&matrixState},
            {TriangleBnd,&triangleState},
            {NumericBnd,&numericState},
            {VertexBnd,&vertexState},
            {BasisBnd,&basisState},
            {PierceBnd,&pierceState},
            {PokeBnd,&pokeState},
            {PeekBnd,&peekState},
            {AcquireBnd,&acquireState},
            {PresentBnd,&presentState},
            {DrawBnd,&drawState},
            {BindBnd,&bindState},
            {Binds,0}},
        threadState(logicalState.device,&copyState),
        copyState(&threadState,enumState),
        testState(&copyState,&swapState,&bindState) {
        std::cout << "MainState" << std::endl;
    }
    ~MainState() {
        std::cout << "~MainState" << std::endl;
    }
};
MainState *mptr = 0;

// TODO define glfw callbacks

void vulkanWake(Center *ptr, int sub) {
    mptr->testState.wake.wait();
}
void vulkanWait(Center *ptr, int sub) {
    glfwWaitEventsTimeout(0.001); // TODO move to WindowState
}
void vulkanPass(Center *ptr, int sub) {
    freeCenter(ptr); allocCenter(&ptr,0);
}
void vulkanForce(Center *ptr, int sub) {
    std::cerr << "unexpected copy fail!" << std::endl; slog.clr(); exit(-1);
}
void vulkanCopy(Center *ptr, int sub) {
    // TODO use Configure to decide between registered Fnc structs
    mptr->copyState.push(ptr,sub,Fnc{false,planePass,false,planeFail,false},SmartState());
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
const char *vulkanCmnd(int req) {
    if (req < 0 || req >= cmdl.size()) return 0;
    return cmdl[req];
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
    // TODO parse argv for arguments to main and push only unparsed to cmdl
    for (int i = 1; i < argc; i++) cmdl.push_back(argv[i]);
    // TODO pass parsed arguments to main
    MainState main;
    mptr = &main;
    main.copyState.call(RegisterOpen,vulkanBack);
    planeInit(vulkanCopy,vulkanCall,vulkanFork,vulkanInfo,vulkanJnfo,vulkanKnfo,vulkanCmnd);
    // TODO move glfw functions to WindowState
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
    VkBuffer& buffer, VkDeviceMemory& memory) {
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
    if (vkAllocateMemory(device, &allocInfo, nullptr, &memory) != VK_SUCCESS)
    {std::cerr << "failed to allocate buffer memory!" << std::endl; exit(-1);}
    vkBindBufferMemory(device, buffer, memory, 0);
}

VkSurfaceCapabilitiesKHR SwapState::findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device) {
    VkSurfaceCapabilitiesKHR capabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &capabilities);
    capabilities.currentExtent = chooseSwapExtent(window,capabilities);
    return capabilities;
}
VkExtent2D SwapState::chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities) {
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
    std::vector<VkVertexInputBindingDescription> bindingDescriptions;
    std::vector<VkVertexInputAttributeDescription> attributeDescriptions;
    if (VertexBind__Micro__Int__Bind(micro))
    for (int i = 0; VertexBind__Micro__Int__Bind(micro)(i) != Binds; i++) {
        Bind bind = VertexBind__Micro__Int__Bind(micro)(i);
        VkVertexInputBindingDescription bindingDescription{};
        bindingDescription.binding = i;
        bindingDescription.stride = BindStride__Bind__Int(bind);
        bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
        bindingDescriptions.push_back(bindingDescription);
    for (int j = 0; BindFormat__Bind__Int__Format(bind)(j) != Formats; j++) {
        VkVertexInputAttributeDescription attributeDescription{};
        attributeDescription.binding = i;
        attributeDescription.location = j;
        switch (BindFormat__Bind__Int__Format(bind)(j)) {
        default: {std::cerr << "invalid vertex format!" << std::endl; exit(-1);}
        case (VecFormat): attributeDescription.format = VK_FORMAT_R32G32B32A32_SFLOAT; break;
        case (UvecFormat): attributeDescription.format = VK_FORMAT_R32G32B32A32_UINT; break;}
        attributeDescription.offset = BindOffset__Bind__Int__Int(bind)(j);
        attributeDescriptions.push_back(attributeDescription);}}
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
void LayoutState::transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image,
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
    VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore,
    VkBuffer stagingBuffer, VkCommandBuffer commandBuffer) {
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
    int size, int index, VkDescriptorSet descriptorSet) {
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
    int size, int index, VkDescriptorSet descriptorSet) {
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
    vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
    vkCmdDrawIndexed(commandBuffer, indices, 1, 0, 0, 0);
    vkCmdEndRenderPass(commandBuffer);
    if (vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
    {std::cerr << "failed to record command buffer!" << std::endl; exit(-1);}
}
void DrawState::drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, void *ptr, int loc, int siz, Micro micro,
    VkSemaphore acquire, VkSemaphore after, VkFence fence, VkSemaphore before) {
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
