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
        depthFormat(findSupportedFormat(physicalDevice, candidates, sizeof(candidates)/sizeof(VkFormat), VK_IMAGE_TILING_OPTIMAL/*TODO stbi_load Peekz and Pokez require LINEAR*/, VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)),
        renderPass(createRenderPass(device,imageFormat,depthFormat)) {
        std::cout << "LogicalState" << std::endl;
    }
    ~LogicalState() {
        vkDestroyRenderPass(device, renderPass, nullptr);
        vkDestroyCommandPool(device, commandPool, nullptr);
        vkDestroyDevice(device, nullptr);
        std::cout << "~LogicalState" << std::endl;
    }
    static VkDevice createDevice(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily, const char **validationLayers, const char **deviceExtensions);
    static VkQueue createQueue(VkDevice device, uint32_t family);
    static VkCommandPool createCommandPool(VkDevice device, uint32_t family);
    static VkFormat findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size, VkImageTiling tiling, VkFormatFeatureFlags features);
    static VkRenderPass createRenderPass(VkDevice device, VkFormat imageFormat, VkFormat depthFormat);
};

struct BaseState;
struct StackState {
    static const int frames = 2;
    static const int images = 2;
    static const int comnds = 20;
    virtual BaseState *buffer() = 0; // no block beween push and advance
    virtual BaseState *prebuf() = 0; // current available for read while next is written
    virtual BaseState *prebuf(int i) = 0;
    virtual void advance() = 0;
    virtual void advance(int i) = 0;
    virtual Resrc buftyp() = 0;
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

template <class State, Resrc Type, int Size> struct ArrayState : public StackState {
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
        if (i < 0 || i >= Size) exit(-1);
        safe.wait(); State *ptr = &state[i]; safe.post(); return ptr;}
    void advance() override {safe.wait(); idx = (idx+1)%Size; safe.post();}
    void advance(int i) override {
        if (i < 0 || i >= Size) exit(-1);
        safe.wait(); idx = i; safe.post();}
    Resrc buftyp() override {return Type;}
    int bufsiz() override {return sizeof(State);}
    const char *bufnam() override {
        switch (Type) {
        default: exit(-1);
        case (SwapRes): return "SwapRes";
        case (PipeRes): return "PipeRes";
        case (IndexRes): return "IndexRes";
        case (BringupRes): return "BringupRes";
        case (ImageRes): return "ImageRes";
        case (UniformRes): return "UniformRes";
        case (MatrixRes): return "MatrixRes";
        case (TriangleRes): return "TriangleRes";
        case (NumericRes): return "NumericRes";
        case (VertexRes): return "VertexRes";
        case (BasisRes): return "BasisRes";
        case (PierceRes): return "PierceRes";
        case (ChainRes): return "ChainRes";
        case (DrawRes): return "DrawRes";
        case (BindRes): return "BindRes";}
        return 0;
    }
};

struct SizeState {
    Extent tag;
    union {
    struct {int base,size;};
    struct {VkImageLayout src,dst;};
    VkExtent2D extent;
    Micro micro;
    Resrc resrc;};
    SizeState() {
        tag = InitExt;
    }
    SizeState(Extent ext, int base, int size) {
        tag = ext; switch (ext) {
        default: exit(-1);
        break; case (InitExt):
        break; case (IntExt): this->base = base; this->size = size;
        break; case (FormExt): src = (VkImageLayout)base; dst = (VkImageLayout)size;
        break; case (ExtentExt): extent = VkExtent2D{(uint32_t)base,(uint32_t)size};
        break; case (MicroExt): micro = (Micro)base;
        break; case (ResrcExt): resrc = (Resrc)base;
        break; case (TrueExt):
        break; case (FalseExt):;}
    }
    SizeState(int base, int size) {
        tag = IntExt;
        this->base = base;
        this->size = size;
    }
    SizeState(VkImageLayout src, VkImageLayout dst) {
        tag = FormExt;
        this->src = src;
        this->dst = dst;
    }
    SizeState(VkExtent2D extent) {
        tag = ExtentExt;
        this->extent = extent;
    }
    SizeState(Micro micro) {
        tag = MicroExt;
        this->micro = micro;
    }
    SizeState(Resrc resrc) {
        tag = ResrcExt;
        this->resrc = resrc;
    }
    SizeState(Extent tag) {
        this->tag = tag;
    }
    bool operator==(const SizeState &other) const {
        if (tag == InitExt && other.tag == InitExt) return true;
        if (tag == IntExt && other.tag == IntExt &&
        base == other.base && size == other.size) return true;
        if (tag == FormExt && other.tag == FormExt &&
        src == other.src && dst == other.dst) return true;
        if (tag == ExtentExt && other.tag == ExtentExt &&
        extent.width == other.extent.width &&
        extent.height == other.extent.height) return true;
        if (tag == MicroExt && other.tag == MicroExt &&
        micro == other.micro) return true;
        if (tag == ResrcExt && other.tag == ResrcExt &&
        resrc == other.resrc) return true;
        if (tag == TrueExt && other.tag == TrueExt) return true;
        return false;
    }
};
std::ostream& operator<<(std::ostream& os, const SizeState& size) {
    switch (size.tag) {default: os << "MicroSize()"; break;
    case (InitExt): os << "InitSize()"; break;
    case (IntExt): os << "IntSize(" << size.base << "," << size.size << ")"; break;
    case (FormExt): os << "FormSize(" << size.src << "," << size.dst << ")"; break;
    case (ExtentExt): os << "ExtentSize(" << size.extent.width << "," << size.extent.height << ")"; break;
    case (MicroExt): os << "MicroSize(" << size.micro << ")"; break;
    case (ResrcExt): os << "ResrcSize(" << size.resrc << ")"; break;
    case (TrueExt): os << "TrueSize()"; break;
    case (FalseExt): os << "FalseSize()"; break;}
    return os;
}

struct Syn {
    VkFence fen = VK_NULL_HANDLE; VkSemaphore sem = VK_NULL_HANDLE;
};
struct BaseState;
struct Lnk {
    BaseState *ptr = 0; ResrcLoc loc;
};
struct Loc {
    SizeState max; Con con; Req req; Rsp rsp; Syn syn; Lnk lst; Lnk nxt;
};
ResrcLoc &operator*(Loc &loc) {
    return loc.con.loc;
}
struct BindState;
struct BaseState {
    StackState *item;
    SafeState safe;
    bool valid;
    int plock, rlock, wlock;
    BindState *lock;
    Loc ploc[ResrcLocs];
    int mask; // which ploc have valid max
    int nask; // which ploc max initialized since push
    char debug[64];
    BaseState(const char *name, StackState *ptr) :
        item(ptr),
        safe(1),
        valid(false),
        plock(0),
        rlock(0),
        wlock(0),
        lock(0),
        mask(0),
        nask(0),
        debug{0} {
        sprintf(debug,"%s_%s_%d",name,item->bufnam(),StackState::debug++);
        std::cout << debug << std::endl;
    }
    ~BaseState() {
        std::cout << "~" << debug << std::endl;
    }
    bool push(int pdec, int rdec, int wdec, BindState *ptr, Con con, Req req, Rsp rsp, SmartState log) {
        // reserve before pushing to thread
        safe.wait();
        if (plock-pdec || rlock-rdec || wlock-wdec) {
        log << "push lock fail " << debug << std::endl;
        safe.post(); return false;}
        ResrcLoc loc = con.loc;
        nask &= 1<<loc;
        log << "push pass " << debug << " loc:" << loc << " tag:" << req.tag << " plock:" << plock << std::endl;
        ploc[loc].req = req;
        plock += 1;
        safe.post();
        if (lock != 0 && lock != ptr) exit(-1);
        lock = ptr;
        ploc[loc].rsp = rsp;
        ploc[loc].con = con;
        return true;
    }
    void done(SmartState log) {
        // unreserve after done in thread or upon error
        safe.wait();
        if (plock <= 0) exit(-1);
        plock -= 1;
        if (plock == 0) lock = 0;
        log << "done " << debug << " " << plock << std::endl;
        safe.post();
    }
    void setre(ResrcLoc loc, Extent ext, int base, int size, SmartState log) {
        push(0,0,0,0,Con{.tag=Constants,.loc=loc},Req{SizeReq,0,0,0,ext,base,size,false},Rsp{},log);
        baseres(loc,log); baseups(loc,log);
    }
    void reset(SmartState log) {
        for (int i = 0; i < ResrcLocs; i++)
        if (ploc[i].max == SizeState(InitExt));
        else unsize(ploc[i],log);
    }
    bool recall(Loc &loc, SmartState log) {
        SizeState max = SizeState(loc.req.ext,loc.req.base,loc.req.size);
        SizeState ini = SizeState(InitExt);
        log << "recall " << max << std::endl;
        int msk = 1<<*loc;
        if (loc.max == max); else {
        if (loc.max == ini); else {mask &= ~msk; if (mask == 0) valid = false; unsize(loc,log);}
        loc.max = max;
        if (loc.max == ini); else {resize(loc,log); if (mask == 0) valid = true; mask |= msk; nask |= msk;
        return true;}}
        return false;
    }
    VkFence basesiz(ResrcLoc loc, SmartState log) {
        // resize and setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != BothReq) exit(-1);
        safe.post();
        recall(ploc[loc],log);
        return setup(ploc[loc],log);
    }
    void baseres(ResrcLoc loc, SmartState log) {
        // resize only
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != SizeReq) exit(-1);
        safe.post();
        recall(ploc[loc],log);
    }
    VkFence basesup(ResrcLoc loc, SmartState log) {
        // setup only
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != LockReq) exit(-1);
        safe.post();
        return setup(ploc[loc],log);
    }
    VkFence basexor(ResrcLoc loc, SmartState log) {
        // resize and maybe setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != ExclReq) exit(-1);
        safe.post();
        if (!recall(ploc[loc],log)) return VK_NULL_HANDLE;
        return setup(ploc[loc],log);        
    }
    void unlock(Loc &loc, SmartState log);
    void baseups(ResrcLoc loc, SmartState log) {
        // after fence triggered
        safe.wait();
        if (plock <= 0) exit(-1);
        safe.post();
        if (ploc[loc].req.pre) log << "baseups " << debug << " " << item->debug << std::endl;
        else log << "baseups " << debug << std::endl;
        if (ploc[loc].req.pre) item->advance();
        upset(ploc[loc],log);
        unlock(ploc[loc],log);
        done(log);
    }
    bool incr(bool elock, int psav, int rsav, int wsav) {
        safe.wait();
        if (plock < psav || wlock < wsav || rlock < rsav) exit(-1);
        if (!valid || plock-psav || wlock-wsav || (elock && rlock-rsav)) {
        safe.post(); return false;}
        (elock ? wlock : rlock) += 1;
        safe.post();
        return true;
    }
    void decr(bool elock) {
        safe.wait();
        if ((elock ? wlock : rlock) <= 0) exit(-1);
        (elock ? wlock : rlock) -= 1;
        safe.post();
    }
    BaseState *res(Resrc typ);
    Lnk *lnk(ResrcLoc loc, BaseState *ptr, ResrcLoc lst, Lnk *lnk) {
        if ((int)loc < 0 || (int)loc >= ResrcLocs) exit(-1);
        if (lnk) {lnk->ptr = this; lnk->loc = loc;}
        ploc[loc].lst.ptr = ptr; ploc[loc].lst.loc = lst;
        ploc[loc].nxt.ptr = 0; ploc[loc].nxt.loc = ResrcLocs;
        return &ploc[loc].nxt;
    }
    Loc &get(ResrcLoc loc) {
        if ((int)loc < 0 || (int)loc >= ResrcLocs) exit(-1);
        return ploc[loc];
    }
    Request tag(ResrcLoc loc) {
        if ((int)loc < 0 || (int)loc >= ResrcLocs) exit(-1);
        return ploc[loc].req.tag;
    }
    Resrc res() {return item->buftyp();}
    int msk() {return mask;}
    int nsk() {return nask;}
    static Loc &lst(Loc &loc) {return loc.lst.ptr->get(loc.lst.loc);}
    static Loc &nxt(Loc &loc) {return loc.nxt.ptr->get(loc.nxt.loc);}
    static Loc &lst(Loc &loc, ResrcLoc idx) {return loc.lst.ptr->get(idx);}
    static Loc &nxt(Loc &loc, ResrcLoc idx) {return loc.nxt.ptr->get(idx);}
    static VkSemaphore &sem(Loc &loc) {return loc.syn.sem;}
    static VkFence &fen(Loc &loc) {return loc.syn.fen;}
    static Request &tag(Loc &loc) {return loc.req.tag;}
    static void *&ptr(Loc &loc) {return loc.req.ptr;}
    static int &idx(Loc &loc) {return loc.req.idx;}
    static int &siz(Loc &loc) {return loc.req.siz;}
    static SizeState &max(Loc &loc) {return loc.max;}
    static Extent &ext(Loc &loc) {return loc.max.tag;}
    static Memory mem(Loc &loc) {return (loc.con.tag == MemoryCon ? loc.con.mem : Memorys);}
    virtual void unsize(Loc &loc, SmartState log) {exit(-1);}
    virtual void resize(Loc &loc, SmartState log) {exit(-1);}
    virtual VkFence setup(Loc &loc, SmartState log) {exit(-1);}
    virtual void upset(Loc &loc, SmartState log) {exit(-1);}
    virtual BindState *getBind() {exit(-1);}
    virtual VkImage getImage() {exit(-1);}
    virtual VkSwapchainKHR getSwapChain() {exit(-1);}
    virtual uint32_t getImageIndex() {exit(-1);}
    virtual ResrcLoc getImageLoc() {exit(-1);}
    virtual VkFramebuffer getFramebuffer() {exit(-1);}
    virtual VkFramebuffer getFramebuffer(int i) {exit(-1);}
    virtual VkPipeline getPipeline() {exit(-1);}
    virtual VkPipelineLayout getPipelineLayout() {exit(-1);}
    virtual VkBuffer getBuffer() {exit(-1);}
    virtual VkDeviceMemory getMemory() {exit(-1);}
    virtual int getRange() {exit(-1);}
    virtual VkImageView getImageView() {exit(-1);}
    virtual VkSampler getTextureSampler() {exit(-1);}
    virtual VkDescriptorPool getDescriptorPool() {exit(-1);}
    virtual VkDescriptorSetLayout getDescriptorSetLayout() {exit(-1);}
    virtual VkExtent2D getExtent() {exit(-1);}
    static uint32_t findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags, VkPhysicalDeviceMemoryProperties memProperties);
    static VkCommandBuffer createCommandBuffer(VkDevice device, VkCommandPool pool);
    static VkFence createFence(VkDevice device);
    static VkSemaphore createSemaphore(VkDevice device);
    static VkImageView createImageView(VkDevice device, VkImage image, VkFormat format, VkImageAspectFlags aspectFlags);
    static void createBuffer(VkDevice device, VkPhysicalDevice physical, VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties, VkBuffer& buffer, VkDeviceMemory& memory);
    static void createImage(VkDevice device, VkPhysicalDevice physical, uint32_t width, uint32_t height, VkFormat format, VkImageTiling tiling, VkImageLayout layout, VkImageUsageFlags usage, VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties, VkImage& image, VkDeviceMemory& imageMemory);
    static void createFramebuffer(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass, VkImageView swapChainImageView, VkImageView depthImageView, VkFramebuffer &framebuffer);
};

struct BindState : public BaseState {
    BaseState *bind[Resrcs];
    int psav[Resrcs];
    int rsav[Resrcs];
    int wsav[Resrcs];
    int lock; bool excl;
    HeapState<Ins> rsp;
    BindState() :
        BaseState("BindState",StackState::self),
        lock(0),
        excl(false),
        rsp(StackState::comnds) {
        for (int i = 0; i < Resrcs; i++) {
        bind[i] = 0; psav[i] = rsav[i] = wsav[i] = 0;}
    }
    BindState *getBind() override {
        safe.wait();
        if (excl) {safe.post(); return 0;}
        excl = true;
        if (lock != 0) exit(-1);
        safe.post();
        return this;
    }
    BaseState *get(Resrc i) {
        if (bind[i] == 0) exit(-1);
        return bind[i];
    }
    bool push(Resrc i, BaseState *buf, Con con, Req req, Rsp rsp, SmartState log) {
        if (!excl) exit(-1);
        log << "push " << debug << " lock:" << lock << std::endl;
        if (!buf->push(psav[i],rsav[i],wsav[i],this,con,req,rsp,log)) return false;
        if (bind[i] == 0) lock += 1;
        if (bind[i] != 0 && bind[i] != buf) exit(-1);
        bind[i] = buf;
        psav[i] += 1;
        return true;
    }
    void push(Ins ins, SmartState log) {
        if (!excl) exit(-1);
        rsp<<ins;
    }
    void done(Resrc i, SmartState log) {
        if (!excl) exit(-1);
        if (psav[i] <= 0) exit(-1);
        psav[i] -= 1;
        log << "done " << debug << " " << bind[i]->debug << " psav:" << psav[i] << " rsav:" << rsav[i] << " wsav:" << wsav[i] << " lock:" << lock << std::endl;
        if (psav[i] == 0 && rsav[i] == 0 && wsav[i] == 0) {bind[i] = 0; lock -= 1;}
        if (lock == 0) {rsp.clear(); safe.wait(); excl = false; safe.post();}
    }
    void done(Rsp rsp, SmartState log) {
        if (!excl) exit(-1);
        log << "done idx:" << rsp.idx << " siz:" << rsp.siz << "/" << this->rsp.size() << std::endl;
        if (rsp.siz > this->rsp.size()) {slog.clr(); exit(-1);}
        for (int i = 0; i < rsp.siz; i++) {
        Resrc res = this->rsp[rsp.idx+i].res;
        switch (this->rsp[rsp.idx+i].ins) {default:
        break; case (RDeeIns): case (IRDeeIns): rdec(res,log);
        break; case (WDeeIns): wdec(res,log);}}
    }
    void done(SmartState log) {
        if (!excl || lock != 0) exit(-1);
        safe.wait();
        excl = false;
        safe.post();
    }
    bool incr(Resrc i, BaseState *buf, bool elock, SmartState log) {
        if (!excl) exit(-1);
        if (bind[i] != 0 && bind[i] != buf) exit(-1);
        if (!buf->incr(elock,psav[i],rsav[i],wsav[i])) {
        if (lock == 0) {safe.wait(); excl = false; safe.post();}
        log << "incr fail " << buf->debug << std::endl;
        return false;}
        if (bind[i] == 0) lock += 1;
        bind[i] = buf;
        (elock ? wsav[i] : rsav[i]) += 1;
        log << "incr pass " << buf->debug << " lock:" << lock-1 << std::endl;
        return true;
    }
    void decr(Resrc i, bool elock, SmartState log) {
        if (!excl) exit(-1);
        if (lock <= 0 || bind[i] == 0) exit(-1);
        bind[i]->decr(elock);
        if ((elock ? wsav[i] : rsav[i]) <= 0) exit(-1);
        (elock ? wsav[i] : rsav[i]) -= 1;
        log << "decr " << debug << " " << bind[i]->debug << " psav:" << psav[i] << " rsav:" << rsav[i] << " wsav:" << wsav[i] << " lock:" << lock << std::endl;
        if (psav[i] == 0 && rsav[i] == 0 && wsav[i] == 0) {bind[i] = 0; lock -= 1;}
        if (lock == 0) {rsp.clear(); safe.wait(); excl = false; safe.post();}
    }
    bool rinc(Resrc i, BaseState *buf, SmartState log) {
        return incr(i,buf,false,log);
    }
    bool winc(Resrc i, BaseState *buf, SmartState log) {
        return incr(i,buf,true,log);
    }
    void rdec(Resrc i, SmartState log) {
        decr(i,false,log);
    }
    void wdec(Resrc i, SmartState log) {
        decr(i,true,log);
    }
};
BaseState *BaseState::res(Resrc typ) {
    if (lock == 0) exit(-1);
    return lock->get(typ);
}
void BaseState::unlock(Loc &loc, SmartState log) {
    log << "unlock " << debug << std::endl;
    if (lock) {lock->done(loc.rsp,log); lock->done(res(),log);}
}

struct Push {
    SmartState log;
    ResrcLoc loc;
    BaseState *base;
    Center *ptr;
    int sub;
    void (*fnc)(Center*,int);
    VkFence fence;
};
struct ThreadState : public DoneState {
    const VkDevice device;
    ChangeState<Configure,Configures> *copy;
    SafeState safe; SafeState wake;
    std::deque<Push> before;
    std::deque<Push> after;
    std::map<int,Push> topush;
    int seqnum;
    bool goon;
    ThreadState(VkDevice device, ChangeState<Configure,Configures> *copy) :
        device(device),
        copy(copy),
        safe(1),
        wake(0),
        seqnum(0),
        goon(true) {
        strcpy(debug,"ThreadState");
        std::cout << debug << std::endl;
    }
    ~ThreadState() {
        std::cout << "~" << debug << std::endl;
    }
    void push(Push push) {
        push.fence = VK_NULL_HANDLE;
        safe.wait();
        before.push_back(push);
        safe.post();
        wake.wake();
    }
    bool stage() {
        while (1) {while (1) {
        safe.wait();
        if (before.empty()) {safe.post(); break;}
        Push push = before.front(); before.pop_front(); safe.post();
        if (push.base) {
        Request tag = push.base->tag(push.loc);
        switch (tag) {
        default: exit(-1);
        break; case(SizeReq): push.fence = VK_NULL_HANDLE; push.base->baseres(push.loc,push.log);
        break; case(LockReq): push.fence = push.base->basesup(push.loc,push.log);
        break; case(BothReq): push.fence = push.base->basesiz(push.loc,push.log);
        break; case(ExclReq): push.fence = push.base->basexor(push.loc,push.log);}}
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
        if (after.empty()) exit(-1);
        Push push = after.front(); after.pop_front();
        if (push.fence != VK_NULL_HANDLE) {
        VkResult result = vkWaitForFences(device,1,&push.fence,VK_FALSE,NANOSECONDS);
        if (result != VK_SUCCESS) exit(-1);}
        if (push.base) push.base->baseups(push.loc,push.log);
        if (push.fnc) push.fnc(push.ptr,push.sub);
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
    Resrc key = Resrcs; StackState *val = 0;
};
struct Arg {
    Instr ins = Instrs; Resrc res = Resrcs; ResrcLoc loc; Format fmt = Formats;
};
struct Fnc {
    bool pnow = false; void (*pass)(Center*,int) = 0;
    bool fnow = false; void (*fail)(Center*,int) = 0;
    bool goon = false;
};
void vulkanForce(Center *ptr, int sub);
struct CopyState : public ChangeState<Configure,Configures> {
    ThreadState *thread;
    StackState *stack[Resrcs];
    BaseState *buffer[Resrcs];
    int first[Resrcs];
    int final[Resrcs];
    CopyState(ThreadState *thread, EnumState *stack) :
        thread(thread),
        stack{0},
        buffer{0},
        first{0},
        final{0} {
        std::cout << "CopyState" << std::endl;
        for (EnumState *i = stack; i->key != Resrcs; i++) this->stack[i->key] = i->val;
        for (int i = 0; i < Resrcs; i++) buffer[i] = 0;
    }
    ~CopyState() {
        std::cout << "~CopyState" << std::endl;
    }
    BaseState *&dst(Resrc res) {
        if ((int)res < 0 || (int)res >= Resrcs) exit(-1);
        return buffer[res];
    }
    StackState *src(Resrc res) {
        if ((int)res < 0 || (int)res >= Resrcs) exit(-1);
        return stack[res];
    }
    static int get(int *arg, int siz, int &idx) {
        if (idx >= siz) {slog.clr(); *(int*)0=0; exit(-1);}
        return arg[idx++];
    }
    static void *get(void *ptr, int siz) {
        if (!ptr) return 0;
        if (*(int*)ptr >= 0) {
        if (*(int*)ptr != siz) exit(-1);
        return (void*)(((int*)ptr)+1);}
        struct UniDat *uni = (struct UniDat *)ptr;
        if (uni->siz != siz) exit(-1);
        return uni->ptr;
    }
    void push(HeapState<Ins> &ins, Fnc fnc, Center *ptr, int sub, SmartState log) {
        // four orderings, in same list: acquire reserve submit notify
        int num = ins.size(); // number that might be reserved
        bool goon = true; while (goon) {goon = false;
        // choose buffers
        int count = 0; // actual number of reservations
        for (int i = 0; i < num; i++) {
            Resrc res = ins[i].res;
            switch (ins[i].ins) {default:
            break; case(DerIns):
            if (dst(res) == 0) {dst(res) = src(res)->buffer(); first[res] = i;}
            final[res] = i; count += 1;
            log << "DerIns " << dst(res)->debug << std::endl;
            break; case(PDerIns):
            if (dst(res) == 0) {dst(res) = src(res)->prebuf(); first[res] = i;}
            final[res] = i; count += 1;
            log << "PDerIns " << dst(res)->debug << std::endl;
            break; case(IDerIns):
            if (dst(res) == 0) {dst(res) = src(res)->prebuf(ins[i].idx); first[res] = i;}
            final[res] = i; count += 1;
            log << "IDerIns " << ins[i].idx << " " << dst(res)->debug << std::endl;
            break; case(RDeeIns): case(WDeeIns):
            if (dst(res) == 0) dst(res) = src(res)->buffer();
            count += 1;
            log << "RWDeeIns " << dst(res)->debug << std::endl;
            break; case(IRDeeIns):
            if (dst(res) == 0) dst(res) = src(res)->prebuf(ins[i].idx);
            count += 1;
            log << "IRDeeIns " << ins[i].idx << " " << dst(res)->debug << std::endl;}}
        // choose binding
        BindState *bind = 0;
        if (count > 1) bind = stack[BindRes]->buffer()->getBind();
        int lim = num; // number checked for reservation
        if (count > 1 && bind == 0) lim = -1;
        // reserve chosen
        for (int i = 0; i < num && i < lim; i++) {
            Resrc res = ins[i].res;
            switch (ins[i].ins) {default:
            break; case(DerIns): case(PDerIns): case(IDerIns):
            ins[i].req.pre = (ins[i].ins == PDerIns && final[res] == i);
            if (bind) {if (!bind->push(res,dst(res),ins[i].con,ins[i].req,ins[i].rsp,log)) lim = i;}
            else {if (!dst(res)->push(0,0,0,0,ins[i].con,ins[i].req,ins[i].rsp,log)) lim = i;}
            break; case(RDeeIns):
            if (!bind->rinc(res,dst(res),log)) lim = i;
            break; case(IRDeeIns):
            if (!bind->rinc(res,dst(res),log)) lim = i;
            break; case(WDeeIns):
            if (!bind->winc(res,dst(res),log)) lim = i;}}
        if (lim == num) {
        BaseState *last = 0;
        // link list
        Lnk *lnk = 0; ResrcLoc lst = ResrcLocs; BaseState *bas = 0;
        for (int i = 0; i < num; i++) {
            Resrc res = ins[i].res;
            switch(ins[i].ins) {default:
            break; case(DerIns): case (PDerIns): case (IDerIns):
            lnk = dst(res)->lnk(ins[i].con.loc,bas,lst,lnk);
            bas = dst(res); lst = ins[i].con.loc;}}
        // record bindings
        for (int i = 0; i < num; i++) {
            switch (ins[i].ins) {default:
            break; case(RDeeIns): case(IRDeeIns): case (WDeeIns):
            if (bind) bind->push(ins[i],log);}}
        // submit buffers
        for (int i = 0; i < num; i++) {
            Resrc res = ins[i].res;
            switch (ins[i].ins) {default:
            break; case(DerIns):
            if (first[res] == i) src(res)->advance();
            thread->push({log,ins[i].con.loc,dst(res)});
            break; case(PDerIns):
            thread->push({log,ins[i].con.loc,dst(res)});
            break; case(IDerIns):
            if (first[res] == i) src(res)->advance(ins[i].idx);
            thread->push({log,ins[i].con.loc,dst(res)});}}
        // clean up
        for (int i = 0; i < num; i++) {
            Resrc res = ins[i].res;
            switch (ins[i].ins) {default:
            break; case(DerIns): case(PDerIns): case(IDerIns):
            dst(res) = 0;
            break; case(RDeeIns): case(WDeeIns): case(IRDeeIns):
            dst(res) = 0;}}
        // notify pass
        if (fnc.pnow && fnc.pass) fnc.pass(ptr,sub);
        else if (fnc.pass) thread->push({log,ResrcLocs,0,ptr,sub,fnc.pass});
        if (bind) stack[BindRes]->advance();
        } else {
        // release reserved
        for (int i = 0; i < lim; i++) {
            Resrc res = ins[i].res;
            switch (ins[i].ins) {default:
            break; case(DerIns): case(PDerIns): case(IDerIns):
            if (bind) bind->done(res,log);
            dst(res)->done(log);
            break; case(RDeeIns): case(IRDeeIns):
            bind->rdec(res,log);
            break; case(WDeeIns):
            bind->wdec(res,log);}}
        // clean up
        for (int i = 0; i < num; i++) {
            Resrc res = ins[i].res;
            switch (ins[i].ins) {default:
            break; case(DerIns): case(PDerIns): case(IDerIns):
            dst(res) = 0;
            break; case(RDeeIns): case(WDeeIns): case(IRDeeIns):
            dst(res) = 0;}}
        // notify fail
        if (fnc.fnow && fnc.fail) fnc.fail(ptr,sub);
        else if (fnc.fail) thread->push({log,ResrcLocs,0,ptr,sub,fnc.fail});
        if (fnc.goon) goon = true;
        }}
    }
    static Rsp response(HeapState<Arg> &dot, int i, int &count, SmartState log) {
        Rsp rsp = {};
        switch (dot[i].ins) {default:
        break; case (DerIns): case (IDerIns): case(PDerIns):
        break; case (RDeeIns): case (IRDeeIns): case(WDeeIns):
        return rsp;}
        rsp.idx = count;
        for (int j = i+1; j < dot.size(); j++)
        switch (dot[j].ins) {default:
        break; case (DerIns): case (IDerIns): case(PDerIns):
        return rsp;
        break; case (RDeeIns): case (IRDeeIns): case (WDeeIns):
        count += 1; rsp.siz += 1;}
        return rsp;
    }
    static Req request(Instr ins, Format frm, void *val, int *arg, int siz, int &idx, SmartState log) {
        Req req = {Requests,0,0,0,Extents,0,0,0};
        if (ins != DerIns && ins != IDerIns && ins != PDerIns) return req;
        switch (frm) {default:
        {slog.clr(); exit(-1);}
        // ImageFrm undefined to readonly
        // WonlyFrm readonly to dst
        // RonlyFrm dst to readonly
        // PierceFrm undefined to color
        // PeekFrm color to src
        // SourceFrm src to color
        // PokeFrm color to dst
        // DestFrm dst to color
        break; case (ImageFrm):
        req.tag = ExclReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        break; case (WonlyFrm):
        req.tag = BothReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (RonlyFrm):
        req.tag = BothReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        break; case (PierceFrm):
        req.tag = ExclReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        break; case (PeekFrm):
        req.tag = BothReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (SourceFrm):
        req.tag = BothReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL; req.size = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        break; case (PokeFrm):
        req.tag = BothReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (DestFrm):
        req.tag = BothReq; req.ext = FormExt;
        req.siz = get(arg,siz,idx); req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        break; case (ExtentFrm):
        req.tag = SizeReq; req.ext = ExtentExt;
        req.base = get(arg,siz,idx); req.size = get(arg,siz,idx);
        break; case (SizeFrm):
        req.tag = SizeReq; req.ext = IntExt;
        req.base = get(arg,siz,idx); req.size = get(arg,siz,idx);
        break; case (HighFrm):
        req.tag = BothReq; req.ext = ExtentExt;
        req.ptr = val; req.siz = get(arg,siz,idx); req.base = get(arg,siz,idx); req.size = get(arg,siz,idx);
        break; case (WholeFrm):
        req.tag = BothReq; req.ext = IntExt;
        req.ptr = val; req.idx = get(arg,siz,idx); req.siz = get(arg,siz,idx); req.base = req.idx; req.size = req.siz;
        break; case (IndexFrm):
        req.tag = SizeReq; req.ext = MicroExt; req.base = get(arg,siz,idx);
        break; case (CastFrm):
        req.tag = BothReq; req.ext = ExtentExt;
        req.ptr = val; req.siz = get(arg,siz,idx); req.base = get(arg,siz,idx); req.size = get(arg,siz,idx);
        break; case (MicroFrm):
        req.tag = BothReq; req.ext = MicroExt;
        req.idx = get(arg,siz,idx); req.siz = get(arg,siz,idx); req.base = get(arg,siz,idx);
        break; case (ConstFrm):
        req.tag = SizeReq; req.ext = MicroExt;
        req.base = get(arg,siz,idx);
        break; case (FalseFrm):
        req.tag = SizeReq; req.ext = FalseExt;
        break; case (TrueFrm):
        req.tag = SizeReq; req.ext = TrueExt;
        }
        return req;
    }
    static Con constant(Instr ins, Micro typ, ResrcLoc loc, SmartState log) {
        return Con{.tag = MicroCon, .mic = typ, .loc = loc};
    }
    static Con constant(Instr ins, Memory typ, ResrcLoc loc, SmartState log) {
        return Con{.tag = MemoryCon, .mem = typ, .loc = loc};
    }
    static Con constant(Instr ins, Resrc typ, ResrcLoc loc, SmartState log) {
        return Con{.tag = ResrcCon, .res = typ, .loc = loc};
    }
    template <class Type> static Ins instruct(HeapState<Arg> &dot, int i, Type typ, void *val, int *arg, int siz, int &idx, int &count, SmartState log) {
        log << "instruct " << dot[i].ins << " " << dot[i].fmt << " " << idx << std::endl;
        int pre = (dot[i].ins == IDerIns || dot[i].ins == IRDeeIns ? get(arg,siz,idx) : 0);
        Con con = constant(dot[i].ins,typ,dot[i].loc,log);
        Req req = request(dot[i].ins,dot[i].fmt,val,arg,siz,idx,log);
        Rsp rsp = response(dot,i,count,log);
        return Ins{dot[i].ins,dot[i].res,con,req,rsp,pre};
    }
    template <class Type, class Fnc, class Arg> static bool builtin(Type &sav, Type &arg, Fnc fnc, Arg typ, int i, Type inv, SmartState log) {
        Type val = (fnc&&fnc(typ)?fnc(typ)(i):inv);
        arg = val;
        if (arg == inv) arg = sav; else sav = arg;
        return (val != inv);
    }
    static bool iterate(Memory typ, int sub, Arg &sav, Arg &dot, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {PDerIns,Resrcs,MiddleLoc,WholeFrm};
        if (builtin(sav.ins,dot.ins,MemoryIns__Memory__Int__Instr,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.res,dot.res,MemoryIns__Memory__Int__Resrc,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.loc,dot.loc,MemoryIns__Memory__Int__ResrcLoc,typ,sub,ResrcLocs,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,MemoryIns__Memory__Int__Format,typ,sub,Formats,log)) done = false;
        return !done;
    }
    static bool iterate(Resrc typ, int sub, Arg &sav, Arg &dot, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {DerIns,Resrcs,ResizeLoc,SizeFrm};
        if (builtin(sav.ins,dot.ins,ResrcIns__Resrc__Int__Instr,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.res,dot.res,ResrcIns__Resrc__Int__Resrc,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ResrcIns__Resrc__Int__ResrcLoc,typ,sub,ResrcLocs,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ResrcIns__Resrc__Int__Format,typ,sub,Formats,log)) done = false;
        return !done;
    }
    static bool iterate(Micro typ, int sub, Arg &sav, Arg &dot, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {DerIns,Resrcs,ResizeLoc,SizeFrm};
        if (builtin(sav.ins,dot.ins,MicroIns__Micro__Int__Instr,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.res,dot.res,MicroIns__Micro__Int__Resrc,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.loc,dot.loc,MicroIns__Micro__Int__ResrcLoc,typ,sub,ResrcLocs,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,MicroIns__Micro__Int__Format,typ,sub,Formats,log)) done = false;
        return !done;
    }
    template <class Type> void push(Type typ, void *val, int *arg, int siz, int &idx, Center *ptr, int sub, Fnc fnc, SmartState log) {
        HeapState<Ins> lst; int count = 0; Ins ins; Arg sav; Arg tmp; HeapState<Arg> dot;
        for (int i = 0; iterate(typ,i,sav,tmp,log); i++) dot << tmp;
        for (int i = 0; i < dot.size(); i++) lst << instruct(dot,i,typ,val,arg,siz,idx,count,log);
        if (idx != siz) exit(-1);
        push(lst,fnc,ptr,sub,log);
    }
    void push(Center *center, int sub, Fnc fnc, SmartState log) {
        auto f = MemoryIns__Memory__Int__Resrc(center->mem);
        Resrc res = (f?f(0):Resrcs);
        if (res == Resrcs) exit(-1);
        int mod = src(res)->bufsiz(); int idx = center->idx*mod; int siz = center->siz*mod;
        int arg[] = {idx,siz}; int aiz = sizeof(arg)/sizeof(int); int adx = 0;
        // TODO allow for Configure Base and Size
        switch (center->mem) {default: exit(-1);
        break; case (Indexz): push(center->mem,(void*)center->ind,arg,aiz,adx,center,sub,fnc,log);
        break; case (Bringupz): push(center->mem,(void*)center->ver,arg,aiz,adx,center,sub,fnc,log);
        break; case (Imagez): for (int k = 0; k < center->siz; k++) { // center->idx/center->siz is a range of resources
            int idx = center->idx+k; int wid = center->img[k].wid; int hei = center->img[k].hei;
            int tot = datxVoids(center->img[k].dat); int marg[] = {
            idx,wid,hei,
            idx,tot,
            idx,tot,
            idx,tot,wid,hei,
            idx,tot};
            int msiz = sizeof(marg)/sizeof(int); int midx = 0;
            push(center->mem,(void*)datxVoidz(0,center->img[k].dat),marg,msiz,midx,center,sub,fnc,log);}
        break; case (Uniformz): push(center->mem,(void*)center->uni,arg,aiz,adx,center,sub,fnc,log);
        break; case (Matrixz): push(center->mem,(void*)center->mat,arg,aiz,adx,center,sub,fnc,log);
        break; case (Trianglez): push(center->mem,(void*)center->tri,arg,aiz,adx,center,sub,fnc,log);
        break; case (Numericz): push(center->mem,(void*)center->num,arg,aiz,adx,center,sub,fnc,log);
        break; case (Vertexz): push(center->mem,(void*)center->vtx,arg,aiz,adx,center,sub,fnc,log);
        break; case (Basisz): push(center->mem,(void*)center->bas,arg,aiz,adx,center,sub,fnc,log);
        break; case (Peekz): { // center->idx is the resource and center->siz is number of locations in the resource
            VkExtent2D ext = src(SwapRes)->buffer()->getExtent(); // TODO unsafe if SwapRes is changing
            int idx = center->idx; int siz = center->siz; int wid = ext.width; int hei = ext.height;
            int tot = wid*hei*4; int marg[] = {
            idx,wid,hei,
            idx,tot,
            idx,siz,wid,hei,
            idx,tot};
            int msiz = sizeof(marg)/sizeof(int); int midx = 0;
            push(center->mem,(void*)center->eek,marg,msiz,midx,center,sub,fnc,log);}
        break; case (Pokez): { // center->idx is the resource and center->siz is number of locations in the resource
            VkExtent2D ext = src(SwapRes)->buffer()->getExtent(); // TODO unsafe if SwapRes is changing
            int idx = center->idx; int siz = center->siz; int wid = ext.width; int hei = ext.height;
            int tot = wid*hei*4; int marg[] = {
            idx,wid,hei,
            idx,tot,
            idx,siz,wid,hei,
            idx,tot};
            int msiz = sizeof(marg)/sizeof(int); int midx = 0;
            push(center->mem,(void*)center->oke,marg,msiz,midx,center,sub,fnc,log);}
        break; case (Drawz): {HeapState<Ins> ins(StackState::comnds);
        for (int i = 0; i < center->siz; i++); // TODO switch on tag to call push(mem/res/drw)
        // TODO use Configure or Draw fields to decide between registered Fnc structs
        push(ins,Fnc{true,planePass,true,planeFail,false},center,sub,log);}
        break; case (Instrz): {HeapState<Ins> ins(StackState::comnds);
        for (int i = 0; i < center->siz; i++) ins<<center->com[i];
        // TODO use Configure or Draw fields to decide between registered Fnc structs
        push(ins,Fnc{true,planePass,true,planeFail,false},center,sub,log);}
        break; case (Configurez): // TODO alias Uniform* Configure to Uniformz fields
        for (int i = 0; i < center->siz; i++) write(center->cfg[i],center->val[i]);
        if (fnc.pass) thread->push({log,ResrcLocs,0,center,0,fnc.pass});}
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
    int xsiz = 800; int ysiz = 600; int idx = 0;
    Fnc fnc = Fnc{false,0,true,vulkanForce,false};
    Fnc wfnc = Fnc{false,0,true,vulkanForce,true};
    Fnc cfnc = Fnc{false,vulkanPass,false,vulkanForce,false};
    copy->write(WindowLeft,-xsiz/2); copy->write(WindowBase,-ysiz/2);
    copy->write(WindowWidth,xsiz); copy->write(WindowHeight,ysiz);
    copy->write(FocalLength,10); copy->write(FocalDepth,10);
    //
    copy->push(SwapRes,0,0,0,idx,0,0,wfnc,SmartState());
    //
    for (int i = 0; i < StackState::frames; i++)
    copy->push(ChainRes,0,0,0,idx,0,0,fnc,SmartState());
    //
    Center *vtx = 0; allocCenter(&vtx,1);
    vtx->mem = Bringupz; vtx->siz = vertices.size(); allocVertex(&vtx->ver,vtx->siz);
    for (int i = 0; i < vtx->siz; i++) memcpy(&vtx->ver[i],&vertices[i],sizeof(Vertex));
    copy->push(vtx,0,cfnc,SmartState());
    //
    Center *ind = 0; allocCenter(&ind,1);
    int isiz = indices.size()*sizeof(uint16_t);
    ind->mem = Indexz; ind->siz = isiz/sizeof(int32_t); allocInt32(&ind->ind,ind->siz);
    memcpy(ind->ind,indices.data(),isiz);
    copy->push(ind,0,cfnc,SmartState());
    //
    Center *img = 0; allocCenter(&img,1);
    img->mem = Imagez; img->idx = 0; img->siz = 1; allocImage(&img->img,img->siz);
    fmtxStbi(&img->img[0].dat,&img->img[0].wid,&img->img[0].hei,&img->img[0].cha,"texture.jpg");
    copy->push(img,0,cfnc,SmartState());
    //
    /*VkExtent2D ext = copy->src(SwapRes)->buffer()->getExtent(); // TODO unsafe if SwapRes is changing
    Center *pie = 0; allocCenter(&pie,1);
    pie->mem = Pokez; pie->idx = 0; pie->siz = 1; allocPierce(&pie->oke,pie->siz);
    pie->oke[0].wid = ext.width/2; pie->oke[0].hei = ext.height/2; pie->oke[0].val = 0x600dbeef;
    copy->push(pie,0,cfnc,SmartState());*/
    //
    int arg[] = {
    /*DerIns ChainRes*//*req.idx*/0,/*req.siz*/static_cast<int>(indices.size()),/*req.base*/MicroTest,
    /*DerIns DrawRes*//*req.idx*/0,/*req.siz*/static_cast<int>(indices.size()),/*req.base*/MicroTest,
    /*IDeeIns PipeRes*//*ins.idx*/MicroTest,
    /*DerIns ChainRes*//*req.idx*/0,/*req.siz*/static_cast<int>(indices.size()),/*req.base*/MicroTest,
    /*IDeeIns PipeRes*//*ins.idx*/MicroTest};
    bool temp; while (safe.wait(), temp = goon, safe.post(), temp) {
    //
    SmartState mlog;
    glm::mat4 model, view, proj, debug;
    BindState *bptr = bind->buffer()->getBind();
    if (!bptr) {mlog << "bptr continue" << std::endl; vulkanWake(0,0); continue;}
    BaseState *sptr = swap->buffer();
    if (!bptr->rinc(SwapRes,sptr,mlog)) {
    mlog << "rinc continue" << std::endl; vulkanWake(0,0); continue;}
    testUpdate(sptr->getExtent(),model,view,proj,debug);
    bptr->rdec(SwapRes,mlog);
    Center *mat = 0; allocCenter(&mat,1);
    mat->mem = Matrixz; mat->siz = 4; allocMatrix(&mat->mat,mat->siz);
    memcpy(&mat->mat[0],&model,sizeof(Matrix));
    memcpy(&mat->mat[1],&view,sizeof(Matrix));
    memcpy(&mat->mat[2],&proj,sizeof(Matrix));
    memcpy(&mat->mat[3],&debug,sizeof(Matrix));
    copy->push(mat,0,Fnc{false,vulkanPass,false,vulkanPass,false},mlog);
    //
    // TODO periodically push Peekz that fills a Dat in a center from PierceRes without changing its size
    // TODO periodically push MicroDebug that writes to the entire PierceRes
    int idx = 0; copy->push(MicroTest,0,arg,sizeof(arg)/sizeof(int),idx,0,0,Fnc{true,vulkanWake,true,vulkanWake,false},SmartState());}
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
    VkExtent2D getExtent() override {return capabilities.currentExtent;}
    void resize(Loc &loc, SmartState log) override {
        capabilities = findCapabilities(window,surface,physical);
        swapChain = createSwapChain(surface,device,getExtent(),surfaceFormat,presentMode, capabilities,graphicsFamily,presentFamily);
        createSwapChainImages(device,swapChain,swapChainImages);
        swapChainImageViews.resize(swapChainImages.size());
        for (int i = 0; i < swapChainImages.size(); i++)
        swapChainImageViews[i] = createImageView(device, swapChainImages[i], imageFormat, VK_IMAGE_ASPECT_COLOR_BIT);
        createImage(device, physical, getExtent().width, getExtent().height, depthFormat, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties,/*output*/ depthImage, depthImageMemory);
        depthImageView = createImageView(device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT);
        createFramebuffers(device,getExtent(),renderPass,swapChainImageViews,depthImageView,framebuffers);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthImageMemory, nullptr);
        for (auto framebuffer : framebuffers)
            vkDestroyFramebuffer(device, framebuffer, nullptr);
        for (auto imageView : swapChainImageViews)
            vkDestroyImageView(device, imageView, nullptr);
        vkDestroySwapchainKHR(device, swapChain, nullptr);
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        return VK_NULL_HANDLE;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << std::endl;
    }
    static VkSurfaceCapabilitiesKHR findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkExtent2D chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities);
    static VkSwapchainKHR createSwapChain(VkSurfaceKHR surface, VkDevice device, VkExtent2D swapChainExtent, VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode, VkSurfaceCapabilitiesKHR capabilities, uint32_t graphicsFamily, uint32_t presentFamily);
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
        device(StackState::device),
        micro((Micro)StackState::micro++),
        descriptorPool(createDescriptorPool(StackState::device,StackState::frames)),
        descriptorSetLayout(createDescriptorSetLayout(StackState::device,micro)),
        pipelineLayout(createPipelineLayout(StackState::device,descriptorSetLayout)),
        pipeline(createGraphicsPipeline(StackState::device,StackState::renderPass,pipelineLayout,micro)) {
        std::cout << debug << " " << this <<  std::endl;
        setre(ResizeLoc,MicroExt,micro,0,SmartState());
        std::cout << "after " << debug << std::endl;
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
    void resize(Loc &loc, SmartState log) override {
    }
    void unsize(Loc &loc, SmartState log) override {
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << std::endl;
    }
    static VkDescriptorPool createDescriptorPool(VkDevice device, int frames);
    static VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, Micro micro);
    static VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout);
    static std::vector<char> readFile(const std::string& filename);
    static VkPipeline createGraphicsPipeline(VkDevice device, VkRenderPass renderPass, VkPipelineLayout pipelineLayout, Micro micro);
    static VkShaderModule createShaderModule(VkDevice device, const std::vector<char>& code);
};

struct UniformState : public BaseState {
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceMemoryProperties memProperties;
    VkBuffer buffer;
    VkDeviceMemory memory;
    int range;
    void* mapped;
    UniformState() :
        BaseState("UniformState",StackState::self),
        device(StackState::device),
        physical(StackState::physical),
        memProperties(StackState::memProperties) {
    }
    ~UniformState() {
        reset(SmartState());
    }
    VkBuffer getBuffer() override {return buffer;}
    int getRange() override {return range;}
    void resize(Loc &loc, SmartState log) override {
        range = max(loc).size;
        VkDeviceSize bufferSize = max(loc).size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
            memProperties, buffer, memory);
        vkMapMemory(device, memory, 0, bufferSize, 0, &mapped);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkUnmapMemory(device,memory);
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        int tmp = idx(loc) - max(loc).base;
        if (tmp < 0 || siz(loc) < 0 || tmp+siz(loc) > max(loc).size)
        exit(-1);
        log << "memcpy " << debug << " " << ptr(loc) << " " << idx << " " << siz(loc) << std::endl;
        memcpy((void*)((char*)mapped+tmp), ptr(loc), siz(loc));
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(Loc &loc, SmartState log) override {
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
    int range;
    VkCommandBuffer commandBuffer;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    BufferState() :
        BaseState("BufferState",StackState::self),
        device(StackState::device),
        physical(StackState::physical),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool),
        memProperties(StackState::memProperties),
        flags(StackState::flags) {
    }
    ~BufferState() {
        reset(SmartState());
    }
    VkBuffer getBuffer() override {return buffer;}
    VkDeviceMemory getMemory() override {return memory;}
    int getRange() override {return range;}
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << " " << max(loc).size << std::endl;
        range = max(loc).size;
        VkDeviceSize bufferSize = max(loc).size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | flags,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, buffer, memory);
        commandBuffer = createCommandBuffer(device,commandPool);
        fen(loc) = createFence(device);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkDestroyFence(device, fen(loc), nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        int tmp = idx(loc) - max(loc).base;
        if (tmp < 0 || siz(loc) < 0 || tmp+siz(loc) > max(loc).size)
        exit(-1);
        VkDeviceSize bufferSize = max(loc).size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, stagingBuffer, stagingBufferMemory);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, &data);
        memcpy((void*)((char*)data+tmp),ptr(loc),siz(loc));
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetFences(device, 1, &fen(loc));
        copyBuffer(device, graphics, stagingBuffer, buffer, bufferSize, commandBuffer,
        fen(loc),VK_NULL_HANDLE,VK_NULL_HANDLE);
        return fen(loc);
    }
    void upset(Loc &loc, SmartState log) override {
        vkUnmapMemory(device, stagingBufferMemory);
        vkFreeMemory(device, stagingBufferMemory, nullptr);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
    }
    static void copyBuffer(VkDevice device, VkQueue graphics, VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size, VkCommandBuffer commandBuffer, VkFence fence, VkSemaphore before, VkSemaphore after);
};

struct ImageState : public BaseState {
    // ResizeLoc create image buffer
    // BeforeLoc format for writing to image
    // MiddleLoc write data to buffer
    // AfterLoc format for use as textue
    const VkDevice device;
    const VkPhysicalDevice physical;
    const VkPhysicalDeviceProperties properties;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const VkPhysicalDeviceMemoryProperties memProperties;
    const VkFormat depthFormat;
    const VkRenderPass renderPass;
    VkImage image;
    VkDeviceMemory imageMemory;
    VkImageView imageView;
    VkExtent2D extent;
    VkImage depthImage;
    VkDeviceMemory depthMemory;
    VkImageView depthImageView;
    VkFramebuffer framebuffer;
    VkSampler textureSampler;
    VkCommandBuffer commandReform;
    VkCommandBuffer commandBefore;
    VkCommandBuffer commandBuffer;
    VkCommandBuffer commandAfter;
    // temporary between sup and ups:
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    VkImage getImage() override {return image;}
    VkDeviceMemory getMemory() override {return imageMemory;}
    VkImageView getImageView() override {return imageView;}
    VkExtent2D getExtent() override {return extent;}
    VkSampler getTextureSampler() override {return textureSampler;}
    VkFramebuffer getFramebuffer() override {return framebuffer;}
    ImageState() :
        BaseState("ImageState",StackState::self),
        device(StackState::device),
        physical(StackState::physical),
        properties(StackState::properties),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool),
        memProperties(StackState::memProperties),
        depthFormat(StackState::depthFormat),
        renderPass(StackState::renderPass) {
    }
    ~ImageState() {
        reset(SmartState());
    }
    static void range(int &x, int &y, int &w, int &h, int &tw, int &th, bool &wr, VkDeviceSize &is, Pierce *&pie, Loc &loc, Loc &got) {
        if (max(got).tag != ExtentExt) exit(-1);
        tw = max(loc).extent.width;
        th = max(loc).extent.height;
        is = tw * th * 4;
        pie = 0; x = 0; y = 0; w = tw; h = th; wr = false;
        if (idx(loc) != 0) exit(-1);
        if (mem(loc) == Pokez || mem(loc) == Peekz) {
        pie = (Pierce*)ptr(loc); x = tw; y = th; w = 0; h = 0;
        if (siz(loc) == 0) {x = 0; y = 0;}
        for (int i = 0; i < siz(loc); i++) {
        if (pie[i].wid < x) x = pie[i].wid;
        if (pie[i].hei < y) y = pie[i].hei;
        if (pie[i].wid > w) w = pie[i].wid+pie[i].wid;
        if (pie[i].hei > h) h = pie[i].hei+pie[i].hei;}
        w = w-x; h = h-y;}
        if (mem(loc) == Peekz) wr = true;
        if (x < 0 || w < 0 || x + w > tw) exit(-1);
        if (y < 0 || h < 0 || y + h > th) exit(-1);
        if (mem(loc) == Imagez && siz(loc) != is) exit(-1);
    }
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << std::endl;
        if (*loc == ResizeLoc) {
        int texWidth = max(loc).extent.width;
        int texHeight = max(loc).extent.height;
        extent = max(loc).extent;
        VkImageUsageFlagBits flags;
        if (res() == ImageRes) flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_SAMPLED_BIT | (int)VK_IMAGE_USAGE_TRANSFER_DST_BIT);
        if (res() == PierceRes) flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | (int)VK_IMAGE_USAGE_TRANSFER_DST_BIT | (int)VK_IMAGE_USAGE_TRANSFER_SRC_BIT);
        createImage(device, physical, texWidth, texHeight, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_LAYOUT_UNDEFINED, flags, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, /*output*/ image, imageMemory);
        imageView = createImageView(device, image, VK_FORMAT_R8G8B8A8_SRGB, VK_IMAGE_ASPECT_COLOR_BIT);
        if (res() == ImageRes) {
        textureSampler = createTextureSampler(device,properties);}
        if (res() == PierceRes) {
        createImage(device, physical, max(loc).extent.width, max(loc).extent.height, depthFormat, VK_IMAGE_TILING_OPTIMAL, VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties,/*output*/ depthImage, depthMemory);
        depthImageView = createImageView(device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT);
        createFramebuffer(device,max(loc).extent,renderPass,imageView,depthImageView,framebuffer);}}
        if (*loc == ReformLoc) commandReform = createCommandBuffer(device,commandPool); 
        if (*loc == BeforeLoc) commandBefore = createCommandBuffer(device,commandPool);
        if (*loc == MiddleLoc) commandBuffer = createCommandBuffer(device,commandPool);
        if (*loc == AfterLoc) commandAfter = createCommandBuffer(device,commandPool);
        sem(loc) = createSemaphore(device); // TODO as needed
        fen(loc) = createFence(device); // TODO as needed
    }
    void unsize(Loc &loc, SmartState log) override {
        log << "unsize " << debug << std::endl;
        vkDestroyFence(device, fen(loc), nullptr);
        vkDestroySemaphore(device, sem(loc), nullptr);
        if (*loc == AfterLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandAfter);
        if (*loc == MiddleLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        if (*loc == BeforeLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandBefore);
        if (*loc == ReformLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandReform);
        if (*loc == ResizeLoc) {
        if (res() == PierceRes) {
        vkDestroyFramebuffer(device, framebuffer, nullptr);
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthMemory, nullptr);}
        if (res() == ImageRes) {
        vkDestroySampler(device, textureSampler, nullptr);}
        vkDestroyImageView(device, imageView, nullptr);
        vkDestroyImage(device, image, nullptr);
        vkFreeMemory(device, imageMemory, nullptr);}
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        VkFence fence = (*loc==AfterLoc?fen(loc):VK_NULL_HANDLE);
        VkSemaphore before = (*loc!=ReformLoc?sem(lst(loc)):VK_NULL_HANDLE); // TODO if ReformLoc check nsk() for whether there is a before
        VkSemaphore after = (*loc!=AfterLoc?sem(loc):VK_NULL_HANDLE);
        Resrc rsc = ImageRes; if (mem(loc) != Imagez) rsc = PierceRes;
        if (fence != VK_NULL_HANDLE) vkResetFences(device, 1, &fence);
        if (*loc == ReformLoc) {
        log << "reform " << max(loc).src << max(loc).dst << std::endl; slog.clr();
        vkResetCommandBuffer(commandReform, /*VkCommandBufferResetFlagBits*/ 0);
        transitionImageLayout(device, graphics, commandReform, res(rsc)->getImage(), before, after, fence, VK_FORMAT_R8G8B8A8_SRGB, max(loc).src, max(loc).dst);}
        if (*loc == BeforeLoc) {
        log << "before " << max(loc).src << max(loc).dst << std::endl; slog.clr();
        vkResetCommandBuffer(commandBefore, /*VkCommandBufferResetFlagBits*/ 0);
        transitionImageLayout(device, graphics, commandBefore, res(rsc)->getImage(), before, after, fence, VK_FORMAT_R8G8B8A8_SRGB, max(loc).src, max(loc).dst);}
        if (*loc == AfterLoc) {
        log << "after " << max(loc).src << max(loc).dst << std::endl; slog.clr();
        vkResetCommandBuffer(commandAfter, /*VkCommandBufferResetFlagBits*/ 0);
        transitionImageLayout(device, graphics, commandAfter, res(rsc)->getImage(), before, after, fence, VK_FORMAT_R8G8B8A8_SRGB, max(loc).src, max(loc).dst);}
        if (*loc == MiddleLoc) {
        Pierce *pie; int x, y, w, h, texWidth, texHeight; bool write; VkDeviceSize imageSize;
        range(x,y,w,h,texWidth,texHeight,write,imageSize,pie,loc,get(ResizeLoc));
        createBuffer(device, physical, imageSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, memProperties, stagingBuffer, stagingBufferMemory);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, imageSize, 0, &data); // TODO stage only the altered range?
        if (mem(loc) == Imagez) memcpy(data, ptr(loc), siz(loc));
        if (mem(loc) == Pokez) for (int i = 0; i < siz(loc); i++) memcpy((void*)((char*)data + x + y*texWidth), &pie[i].val, sizeof(pie[i].val));
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        copyTextureImage(device, graphics, memProperties, res(rsc)->getImage(), x, y, w, h, before, after, stagingBuffer, commandBuffer, write);}
        return fence;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << " " << *loc << "(" << ResizeLoc << "," << BeforeLoc << "," << MiddleLoc << "," << AfterLoc << ")" << std::endl;
        if (*loc == MiddleLoc) {
        if (mem(loc) == Peekz) {
        Pierce *pie; int x, y, w, h, texWidth, texHeight; bool write; VkDeviceSize imageSize;
        range(x,y,w,h,texWidth,texHeight,write,imageSize,pie,loc,get(ResizeLoc));
        void* data; vkMapMemory(device, stagingBufferMemory, 0, imageSize, 0, &data);
        for (int i = 0; i < siz(loc); i++) memcpy(&pie[i].val, (void*)((char*)data + x + y*texWidth), sizeof(pie[i].val));} // TODO do Pierce::idx too
        vkUnmapMemory(device, stagingBufferMemory);
        vkDestroyBuffer(device, stagingBuffer, nullptr);
        vkFreeMemory(device, stagingBufferMemory, nullptr);}
    }
    static VkSampler createTextureSampler(VkDevice device, VkPhysicalDeviceProperties properties);
    static void copyTextureImage(VkDevice device, VkQueue graphics, VkPhysicalDeviceMemoryProperties memProperties, VkImage textureImage, int offsWidth, int offsHeight, int texWidth, int texHeight, VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore, VkBuffer stagingBuffer, VkCommandBuffer commandBuffer, bool direction);
    static void transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image, VkSemaphore semaphoreIn, VkSemaphore semaphoreOut, VkFence fenceOut, VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout);
};

struct ChainState : public BaseState {
    const VkDevice device;
    const VkQueue present;
    ChangeState<Configure,Configures> *copy;
    uint32_t imageIndex;
    ResrcLoc imageLoc;
    VkFramebuffer framebuffer;
    ChainState() :
        BaseState("ChainState",StackState::self),
        device(StackState::device),
        present(StackState::present),
        copy(StackState::copy) {}
    ~ChainState() {
        reset(SmartState());
    }
    uint32_t getImageIndex() override {return imageIndex;}
    ResrcLoc getImageLoc() override {return imageLoc;}
    VkFramebuffer getFramebuffer() override {return framebuffer;}
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << std::endl;
        if (*loc == BeforeLoc) {
        sem(loc) = createSemaphore(device);}
    }
    void unsize(Loc &loc, SmartState log) override {
        if (*loc == BeforeLoc) {
        vkDestroySemaphore(device, sem(loc), nullptr);}
        log << "usize " << debug << std::endl;
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        if (*loc == BeforeLoc) {
        VkResult result = vkAcquireNextImageKHR(device,
        res(SwapRes)->getSwapChain(), UINT64_MAX, sem(loc), VK_NULL_HANDLE, &imageIndex);
        imageLoc = (ResrcLoc)imageIndex;
        if (imageLoc < 0 || imageLoc >= ResrcLocs) exit(-1);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) copy->wots(RegisterMask,1<<SizeMsk);
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) exit(-1);
        framebuffer = res(SwapRes)->getFramebuffer(imageIndex);}
        if (*loc == AfterLoc) {
        VkSemaphore before = sem(lst(loc,(ResrcLoc)(imageIndex%(uint32_t)ResrcLocs)));
        if (!presentFrame(present,res(SwapRes)->getSwapChain(),imageIndex,before))
        copy->wots(RegisterMask,1<<SizeMsk);}
        return VK_NULL_HANDLE;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << std::endl;
    }
    static bool presentFrame(VkQueue present, VkSwapchainKHR swapChain, uint32_t imageIndex, VkSemaphore before);
};

struct DrawState : public BaseState {
    const VkDevice device;
    const VkRenderPass renderPass;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const int frames;
    ChangeState<Configure,Configures> *copy;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorLayout;
    VkDescriptorSet descriptorSet;
    VkCommandBuffer commandBuffer;
    DrawState() :
        BaseState("DrawState",StackState::self),
        device(StackState::device),
        renderPass(StackState::renderPass),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool),
        frames(StackState::frames),
        copy(StackState::copy) {
    }
    ~DrawState() {
        reset(SmartState());
    }
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << " " << res(PipeRes)->debug << std::endl; slog.clr();
        descriptorPool = res(PipeRes)->getDescriptorPool();
        descriptorLayout = res(PipeRes)->getDescriptorSetLayout();
        descriptorSet = createDescriptorSet(device,descriptorPool,descriptorLayout,frames);
        commandBuffer = createCommandBuffer(device,commandPool);
        for (int i = 0; i < ResrcLocs; i++) sem(get((ResrcLoc)i)) = createSemaphore(device);
        fen(loc) = createFence(device);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkWaitForFences(device, 1, &fen(loc), VK_TRUE, UINT64_MAX);
        for (int i = 0; i < ResrcLocs; i++) vkDestroySemaphore(device, sem(get((ResrcLoc)i)), nullptr);
        vkDestroyFence(device, fen(loc), nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeDescriptorSets(device,descriptorPool,1,&descriptorSet);
        log << "unsize " << debug << std::endl;
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << std::endl;
        if (ptr(loc) != 0 || idx(loc) != 0) exit(-1);
        vkResetFences(device, 1, &fen(loc));
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        BaseState *pipePtr = 0;
        BaseState *swapPtr = 0;
        BaseState *framePtr = 0;
        BaseState *indexPtr = 0;
        BaseState *fetchPtr = 0;
        BaseState *imagePtr = 0;
        BaseState *matrixPtr = 0;
        int imageIdx = 0;
        int matrixIdx = 0;
        int index = 0;
        bool middle = false;
        log << "micro " << debug << " " << max(loc) << std::endl;
        Arg sav; Arg tmp; HeapState<Arg> dot;
        for (int i = 0; CopyState::iterate(max(loc).micro,i,sav,tmp,log); i++) dot << tmp;
        for (int i = 0; i < dot.size(); i++)
        if (dot[i].loc == MiddleLoc && dot[i].ins == RDeeIns ||
        dot[i].loc == MiddleLoc && dot[i].ins == IRDeeIns ||
        dot[i].loc == MiddleLoc && dot[i].ins == WDeeIns)
        switch (dot[i].res) {
        default: exit(-1);
        break; case (PipeRes): pipePtr = res(PipeRes);
        break; case (SwapRes): swapPtr = res(SwapRes);
        break; case (ChainRes): framePtr = res(ChainRes);
        break; case (IndexRes): indexPtr = res(IndexRes);
        break; case (BringupRes): fetchPtr = res(BringupRes);
        break; case (ImageRes): imagePtr = res(ImageRes); imageIdx = index++;
        break; case (MatrixRes): matrixPtr = res(MatrixRes); matrixIdx = index++;}
        /*if (trianglePtr) {
            updateStorageDescriptor(device,trianglePtr->getBuffer(),
                trianglePtr->getRange(),pierceIdx,descriptorSet);}*/ // TODO vertexPtr and basisPtr etc for MicroSculpt
        if (matrixPtr) {
            if (matrixPtr->getBuffer() == VK_NULL_HANDLE) exit(-1);
            updateUniformDescriptor(device,matrixPtr->getBuffer(),matrixPtr->getRange(),matrixIdx,descriptorSet);}
        if (imagePtr) {
            updateTextureDescriptor(device,imagePtr->getImageView(),imagePtr->getTextureSampler(),imageIdx,descriptorSet);}
        if (pipePtr && framePtr && indexPtr && fetchPtr) {
            VkExtent2D extent = swapPtr->getExtent();
            recordCommandBuffer(commandBuffer,renderPass,descriptorSet,extent,max(loc).micro,siz(loc),framePtr->getFramebuffer(),pipePtr->getPipeline(),pipePtr->getPipelineLayout(),fetchPtr->getBuffer(),indexPtr->getBuffer());
            VkSemaphore after = sem(get(framePtr->getImageLoc()));
            drawFrame(commandBuffer,graphics,ptr(loc),idx(loc),siz(loc),max(loc).micro,sem(lst(loc)),after,fen(loc),VK_NULL_HANDLE);}
        else exit(-1);
        return fen(loc);
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << std::endl;
    }
    static VkDescriptorSet createDescriptorSet(VkDevice device, VkDescriptorPool descriptorPool, VkDescriptorSetLayout descriptorSetLayout, int frames);
    static void updateStorageDescriptor(VkDevice device, VkBuffer buffer,
        int size, int index, VkDescriptorSet descriptorSet);
    static void updateUniformDescriptor(VkDevice device, VkBuffer buffer,
        int size, int index, VkDescriptorSet descriptorSet);
    static void updateTextureDescriptor(VkDevice device,
        VkImageView textureImageView, VkSampler textureSampler,
        int index, VkDescriptorSet descriptorSet);
    static void recordCommandBuffer(VkCommandBuffer commandBuffer, VkRenderPass renderPass, VkDescriptorSet descriptorSet, VkExtent2D renderArea, Micro micro, uint32_t indices, VkFramebuffer framebuffer, VkPipeline graphicsPipeline, VkPipelineLayout pipelineLayout, VkBuffer vertexBuffer, VkBuffer indexBuffer);
    static void drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, void *ptr, int loc, int siz, Micro micro, VkSemaphore acquire, VkSemaphore after, VkFence fence, VkSemaphore before);
};

struct MainState {
    WindowState windowState;
    VulkanState vulkanState;
    PhysicalState physicalState;
    LogicalState logicalState;
    ArrayState<SwapState,SwapRes,1> swapState;
    ArrayState<PipeState,PipeRes,Micros> pipelineState;
    ArrayState<BufferState,IndexRes,StackState::frames> indexState;
    ArrayState<BufferState,BringupRes,StackState::frames> bringupState;
    ArrayState<ImageState,ImageRes,StackState::images> imageState;
    ArrayState<UniformState,UniformRes,StackState::frames> uniformState;
    ArrayState<UniformState,MatrixRes,StackState::frames> matrixState;
    ArrayState<BufferState,TriangleRes,StackState::frames> triangleState;
    ArrayState<BufferState,NumericRes,StackState::frames> numericState;
    ArrayState<BufferState,VertexRes,StackState::frames> vertexState;
    ArrayState<BufferState,BasisRes,StackState::frames> basisState;
    ArrayState<ImageState,PierceRes,StackState::frames> pierceState;
    ArrayState<ChainState,ChainRes,StackState::frames> chainState;
    ArrayState<DrawState,DrawRes,StackState::frames> drawState;
    ArrayState<BindState,BindRes,StackState::frames> bindState;
    EnumState enumState[Resrcs+1];
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
            {SwapRes,&swapState},
            {PipeRes,&pipelineState},
            {IndexRes,&indexState},
            {BringupRes,&bringupState},
            {ImageRes,&imageState},
            {UniformRes,&uniformState},
            {MatrixRes,&matrixState},
            {TriangleRes,&triangleState},
            {NumericRes,&numericState},
            {VertexRes,&vertexState},
            {BasisRes,&basisState},
            {PierceRes,&pierceState},
            {ChainRes,&chainState},
            {DrawRes,&drawState},
            {BindRes,&bindState},
            {Resrcs,0}},
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
    exit(-1);
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
std::vector<const char *> cfg;
const char *vulkanCmnd(int req) {
    if (req < 0 || req >= cfg.size()) return 0;
    return cfg[req];
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

extern "C" {
void whereIsExit(int val, void *arg) {
    slog.clr();
    if (val < 0) *(int*)0=0;
}
};

int main(int argc, const char **argv) {
    on_exit(whereIsExit,0);
    // TODO parse argv for arguments to main and push only unparsed to cfg
    for (int i = 1; i < argc; i++) cfg.push_back(argv[i]);
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
        if (!found) exit(-1);}}
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
    exit(-1);
    return instance;
}
VkDebugUtilsMessengerEXT VulkanState::createDebug(VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info,
    const char **validationLayers) {
    VkDebugUtilsMessengerEXT debug{};
    if (!validationLayers) return debug;
    auto func = (PFN_vkCreateDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
    if (func == nullptr || func(instance, &info, nullptr, &debug) != VK_SUCCESS)
    exit(-1);
    return debug;
}
VkSurfaceKHR VulkanState::createSurface(VkInstance instance, GLFWwindow* window) {
    VkSurfaceKHR surface;
    if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
    exit(-1);
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
    if (count == 0) exit(-1);
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
    if (!found) exit(-1);
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
    if (format.format == /*VK_FORMAT_B8G8R8A8_SRGB*/VK_FORMAT_R8G8B8A8_SRGB && format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
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
    exit(-1);
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
    exit(-1);
    return pool;
}
VkFormat LogicalState::findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size,
    VkImageTiling tiling, VkFormatFeatureFlags features) {
    for (int i = 0; i < size; i++) {
    VkFormatProperties props;
    vkGetPhysicalDeviceFormatProperties(physicalDevice, candidates[i], &props);
    if (tiling == VK_IMAGE_TILING_LINEAR && (props.linearTilingFeatures & features) == features) return candidates[i];
    else if (tiling == VK_IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures & features) == features) return candidates[i];}
    exit(-1);
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
    exit(-1);
    return renderPass;
}

uint32_t BaseState::findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags,
    VkPhysicalDeviceMemoryProperties memProperties) {
    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
    if ((filter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & flags) == flags) return i;
    exit(-1);
}
VkCommandBuffer BaseState::createCommandBuffer(VkDevice device, VkCommandPool pool) {
    VkCommandBuffer commandBuffer;
    VkCommandBufferAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    allocInfo.commandPool = pool;
    allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    allocInfo.commandBufferCount = (uint32_t)(1);
    if (vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer) != VK_SUCCESS)
    exit(-1);
    return commandBuffer;
}
VkFence BaseState::createFence(VkDevice device) {
    VkFence fence;
    VkFenceCreateInfo fenceInfo{};
    fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;
    if (vkCreateFence(device, &fenceInfo, nullptr, &fence) != VK_SUCCESS)
    exit(-1);
    return fence;
}
VkSemaphore BaseState::createSemaphore(VkDevice device) {
    VkSemaphore semaphore;
    VkSemaphoreCreateInfo semaphoreInfo{};
    semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    if (vkCreateSemaphore(device, &semaphoreInfo, nullptr, &semaphore) != VK_SUCCESS)
    exit(-1);
    return semaphore;
}
void BaseState::createImage(VkDevice device, VkPhysicalDevice physical,
    uint32_t width, uint32_t height, VkFormat format, VkImageTiling tiling, VkImageLayout layout,
    VkImageUsageFlags usage, VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties,
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
    imageInfo.initialLayout = layout;
    imageInfo.usage = usage;
    imageInfo.samples = VK_SAMPLE_COUNT_1_BIT;
    imageInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    if (vkCreateImage(device, &imageInfo, nullptr, &image) != VK_SUCCESS)
    exit(-1);
    VkMemoryRequirements memRequirements;
    vkGetImageMemoryRequirements(device, image, &memRequirements);
    VkMemoryAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize = memRequirements.size;
    allocInfo.memoryTypeIndex = findMemoryType(physical, memRequirements.memoryTypeBits, properties, memProperties);
    if (vkAllocateMemory(device, &allocInfo, nullptr, &imageMemory) != VK_SUCCESS)
    exit(-1);
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
    exit(-1);
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
    exit(-1);
    VkMemoryRequirements memRequirements;
    vkGetBufferMemoryRequirements(device, buffer, &memRequirements);
    VkMemoryAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize = memRequirements.size;
    allocInfo.memoryTypeIndex = findMemoryType(physical, memRequirements.memoryTypeBits, properties, memProperties);
    if (vkAllocateMemory(device, &allocInfo, nullptr, &memory) != VK_SUCCESS)
    exit(-1);
    vkBindBufferMemory(device, buffer, memory, 0);
}
void BaseState::createFramebuffer(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
    VkImageView swapChainImageView, VkImageView depthImageView, VkFramebuffer &framebuffer) {
    std::array<VkImageView, 2> attachments = {swapChainImageView,depthImageView};
    VkFramebufferCreateInfo framebufferInfo{};
    framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    framebufferInfo.renderPass = renderPass;
    framebufferInfo.attachmentCount = static_cast<uint32_t>(attachments.size());
    framebufferInfo.pAttachments = attachments.data();
    framebufferInfo.width = swapChainExtent.width;
    framebufferInfo.height = swapChainExtent.height;
    framebufferInfo.layers = 1;
    if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &framebuffer) != VK_SUCCESS)
    exit(-1);
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
    exit(-1);
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
    for (size_t i = 0; i < swapChainImageViews.size(); i++)
    createFramebuffer(device,swapChainExtent,renderPass,swapChainImageViews[i],depthImageView,framebuffers[i]);
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
    exit(-1);
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
    exit(-1);
    return descriptorSetLayout;
}
VkPipelineLayout PipeState::createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout) {
    VkPipelineLayout pipelineLayout;
    VkPipelineLayoutCreateInfo pipelineLayoutInfo{};
    pipelineLayoutInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pipelineLayoutInfo.setLayoutCount = 1;
    pipelineLayoutInfo.pSetLayouts = &descriptorSetLayout;
    if (vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout) != VK_SUCCESS)
    exit(-1);
    return pipelineLayout;
}
std::vector<char> PipeState::readFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::ate | std::ios::binary);
    if (!file.is_open()) exit(-1);
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
    exit(-1);
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
    if (VertexResrc__Micro__Int__Resrc(micro))
    for (int i = 0; VertexResrc__Micro__Int__Resrc(micro)(i) != Resrcs; i++) {
        Resrc res = VertexResrc__Micro__Int__Resrc(micro)(i);
        VkVertexInputBindingDescription bindingDescription{};
        bindingDescription.binding = i;
        bindingDescription.stride = ResrcStride__Resrc__Int(res);
        bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
        bindingDescriptions.push_back(bindingDescription);
    for (int j = 0; ResrcFormat__Resrc__Int__Format(res)(j) != Formats; j++) {
        VkVertexInputAttributeDescription attributeDescription{};
        attributeDescription.binding = i;
        attributeDescription.location = j;
        switch (ResrcFormat__Resrc__Int__Format(res)(j)) {
        default: exit(-1);
        case (VecFrm): attributeDescription.format = VK_FORMAT_R32G32B32A32_SFLOAT; break;
        case (UvecFrm): attributeDescription.format = VK_FORMAT_R32G32B32A32_UINT; break;}
        attributeDescription.offset = ResrcOffset__Resrc__Int__Int(res)(j);
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
    exit(-1);
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

VkSampler ImageState::createTextureSampler(VkDevice device, VkPhysicalDeviceProperties properties) {
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
    exit(-1);
    return textureSampler;
}
void ImageState::transitionImageLayout(VkDevice device, VkQueue graphics, VkCommandBuffer commandBuffer, VkImage image,
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
    switch (oldLayout) {default: exit(-1);
    break; case (VK_IMAGE_LAYOUT_UNDEFINED):
    barrier.srcAccessMask = 0;
    sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
    sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
    sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_SHADER_WRITE_BIT;
    sourceStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    break; case (VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
    sourceStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;}
    switch (newLayout) {default: exit(-1);
    break; case (VK_IMAGE_LAYOUT_UNDEFINED):
    barrier.dstAccessMask = 0;
    destinationStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL):
    barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
    destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL):
    barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
    destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL):
    barrier.dstAccessMask = VK_ACCESS_SHADER_WRITE_BIT;
    destinationStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    break; case (VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL):
    barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
    destinationStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;}
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
void ImageState::copyTextureImage(VkDevice device, VkQueue graphics,
    VkPhysicalDeviceMemoryProperties memProperties, VkImage textureImage,
    int offsWidth, int offsHeight, int texWidth, int texHeight,
    VkSemaphore beforeSemaphore, VkSemaphore afterSemaphore,
    VkBuffer stagingBuffer, VkCommandBuffer commandBuffer, bool direction) {
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
    region.imageOffset = {static_cast<int32_t>(offsWidth), static_cast<int32_t>(offsHeight), 0};
    region.imageExtent = {static_cast<uint32_t>(texWidth), static_cast<uint32_t>(texHeight), 1};
    if (direction) vkCmdCopyImageToBuffer(commandBuffer, textureImage, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, stagingBuffer, 1, &region);
    else vkCmdCopyBufferToImage(commandBuffer, stagingBuffer, textureImage, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);
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

bool ChainState::presentFrame(VkQueue present, VkSwapchainKHR swapChain, uint32_t imageIndex, VkSemaphore before) {
    VkPresentInfoKHR presentInfo{};
    presentInfo.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    VkSemaphore signalSemaphores[] = {before};
    presentInfo.waitSemaphoreCount = (before==VK_NULL_HANDLE?0:1);
    presentInfo.pWaitSemaphores = signalSemaphores;
    VkSwapchainKHR swapChains[] = {swapChain};
    uint32_t imageIndices[] = {imageIndex};
    presentInfo.swapchainCount = 1;
    presentInfo.pSwapchains = swapChains;
    presentInfo.pImageIndices = imageIndices;
    VkResult result = vkQueuePresentKHR(present, &presentInfo);
    if (result != VK_ERROR_OUT_OF_DATE_KHR && result != VK_SUBOPTIMAL_KHR && result != VK_SUCCESS)
    exit(-1);
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
    {slog.clr(); exit(-1);}
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
    exit(-1);
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
    exit(-1);
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
    exit(-1);
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
