#define GLFW_INCLUDE_VULKAN
#define _GLFW_X11
#define _GLFW_WAYLAND
#include <GLFW/glfw3.h>

#include <iostream>
#include <fstream>
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include <limits>
#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
extern "C" {
#include "proto.h"
#include "face.h"
#include "type.h"
#include "plane.h"
};
#include "stlx.h"

void vulkanExit();
#define EXIT {slog.clr();fflush(stdout);/*vulkanExit();*/*(int*)0=0;exit(-1);}

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
    static VkFormat vulkanFormat(Render i) {
        switch (i) {default:
        break; case (SwapBuf): return VK_FORMAT_R8G8B8A8_SRGB;
        break; case (DebugBuf): return VK_FORMAT_R32_SFLOAT;
        break; case (PierceBuf): return VK_FORMAT_R32_UINT;
        break; case (DepthBuf): return VK_FORMAT_R32_SFLOAT;}
        return VK_FORMAT_R8G8B8A8_SRGB;
    }
    static VkFormat vulkanFormat(Packing i) {
        switch (i) {default:
        break; case(VecFrm): return VK_FORMAT_R32G32B32A32_SFLOAT;
        break; case(UvecFrm): return VK_FORMAT_R32G32B32A32_UINT;
        break; case(UintFrm): return VK_FORMAT_R32_UINT;
        break; case(SfloatFrm): return VK_FORMAT_R32_SFLOAT;
        break; case(SrgbFrm): return VK_FORMAT_R8G8B8A8_SRGB;}
        return VK_FORMAT_R8G8B8A8_SRGB;
    }
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
        surfaceFormat(chooseSwapSurfaceFormat(vulkanFormat(SwapBuf),VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,surface,device)),
        presentMode(chooseSwapPresentMode(surface,device)),
        memProperties(findMemoryProperties(device)) {
        std::cout << "PhysicalState " << properties.deviceName << std::endl;
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
    static VkSurfaceFormatKHR chooseSwapSurfaceFormat(VkFormat format, VkColorSpaceKHR space, VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkPresentModeKHR chooseSwapPresentMode(VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkPhysicalDeviceMemoryProperties findMemoryProperties(VkPhysicalDevice device);
};
const char *PhysicalState::deviceExtensions[] = {VK_KHR_SWAPCHAIN_EXTENSION_NAME,0};

struct LogicalState {
    static const int passes = Renders;
    VkDevice device;
    VkQueue graphics;
    VkQueue present;
    VkCommandPool commandPool;
    HeapState<VkFormat,passes> imageFormat;
    VkFormat depthFormat;
    HeapState<VkRenderPass,passes> renderPass;
    static constexpr VkFormat candidates[] = {VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT};
    LogicalState(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily,
        const char **validationLayers, const char **deviceExtensions) :
        device(createDevice(physicalDevice,graphicsFamily,presentFamily,validationLayers,deviceExtensions)),
        graphics(createQueue(device,graphicsFamily)),
        present(createQueue(device,presentFamily)),
        commandPool(createCommandPool(device,graphicsFamily)),
        depthFormat(findSupportedFormat(physicalDevice, candidates, sizeof(candidates)/sizeof(VkFormat))) {
        std::cout << "LogicalState" << std::endl;
        for (int i = 0; i < passes; i++) imageFormat[i] = PhysicalState::vulkanFormat((Render)i);
        for (int i = 0; i < passes; i++) renderPass[i] = createRenderPass(device,imageFormat[i],depthFormat);
    }
    ~LogicalState() {
        for (int i = 0; i < passes; i++) vkDestroyRenderPass(device, renderPass[i], nullptr);
        vkDestroyCommandPool(device, commandPool, nullptr);
        vkDestroyDevice(device, nullptr);
        std::cout << "~LogicalState" << std::endl;
    }
    static VkDevice createDevice(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily, const char **validationLayers, const char **deviceExtensions);
    static VkQueue createQueue(VkDevice device, uint32_t family);
    static VkCommandPool createCommandPool(VkDevice device, uint32_t family);
    static VkFormat findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size);
    static VkRenderPass createRenderPass(VkDevice device, VkFormat imageFormat, VkFormat depthFormat);
};

struct ConstState {
    decltype(MemoryIns__Memory__Int__Instr) *memins;
    decltype(MemoryIns__Memory__Int__ResrcLoc) *memloc;
    decltype(MemoryIns__Memory__Int__Format) *memfmt;
    decltype(MemoryIns__Memory__Int__Resrc) *memres;
    decltype(MemoryIns__Memory__Int__Memory) *memmem;
    decltype(MemoryIns__Memory__Int__Micro) *memmic;
    decltype(MemoryIns__Memory__Int__Quality) *memkey;
    decltype(MemoryIns__Memory__Int__Default) *memdef;
    decltype(MemoryIns__Memory__Int__Int) *memval;
    decltype(ResrcIns__Resrc__Int__Instr) *resins;
    decltype(ResrcIns__Resrc__Int__ResrcLoc) *resloc;
    decltype(ResrcIns__Resrc__Int__Format) *resfmt;
    decltype(ResrcIns__Resrc__Int__Resrc) *resres;
    decltype(ResrcIns__Resrc__Int__Memory) *resmem;
    decltype(ResrcIns__Resrc__Int__Micro) *resmic;
    decltype(ResrcIns__Resrc__Int__Quality) *reskey;
    decltype(ResrcIns__Resrc__Int__Default) *resdef;
    decltype(ResrcIns__Resrc__Int__Int) *resval;
    decltype(MicroIns__Micro__Int__Instr) *micins;
    decltype(MicroIns__Micro__Int__ResrcLoc) *micloc;
    decltype(MicroIns__Micro__Int__Format) *micfmt;
    decltype(MicroIns__Micro__Int__Resrc) *micres;
    decltype(MicroIns__Micro__Int__Memory) *micmem;
    decltype(MicroIns__Micro__Int__Micro) *micmic;
    decltype(MicroIns__Micro__Int__Quality) *mickey;
    decltype(MicroIns__Micro__Int__Default) *micdef;
    decltype(MicroIns__Micro__Int__Int) *micval;
};

struct Onl {
    int hdl; Quality key; int val;
};
struct BaseState;
struct StackState {
    static const int descrs = 4; // maximum descriptor sets in use
    static const int micros = 3; // eventually equal to Micros
    static const int frames = 2; // prevent blocking on resources
    static const int images = 10; // textures and feedback framebuffers
    static const int instrs = 20; // maximum steps in a binded submit
    static const int resrcs = 2; // resource classification greediness
    static const int handls = 4; // maximum number of classifications
    virtual void qualify(int hdl, Quality key, int val) = 0;
    virtual void test(Instr ins, int hdl, Quality key, int val) = 0;
    virtual bool compare(Onl one, Onl oth) = 0;
    virtual BaseState *newbuf(int hdl, Quality key, int val) = 0;
    virtual BaseState *oldbuf(int hdl, Quality key, int val) = 0;
    virtual BaseState *getbuf(int hdl, Quality key, int val) = 0;
    virtual BaseState *idxbuf(int i) = 0;
    virtual BaseState *newbuf() = 0;
    virtual BaseState *oldbuf() = 0;
    virtual void advance(int hdl, Quality key, int val) = 0;
    virtual void advance(int i) = 0;
    virtual void advance() = 0;
    virtual int setord() = 0;
    virtual int getord(int i) = 0;
    virtual int limord(int i) = 0;
    virtual void clrord(int i) = 0;
    virtual int buftag(int i, Quality t) = 0;
    virtual Resrc buftyp() = 0;
    virtual const char *bufnam() = 0;
    static StackState* self;
    static int debug;
    static int micro;
    static int index;
    static ChangeState<Configure,Configures> *change;
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
    static HeapState<VkRenderPass,LogicalState::passes> renderPass;
    static HeapState<VkFormat,LogicalState::passes> imageFormat;
    static VkFormat depthFormat;
    static VkQueue graphics;
    static VkQueue present;
    static VkBufferUsageFlags flags;
    static ConstState *constState;
    StackState(
        ChangeState<Configure,Configures> *change,
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
        HeapState<VkRenderPass,LogicalState::passes> renderPass,
        HeapState<VkFormat,LogicalState::passes> imageFormat,
        VkFormat depthFormat,
        VkQueue graphics,
        VkQueue present) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::index = 0;
        StackState::change = change;
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
    StackState(ConstState *constState) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::index = 0;
        StackState::constState = constState;
    }
    StackState(VkBufferUsageFlags flags) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::index = 0;
        StackState::flags = flags;        
    }
    StackState() {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;        
        StackState::index = 0;
    }
};
StackState* StackState::self;
int StackState::debug;
int StackState::micro;
int StackState::index;
ChangeState<Configure,Configures> *StackState::change;
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
HeapState<VkRenderPass,LogicalState::passes> StackState::renderPass;
HeapState<VkFormat,LogicalState::passes> StackState::imageFormat;
VkFormat StackState::depthFormat;
VkQueue StackState::graphics;
VkQueue StackState::present;
VkBufferUsageFlags StackState::flags;
ConstState *StackState::constState;

struct SizeState {
    Extent tag;
    union {
    struct {int base,size;};
    struct {VkImageLayout src,dst;};
    VkExtent2D extent;
    int value;
    Micro micro;
    Resrc resrc;};
    SizeState() {
        tag = InitExt;
    }
    SizeState(Extent ext, int base, int size) {
        tag = ext; switch (ext) {
        default: EXIT
        break; case (InitExt):
        break; case (IntExt): this->base = base; this->size = size;
        break; case (FormExt): src = (VkImageLayout)base; dst = (VkImageLayout)size;
        break; case (ExtentExt): extent = VkExtent2D{(uint32_t)base,(uint32_t)size};
        break; case (FillExt): value = base;
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
    SizeState(int value) {
        tag = FillExt;
        this->value = value;
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
        if (tag == FillExt && other.tag == FillExt &&
        value == other.value) return true;
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
    case (FillExt): os << "FillSize(" << size.value << ")"; break;
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
enum Advance {
    PushAdv,
    FnceAdv,
    QualAdv,
};
struct Adv {
    Advance adv; // when to advance
    int hdl; // which to use for advance
    Quality key; // which to change upon advance
    int val; // what to change to upon advance
};
struct Unl {
    Resrc der; // index into bind->bind
    int hdl; // index into bind->bind[der]
    int dee; // offset into bind->resp
    int siz; // number to unreserve of bind->resp
};
struct Loc {
    ResrcLoc loc; SizeState max; Requ req; Unl unl; Syn syn; Lnk lst; Lnk nxt; ConstState *ary;
};
ResrcLoc &operator*(Loc &loc) {
    return loc.loc;
}
struct BindState;
struct BaseState {
    StackState *item;
    int indx;
    SafeState safe;
    bool valid;
    int plock, rlock, wlock;
    BindState *lock;
    Loc ploc[ResrcLocs];
    int mask; // which ploc have valid max
    int nask; // which ploc setup called for
    Adv adv;
    char debug[64];
    BaseState(const char *name, StackState *ptr) :
        item(ptr),
        indx(StackState::index++),
        safe(1),
        valid(false),
        plock(0),
        rlock(0),
        wlock(0),
        lock(0),
        mask(0),
        nask(0),
        debug{} {
        sprintf(debug,"%s_%s_%d",name,item->bufnam(),StackState::debug++);
        std::cout << debug << std::endl;
    }
    ~BaseState() {
        std::cout << "~" << debug << std::endl;
    }
    bool push(int pdec, int rdec, int wdec, BindState *ptr, ResrcLoc loc, Requ req, Unl unl, ConstState *ary, SmartState log) {
        // reserve before pushing to thread
        safe.wait();
        if (plock-pdec || rlock-rdec || wlock-wdec) {
        log << "push fail" << " plock-pdec:" << plock-pdec << " rlock-rdec:" << rlock-rdec << " wlock-wdec:" << wlock-wdec << " " << debug << '\n';
        safe.post(); return false;}
        log << "push pass " << debug << " loc:" << loc << '\n';
        plock += 1;
        safe.post();
        if (lock != 0 && lock != ptr) EXIT
        lock = ptr;
        ploc[loc].req = req;
        ploc[loc].unl = unl;
        ploc[loc].loc = loc;
        ploc[loc].ary = ary;
        return true;
    }
    void push(Adv adv, SmartState log) {
        safe.wait();
        if (plock <= 0) EXIT
        this->adv = adv;
        safe.post();
    }
    void fail(SmartState log) {
        safe.wait();
        if (plock != 1) EXIT
        plock = 0; lock = 0; nask = 0;
        safe.post();
    }
    void reset(SmartState log) {
        for (int i = 0; i < ResrcLocs; i++)
        if (ploc[i].max == SizeState(InitExt));
        else unsize(ploc[i],log);
    }
    bool recall(Loc &loc, SmartState log) {
        SizeState max = SizeState(loc.req.ext,loc.req.base,loc.req.size);
        SizeState ini = SizeState(InitExt);
        int msk = 1<<*loc;
        if (loc.max == max); else {
        if (loc.max == ini); else {mask &= ~msk; if (mask == 0) valid = false; unsize(loc,log);}
        loc.max = max;
        if (loc.max == ini); else {resize(loc,log); if (mask == 0) valid = true; mask |= msk;
        return true;}}
        return false;
    }
    void precall(Loc &loc, SmartState log) {
        SizeState max = SizeState(loc.req.ext,loc.req.base,loc.req.size);
        SizeState ini = SizeState(InitExt);
        int msk = 1<<*loc;
        if (loc.max == ini) {
        loc.max = max;
        if (loc.max == ini); else {resize(loc,log); if (mask == 0) valid = true; mask |= msk;}}
    }
    VkFence basesiz(ResrcLoc loc, SmartState log) {
        // resize and setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != BothReq) EXIT
        safe.post();
        recall(ploc[loc],log);
        nask |= 1<<loc;
        return setup(ploc[loc],log);
    }
    void baseres(ResrcLoc loc, SmartState log) {
        // resize only
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != SizeReq) EXIT
        safe.post();
        recall(ploc[loc],log);
    }
    VkFence basesup(ResrcLoc loc, SmartState log) {
        // setup only
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != LockReq) EXIT
        safe.post();
        nask |= 1<<loc;
        return setup(ploc[loc],log);
    }
    VkFence basexor(ResrcLoc loc, SmartState log) {
        // resize and maybe setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != ExclReq) EXIT
        safe.post();
        if (!recall(ploc[loc],log)) return VK_NULL_HANDLE;
        nask |= 1<<loc;
        return setup(ploc[loc],log);        
    }
    VkFence baseone(ResrcLoc loc, SmartState log) {
        // resize from initial and setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != OnceReq) EXIT
        safe.post();
        precall(ploc[loc],log);
        nask |= 1<<loc;
        return setup(ploc[loc],log);
    }
    void unlock(Loc &loc, SmartState log);
    void baseups(ResrcLoc loc, SmartState log) {
        // after fence triggered
        log << "baseups " << debug << '\n';
        upset(ploc[loc],log);
        unlock(ploc[loc],log);
        safe.wait();
        if (plock <= 0) EXIT
        plock -= 1;
        if (plock == 0) {
        lock = 0; nask = 0;
        if (adv.adv == FnceAdv)
        item->advance(adv.hdl,adv.key,adv.val);}
        safe.post();
    }
    bool incr(bool elock, int psav, int rsav, int wsav) {
        safe.wait();
        if (plock < psav || wlock < wsav || rlock < rsav) EXIT
        if (!valid || plock-psav || wlock-wsav || (elock && rlock-rsav)) {
        safe.post(); return false;}
        (elock ? wlock : rlock) += 1;
        safe.post();
        return true;
    }
    void decr(bool elock) {
        safe.wait();
        if ((elock ? wlock : rlock) <= 0) EXIT
        (elock ? wlock : rlock) -= 1;
        safe.post();
    }
    Lnk *lnk(ResrcLoc loc, BaseState *ptr, ResrcLoc lst, Lnk *lnk) {
        if ((int)loc < 0 || (int)loc >= ResrcLocs) EXIT
        if (lnk) {lnk->ptr = this; lnk->loc = loc;}
        ploc[loc].lst.ptr = ptr; ploc[loc].lst.loc = lst;
        ploc[loc].nxt.ptr = 0; ploc[loc].nxt.loc = ResrcLocs;
        return &ploc[loc].nxt;
    }
    Loc &get(ResrcLoc loc) {
        if ((int)loc < 0 || (int)loc >= ResrcLocs) EXIT
        return ploc[loc];
    }
    Request tag(ResrcLoc loc) {
        if ((int)loc < 0 || (int)loc >= ResrcLocs) EXIT
        return ploc[loc].req.tag;
    }
    BaseState *res(Resrc typ, int hdl); // TODO remove
    BaseState *res(Resrc typ);
    void res();
    Resrc typ() {return item->buftyp();}
    int tag(Quality tag) {return item->buftag(indx,tag);}
    bool msk(ResrcLoc loc) {return ((mask&(1<<loc)) != 0);}
    bool nsk(ResrcLoc loc) {return ((nask&(1<<loc)) != 0);}
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
    static ConstState *ary(Loc &loc) {return loc.ary;}
    virtual void unsize(Loc &loc, SmartState log) EXIT
    virtual void resize(Loc &loc, SmartState log) EXIT
    virtual VkFence setup(Loc &loc, SmartState log) EXIT
    virtual void upset(Loc &loc, SmartState log) EXIT
    virtual BindState *getBind(SmartState log) EXIT
    virtual VkImage getImage() EXIT
    virtual VkSwapchainKHR getSwapChain() EXIT
    virtual uint32_t getImageIndex() EXIT
    virtual ResrcLoc getImageLoc() EXIT
    virtual VkFramebuffer getFramebuffer() EXIT
    virtual VkFramebuffer getFramebuffer(int i) EXIT
    virtual VkPipeline getPipeline() EXIT
    virtual VkPipelineLayout getPipelineLayout() EXIT
    virtual VkBuffer getBuffer() EXIT
    virtual VkDeviceMemory getMemory() EXIT
    virtual int getRange() EXIT
    virtual VkImageView getImageView() EXIT
    virtual VkSampler getTextureSampler() EXIT
    virtual VkDescriptorPool getDescriptorPool() EXIT
    virtual VkDescriptorSetLayout getDescriptorSetLayout() EXIT
    virtual VkRenderPass getRenderPass() EXIT
    virtual VkExtent2D getExtent() EXIT
    static uint32_t findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags, VkPhysicalDeviceMemoryProperties memProperties);
    static VkCommandBuffer createCommandBuffer(VkDevice device, VkCommandPool pool);
    static VkFence createFence(VkDevice device);
    static VkSemaphore createSemaphore(VkDevice device);
    static VkImageView createImageView(VkDevice device, VkImage image, VkFormat format, VkImageAspectFlags aspectFlags);
    static void createBuffer(VkDevice device, VkPhysicalDevice physical, VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties, VkPhysicalDeviceMemoryProperties memProperties, VkBuffer& buffer, VkDeviceMemory& memory);
    static void createImage(VkDevice device, VkPhysicalDevice physical, uint32_t width, uint32_t height, VkFormat format, VkImageUsageFlags usage, VkPhysicalDeviceMemoryProperties memProperties, VkImage& image, VkDeviceMemory& imageMemory);
    static void createFramebuffer(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass, VkImageView swapChainImageView, VkImageView depthImageView, VkFramebuffer &framebuffer);
};

template <class State, Resrc Type, int Size> struct ArrayState : public StackState {
    SafeState safe;
    int idx;
    SimpleState<Size,Qualitys,StackState::resrcs> tag;
    State state[Size];
    ArrayState(
        ChangeState<Configure,Configures> *change,
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
        HeapState<VkRenderPass,LogicalState::passes> renderPass,
        HeapState<VkFormat,LogicalState::passes> imageFormat,
        VkFormat depthFormat,
        VkQueue graphics,
        VkQueue present) :
    StackState(
        change,
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
    ArrayState(ConstState *constState) : StackState(constState), safe(1), idx(0) {
    }
    ArrayState() : safe(1), idx(0) {
    }
    void test(Instr ins, int hdl, Quality key, int val) override {
        safe.wait();
        // TODO test current tags
        safe.post();
    }
    void qualify(int hdl, Quality key, int val) override { // set current tags
        safe.wait(); tag.qualify(hdl,key,val); safe.post();
    }
    bool compare(Onl one, Onl oth) override {
        safe.wait(); bool ret = (tag.get(one.hdl,one.key,one.val) == tag.get(oth.hdl,oth.key,oth.val)); safe.post(); return ret;
    }
    BaseState *newbuf(int hdl, Quality key, int val) override { // buffer of newest, with current tags
        safe.wait(); int idx = tag.newbuf(hdl,key,val); BaseState *ptr = &state[idx]; safe.post(); return ptr;
    }
    BaseState *oldbuf(int hdl, Quality key, int val) override { // buffer of oldest, with current tags
        safe.wait(); int idx = tag.oldbuf(hdl,key,val); BaseState *ptr = &state[idx]; safe.post(); return ptr;
    }
    BaseState *getbuf(int hdl, Quality key, int val) override { // buffer of oldest, with current tags
        safe.wait(); int idx = tag.getbuf(hdl,key,val); BaseState *ptr = &state[idx]; safe.post(); return ptr;
    }
    BaseState *idxbuf(int i) override {
        if (i < 0 || i >= Size) EXIT
        safe.wait(); State *ptr = &state[i]; safe.post(); return ptr;
    }
    BaseState *newbuf() override {
        safe.wait(); State *ptr = &state[idx]; safe.post(); return ptr;
    }
    BaseState *oldbuf() override {
        safe.wait(); State *ptr = &state[(idx+1)%Size]; safe.post(); return ptr;
    }
    void advance(int hdl, Quality key, int val) override { // make oldest into newest, with current tags
        safe.wait(); int i = tag.oldbuf(hdl,key,val); tag.remove(i); if (tag.insert(hdl,key,val)!=i) EXIT safe.post();
    }
    void advance(int i) override {
        if (i < 0 || i >= Size) EXIT
        safe.wait(); idx = i; safe.post();
    }
    void advance() override {
        safe.wait(); idx = (idx+1)%Size; safe.post();
    }
    int setord() override {
        safe.wait(); int idx = tag.setord(); safe.post(); return idx;
    }
    int getord(int i) override {
        safe.wait(); int idx = tag.getord(i); safe.post(); return idx;
    }
    int limord(int i) override {
        safe.wait(); int idx = tag.limord(i); safe.post(); return idx;
    }
    void clrord(int i) override {
        safe.wait(); tag.clrord(i); safe.post();
    }
    int buftag(int i, Quality t) override {
        if (i < 0 || i >= Size || t < 0 || t >= Qualitys) EXIT
        safe.wait(); int val = tag.get(i,t); safe.post(); return val;
    }
    Resrc buftyp() override {
        return Type;
    }
    const char *bufnam() override {
        switch (Type) {
        default: EXIT
        case (SwapRes): return "SwapRes";
        case (PipeRes): return "PipeRes";
        case (IndexRes): return "IndexRes";
        case (BringupRes): return "BringupRes";
        case (ImageRes): return "ImageRes";
        case (RelateRes): return "RelateRes";
        case (UniformRes): return "UniformRes";
        case (MatrixRes): return "MatrixRes";
        case (TriangleRes): return "TriangleRes";
        case (NumericRes): return "NumericRes";
        case (VertexRes): return "VertexRes";
        case (BasisRes): return "BasisRes";
        case (DebugRes): return "DebugRes";
        case (PierceRes): return "PierceRes";
        case (DepthRes): return "DepthRes";
        case (ChainRes): return "ChainRes";
        case (DrawRes): return "DrawRes";
        case (BindRes): return "BindRes";}
        return 0;
    }
};

struct Dec {
    Resrc typ;
    int hdl;
    Instr ins;
};
struct SaveState {
    BaseState *bind; int psav, rsav, wsav;
    BaseState *buf; int fst, fin; Onl onl;
    SaveState() : bind(0), psav(0), rsav(0), wsav(0), buf(0), fst(0), fin(0), onl{0,Qualitys,0} {}
};
struct BindState : public BaseState {
    HeapState<SaveState,StackState::handls> bind[Resrcs];
    int hand[Resrcs]; int size[Resrcs]; int seqn[Resrcs];
    int wrap; int lock; bool excl;
    HeapState<Dec,StackState::instrs> resp;
    BindState() :
        BaseState("BindState",StackState::self),
        wrap(0), lock(0), excl(false) {
        for (int i = 0; i < StackState::handls; i++) hand[i] = -1;
        for (int i = 0; i < StackState::handls; i++) size[i] = 0;
        for (int i = 0; i < StackState::handls; i++) seqn[i] = 0;
    }
    BindState *getBind(SmartState log) override {
        safe.wait();
        if (excl) {log << "bind fail " << debug << '\n'; safe.post(); return 0;}
        log << "bind pass " << debug << '\n';
        excl = true;
        safe.post();
        if (lock != 0) EXIT
        lock = 1;
        return this;
    }
    int &hdl(Resrc typ) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        return hand[typ];
    }
    int &siz(Resrc typ) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        return size[typ];
    }
    int &wrp() {
        if (!excl) EXIT
        return wrap;
    }
    SaveState *chk(Resrc typ) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (seqn[typ] != wrap) {hand[typ] = 0; seqn[typ] = wrap;}
        int hdl = hand[typ];
        if (size[typ] <= 0 || size[typ] > StackState::handls)
        if (hdl < 0 || hdl >= size[typ]) EXIT
        return &bind[typ][hdl];
    }
    SaveState *vld(Resrc typ) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        hand[typ] = size[typ];
        size[typ] += 1;
        return chk(typ);
    }
    BaseState *res(Resrc typ, int hdl) { // TODO remve
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (hdl < 0 || hdl >= StackState::handls) EXIT
        SaveState &ref = bind[typ][hdl];
        if (ref.bind == 0) EXIT
        return ref.bind;
    }
    BaseState *res(Resrc typ) {
        SaveState *sav = chk(typ);
        if (sav->bind == 0) EXIT
        return sav->bind;
    }
    void res() {
        if (!excl) EXIT
        wrap += 1;
    }
    bool push(Resrc typ, ResrcLoc loc, Requ req, Unl unl, ConstState *ary, SmartState log) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        int hdl = hand[typ];
        if (hdl < 0 || hdl >= StackState::handls) EXIT
        SaveState &ref = bind[typ][hdl];
        if (!ref.buf) EXIT
        if (!ref.buf->push(ref.psav,ref.rsav,ref.wsav,this,loc,req,unl,ary,log)) return false;
        log << "push " << debug << " " << ref.buf->debug << " lock:" << lock << '\n';
        if (ref.bind == 0) lock += 1;
        if (ref.bind != 0 && ref.bind != ref.buf) EXIT
        ref.bind = ref.buf;
        ref.psav += 1;
        return true;
    }
    void push(Resrc typ, Instr ins, SmartState log) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        int hdl = hand[typ];
        if (hdl < 0 || hdl >= StackState::handls) EXIT
        resp<<Dec{typ,hdl,ins};
    }
    void done(Resrc typ, int hdl, SmartState log) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (hdl < 0 || hdl >= StackState::handls) EXIT
        SaveState &ref = bind[typ][hdl];
        if (ref.psav <= 0) EXIT
        ref.psav -= 1;
        BaseState *dbg = ref.bind;
        if (ref.psav == 0 && ref.rsav == 0 && ref.wsav == 0) {
        ref.bind = 0; lock -= 1;}
        log << "done " << debug << " " << dbg->debug << " lock:" << lock << '\n';
    }
    void done(Resrc typ, SmartState log) {
        if (typ < 0 || typ >= Resrcs) EXIT
        done(typ,hand[typ],log);
    }
    void done(SmartState log) {
        if (!excl) EXIT
        if (lock == 1) {
        lock = 0; resp.clear();
        safe.wait(); excl = false; safe.post();}
    }
    void done(Unl unl, SmartState log) {
        if (!excl) EXIT
        if (unl.dee+unl.siz > resp.size()) EXIT
        for (int i = 0; i < unl.siz; i++) {
        Resrc typ = resp[unl.dee+i].typ;
        int hdl = resp[unl.dee+i].hdl;
        switch (resp[unl.dee+i].ins) {default:
        break; case (RDeeIns): case (IDeeIns): rdec(typ,hdl,log);
        break; case (WDeeIns): wdec(typ,hdl,log);}}
        done(unl.der,unl.hdl,log); done(log);
    }
    bool incr(Resrc typ, BaseState *buf, bool elock, SmartState log) {
        if (!buf) EXIT
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        int hdl = hand[typ];
        if (hdl < 0 || hdl >= StackState::handls) EXIT
        SaveState &ref = bind[typ][hdl];
        if (ref.bind && ref.bind != buf) EXIT
        if (!buf->incr(elock,ref.psav,ref.rsav,ref.wsav)) {
        log << "incr fail " << buf->debug << '\n';
        return false;}
        log << "incr " << debug << " " << buf->debug << " lock:" << lock << '\n';
        if (ref.bind == 0) lock += 1;
        ref.bind = buf;
        (elock ? ref.wsav : ref.rsav) += 1;
        return true;
    }
    void decr(Resrc typ, int hdl, bool elock, SmartState log) {
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (hdl < 0 || hdl >= StackState::handls) EXIT
        SaveState &ref = bind[typ][hdl];
        if (lock <= 0 || ref.bind == 0) EXIT
        ref.bind->decr(elock);
        if ((elock ? ref.wsav : ref.rsav) <= 0) EXIT
        (elock ? ref.wsav : ref.rsav) -= 1;
        BaseState *dbg = ref.bind;
        if (ref.psav == 0 && ref.rsav == 0 && ref.wsav == 0) {
        ref.bind = 0; lock -= 1;}
        log << "decr " << debug << " " << dbg->debug << " lock:" << lock << '\n';
    }
    bool rinc(Resrc typ, BaseState *buf, SmartState log) {
        return incr(typ,buf,false,log);
    }
    bool winc(Resrc typ, BaseState *buf, SmartState log) {
        return incr(typ,buf,true,log);
    }
    void rdec(Resrc typ, SmartState log) {
        if (typ < 0 || typ >= Resrcs) EXIT
        decr(typ,hand[typ],false,log);
    }
    void rdec(Resrc typ, int hdl, SmartState log) {
        decr(typ,hdl,false,log);
    }
    void wdec(Resrc typ, SmartState log) {
        if (typ < 0 || typ >= Resrcs) EXIT
        decr(typ,hand[typ],true,log);
    }
    void wdec(Resrc typ, int hdl, SmartState log) {
        decr(typ,hdl,true,log);
    }
};
void BaseState::res() {
    if (lock == 0) EXIT
    lock->res();
}
BaseState *BaseState::res(Resrc typ) {
    if (lock == 0) EXIT
    return lock->res(typ);
}
BaseState *BaseState::res(Resrc typ, int hdl) { // TODO remove
    if (lock == 0) EXIT
    return lock->res(typ,hdl);
}
void BaseState::unlock(Loc &loc, SmartState log) {
    if (lock) {lock->done(loc.unl,log);}
}

struct Push {
    SmartState log;
    BaseState *base; ResrcLoc loc;
    Center *ptr; int sub;
    VkFence fence;
};
struct ThreadState : public DoneState {
    const VkDevice device;
    ChangeState<Configure,Configures> *change;
    SafeState safe; SafeState wake;
    HeapState<Push> before;
    HeapState<Push> after;
    int seqnum;
    bool goon;
    ThreadState(VkDevice device, ChangeState<Configure,Configures> *change) :
        device(device),
        change(change),
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
        before << push;
        safe.post();
        wake.post();
    }
    void push(SmartState log, BaseState *base, ResrcLoc loc) {
        push({log,base,loc,0,0});
    }
    void push(SmartState log, Center *ptr, int sub) {
        push({log,0,ResrcLocs,ptr,sub});
    }
    bool stage() {
        while (1) {while (1) {
        safe.wait();
        if (before.size() == 0) {safe.post(); break;}
        Push push = before[0]; before.clear(1); safe.post();
        if (push.base) {
        Request tag = push.base->tag(push.loc);
        switch (tag) {
        default: EXIT
        break; case(SizeReq): push.fence = VK_NULL_HANDLE; push.base->baseres(push.loc,push.log);
        break; case(LockReq): push.fence = push.base->basesup(push.loc,push.log);
        break; case(BothReq): push.fence = push.base->basesiz(push.loc,push.log);
        break; case(ExclReq): push.fence = push.base->basexor(push.loc,push.log);
        break; case(OnceReq): push.fence = push.base->baseone(push.loc,push.log);}}
        after << push;}
        if (after.size() != 0) break;
        safe.wait();
        bool empty = (before.size() == 0);
        bool done = (empty && !goon);
        safe.post();
        if (done) return false;
        if (empty) wake.wait();}
        return true;
    }
    void call() override {
        while (stage()) {
        if (after.size() == 0) EXIT
        Push push = after[0]; after.clear(1);
        if (push.fence != VK_NULL_HANDLE) {
        VkResult result = vkWaitForFences(device,1,&push.fence,VK_FALSE,NANOSECONDS);
        if (result != VK_SUCCESS) EXIT}
        if (push.base) {
        push.base->baseups(push.loc,push.log);}
        if (push.ptr) {
        centerPlace(push.ptr,push.sub);}
        change->wots(RegisterMask,1<<FnceMsk);}
        vkDeviceWaitIdle(device);
    }
    void done() override {
        safe.wait();
        goon = false;
        safe.post();
        wake.post();
    }
    void heap() override {}
    void noop() override {
        wake.post();
    }
};

extern "C" {
int datxVoids(void *dat);
void *datxVoidz(int num, void *dat);
};
void vulkanWait();
struct EnumState {
    Resrc key = Resrcs; StackState *val = 0;
};
struct Arg {
    Instr ins = Instrs;
    ResrcLoc loc; Format fmt = Formats; Quality key = Qualitys;
    Resrc res = Resrcs; Memory mem = Memorys; Micro mic = Micros;
};
struct CopyState {
    ChangeState<Configure,Configures> *change;
    ThreadState *thread;
    StackState *stack[Resrcs];
    ConstState *array;
    CopyState(ChangeState<Configure,Configures> *change,
        ThreadState *thread, EnumState *stack, ConstState *ary) :
        change(change),
        thread(thread),
        stack{},
        array(ary) {
        std::cout << "CopyState" << std::endl;
        for (EnumState *i = stack; i->key != Resrcs; i++) this->stack[i->key] = i->val;
    }
    ~CopyState() {
        std::cout << "~CopyState" << std::endl;
    }
    StackState *src(Resrc res) {
        if ((int)res < 0 || (int)res >= Resrcs) EXIT
        return stack[res];
    }
    bool vld(Resrc typ, Onl onl, BindState *bnd) {
        if (!bnd->siz(typ)) return false;
        return src(typ)->compare(bnd->chk(typ)->onl,onl);
    }
    static int get(int *arg, int siz, int &idx, SmartState log, const char *str) {
        log << "get:" << str << ":" << idx << '\n';
        if (idx >= siz) {std::cerr << "not enough int arguments " << idx << ">=" << siz << std::endl; EXIT}
        int tmp = idx; idx += 1;
        return arg[tmp];
    }
    BaseState *get(Instr ins, Resrc res, int idx, Quality key, int val) {
        switch (ins) {default: break;
        case(QDerIns): case(TDerIns):
        case(WDeeIns): case(TDeeIns):
        case(RDeeIns): case(SDeeIns):
        return src(res)->newbuf(idx,key,val);
        break; case(PDerIns): case(SDerIns):
        return src(res)->getbuf(idx,key,val);
        break; case(ODerIns): case(RDerIns):
        return src(res)->oldbuf(idx,key,val);
        break; case(IDerIns): case(IDeeIns):
        return src(res)->idxbuf(idx);}
        return 0;
    }
    Onl get(Instr ins, int idx, Quality key, int val) {
        switch (ins) {default: break;
        case(QDerIns): case(TDerIns):
        case(PDerIns): case(SDerIns):
        case(ODerIns): case(RDerIns):
        case(WDeeIns): case(TDeeIns):
        case(RDeeIns): case(SDeeIns):
        return {idx,key,val};}
        return {0,Qualitys,0};
    }
    SaveState *vld(Inst &ins, BindState *bind, SmartState log) {
        switch (ins.ins) {default:
        break; case(QDerIns): case(TDerIns): case(PDerIns): case(SDerIns):
        case(ODerIns): case(RDerIns): case(IDerIns):
        case(WDeeIns): case(TDeeIns): case(RDeeIns): case(SDeeIns): case(IDeeIns): {
        Onl onl = get(ins.ins,ins.idx,ins.key,ins.val);
        if (!vld(ins.res,onl,bind)) bind->hdl(ins.res) += 1;
        if (!vld(ins.res,onl,bind)) EXIT
        return bind->vld(ins.res);}}
        return 0;
    }
    void push(HeapState<Inst,StackState::instrs> &ins, Center *ptr, int sub, Rsp rsp, SmartState log) {
        // four orderings, in same list: acquire reserve submit notify
        int num = ins.size(); // number that might be reserved
        bool goon = true; while (goon) {goon = false;
        log << "count depends" << '\n';
        int count = 0;
        for (int i = 0; i < num; i++) {
            switch (ins[i].ins) {default:
            break; case(QDerIns): case(PDerIns): case(ODerIns): count += 1;
            break; case(TDerIns): case(SDerIns): case(RDerIns): case(IDerIns): count += 1;
            break; case(WDeeIns): case(RDeeIns): count += 1;
            break; case(TDeeIns): case(SDeeIns): case(IDeeIns): count += 1;}}
        log << "choose binding" << '\n';
        BindState *bind = 0; int min = 0;
        if (count > min) bind = stack[BindRes]->getbuf(0,Qualitys,0)->getBind(log);
        int lim = num; // number checked for reservation
        if (count > min && bind == 0) lim = -1;
        log << "check binding" << '\n';
        for (int i = 0; i < num && i < lim; i++) {
            switch (ins[i].ins) {default:
            break; case(QDerIns): case(PDerIns): case(ODerIns): bind->siz(ins[i].res) = 0;
            break; case(TDerIns): case(SDerIns): case(RDerIns): case(IDerIns): bind->siz(ins[i].res) = 0;
            break; case(WDeeIns): case(RDeeIns): bind->siz(ins[i].res) = 0;
            break; case(TDeeIns): case(SDeeIns): case(IDeeIns): bind->siz(ins[i].res) = 0;}}
        if (bind) bind->wrp() = 0;
        log << "choose buffers" << '\n';
        for (int i = 0; i < num && i < lim; i++) {
            switch (ins[i].ins) {default: {std::cerr << "invalid instruction" << std::endl; EXIT}
            break; case (STagIns): {
            log << "STagIns res:" << ins[i].res << " idx:" << ins[i].idx << " key:" << ins[i].key << " val:" << ins[i].val << '\n';
            src(ins[i].res)->qualify(ins[i].idx,ins[i].key,ins[i].val);}
            break; case (ITstIns): case (OTstIns): case (GTstIns): case (NTstIns): {
            log << "TstInst res:" << ins[i].res << " ins:" << ins[i].ins << " val:" << ins[i].val << '\n';
            src(ins[i].res)->test(ins[i].ins,ins[i].idx,ins[i].key,ins[i].val);}
            break; case(QDerIns): case(TDerIns): case(PDerIns): case(SDerIns):
            case(ODerIns): case(RDerIns): case(IDerIns):
            case(WDeeIns): case(TDeeIns): case(RDeeIns): case(SDeeIns): case(IDeeIns): {
            Onl onl = get(ins[i].ins,ins[i].idx,ins[i].key,ins[i].val);
            SaveState *sav = 0; if (!vld(ins[i].res,onl,bind)) {
            BaseState *buf = get(ins[i].ins,ins[i].res,ins[i].idx,ins[i].key,ins[i].val);
            sav = bind->vld(ins[i].res);
            sav->buf = buf; sav->fst = i; sav->onl = onl;}
            else sav = bind->chk(ins[i].res);
            sav->fin = i;
            log << "DepIns idx:" << ins[i].idx << " " << sav->buf->debug << '\n';}}}
        log << "reserve chosen" << '\n';
        if (bind) bind->wrp() += 1;
        int resps = 0;
        for (int i = 0; i < num && i < lim; i++) {
            SaveState *sav = bind->chk(ins[i].res);
            switch (ins[i].ins) {default:
            break; case(QDerIns): case(PDerIns): case(ODerIns):
            case(TDerIns): case(SDerIns): case(RDerIns): case(IDerIns): {
            Unl unl = {.der=ins[i].res,.hdl=bind->hdl(ins[i].res),.dee=resps,.siz=0};
            for (int j = i+1; j < num; j++) switch (ins[j].ins) {default:
            break; case(QDerIns): case(PDerIns): case(ODerIns):
            case(TDerIns): case(SDerIns): case(RDerIns): case(IDerIns): j = num-1;
            break; case(WDeeIns): case(RDeeIns):
            case(TDeeIns): case(SDeeIns): case(IDeeIns): resps += 1; unl.siz += 1;} slog.clr();
            if (!bind->push(ins[i].res,ins[i].req.loc,ins[i].req,unl,array,log)) lim = i;
            if (sav->fin == i && lim == num) {Adv adv = {.adv=PushAdv,.hdl=ins[i].idx,.key=ins[i].key,.val=ins[i].val};
            switch (ins[i].ins) {default: break; case(PDerIns): case(ODerIns): adv.adv = FnceAdv;}
            sav->bind->push(adv,log);}}
            break; case(WDeeIns): case(TDeeIns):
            if (!bind->winc(ins[i].res,sav->buf,log)) lim = i;
            break; case(RDeeIns): case(SDeeIns): case(IDeeIns): {
            if (!bind->rinc(ins[i].res,sav->buf,log)) lim = i;}}}
        if (lim == num) {
        log << "link list" << '\n';
        if (bind) bind->wrp() += 1;
        Lnk *lnk = 0; ResrcLoc lst = ResrcLocs; BaseState *bas = 0;
        for (int i = 0; i < num; i++) {
            SaveState *sav = bind->chk(ins[i].res);
            switch(ins[i].ins) {default:
            break; case(QDerIns): case (PDerIns): case (ODerIns):
            case(TDerIns): case(SDerIns): case(RDerIns): case (IDerIns):
            lnk = sav->buf->lnk(ins[i].req.loc,bas,lst,lnk);
            bas = sav->buf; lst = ins[i].req.loc;}}
        log << "record bindings" << '\n';
        for (int i = 0; i < num; i++) {
            switch (ins[i].ins) {default:
            break; case (WDeeIns): case(RDeeIns):
            case (TDeeIns): case (SDeeIns): case(IDeeIns):
            if (bind) bind->push(ins[i].res,ins[i].ins,log);}}
        log << "submit buffers" << '\n';
        if (bind) bind->wrp() += 1;
        for (int i = 0; i < num; i++) {
            SaveState *sav = bind->chk(ins[i].res);
            switch (ins[i].ins) {default:
            break; case(QDerIns): case(TDerIns):
            if (sav->fst == i) src(ins[i].res)->advance(ins[i].idx,ins[i].key,ins[i].val);
            log << "QDerIns push " << sav->buf->debug << '\n';
            thread->push(log,sav->buf,ins[i].req.loc);
            break; case(PDerIns): case(ODerIns): case(SDerIns): case(RDerIns):
            log << "PDerIns push " << sav->buf->debug << '\n';
            thread->push(log,sav->buf,ins[i].req.loc);
            break; case(IDerIns):
            if (sav->fst == i) src(ins[i].res)->advance(ins[i].idx);
            log << "IDerIns push " << sav->buf->debug << '\n';
            thread->push(log,sav->buf,ins[i].req.loc);}}
        log << "notify pass" << '\n';
        ptr->slf = 0;
        switch (rsp) {default:
        break; case (RetRsp): case (RptRsp): log << "RptRsp " << ptr << '\n'; thread->push(log,ptr,sub);}
        if (bind) stack[BindRes]->advance(0,Qualitys,0);
        } else {
        log << "release reserved " << num << ">" << lim << '\n';
        if (bind) bind->wrp() += 1;
        for (int i = 0; i < lim; i++) {
            SaveState *sav = bind->chk(ins[i].res);
            switch (ins[i].ins) {default:
            break; case(QDerIns): case(PDerIns): case(ODerIns):
            case(TDerIns): case(SDerIns): case(RDerIns): case(IDerIns):
            bind->done(ins[i].res,log);
            sav->buf->fail(log);
            break; case(WDeeIns): case (TDeeIns):
            bind->wdec(ins[i].res,log);
            break; case(RDeeIns): case (SDeeIns): case(IDeeIns):
            bind->rdec(ins[i].res,log);}}
        if (bind) bind->done(log);
        log << "notify fail" << '\n';
        switch (rsp) {default:
        break; case (RptRsp): case (MptRsp): goon = true; vulkanWait();
        break; case (MltRsp): ptr->slf = -1;
        break; case (RetRsp): ptr->slf = -1; thread->push(log,ptr,sub);}}}
    }
    static Requ request(Format frm, ResrcLoc loc, void *val, int *arg, int siz, int &idx, SmartState log) {
        Requ req = {Requests,0,0,0,Extents,0,0,loc};
        switch (frm) {default: EXIT
        // VK_IMAGE_LAYOUT_UNDEFINED(initial)
        // VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL(texture,shadow)
        // VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL(write,fill)
        // VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL(read)
        // VK_IMAGE_LAYOUT_PRESENT_SRC_KHR(render)
        // (SizeReq) {ExclReq} BothReq
        // Imagez: (resize)->{initial}->texture->write->texture
        // RelateRes: (resize)->{initial}->shadow->write->shadow
        break; case (ImageFrm): // initial to texture,shadow
        req.tag = ExclReq; req.ext = FormExt; // ReformLoc
        req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        break; case (WonlyFrm): // texture,shadow to write,fill
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (RonlyFrm): // write,fill to texture,shadow
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        // Peekz: (resize)->{initial}->render->read->render
        break; case (PierceFrm): // initial to render
        req.tag = ExclReq; req.ext = FormExt; // ReformLoc
        req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        break; case (PeekFrm): // render to read
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (SourceFrm): // read to render
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL; req.size = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        // Pokez: (resize)->{initial}->render->write->render
        break; case (PokeFrm): // render to write,fill
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (DestFrm): // write,fill to render
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        // PierceRes: (resize)->{initial}->read->read
        break; case (RelateFrm): // initial to read
        req.tag = ExclReq; req.ext = FormExt; // ReformLoc
        req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (RdwrFrm): // read to write,fill
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (WrrdFrm): // write,fill to read
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (ExtentFrm): // ResizeLoc
        req.tag = SizeReq; req.ext = ExtentExt;
        req.base = get(arg,siz,idx,log,"ExtentFrm.base"); req.size = get(arg,siz,idx,log,"ExtentFrm.size");
        break; case (SizeFrm):
        req.tag = SizeReq; req.ext = IntExt;
        req.base = get(arg,siz,idx,log,"SizeFrm.base"); req.size = get(arg,siz,idx,log,"SizeFrm.size");
        break; case (HighFrm):
        req.tag = BothReq; req.ext = ExtentExt;
        req.ptr = val; req.siz = get(arg,siz,idx,log,"HighFrm.siz");
        req.base = get(arg,siz,idx,log,"HighFrm.base"); req.size = get(arg,siz,idx,log,"HighFrm.size");;
        break; case (FillFrm):
        req.tag = BothReq; req.ext = FillExt;
        req.base = get(arg,siz,idx,log,"FillFrm.base");
        break; case (WholeFrm):
        req.tag = BothReq; req.ext = IntExt;
        req.ptr = val; req.idx = get(arg,siz,idx,log,"WholeFrm.idx"); req.siz = get(arg,siz,idx,log,"WholeFrm.siz");
        req.base = req.idx; req.size = req.siz;
        break; case (OnceFrm):
        req.tag = OnceReq; req.ext = IntExt;
        req.ptr = val; req.idx = get(arg,siz,idx,log,"OnceFrm.idx"); req.siz = get(arg,siz,idx,log,"OnceFrm.siz");
        req.base = req.idx; req.size = req.siz;
        break; case (LockFrm):
        req.tag = LockReq; req.ext = IntExt;
        req.ptr = val; req.idx = get(arg,siz,idx,log,"LockFrm.idx"); req.siz = get(arg,siz,idx,log,"LockFrm.siz");
        break; case (ResrcFrm):
        req.tag = LockReq; req.ext = ResrcExt;
        req.idx = get(arg,siz,idx,log,"ResrcFrm.idx");
        break; case (IndexFrm):
        req.tag = SizeReq; req.ext = MicroExt; req.base = get(arg,siz,idx,log,"IndexFrm.base");
        break; case (MicroFrm):
        req.tag = BothReq; req.ext = MicroExt;
        req.idx = get(arg,siz,idx,log,"MicroFrm.idx"); req.siz = get(arg,siz,idx,log,"MicroFrm.siz");
        req.base = get(arg,siz,idx,log,"MicroFrm.base");
        break; case (ConstFrm):
        req.tag = SizeReq; req.ext = MicroExt;
        req.base = get(arg,siz,idx,log,"ConstFrm.base");
        break; case (FalseFrm):
        req.tag = SizeReq; req.ext = FalseExt;
        break; case (TrueFrm):
        req.tag = SizeReq; req.ext = TrueExt;
        }
        return req;
    }
    template <class Type> static Inst instruct(HeapState<Arg,0> &dot, int i, Type typ, void *val, int *arg, int siz, int &idx, int &count, SmartState log) {
        {char *st0 = 0; showResrc(dot[i].res,&st0);
        char *st1 = 0; showResrcLoc(dot[i].loc,&st1);
        char *st2 = 0; showInstr(dot[i].ins,&st2);
        log << "instruct " << st0 << " " << st1 << " " << st2 << '\n';
        free(st0); free(st1); free(st2);}
        Instr ins = dot[i].ins;
        switch (ins) {default: EXIT
        break; case(QDerIns): case(PDerIns): case(ODerIns): {
        Requ req = request(dot[i].fmt,dot[i].loc,val,arg,siz,idx,log);
        return Inst{.ins=ins,.req=req,.res=dot[i].res,.idx=0,.key=Qualitys,.val=0};}
        break; case(TDerIns): case(SDerIns): case(RDerIns): {
        int pre = get(arg,siz,idx,log,"DerIns.idx");
        int vlu = (dot[i].key>=0&&dot[i].key<Qualitys?get(arg,siz,idx,log,"DerIns.val"):0);
        Requ req = request(dot[i].fmt,dot[i].loc,val,arg,siz,idx,log);
        return Inst{.ins=ins,.req=req,.res=dot[i].res,.idx=pre,.key=dot[i].key,.val=vlu};}
        break; case(IDerIns): {
        int pre = get(arg,siz,idx,log,"IDerIns.idx");
        Requ req = request(dot[i].fmt,dot[i].loc,val,arg,siz,idx,log);
        return Inst{.ins=ins,.req=req,.res=dot[i].res,.idx=pre,.key=Qualitys,.val=0};}
        break; case(WDeeIns): case(RDeeIns): {
        return Inst{.ins=ins,.res=dot[i].res,.idx=0,.key=Qualitys,.val=0};}
        break; case(TDeeIns): case(SDeeIns): {
        int pre = get(arg,siz,idx,log,"DeeIns.idx");
        int vlu = (dot[i].key>=0&&dot[i].key<Qualitys?get(arg,siz,idx,log,"DeeIns.val"):0);
        return Inst{.ins=ins,.res=dot[i].res,.idx=pre,.key=dot[i].key,.val=vlu};}
        break; case(IDeeIns): {
        int pre = get(arg,siz,idx,log,"IDeeIns.idx");
        return Inst{.ins=ins,.res=dot[i].res,.idx=pre,.key=Qualitys,.val=0};}
        break; case(STagIns): case(ITstIns): case(OTstIns): case(GTstIns): case(NTstIns): {
        int pre = get(arg,siz,idx,log,"STagIns.idx");
        int vlu = (dot[i].key>=0&&dot[i].key<Qualitys?get(arg,siz,idx,log,"STagIns.val"):0);
        return Inst{.ins=ins,.res=dot[i].res,.idx=pre,.key=dot[i].key,.val=vlu};}}
        return Inst{.ins=Instrs};
    }
    template <class Type, class Fnc, class Arg> static bool builtin(Type &sav, Type &arg, Fnc fnc, Arg typ, int i, Type inv, SmartState log) {
        Type val = (fnc&&fnc(typ)?fnc(typ)(i):inv);
        arg = val;
        if (arg == inv) arg = sav; else sav = arg;
        return (val != inv);
    }
    static bool iterate(Memory typ, int sub, Arg &sav, Arg &dot, ConstState *ary, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {PDerIns,MiddleLoc,WholeFrm,Qualitys,Resrcs,Memorys,Micros};
        if (builtin(sav.ins,dot.ins,ary->memins,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ary->memloc,typ,sub,ResrcLocs,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ary->memfmt,typ,sub,Formats,log)) done = false;
        if (builtin(sav.key,dot.key,ary->memkey,typ,sub,Qualitys,log)) done = false;
        if (builtin(sav.res,dot.res,ary->memres,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.mem,dot.mem,ary->memmem,typ,sub,Memorys,log)) done = false;
        if (builtin(sav.mic,dot.mic,ary->memmic,typ,sub,Micros,log)) done = false;
        return !done;
    }
    static bool iterate(Resrc typ, int sub, Arg &sav, Arg &dot, ConstState *ary, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {PDerIns,ResizeLoc,SizeFrm,Qualitys,Resrcs,Memorys,Micros};
        if (builtin(sav.ins,dot.ins,ary->resins,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ary->resloc,typ,sub,ResrcLocs,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ary->resfmt,typ,sub,Formats,log)) done = false;
        if (builtin(sav.key,dot.key,ary->reskey,typ,sub,Qualitys,log)) done = false;
        if (builtin(sav.res,dot.res,ary->resres,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.mem,dot.mem,ary->resmem,typ,sub,Memorys,log)) done = false;
        if (builtin(sav.mic,dot.mic,ary->resmic,typ,sub,Micros,log)) done = false;
        return !done;
    }
    static bool iterate(Micro typ, int sub, Arg &sav, Arg &dot, ConstState *ary, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {QDerIns,ResizeLoc,SizeFrm,Qualitys,Resrcs,Memorys,Micros};
        if (builtin(sav.ins,dot.ins,ary->micins,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ary->micloc,typ,sub,ResrcLocs,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ary->micfmt,typ,sub,Formats,log)) done = false;
        if (builtin(sav.key,dot.key,ary->mickey,typ,sub,Qualitys,log)) done = false;
        if (builtin(sav.res,dot.res,ary->micres,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.mem,dot.mem,ary->micmem,typ,sub,Memorys,log)) done = false;
        if (builtin(sav.mic,dot.mic,ary->micmic,typ,sub,Micros,log)) done = false;
        /*
        char *db0 = 0; showResrc(dot.res,&db0);
        char *db1 = 0; showInstr(dot.ins,&db1);
        char *db2 = 0; showMicro(typ,&db2);
        char *db3 = 0; showResrcLoc(dot.loc,&db3);
        std::cerr << "iterate " << db2 << " " << db3 << " " << db1 << " " << db0 << std::endl;
        free(db0); db0 = 0; free(db1); db1 = 0; free(db2); db2 = 0; free(db3); db3 = 0;
        */
        return !done;
    }
    template <class Type> void push(HeapState<Inst,StackState::instrs> &lst, Type typ, void *val, int *arg, int siz, int &idx, int ary, SmartState log) {
        int count = 0; Arg sav; Arg tmp; HeapState<Arg,0> dot;
        for (int i = 0; iterate(typ,i,sav,tmp,&array[ary],log); i++) dot << tmp;
        for (int i = 0; i < dot.size(); i++) {
        Inst ins = instruct(dot,i,typ,val,arg,siz,idx,count,log);
        switch (ins.ins) {default: lst << ins;
        break; case (RIncIns): push(lst,ins.res,val,arg,siz,idx,ary,log);
        break; case (EIncIns): push(lst,ins.mem,val,arg,siz,idx,ary,log);
        break; case (IIncIns): push(lst,ins.mic,val,arg,siz,idx,ary,log);}}
    }
    int size(Micro typ, int ary) {
        int siz = 0; while (dflt(typ,siz,ary) != Defaults) siz += 1; return siz;
    }
    int size(Memory typ, int ary) {
        int siz = 0; while (dflt(typ,siz,ary) != Defaults) siz += 1; return siz;
    }
    int size(Resrc typ, int ary) {
        int siz = 0; while (dflt(typ,siz,ary) != Defaults) siz += 1; return siz;
    }
    Default dflt(Micro typ, int idx, int ary) {
        return (array[ary].micdef(typ) ? array[ary].micdef(typ)(idx) : Defaults);
    }
    Default dflt(Memory typ, int idx, int ary) {
        return (array[ary].memdef(typ) ? array[ary].memdef(typ)(idx) : Defaults);
    }
    Default dflt(Resrc typ, int idx, int ary) {
        return (array[ary].resdef(typ) ? array[ary].resdef(typ)(idx) : Defaults);
    }
    int fill(Micro typ, int idx, int ary) {
        return (array[ary].micval(typ) ? array[ary].micval(typ)(idx) : 0);
    }
    int fill(Memory typ, int idx, int ary) {
        return (array[ary].memval(typ) ? array[ary].memval(typ)(idx) : 0);
    }
    int fill(Resrc typ, int idx, int ary) {
        return (array[ary].resval(typ) ? array[ary].resval(typ)(idx) : 0);
    }
    template <class Type> void push(Type typ, void *dat, int *arg, int *val, int siz, int sze, int &idx, Center *ptr, int sub, Rsp rsp, int ary, SmartState log) {
        // profer means GiveDef uses fill value as count into proferred
        // with both arg/siz and val/sze,
        //  negative arg means profer the val,
        //  non-negative means force val at index arg
        // arg/siz only means profer only
        // val/sze only means packed force
        // neither means default only
        if ((arg == 0) != (siz == 0)) EXIT
        if ((val == 0) != (sze == 0)) EXIT
        int tot = 0;
        if (siz && sze) {
            tot = size(typ,ary);
            // maximum of number of fill values and force index
            for (int i = 0; i < siz; i++)
            if (arg[i] >= tot) tot = arg[i]+1;}
        // maximum of number of fill values and number of packed force values
        else if (sze) tot = sze;
        // otherwise number of fill values
        if (tot < size(typ,ary)) tot = size(typ,ary);
        int vlu[tot];
        // initialize with defaults
        for (int i = 0; i < tot; i++)
            // ignore siz sze since TrivDef fill value is an immediate value
            if (dflt(typ,i,ary) == TrivDef) vlu[i] = fill(typ,i,ary);
            else vlu[i] = 0;
        // copy from given
        if (siz) for (int i = 0; i < tot; i++)
            // ignore GiveDef if there is nothing proferred
            if (dflt(typ,i,ary) == GiveDef) {
            int idx = fill(typ,i,ary);
            // find the proffered val counted by the fill value
            if (sze) {for (int j = 0; j < tot; j++)
            if (arg[j] < 0 && idx-- == 0) vlu[i] = val[j];}
            // siz no sze means all arg would be negative, and arg treated as proferred val
            else if (idx >= 0 && idx < tot) vlu[i] = arg[idx];}
        // force from given
        if (sze) for (int i = 0; i < tot; i++) {
            // siz and sze means force index is from arg
            // sze no siz is as if arg are each index in order
            int idx = (siz ? arg[i] : i);
            // if arg is not negative it is a force index
            if (idx >= 0 && idx < tot) vlu[idx] = val[i];}
        // alias from prior
        for (int i = 0; i < tot; i++)
            // ignore siz sze since BackDef fill value is index into result so far
            if (dflt(typ,i,ary) == BackDef) {
            int idx = fill(typ,i,ary);
            if (idx >= 0 && idx < i) vlu[i] = vlu[idx];}
        HeapState<Inst,StackState::instrs> lst;
        push(lst,typ,dat,vlu,tot,idx,ary,log);
        if (idx != tot) {std::cerr << "wrong number of int arguments " << idx << "!=" << tot << std::endl; EXIT}
        push(lst,ptr,sub,rsp,log);
    }
    void push(Draw &drw, Center *ptr, int sub, Rsp rsp, int ary, SmartState log) {
        int idx = 0; switch (drw.con.tag) {default: ERROR();
        break; case (MicroCon): push(drw.con.mic,drw.ptr,drw.arg,drw.val,drw.siz,drw.sze,idx,ptr,sub,rsp,ary,log);
        break; case (MemoryCon): push(drw.con.mem,drw.ptr,drw.arg,drw.val,drw.siz,drw.sze,idx,ptr,sub,rsp,ary,log);
        break; case (ResrcCon): push(drw.con.res,drw.ptr,drw.arg,drw.val,drw.siz,drw.sze,idx,ptr,sub,rsp,ary,log);}
    }
    void push(Memory mem, void *dat, int idx, int siz, int wid, int hei, int width, int height, Center *ptr, int sub, Rsp rsp, int ary, SmartState log) {
        int mval[] = {
        0,mem, // STagIns
        width,height, // PDerIns ExtentFrm
        siz,wid,hei}; // PDerIns HighFrm
        int msiz = sizeof(mval)/sizeof(int); int midx = 0;
        push(mem,dat,0,mval,0,msiz,midx,ptr,sub,rsp,ary,log);
    }
    void push(Center *ptr, int sub, Rsp rsp, int ary, SmartState log) {
        switch (ptr->mem) {default: {
        int mod = centerMod(ptr); int idx = ptr->idx*mod; int siz = ptr->siz*mod;
        int val[] = {idx,siz}; int aiz = sizeof(val)/sizeof(int); int adx = 0;
        switch (ptr->mem) {default: EXIT
        break; case (Indexz): push(ptr->mem,(void*)ptr->ind,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Bringupz): push(ptr->mem,(void*)ptr->ver,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Uniformz): push(ptr->mem,(void*)ptr->uni,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Matrixz): push(ptr->mem,(void*)ptr->mat,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Trianglez): push(ptr->mem,(void*)ptr->tri,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Numericz): push(ptr->mem,(void*)ptr->num,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Vertexz): push(ptr->mem,(void*)ptr->vtx,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);
        break; case (Basisz): push(ptr->mem,(void*)ptr->bas,0,val,0,aiz,adx,ptr,sub,rsp,ary,log);}}
        break; case (Drawz): {
            int mask = 0;
            for (int i = 0; i < ptr->siz; i++) {
            push(ptr->drw[i],ptr,sub,rsp,ary,log);
            if (ptr->slf) mask |= 1<<(i<32?i:31);} ptr->slf = mask;}
        break; case (Instrz): {
            HeapState<Inst,StackState::instrs> ins;
            for (int i = 0; i < ptr->siz; i++) ins<<ptr->ins[i];
            push(ins,ptr,sub,rsp,log);}
        break; case (Configurez): {
            for (int i = 0; i < ptr->siz; i++) change->write(ptr->cfg[i],ptr->val[i]);}
        break; case (Imagez): {int mask = 0;
            for (int i = 0; i < ptr->siz; i++) { // ptr->idx/ptr->siz is a range of resources
            int idx = ptr->idx+i; int wid = ptr->img[i].wid; int hei = ptr->img[i].hei;
            int tot = datxVoids(ptr->img[i].dat);
            push(ptr->mem,(void*)datxVoidz(0,ptr->img[i].dat),idx,tot,wid,hei,wid,hei,ptr,sub,rsp,ary,log);
            if (ptr->slf) mask |= 1<<(i<32?i:31);} ptr->slf = mask;}
        break; case (Peekz): { // ptr->idx is the resource and ptr->siz is number of locations in the resource
            VkExtent2D ext = src(SwapRes)->newbuf(0,Qualitys,0)->getExtent(); // TODO unsafe if SwapRes is changing
            int idx = ptr->idx; int siz = ptr->siz; int wid = ext.width; int hei = ext.height;
            push(ptr->mem,(void*)ptr->eek,idx,siz,wid,hei,change->read(WindowWidth),change->read(WindowHeight),ptr,sub,rsp,ary,log);}
        break; case (Pokez): { // ptr->idx is the resource and ptr->siz is number of locations in the resource
            VkExtent2D ext = src(SwapRes)->newbuf(0,Qualitys,0)->getExtent(); // TODO unsafe if SwapRes is changing
            int idx = ptr->idx; int siz = ptr->siz; int wid = ext.width; int hei = ext.height;
            push(ptr->mem,(void*)ptr->oke,idx,siz,wid,hei,change->read(WindowWidth),change->read(WindowHeight),ptr,sub,rsp,ary,log);}}
        switch (rsp) {default: break; case (MltRsp): case (MptRsp): thread->push(log,ptr,sub);}
    }
};

struct ForkState : public DoneState {
    Thread thd; int idx; mftype cfnc; mftype dfnc; mftype hfnc; mftype nfnc;
    ForkState (Thread thd, int idx, mftype call, mftype done, mftype heap, mftype noop) :
        thd(thd), idx(idx), cfnc(call), dfnc(done), hfnc(heap), nfnc(noop) {
        strcpy(debug,"ForkState");
    }
    void call() override {cfnc(thd,idx);}
    void done() override {dfnc(thd,idx);}
    void heap() override {hfnc(thd,idx); delete this;}
    void noop() override {nfnc(thd,idx);}
};

struct SwapState : public BaseState {
    ChangeState<Configure,Configures> *change;
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
    HeapState<VkImage> swapChainImages;
    HeapState<VkImageView> swapChainImageViews;
    VkImage depthImage;
    VkDeviceMemory depthImageMemory;
    VkImageView depthImageView;
    HeapState<VkFramebuffer> framebuffers;
    VkSurfaceCapabilitiesKHR capabilities;
    SwapState() :
        BaseState("SwapState",StackState::self),
        change(StackState::change),
        window(StackState::window),
        surface(StackState::surface),
        physical(StackState::physical),
        device(StackState::device),
        surfaceFormat(StackState::surfaceFormat),
        presentMode(StackState::presentMode),
        graphicsFamily(StackState::graphicsFamily),
        presentFamily(StackState::presentFamily),
        imageFormat(StackState::imageFormat[SwapBuf]),
        depthFormat(StackState::depthFormat),
        renderPass(StackState::renderPass[SwapBuf]),
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
        change->write(WindowWidth,getExtent().width); change->write(WindowHeight,getExtent().height);
        std::cout << "extent " << getExtent().width << "/" << getExtent().height << std::endl;
        swapChain = createSwapChain(surface,device,getExtent(),surfaceFormat,presentMode, capabilities,graphicsFamily,presentFamily);
        createSwapChainImages(device,swapChain,swapChainImages);
        swapChainImageViews.fill({},swapChainImages.size());
        for (int i = 0; i < swapChainImages.size(); i++)
        swapChainImageViews[i] = createImageView(device, swapChainImages[i], imageFormat, VK_IMAGE_ASPECT_COLOR_BIT);
        createImage(device, physical, getExtent().width, getExtent().height, depthFormat, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, memProperties,/*output*/ depthImage, depthImageMemory);
        depthImageView = createImageView(device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT);
        createFramebuffers(device,getExtent(),renderPass,swapChainImageViews,depthImageView,framebuffers);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthImageMemory, nullptr);
        for (int i = 0; i < framebuffers.size(); i++)
            vkDestroyFramebuffer(device, framebuffers[i], nullptr);
        for (int i = 0; i < swapChainImageViews.size(); i++)
            vkDestroyImageView(device, swapChainImageViews[i], nullptr);
        vkDestroySwapchainKHR(device, swapChain, nullptr);
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << '\n';
        return VK_NULL_HANDLE;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << '\n';
    }
    static VkSurfaceCapabilitiesKHR findCapabilities(GLFWwindow* window, VkSurfaceKHR surface, VkPhysicalDevice device);
    static VkExtent2D chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities);
    static VkSwapchainKHR createSwapChain(VkSurfaceKHR surface, VkDevice device, VkExtent2D swapChainExtent, VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode, VkSurfaceCapabilitiesKHR capabilities, uint32_t graphicsFamily, uint32_t presentFamily);
    static void createSwapChainImages(VkDevice device, VkSwapchainKHR swapChain, HeapState<VkImage> &swapChainImages);
    static void createFramebuffers(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
        HeapState<VkImageView> &swapChainImageViews, VkImageView depthImageView,
        HeapState<VkFramebuffer> &framebuffers);
};

struct PipeState : public BaseState {
    const VkDevice device;
    const VkRenderPass renderPass;
    const ConstState *constState;
    Micro micro;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorSetLayout;
    VkPipelineLayout pipelineLayout;
    VkPipeline pipeline;
    Render renderIndex(Micro micro, const ConstState *func) {
        auto fnc = func->micres(micro);
        for (int i = 0; fnc(i) != Resrcs; i++)
        switch (fnc(i)) {default:
        break; case (PierceRes): return PierceBuf;
        break; case (DebugRes): return DebugBuf;
        break; case (SwapRes): return SwapBuf;}
        return Renders;
    }
    PipeState() :
        BaseState("PipeState",StackState::self),
        device(StackState::device),
        renderPass(StackState::renderPass[renderIndex((Micro)StackState::micro,StackState::constState)]),
        constState(StackState::constState),
        micro((Micro)StackState::micro++),
        descriptorPool(createDescriptorPool(StackState::device,StackState::descrs)),
        descriptorSetLayout(createDescriptorSetLayout(StackState::device,micro,constState)),
        pipelineLayout(createPipelineLayout(StackState::device,descriptorSetLayout)),
        pipeline(createGraphicsPipeline(StackState::device,getRenderPass(),pipelineLayout,micro,constState)) {
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
    VkRenderPass getRenderPass() override {return renderPass;}
    void resize(Loc &loc, SmartState log) override {}
    void unsize(Loc &loc, SmartState log) override {}
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << '\n';
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << '\n';
    }
    static VkDescriptorPool createDescriptorPool(VkDevice device, int frames);
    static VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, Micro micro, const ConstState *func);
    static VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout);
    static VkPipeline createGraphicsPipeline(VkDevice device, VkRenderPass renderPass, VkPipelineLayout pipelineLayout, Micro micro, const ConstState *func);
    static VkShaderModule createShaderModule(VkDevice device, const char *filename);
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
        log << "setup " << debug << '\n';
        int tmp = idx(loc) - max(loc).base;
        if (tmp < 0 || siz(loc) < 0 || tmp+siz(loc) > max(loc).size) EXIT
        memcpy((void*)((char*)mapped+tmp), ptr(loc), siz(loc));
        return VK_NULL_HANDLE; // return null fence for no wait
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << '\n';
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
        log << "resize " << debug << " " << max(loc) << '\n';
        range = max(loc).size;
        VkDeviceSize bufferSize = max(loc).size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | flags, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, buffer, memory);
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
        log << "setup " << debug << '\n';
        int tmp = idx(loc) - max(loc).base;
        if (tmp < 0 || siz(loc) < 0 || tmp+siz(loc) > max(loc).size) EXIT
        VkDeviceSize bufferSize = max(loc).size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, stagingBuffer, stagingBufferMemory);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, &data);
        memcpy((void*)((char*)data+tmp),ptr(loc),siz(loc));
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetFences(device, 1, &fen(loc));
        copyBuffer(device, graphics, stagingBuffer, buffer, bufferSize, commandBuffer, fen(loc),VK_NULL_HANDLE,VK_NULL_HANDLE);
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
    const HeapState<VkRenderPass,LogicalState::passes> renderPass;
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
    static Render vulkanRender(Resrc i) { // TODO change to depend on Quatity value instead of Resrc
        switch (i) {default:
        break; case (SwapRes): return SwapBuf;
        break; case (DebugRes): return DebugBuf;
        break; case (PierceRes): return PierceBuf;
        break; case (DepthRes): return DepthBuf;}
        return Renders;
    }
    void range(int &x, int &y, int &w, int &h, int &tw, int &th, VkDeviceSize &is, Pierce *&pie, Loc &loc, Loc &got, SmartState log) {
        if (max(got).tag != ExtentExt) EXIT
        tw = max(got).extent.width;
        th = max(got).extent.height;
        is = tw * th * 4;
        pie = 0; x = 0; y = 0; w = tw; h = th;
        if (idx(loc) != 0) EXIT
        log << "buftag:" << tag(RuseQua) << " (Imagez:" << Imagez << ",Peekz:" << Peekz << ",Pokez:" << Pokez << ")" << '\n';
        if (tag(RuseQua) == Pokez || tag(RuseQua) == Peekz) {
        pie = (Pierce*)ptr(loc); x = tw; y = th; w = 0; h = 0;
        if (siz(loc) == 0) {x = 0; y = 0;}
        for (int i = 0; i < siz(loc); i++) {
        if (pie[i].wid < x) x = pie[i].wid;
        if (pie[i].hei < y) y = pie[i].hei;
        if (pie[i].wid > w) w = pie[i].wid;
        if (pie[i].hei > h) h = pie[i].hei;}
        w = w-x+1; h = h-y+1;}
        if (x < 0 || w < 0 || x + w > tw) EXIT
        if (y < 0 || h < 0 || y + h > th) EXIT
        if (tag(RuseQua) == Imagez && siz(loc) != is) EXIT
        // log << "range " << x << "/" << w << "," << y << "/" << h << " " << tw << "," << th << " " << x*4+y*tw*4 << "/" << is << '\n';
    }
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << " location:" << *loc << '\n';
        if (*loc == ResizeLoc) {
        int texWidth = max(loc).extent.width;
        int texHeight = max(loc).extent.height;
        extent = max(loc).extent;
        VkImageUsageFlagBits flags;
        VkFormat forms = PhysicalState::vulkanFormat(vulkanRender(typ()));
        if (typ() == ImageRes) flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_SAMPLED_BIT | (int)VK_IMAGE_USAGE_TRANSFER_DST_BIT);
        if (typ() == DebugRes) flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | (int)VK_IMAGE_USAGE_TRANSFER_SRC_BIT | (int)VK_IMAGE_USAGE_TRANSFER_DST_BIT);
        if (typ() == PierceRes) flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | (int)VK_IMAGE_USAGE_TRANSFER_SRC_BIT);
        createImage(device, physical, texWidth, texHeight, forms, flags, memProperties, /*output*/ image, imageMemory);
        imageView = createImageView(device, image, forms, VK_IMAGE_ASPECT_COLOR_BIT);
        if (typ() == ImageRes) {
        textureSampler = createTextureSampler(device,properties);}
        if (typ() == DebugRes || typ() == PierceRes) {
        createImage(device, physical, max(loc).extent.width, max(loc).extent.height, depthFormat, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, memProperties,/*output*/ depthImage, depthMemory);
        depthImageView = createImageView(device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT);
        createFramebuffer(device,max(loc).extent,renderPass[vulkanRender(typ())],imageView,depthImageView,framebuffer);}}
        if (*loc == ReformLoc) commandReform = createCommandBuffer(device,commandPool); 
        if (*loc == BeforeLoc) commandBefore = createCommandBuffer(device,commandPool);
        if (*loc == MiddleLoc) commandBuffer = createCommandBuffer(device,commandPool);
        if (*loc == AfterLoc) commandAfter = createCommandBuffer(device,commandPool);
        sem(loc) = createSemaphore(device); // TODO as needed
        fen(loc) = createFence(device); // TODO as needed
    }
    void unsize(Loc &loc, SmartState log) override {
        log << "unsize " << debug << " location:" << *loc << '\n';
        vkDestroyFence(device, fen(loc), nullptr); // TODO as needed
        vkDestroySemaphore(device, sem(loc), nullptr); // TODO as needed
        if (*loc == AfterLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandAfter);
        if (*loc == MiddleLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        if (*loc == BeforeLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandBefore);
        if (*loc == ReformLoc) vkFreeCommandBuffers(device, commandPool, 1, &commandReform);
        if (*loc == ResizeLoc) {
        if (typ() == DebugRes || typ() == PierceRes) {
        vkDestroyFramebuffer(device, framebuffer, nullptr);
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthMemory, nullptr);}
        if (typ() == ImageRes) {
        vkDestroySampler(device, textureSampler, nullptr);}
        vkDestroyImageView(device, imageView, nullptr);
        vkDestroyImage(device, image, nullptr);
        vkFreeMemory(device, imageMemory, nullptr);}
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << " location:" << *loc << '\n';
        VkFence fence = (*loc==AfterLoc?fen(loc):VK_NULL_HANDLE);
        VkSemaphore before = (*loc!=ResizeLoc&&nsk(*lst(loc))?sem(lst(loc)):VK_NULL_HANDLE);
        VkSemaphore after = (*loc!=AfterLoc?sem(loc):VK_NULL_HANDLE);
        VkFormat forms = PhysicalState::vulkanFormat(vulkanRender(typ()));
        if (fence != VK_NULL_HANDLE) vkResetFences(device, 1, &fence);
        if (*loc == ReformLoc) {
        vkResetCommandBuffer(commandReform, /*VkCommandBufferResetFlagBits*/ 0);
        transitionImageLayout(device, graphics, commandReform, res(typ(),0)->getImage(), before, after, fence, forms, max(loc).src, max(loc).dst);}
        if (*loc == BeforeLoc) {
        vkResetCommandBuffer(commandBefore, /*VkCommandBufferResetFlagBits*/ 0);
        transitionImageLayout(device, graphics, commandBefore, res(typ(),0)->getImage(), before, after, fence, forms, max(loc).src, max(loc).dst);}
        if (*loc == AfterLoc) {
        vkResetCommandBuffer(commandAfter, /*VkCommandBufferResetFlagBits*/ 0);
        transitionImageLayout(device, graphics, commandAfter, res(typ(),0)->getImage(), before, after, fence, forms, max(loc).src, max(loc).dst);}
        if (*loc == MiddleLoc) {
        Pierce *pie; int x, y, w, h, texWidth, texHeight; VkDeviceSize imageSize;
        range(x,y,w,h,texWidth,texHeight,imageSize,pie,loc,get(ResizeLoc),log);
        createBuffer(device, physical, imageSize, (tag(RuseQua) == Peekz ? VK_BUFFER_USAGE_TRANSFER_DST_BIT : VK_BUFFER_USAGE_TRANSFER_SRC_BIT), VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, memProperties, stagingBuffer, stagingBufferMemory);
        void* data; if (tag(RuseQua) == Imagez || tag(RuseQua) == Pokez) vkMapMemory(device, stagingBufferMemory, 0, imageSize, 0, &data); // TODO stage only the altered range?
        if (tag(RuseQua) == Imagez) memcpy(data, ptr(loc), siz(loc));
        if (tag(RuseQua) == Pokez) for (int i = 0; i < siz(loc); i++) memcpy((void*)((char*)data + x*4 + y*texWidth*4), &pie[i].val, sizeof(pie[i].val));
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        copyTextureImage(device, graphics, memProperties, res(typ(),0)->getImage(), /*x, y, w, h*/0,0,texWidth,texHeight, before, after, stagingBuffer, commandBuffer, tag(RuseQua) == Peekz);}
        return fence;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << " location:" << *loc << '\n';
        if (*loc == MiddleLoc) {
        if (tag(RuseQua) == Peekz) {
        Pierce *pie; int x, y, w, h, texWidth, texHeight; VkDeviceSize imageSize;
        range(x,y,w,h,texWidth,texHeight,imageSize,pie,loc,get(ResizeLoc),log);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, imageSize, 0, &data);
        for (int i = 0; i < siz(loc); i++) memcpy(&pie[i].val, (void*)((char*)data + x*4 + y*texWidth*4), sizeof(pie[i].val));}
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
    ChangeState<Configure,Configures> *change;
    uint32_t imageIndex;
    ResrcLoc imageLoc;
    VkFramebuffer framebuffer;
    ChainState() :
        BaseState("ChainState",StackState::self),
        device(StackState::device),
        present(StackState::present),
        change(StackState::change) {}
    ~ChainState() {
        reset(SmartState());
    }
    uint32_t getImageIndex() override {return imageIndex;}
    ResrcLoc getImageLoc() override {return imageLoc;}
    VkFramebuffer getFramebuffer() override {return framebuffer;}
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << '\n';
        if (*loc == BeforeLoc) {
        sem(loc) = createSemaphore(device);}
    }
    void unsize(Loc &loc, SmartState log) override {
        if (*loc == BeforeLoc) {
        vkDestroySemaphore(device, sem(loc), nullptr);}
        log << "usize " << debug << '\n';
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << '\n';
        if (*loc == BeforeLoc) {
        VkResult result = vkAcquireNextImageKHR(device,
        res(SwapRes,0)->getSwapChain(), UINT64_MAX, sem(loc), VK_NULL_HANDLE, &imageIndex);
        imageLoc = (ResrcLoc)imageIndex;
        if (imageLoc < 0 || imageLoc >= ResrcLocs) EXIT
        if (result == VK_ERROR_OUT_OF_DATE_KHR) change->wots(RegisterMask,1<<SizeMsk);
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) EXIT
        framebuffer = res(SwapRes,0)->getFramebuffer(imageIndex);}
        if (*loc == AfterLoc) {
        VkSemaphore before = sem(lst(loc,(ResrcLoc)(imageIndex%(uint32_t)ResrcLocs)));
        if (!presentFrame(present,res(SwapRes,0)->getSwapChain(),imageIndex,before))
        change->wots(RegisterMask,1<<SizeMsk);}
        return VK_NULL_HANDLE;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << '\n';
    }
    static bool presentFrame(VkQueue present, VkSwapchainKHR swapChain, uint32_t imageIndex, VkSemaphore before);
};

struct DrawState : public BaseState {
    const VkDevice device;
    const VkQueue graphics;
    const VkCommandPool commandPool;
    const int frames;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorLayout;
    VkDescriptorSet descriptorSet;
    VkCommandBuffer commandBuffer;
    DrawState() :
        BaseState("DrawState",StackState::self),
        device(StackState::device),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool),
        frames(StackState::frames) {
        for (int i = 0; i < ResrcLocs; i++) sem(get((ResrcLoc)i)) = createSemaphore(device);
    }
    ~DrawState() {
        for (int i = 0; i < ResrcLocs; i++) vkDestroySemaphore(device, sem(get((ResrcLoc)i)), nullptr);
        reset(SmartState());
    }
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << " " << res(PipeRes,0)->debug << '\n';
        descriptorPool = res(PipeRes,0)->getDescriptorPool();
        descriptorLayout = res(PipeRes,0)->getDescriptorSetLayout();
        descriptorSet = createDescriptorSet(device,descriptorPool,descriptorLayout,frames);
        commandBuffer = createCommandBuffer(device,commandPool);
        fen(loc) = createFence(device);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkDestroyFence(device, fen(loc), nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
        vkFreeDescriptorSets(device,descriptorPool,1,&descriptorSet);
        log << "unsize " << debug << '\n';
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << '\n';
        if (ptr(loc) != 0 || idx(loc) != 0) EXIT
        vkResetFences(device, 1, &fen(loc));
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        BaseState *pipePtr = 0;
        BaseState *swapPtr = 0;
        BaseState *framePtr = 0;
        BaseState *indexPtr = 0;
        BaseState *fetchPtr = 0;
        int index = 0;
        log << "micro " << debug << " " << max(loc) << '\n';
        Arg sav; Arg tmp; HeapState<Arg,0> dot; // TODO remove static from CopyState functions
        for (int i = 0; CopyState::iterate(max(loc).micro,i,sav,tmp,ary(loc),log); i++) dot << tmp;
        // TODO call virtual that increments lock->wrp()
        for (int i = 0; i < dot.size(); i++) // TODO MicroBinding lists all and only, so no need for dot here
        if (dot[i].loc == MiddleLoc && dot[i].ins == RDeeIns ||
        dot[i].loc == MiddleLoc && dot[i].ins == IDeeIns ||
        dot[i].loc == MiddleLoc && dot[i].ins == WDeeIns)
        switch (ResrcPhase__Resrc__Phase(dot[i].res)) {default: EXIT
        break; case (PipePhs): pipePtr = res(dot[i].res,0); // TODO use res that increments hdl
        break; case (FramePhs): framePtr = res(dot[i].res,0); // log << "frame " << framePtr->debug << '\n';
        break; case (SwapPhs): swapPtr = res(dot[i].res,0); // log << "swap " << swapPtr->debug << '\n';
        break; case (RenderPhs): framePtr = swapPtr = res(dot[i].res,0); // log << "frame " << framePtr->debug << " swap " << swapPtr->debug << '\n';
        break; case (IndexPhs): indexPtr = res(IndexRes,0);
        break; case (FetchPhs): fetchPtr = res(BringupRes,0);
        break; case (UniformPhs): {
        BaseState *ptr = res(dot[i].res,0); int idx = ResrcBinding__Resrc__Int(dot[i].res);
        if (ptr->getBuffer() == VK_NULL_HANDLE) EXIT
        updateUniformDescriptor(device,ptr->getBuffer(),ptr->getRange(),idx,descriptorSet);}
        break; case (StoragePhs): {
        BaseState *ptr = res(dot[i].res,0); int idx = ResrcBinding__Resrc__Int(dot[i].res);
        if (ptr->getBuffer() == VK_NULL_HANDLE) EXIT
        updateStorageDescriptor(device,ptr->getBuffer(),ptr->getRange(),idx,descriptorSet);}
        break; case (RelatePhs): {
        BaseState *ptr = res(dot[i].res,0); int idx = ResrcBinding__Resrc__Int(dot[i].res);
        if (ptr->getBuffer() == VK_NULL_HANDLE) EXIT
        updateStorageDescriptor(device,ptr->getBuffer(),ptr->getRange(),idx,descriptorSet);}
        break; case (SamplePhs): {
        BaseState *ptr = res(dot[i].res,0); int idx = ResrcBinding__Resrc__Int(dot[i].res);
        updateTextureDescriptor(device,ptr->getImageView(),ptr->getTextureSampler(),idx,descriptorSet);}}
        if (!pipePtr || !swapPtr || !framePtr || !indexPtr || !fetchPtr) EXIT
        VkExtent2D extent = swapPtr->getExtent();
        recordCommandBuffer(commandBuffer,pipePtr->getRenderPass(),descriptorSet,extent,max(loc).micro,siz(loc),framePtr->getFramebuffer(),pipePtr->getPipeline(),pipePtr->getPipelineLayout(),fetchPtr->getBuffer(),indexPtr->getBuffer());
        VkSemaphore before = VK_NULL_HANDLE; VkSemaphore after = VK_NULL_HANDLE;
        if (framePtr != swapPtr) {before = sem(lst(loc)); after = sem(get(framePtr->getImageLoc()));}
        drawFrame(commandBuffer,graphics,ptr(loc),idx(loc),siz(loc),max(loc).micro,before,after,fen(loc),VK_NULL_HANDLE);
        return fen(loc);
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << '\n';
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
    EnumState enumState[Resrcs+1];
    ConstState constState[2];
    WindowState windowState;
    VulkanState vulkanState;
    PhysicalState physicalState;
    LogicalState logicalState;
    ArrayState<SwapState,SwapRes,1> swapState;
    ArrayState<PipeState,PipeRes,StackState::micros> pipelineState;
    ArrayState<BufferState,IndexRes,StackState::frames> indexState;
    ArrayState<BufferState,BringupRes,StackState::frames> bringupState;
    ArrayState<ImageState,ImageRes,StackState::images> imageState;
    ArrayState<UniformState,UniformRes,StackState::frames> uniformState;
    ArrayState<UniformState,MatrixRes,StackState::frames> matrixState;
    ArrayState<BufferState,TriangleRes,StackState::frames> triangleState;
    ArrayState<BufferState,NumericRes,StackState::frames> numericState;
    ArrayState<BufferState,VertexRes,StackState::frames> vertexState;
    ArrayState<BufferState,BasisRes,StackState::frames> basisState;
    // TODO fold RelateRes PierceRes DebugRes into ImageRes
    ArrayState<ImageState,PierceRes,StackState::frames> pierceState;
    ArrayState<ImageState,DebugRes,StackState::frames> debugState;
    ArrayState<ChainState,ChainRes,StackState::frames> chainState;
    ArrayState<DrawState,DrawRes,StackState::frames> drawState;
    ArrayState<BindState,BindRes,StackState::frames> bindState;
    ThreadState threadState;
    CopyState copyState;
    ChangeState<Configure,Configures> changeState;
    CallState callState;
    MainState() :
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
            {DebugRes,&debugState},
            {ChainRes,&chainState},
            {DrawRes,&drawState},
            {BindRes,&bindState},
            {Resrcs,0}},
        constState{{
            MemoryIns__Memory__Int__Instr,
            MemoryIns__Memory__Int__ResrcLoc,
            MemoryIns__Memory__Int__Format,
            MemoryIns__Memory__Int__Resrc,
            MemoryIns__Memory__Int__Memory,
            MemoryIns__Memory__Int__Micro,
            MemoryIns__Memory__Int__Quality,
            MemoryIns__Memory__Int__Default,
            MemoryIns__Memory__Int__Int,
            ResrcIns__Resrc__Int__Instr,
            ResrcIns__Resrc__Int__ResrcLoc,
            ResrcIns__Resrc__Int__Format,
            ResrcIns__Resrc__Int__Resrc,
            ResrcIns__Resrc__Int__Memory,
            ResrcIns__Resrc__Int__Micro,
            ResrcIns__Resrc__Int__Quality,
            ResrcIns__Resrc__Int__Default,
            ResrcIns__Resrc__Int__Int,
            MicroIns__Micro__Int__Instr,
            MicroIns__Micro__Int__ResrcLoc,
            MicroIns__Micro__Int__Format,
            MicroIns__Micro__Int__Resrc,
            MicroIns__Micro__Int__Memory,
            MicroIns__Micro__Int__Micro,
            MicroIns__Micro__Int__Quality,
            MicroIns__Micro__Int__Default,
            MicroIns__Micro__Int__Int},{
            MemoryAlt__Memory__Int__Instr,
            MemoryAlt__Memory__Int__ResrcLoc,
            MemoryAlt__Memory__Int__Format,
            MemoryAlt__Memory__Int__Resrc,
            MemoryAlt__Memory__Int__Memory,
            MemoryAlt__Memory__Int__Micro,
            MemoryAlt__Memory__Int__Quality,
            MemoryAlt__Memory__Int__Default,
            MemoryAlt__Memory__Int__Int,
            ResrcAlt__Resrc__Int__Instr,
            ResrcAlt__Resrc__Int__ResrcLoc,
            ResrcAlt__Resrc__Int__Format,
            ResrcAlt__Resrc__Int__Resrc,
            ResrcAlt__Resrc__Int__Memory,
            ResrcAlt__Resrc__Int__Micro,
            ResrcAlt__Resrc__Int__Quality,
            ResrcAlt__Resrc__Int__Default,
            ResrcAlt__Resrc__Int__Int,
            MicroAlt__Micro__Int__Instr,
            MicroAlt__Micro__Int__ResrcLoc,
            MicroAlt__Micro__Int__Format,
            MicroAlt__Micro__Int__Resrc,
            MicroAlt__Micro__Int__Memory,
            MicroAlt__Micro__Int__Micro,
            MicroAlt__Micro__Int__Quality,
            MicroAlt__Micro__Int__Default,
            MicroAlt__Micro__Int__Int}},
        vulkanState(windowState.window),
        physicalState(vulkanState.instance,vulkanState.surface),
        logicalState(physicalState.device,physicalState.graphicsFamily,
            physicalState.presentFamily,vulkanState.validationLayers,
            physicalState.deviceExtensions),
        swapState(&changeState,
            windowState.window,vulkanState.surface,physicalState.device,
            physicalState.surfaceFormat,physicalState.presentMode,
            physicalState.graphicsFamily,physicalState.presentFamily,
            physicalState.properties,physicalState.memProperties,
            logicalState.device,logicalState.commandPool,logicalState.renderPass,
            logicalState.imageFormat,logicalState.depthFormat,
            logicalState.graphics,logicalState.present),
        pipelineState(constState),
        indexState(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
        bringupState(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
        triangleState(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
        threadState(logicalState.device,&changeState),
        copyState(&changeState,&threadState,enumState,constState) {
        std::cout << "MainState" << std::endl;
    }
    ~MainState() {
        std::cout << "~MainState" << std::endl;
    }
};

MainState *mptr = 0;
// glfw callbacks
void glfwKeypress(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (key == GLFW_KEY_C && action == GLFW_PRESS && mods == GLFW_MOD_CONTROL) EXIT
}
// copy request
void vulkanCopy(Center *ptr, int sub, Rsp rsp, int ary, const char *dbg) {
    if (dbg) mptr->copyState.push(ptr,sub,rsp,ary,SmartState(dbg));
    else mptr->copyState.push(ptr,sub,rsp,ary,SmartState());
}
// add callback
void vulkanCall(Configure cfg, xftype back) {
    mptr->changeState.call(cfg,back);
}
// add thread
void vulkanFork(Thread thd, int idx, mftype fnc, mftype done, mftype join, mftype wake) {
    mptr->callState.push(new ForkState(thd,idx,fnc,done,join,wake));
}
// register access
int vulkanInfo(Configure cfg, int val, yftype fnc) {
    return mptr->changeState.info(cfg,val,fnc);
}
int vulkanJnfo(Configure cfg, int val, yftype fnc) {
    return mptr->changeState.jnfo(cfg,val,fnc);
}
int vulkanKnfo(Configure cfg, int val, yftype fnc) {
    return mptr->changeState.knfo(cfg,val,fnc);
}
// builtin callback
void vulkanBack(Configure cfg, int sav, int val, int act) {
    if (cfg == RegisterOpen) mptr->callState.open(sav,val,act);
    if (cfg == RegisterWake) mptr->callState.wake(sav,val,act);
}
// startup configuration
HeapState<const char *,0> cfg;
const char *vulkanCmnd(int req) {
    if (req < 0 || req >= cfg.size()) return 0;
    return cfg[req];
}
// mptr wrapper
void vulkanWait() {
    if (mptr->callState.self()) glfwWaitEventsTimeout(mptr->changeState.read(RegisterPoll)*0.001);
    else sleepSec(mptr->changeState.read(RegisterPoll)*0.001);
}
// c debug
void vulkanExit() {
    void *buffer[100];
    int size = backtrace(buffer,100);
    backtrace_symbols_fd(buffer,size,2);
}
void whereIsExit(int val, void *arg) {
    if (val < 0) *(int*)0=0;
}
void errorFunc(const char *str, int num, int idx) {
    std::cout << "errfnc called on " << idx << " in " << str << ": " << num << std::endl;
}
void sigintFunc(int sig) {
    *(int*)0=0;
}

int main(int argc, const char **argv) {
    struct sigaction act;
    act.sa_handler = sigintFunc;
    if (sigaction(SIGINT,&act,0) < 0) ERROR();
    // errFunc(errorFunc); // in case pipe is closed just before written to
    // on_exit(whereIsExit,0);
    // TODO parse argv for arguments to main and push only unparsed to cfg
    for (int i = 1; i < argc; i++) cfg << argv[i];
    // TODO pass parsed arguments to main
    slog.onof(0,10000,123,5);
    MainState main;
    mptr = &main;
    main.changeState.write(ConstantDescrs,StackState::descrs);
    main.changeState.write(ConstantMicros,StackState::micros);
    main.changeState.write(ConstantFrames,StackState::frames);
    main.changeState.write(ConstantImages,StackState::images);
    main.changeState.write(ConstantInstrs,StackState::instrs);
    main.changeState.write(ConstantResrcs,StackState::resrcs);
    main.changeState.write(ConstantHandls,StackState::handls);
    main.changeState.call(RegisterOpen,vulkanBack);
    main.changeState.call(RegisterWake,vulkanBack);
    main.callState.back(&main.threadState,FenceThd);
    planeInit(vulkanCopy,vulkanCall,vulkanFork,vulkanInfo,vulkanJnfo,vulkanKnfo,vulkanCmnd,vulkanWait);
    // TODO move glfw functions to WindowState
    glfwSetKeyCallback(main.windowState.window,glfwKeypress);
    int count = 0;
    while (!glfwWindowShouldClose(main.windowState.window) && planeLoop()) {
    if (main.changeState.read(RegisterPoll) == 0) glfwWaitEvents();
    else glfwWaitEventsTimeout(main.changeState.read(RegisterPoll)*0.001);}
    planeDone();
    int ret = main.changeState.read(RegisterExit);
    return (ret > 0 ? ret-1 : ret);
}

GLFWwindow* WindowState::createWindow(uint32_t WIDTH, uint32_t HEIGHT) {
    glfwInit();
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    return glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
}

VKAPI_ATTR VkBool32 VKAPI_CALL VulkanState::debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData) {
    std::cout << "validation layer: " << pCallbackData->pMessage << " severity:0x" << std::hex << messageSeverity << std::dec << std::endl;
    // if (messageSeverity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) *(int*)0=0;
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
        HeapState<VkLayerProperties> availableLayers; availableLayers.fill({},count);
        vkEnumerateInstanceLayerProperties(&count, availableLayers.data());
        for (const char **name = validationLayers; *name; name++) {
        bool found = false; for (uint32_t i = 0; i < count; i++)
        if (strcmp(*name, availableLayers[i].layerName) == 0) {found = true; break;}
        if (!found) EXIT}}
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
    HeapState<const char *> extensions; extensions.fill(0,size);
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
    EXIT
    return instance;
}
VkDebugUtilsMessengerEXT VulkanState::createDebug(VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info,
    const char **validationLayers) {
    VkDebugUtilsMessengerEXT debug{};
    if (!validationLayers) return debug;
    auto func = (PFN_vkCreateDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
    if (func == nullptr || func(instance, &info, nullptr, &debug) != VK_SUCCESS)
    EXIT
    return debug;
}
VkSurfaceKHR VulkanState::createSurface(VkInstance instance, GLFWwindow* window) {
    VkSurfaceKHR surface;
    if (glfwCreateWindowSurface(instance, window, nullptr, &surface) != VK_SUCCESS)
    EXIT
    return surface;
}

bool PhysicalState::foundIndices(VkSurfaceKHR surface, VkPhysicalDevice device) {
    bool foundGraphics = false; bool foundPresent = false;
    uint32_t count = 0; vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);
    HeapState<VkQueueFamilyProperties> queueFamilies; queueFamilies.push(count);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, queueFamilies.data());
    uint32_t i = 0; for (int j = 0; j < count; j++) {
        VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i++, surface, &support);
        if (support) foundPresent = true;
        if (queueFamilies[j].queueFlags & VK_QUEUE_GRAPHICS_BIT) foundGraphics = true;
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
    if (count == 0) EXIT
    HeapState<VkPhysicalDevice> devices; devices.fill({},count);
    vkEnumeratePhysicalDevices(instance, &count, devices.data());
    bool found = false; for (int i = 0; i < devices.size(); i++) {
        if (!foundIndices(surface,devices[i])) continue;
    if (!foundDetails(surface,devices[i])) continue;
        count = 0; vkEnumerateDeviceExtensionProperties(devices[i], nullptr, &count, nullptr);
        HeapState<VkExtensionProperties> properties; properties.fill({},count);
        vkEnumerateDeviceExtensionProperties(devices[i], nullptr, &count, properties.data());
        bool found0 = true; for (const char **name = deviceExtensions; *name; name++) {
        bool found1 = false; for (uint32_t j = 0; j < count; j++)
        if (strcmp(*name,properties[j].extensionName) == 0)
        {found1 = true; break;} if (!found1) {found0 = false; break;}} if (!found0) continue;
        VkPhysicalDeviceFeatures supportedFeatures;
        vkGetPhysicalDeviceFeatures(devices[i], &supportedFeatures);
        if (!supportedFeatures.samplerAnisotropy) continue;
        found = true; retdev = devices[i]; break;}
    if (!found) EXIT
    return retdev;
}
uint32_t PhysicalState::findGraphicsFamily(VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t count = 0; vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);
    HeapState<VkQueueFamilyProperties> queueFamilies; queueFamilies.fill({},count);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, queueFamilies.data());
    uint32_t i = 0; for (int j = 0; j < queueFamilies.size(); j++) {
        VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &support);
        if (support && (queueFamilies[j].queueFlags & VK_QUEUE_GRAPHICS_BIT)) return i; i++;}
    i = 0; for (int j = 0; j < queueFamilies.size(); j++) {
        if (queueFamilies[j].queueFlags & VK_QUEUE_GRAPHICS_BIT) return i; i++;}
    return 0;
}
uint32_t PhysicalState::findPresentFamily(VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t count = 0; vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);
    HeapState<VkQueueFamilyProperties> queueFamilies; queueFamilies.fill({},count);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, queueFamilies.data());
    uint32_t i = 0; for (int j = 0; j < queueFamilies.size(); j++) {
        VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &support);
        if (support && (queueFamilies[j].queueFlags & VK_QUEUE_GRAPHICS_BIT)) return i; i++;}
    i = 0; for (int j = 0; j < queueFamilies.size(); j++) {
        VkBool32 support = false; vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &support);
        if (support) return i; i++;}
    return 0;
}
VkPhysicalDeviceProperties PhysicalState::findProperties(VkPhysicalDevice device) {
    VkPhysicalDeviceProperties properties{};
    vkGetPhysicalDeviceProperties(device, &properties);
    return properties;
}
VkSurfaceFormatKHR PhysicalState::chooseSwapSurfaceFormat(VkFormat format, VkColorSpaceKHR space, VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t count = 0; vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, nullptr);
    HeapState<VkSurfaceFormatKHR> formats; formats.fill({},count);
    if (count != 0) vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, formats.data());
    for (int i = 0; i < formats.size(); i++)
    if (formats[i].format == format && formats[i].colorSpace == space)
    return formats[i];
    return formats[0];
}
VkPresentModeKHR PhysicalState::chooseSwapPresentMode(VkSurfaceKHR surface, VkPhysicalDevice device) {
    uint32_t count = 0; vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, nullptr);
    HeapState<VkPresentModeKHR> presentModes; presentModes.fill({},count);
    if (count != 0) vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, presentModes.data());
    for (int i = 0; i < presentModes.size(); i++) {
    if (presentModes[i] == VK_PRESENT_MODE_MAILBOX_KHR) {
    return presentModes[i];}}
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
    HeapState<VkDeviceQueueCreateInfo> queueCreateInfos;
    uint32_t queueFamilies[] = {graphicsFamily, presentFamily};
    float queuePriority = 1.0f;
    for (uint32_t i = 0; i < (graphicsFamily == presentFamily ? 1 : 2); i++) {
        VkDeviceQueueCreateInfo queueCreateInfo{};
        queueCreateInfo.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        queueCreateInfo.queueFamilyIndex = queueFamilies[i];
        queueCreateInfo.queueCount = 1;
        queueCreateInfo.pQueuePriorities = &queuePriority;
        queueCreateInfos << queueCreateInfo;}
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
    EXIT
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
    EXIT
    return pool;
}
VkFormat LogicalState::findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size) {
    VkImageTiling tiling = VK_IMAGE_TILING_OPTIMAL;
    VkFormatFeatureFlags features = VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT;
    for (int i = 0; i < size; i++) {
    VkFormatProperties props;
    vkGetPhysicalDeviceFormatProperties(physicalDevice, candidates[i], &props);
    if (tiling == VK_IMAGE_TILING_LINEAR && (props.linearTilingFeatures & features) == features) return candidates[i];
    else if (tiling == VK_IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures & features) == features) return candidates[i];}
    EXIT
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
    VkAttachmentDescription attachments[] = {colorAttachment, depthAttachment};
    VkRenderPassCreateInfo renderPassInfo{};
    renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    renderPassInfo.attachmentCount = static_cast<uint32_t>(sizeof(attachments)/sizeof(VkAttachmentDescription));
    renderPassInfo.pAttachments = attachments;
    renderPassInfo.subpassCount = 1;
    renderPassInfo.pSubpasses = &subpass;
    renderPassInfo.dependencyCount = 1;
    renderPassInfo.pDependencies = &dependency;
    VkRenderPass renderPass;
    if (vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPass) != VK_SUCCESS)
    EXIT
    return renderPass;
}

uint32_t BaseState::findMemoryType(VkPhysicalDevice device, uint32_t filter, VkMemoryPropertyFlags flags,
    VkPhysicalDeviceMemoryProperties memProperties) {
    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++)
    if ((filter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & flags) == flags) return i;
    EXIT
}
VkCommandBuffer BaseState::createCommandBuffer(VkDevice device, VkCommandPool pool) {
    VkCommandBuffer commandBuffer;
    VkCommandBufferAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    allocInfo.commandPool = pool;
    allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    allocInfo.commandBufferCount = (uint32_t)(1);
    if (vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer) != VK_SUCCESS)
    EXIT
    return commandBuffer;
}
VkFence BaseState::createFence(VkDevice device) {
    VkFence fence;
    VkFenceCreateInfo fenceInfo{};
    fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;
    if (vkCreateFence(device, &fenceInfo, nullptr, &fence) != VK_SUCCESS)
    EXIT
    return fence;
}
VkSemaphore BaseState::createSemaphore(VkDevice device) {
    VkSemaphore semaphore;
    VkSemaphoreCreateInfo semaphoreInfo{};
    semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    if (vkCreateSemaphore(device, &semaphoreInfo, nullptr, &semaphore) != VK_SUCCESS)
    EXIT
    return semaphore;
}
void BaseState::createImage(VkDevice device, VkPhysicalDevice physical, uint32_t width, uint32_t height, VkFormat format, VkImageUsageFlags usage, VkPhysicalDeviceMemoryProperties memProperties, VkImage& image, VkDeviceMemory& imageMemory) {
    VkImageCreateInfo imageInfo{};
    imageInfo.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    imageInfo.imageType = VK_IMAGE_TYPE_2D;
    imageInfo.extent.width = width;
    imageInfo.extent.height = height;
    imageInfo.extent.depth = 1;
    imageInfo.mipLevels = 1;
    imageInfo.arrayLayers = 1;
    imageInfo.format = format;
    imageInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
    imageInfo.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
    imageInfo.usage = usage;
    imageInfo.samples = VK_SAMPLE_COUNT_1_BIT;
    imageInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    if (vkCreateImage(device, &imageInfo, nullptr, &image) != VK_SUCCESS)
    EXIT
    VkMemoryRequirements memRequirements;
    vkGetImageMemoryRequirements(device, image, &memRequirements);
    VkMemoryAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize = memRequirements.size;
    allocInfo.memoryTypeIndex = findMemoryType(physical, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties);
    if (vkAllocateMemory(device, &allocInfo, nullptr, &imageMemory) != VK_SUCCESS)
    EXIT
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
    EXIT
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
    EXIT
    VkMemoryRequirements memRequirements;
    vkGetBufferMemoryRequirements(device, buffer, &memRequirements);
    VkMemoryAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize = memRequirements.size;
    allocInfo.memoryTypeIndex = findMemoryType(physical, memRequirements.memoryTypeBits, properties, memProperties);
    if (vkAllocateMemory(device, &allocInfo, nullptr, &memory) != VK_SUCCESS)
    EXIT
    vkBindBufferMemory(device, buffer, memory, 0);
}
void BaseState::createFramebuffer(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
    VkImageView swapChainImageView, VkImageView depthImageView, VkFramebuffer &framebuffer) {
    VkImageView attachments[] = {swapChainImageView,depthImageView};
    VkFramebufferCreateInfo framebufferInfo{};
    framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    framebufferInfo.renderPass = renderPass;
    framebufferInfo.attachmentCount = static_cast<uint32_t>(sizeof(attachments)/sizeof(VkImageView));
    framebufferInfo.pAttachments = attachments;
    framebufferInfo.width = swapChainExtent.width;
    framebufferInfo.height = swapChainExtent.height;
    framebufferInfo.layers = 1;
    if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &framebuffer) != VK_SUCCESS)
    EXIT
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
    EXIT
    return swapChain;
}
void SwapState::createSwapChainImages(VkDevice device, VkSwapchainKHR swapChain, HeapState<VkImage> &swapChainImages) {
    uint32_t imageCount;
    vkGetSwapchainImagesKHR(device, swapChain, &imageCount, nullptr);
    swapChainImages.fill({},imageCount);
    vkGetSwapchainImagesKHR(device, swapChain, &imageCount, swapChainImages.data());
}
void SwapState::createFramebuffers(VkDevice device, VkExtent2D swapChainExtent, VkRenderPass renderPass,
    HeapState<VkImageView> &swapChainImageViews, VkImageView depthImageView,
    HeapState<VkFramebuffer> &framebuffers) {
    framebuffers.fill({},swapChainImageViews.size());
    for (int i = 0; i < swapChainImageViews.size(); i++)
    createFramebuffer(device,swapChainExtent,renderPass,swapChainImageViews[i],depthImageView,framebuffers[i]);
}

VkDescriptorPool PipeState::createDescriptorPool(VkDevice device, int frames) {
    VkDescriptorPool descriptorPool;
    HeapState<VkDescriptorPoolSize, 3> poolSizes; poolSizes.fill({});
    poolSizes[0].type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    poolSizes[0].descriptorCount = static_cast<uint32_t>(frames);
    poolSizes[1].type = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    poolSizes[1].descriptorCount = static_cast<uint32_t>(frames);
    poolSizes[2].type = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    poolSizes[2].descriptorCount = static_cast<uint32_t>(frames);
    VkDescriptorPoolCreateInfo descriptorPoolInfo{};
    descriptorPoolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
    descriptorPoolInfo.flags = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT;
    descriptorPoolInfo.poolSizeCount = static_cast<uint32_t>(poolSizes.size());
    descriptorPoolInfo.pPoolSizes = poolSizes.data();
    descriptorPoolInfo.maxSets = static_cast<uint32_t>(frames);
    if (vkCreateDescriptorPool(device, &descriptorPoolInfo, nullptr, &descriptorPool) != VK_SUCCESS)
    EXIT
    return descriptorPool;
}
VkDescriptorSetLayout PipeState::createDescriptorSetLayout(VkDevice device, Micro micro, const ConstState *func) {
    VkDescriptorSetLayout descriptorSetLayout;
    HeapState<VkDescriptorSetLayoutBinding> bindings;
    auto fnc = func->micres(micro); // TODO resources depends on micro only
    for (int i = 0; fnc(i) != Resrcs; i++)
    switch (ResrcPhase__Resrc__Phase(fnc(i))) {default: // TODO phase depends on micro only
    break; case (UniformPhs): {
    VkDescriptorSetLayoutBinding uboLayoutBinding{};
    uboLayoutBinding.binding = ResrcBinding__Resrc__Int(fnc(i)); // TODO binding depends on micro only
    uboLayoutBinding.descriptorCount = 1;
    uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    uboLayoutBinding.pImmutableSamplers = nullptr;
    uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
    bindings << uboLayoutBinding;}
    break; case (StoragePhs): {
    VkDescriptorSetLayoutBinding storageLayoutBinding{};
    storageLayoutBinding.binding = ResrcBinding__Resrc__Int(fnc(i));
    storageLayoutBinding.descriptorCount = 1;
    storageLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    storageLayoutBinding.pImmutableSamplers = nullptr;
    storageLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
    bindings << storageLayoutBinding;}
    break; case (RelatePhs): {
    VkDescriptorSetLayoutBinding relateLayoutBinding{};
    relateLayoutBinding.binding = ResrcBinding__Resrc__Int(fnc(i));
    relateLayoutBinding.descriptorCount = 1;
    relateLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    relateLayoutBinding.pImmutableSamplers = nullptr;
    relateLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
    bindings << relateLayoutBinding;}
    break; case (SamplePhs): {
    VkDescriptorSetLayoutBinding samplerLayoutBinding{};
    samplerLayoutBinding.binding = 7; // bindings.size();
    samplerLayoutBinding.descriptorCount = 1;
    samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    samplerLayoutBinding.pImmutableSamplers = nullptr;
    samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
    bindings << samplerLayoutBinding;}}
    VkDescriptorSetLayoutCreateInfo layoutInfo{};
    layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    layoutInfo.bindingCount = static_cast<uint32_t>(bindings.size());
    layoutInfo.pBindings = bindings.data();
    if (vkCreateDescriptorSetLayout(device, &layoutInfo, nullptr, &descriptorSetLayout) != VK_SUCCESS)
    EXIT
    return descriptorSetLayout;
}
VkPipelineLayout PipeState::createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout) {
    VkPipelineLayout pipelineLayout;
    VkPipelineLayoutCreateInfo pipelineLayoutInfo{};
    pipelineLayoutInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pipelineLayoutInfo.setLayoutCount = 1;
    pipelineLayoutInfo.pSetLayouts = &descriptorSetLayout;
    if (vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout) != VK_SUCCESS)
    EXIT
    return pipelineLayout;
}
VkShaderModule PipeState::createShaderModule(VkDevice device, const char *filename) {
    std::ifstream file(filename, std::ios::ate | std::ios::binary);
    if (!file.is_open()) EXIT
    size_t fileSize = (size_t) file.tellg();
    char *buffer = (char*)malloc(fileSize);
    file.seekg(0);
    file.read(buffer, fileSize);
    file.close();
    VkShaderModuleCreateInfo createInfo{};
    createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    createInfo.codeSize = fileSize;
    createInfo.pCode = (uint32_t*)buffer;
    VkShaderModule shaderModule;
    if (vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule) != VK_SUCCESS) EXIT
    free(buffer);
    return shaderModule;
}
VkPipeline PipeState::createGraphicsPipeline(VkDevice device, VkRenderPass renderPass,
    VkPipelineLayout pipelineLayout, Micro micro, const ConstState *func) {
    VkPipeline pipeline;
    VkShaderModule vertShaderModule = createShaderModule(device,VertexFile__Micro__Str(micro));
    VkShaderModule fragShaderModule = createShaderModule(device,FragmentFile__Micro__Str(micro));
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
    HeapState<VkVertexInputBindingDescription> bindingDescriptions;
    HeapState<VkVertexInputAttributeDescription> attributeDescriptions;
    auto fnc = func->micres(micro); // TODO resources depends on micro only
    for (int i = 0; fnc(i) != Resrcs; i++)
    switch (ResrcPhase__Resrc__Phase(fnc(i))) {default: // TODO phase depends on micro only
    break; case (FetchPhs): {
    VkVertexInputBindingDescription bindingDescription{};
    bindingDescription.binding = ResrcBinding__Resrc__Int(fnc(i)); // TODO binding depends on micro only
    bindingDescription.stride = ResrcStride__Resrc__Int(fnc(i));
    bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
    bindingDescriptions << bindingDescription;
    for (int j = 0; ResrcFormat__Resrc__Int__Packing(fnc(i))(j) != Packings; j++) {
    VkVertexInputAttributeDescription attributeDescription{};
    attributeDescription.binding = ResrcBinding__Resrc__Int(fnc(i));
    attributeDescription.location = j;
    attributeDescription.format = PhysicalState::vulkanFormat(ResrcFormat__Resrc__Int__Packing(fnc(i))(j));
    attributeDescription.offset = ResrcOffset__Resrc__Int__Int(fnc(i))(j);
    attributeDescriptions << attributeDescription;}}}
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
    rasterizer.cullMode = VK_CULL_MODE_NONE; // VK_CULL_MODE_BACK_BIT;
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
    colorBlendAttachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
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
    VkDynamicState dynamicStates[] = {VK_DYNAMIC_STATE_VIEWPORT,VK_DYNAMIC_STATE_SCISSOR};
    VkPipelineDynamicStateCreateInfo dynamicState{};
    dynamicState.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
    dynamicState.dynamicStateCount = static_cast<uint32_t>(sizeof(dynamicStates)/sizeof(VkDynamicState));
    dynamicState.pDynamicStates = dynamicStates;
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
    if (vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &pipeline) != VK_SUCCESS) EXIT
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
    EXIT
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
    switch (oldLayout) {default: EXIT
    break; case (VK_IMAGE_LAYOUT_UNDEFINED):
    barrier.srcAccessMask = 0;
    sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
    sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
    sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_PRESENT_SRC_KHR):
    barrier.srcAccessMask = VK_ACCESS_SHADER_WRITE_BIT;
    sourceStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    break; case (VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL):
    barrier.srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
    sourceStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;}
    switch (newLayout) {default: EXIT
    break; case (VK_IMAGE_LAYOUT_UNDEFINED):
    barrier.dstAccessMask = 0;
    destinationStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL):
    barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
    destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL):
    barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
    destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    break; case (VK_IMAGE_LAYOUT_PRESENT_SRC_KHR):
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
    EXIT
    return (result == VK_SUCCESS);
}

VkDescriptorSet DrawState::createDescriptorSet(VkDevice device, VkDescriptorPool descriptorPool,
    VkDescriptorSetLayout descriptorSetLayout, int frames) {
    VkDescriptorSet descriptorSet;
    HeapState<VkDescriptorSetLayout> layouts; layouts.fill(descriptorSetLayout,frames);
    VkDescriptorSetAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
    allocInfo.descriptorPool = descriptorPool;
    allocInfo.descriptorSetCount = static_cast<uint32_t>(1);
    allocInfo.pSetLayouts = layouts.data();
    if (vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet) != VK_SUCCESS) EXIT
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
    vkUpdateDescriptorSets(device, 1, descriptorWrites, 0, nullptr);
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
    EXIT
    VkRenderPassBeginInfo renderPassInfo{};
    renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    renderPassInfo.renderPass = renderPass;
    renderPassInfo.framebuffer = framebuffer;
    renderPassInfo.renderArea.offset = {0, 0};
    renderPassInfo.renderArea.extent = renderArea;
    HeapState<VkClearValue, 2> clearValues; clearValues.fill({});
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
    EXIT
}
void DrawState::drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, void *ptr, int loc, int siz, Micro micro,
    VkSemaphore acquire, VkSemaphore after, VkFence fence, VkSemaphore before) {
    VkSubmitInfo submitInfo{};
    submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    VkSemaphore waitSemaphores[] = {acquire,before};
    VkPipelineStageFlags waitStages[] = {
    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    VK_PIPELINE_STAGE_ALL_COMMANDS_BIT};
    submitInfo.waitSemaphoreCount = (acquire == VK_NULL_HANDLE ? 0 : (before == VK_NULL_HANDLE ? 1 : 2));
    submitInfo.pWaitSemaphores = waitSemaphores;
    submitInfo.pWaitDstStageMask = waitStages;
    VkCommandBuffer commandBuffers[] = {commandBuffer};
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &commandBuffer;
    VkSemaphore signalSemaphores[] = {after};
    submitInfo.signalSemaphoreCount = (after == VK_NULL_HANDLE ? 0 : 1);
    submitInfo.pSignalSemaphores = signalSemaphores;
    if (vkQueueSubmit(graphics, 1, &submitInfo, fence) != VK_SUCCESS)
    EXIT
}
