#define GLFW_INCLUDE_VULKAN
#define _GLFW_X11
#define _GLFW_WAYLAND
#include <GLFW/glfw3.h>

#include <fstream>
#include <cstring>
#include <signal.h>
extern "C" {
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
        // std::cout << "WindowState" << std::endl;
    }
    ~WindowState() {
        glfwDestroyWindow(window);
        glfwTerminate();
        // std::cout << "~WindowState" << std::endl;
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
        // std::cout << "VulkanState" << std::endl;
    }
    ~VulkanState() {
        vkDestroySurfaceKHR(instance, surface, nullptr);
        auto func = (PFN_vkDestroyDebugUtilsMessengerEXT)
        vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
        if (func != nullptr) func(instance, debug, nullptr);
        vkDestroyInstance(instance, nullptr);
        // std::cout << "~VulkanState" << std::endl;
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
    // fragment shader can output vectors as well as uint or float
    static VkFormat vulkanFormat(Render i) {
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
        surfaceFormat(chooseSwapSurfaceFormat(vulkanFormat(SrgbFrm),VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,surface,device)),
        presentMode(chooseSwapPresentMode(surface,device)),
        memProperties(findMemoryProperties(device)) {
        // std::cout << "PhysicalState " << properties.deviceName << std::endl;
    }
    ~PhysicalState() {
        // std::cout << "~PhysicalState" << std::endl;
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
        // std::cout << "LogicalState" << std::endl;
        for (int i = 0; i < passes; i++) imageFormat[i] = PhysicalState::vulkanFormat((Render)i);
        for (int i = 0; i < passes; i++) renderPass[i] = createRenderPass(device,imageFormat[i],depthFormat);
    }
    ~LogicalState() {
        for (int i = 0; i < passes; i++) vkDestroyRenderPass(device, renderPass[i], nullptr);
        vkDestroyCommandPool(device, commandPool, nullptr);
        vkDestroyDevice(device, nullptr);
        // std::cout << "~LogicalState" << std::endl;
    }
    static VkDevice createDevice(VkPhysicalDevice physicalDevice, uint32_t graphicsFamily, uint32_t presentFamily, const char **validationLayers, const char **deviceExtensions);
    static VkQueue createQueue(VkDevice device, uint32_t family);
    static VkCommandPool createCommandPool(VkDevice device, uint32_t family);
    static VkFormat findSupportedFormat(VkPhysicalDevice physicalDevice, const VkFormat candidates[], int size);
    static VkRenderPass createRenderPass(VkDevice device, VkFormat imageFormat, VkFormat depthFormat);
};

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
    BaseState *ptr = 0; Reloc loc;
};
enum Advance {
    PushAdv, FnceAdv, QualAdv,
};
struct Adv {
    Advance adv; int hdl; Quality key; int val;
};
struct SaveState;
struct Unl {
    SaveState *der; int dee; int siz;
};
struct Bnd {
    BaseState *buf; Resrc typ; Phase phs; int bnd; Instr ins;
};
struct Loc {
    SizeState max;
    Requ req;
    Unl unl;
    Syn syn;
    Lnk lst; 
    Lnk nxt;
    VkCommandBuffer commandBuffer;
    VkBuffer stagingBuffer;
    VkDeviceMemory stagingBufferMemory;
    Requ *operator->() {return &req;}
};
Reloc &operator*(Loc &loc) {
    return loc.req.loc;
}
struct StackState;
struct BindState;
struct BaseState {
    StackState *item;
    int indx;
    SafeState safe;
    bool valid;
    int plock, rlock, wlock;
    BindState *lock;
    Loc ploc[Relocs];
    int mask; // which ploc have valid max
    Adv adv;
    char debug[64];
    int index();
    int count();
    const char *bname();
    BaseState(const char *name, StackState *ptr) :
        item(ptr),
        indx(index()),
        safe(1),
        valid(false),
        plock(0),
        rlock(0),
        wlock(0),
        lock(0),
        mask(0),
        debug{} {
        sprintf(debug,"%s_%s_%d",name,bname(),count());
        // std::cout << debug << std::endl;
    }
    ~BaseState() {
        // std::cout << "~" << debug << std::endl;
    }
    bool check(SaveState *sav);
    bool push(BindState *ptr, Requ req, Unl unl, SmartState log) {
        // reserve before pushing to thread
        safe.wait();
        if (check(unl.der)) {
        {char *st0 = 0; char *st1 = 0;
        showReloc(req.loc,&st0); showReuse(req.use,&st1);
        log << debug << ":fail " << st0 << " " << st1 << '\n';
        free(st0); free(st1);}
        safe.post(); return false;}
        {char *st0 = 0; char *st1 = 0;
        showReloc(req.loc,&st0); showReuse(req.use,&st1);
        log << debug << ":pass " << st0 << " " << st1 << '\n';
        free(st0); free(st1);}
        plock += 1;
        safe.post();
        if (lock != 0 && lock != ptr) EXIT
        lock = ptr;
        ploc[req.loc].req = req;
        ploc[req.loc].unl = unl;
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
        plock = 0; lock = 0;
        safe.post();
    }
    void reset(SmartState log) {
        for (int i = 0; i < Relocs; i++)
        if (ploc[i].max == SizeState(InitExt));
        else unsize(ploc[i],log);
    }
    void finish() {
        safe.wait();
        valid = false;
        safe.post();
    }
    bool recall(Loc &loc, SmartState log) {
        SizeState max = SizeState(loc.req.ext,loc.req.base,loc.req.size);
        SizeState ini = SizeState(InitExt);
        int msk = 1<<*loc;
        safe.wait(); bool temp = valid; safe.post();
        if (temp && loc.max == max); else {
        if (loc.max == ini); else {
        mask &= ~msk;
        if (mask == 0) {safe.wait(); valid = false; safe.post();}
        unsize(loc,log);}
        loc.max = max;
        if (loc.max == ini); else {
        resize(loc,log);
        if (mask == 0) {safe.wait(); valid = true; safe.post();}
        mask |= msk;
        return true;}}
        return false;
    }
    VkFence basesiz(Reloc loc, SmartState log) {
        // resize and setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != BothReq) EXIT
        safe.post();
        recall(ploc[loc],log);
        return setup(ploc[loc],log);
    }
    void basenul(Reloc loc, SmartState log) {
        // resize and setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != NullReq) EXIT
        safe.post();
        recall(ploc[loc],log);
        if (setup(ploc[loc],log) != VK_NULL_HANDLE) EXIT;
    }
    void baseres(Reloc loc, SmartState log) {
        // resize only
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != SizeReq) EXIT
        safe.post();
        recall(ploc[loc],log);
    }
    VkFence basesup(Reloc loc, SmartState log) {
        // setup only
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != LockReq) EXIT
        safe.post();
        return setup(ploc[loc],log);
    }
    VkFence basexor(Reloc loc, SmartState log) {
        // resize and maybe setup
        safe.wait();
        if (plock <= 0 || ploc[loc].req.tag != ExclReq) EXIT
        safe.post();
        if (!recall(ploc[loc],log)) return VK_NULL_HANDLE;
        return setup(ploc[loc],log);        
    }
    void unlock(Unl &unl, SmartState log);
    void advance(Adv &adv, SmartState log);
    void baseups(Reloc loc, SmartState log) {
        // after fence triggered
        log << "baseups " << debug << '\n';
        upset(ploc[loc],log);
        if (lock) unlock(ploc[loc].unl,log);
        safe.wait();
        if (plock <= 0) EXIT
        plock -= 1;
        if (plock == 0) {
        lock = 0;
        if (adv.adv == FnceAdv) advance(adv,log);}
        safe.post();
    }
    bool incr(bool elock, int psav, int rsav, int wsav) {
        safe.wait();
        if (plock < psav || wlock < wsav || rlock < rsav) EXIT
        if (!(valid || psav) || plock-psav || wlock-wsav || (elock && rlock-rsav)) {
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
    Lnk *link(Reloc loc, BaseState *ptr, Reloc lst, Lnk *lnk) {
        if ((int)loc < 0 || (int)loc >= Relocs) EXIT
        Loc &ref = ploc[loc];
        SizeState max = SizeState(ref.req.ext,ref.req.base,ref.req.size);
        SizeState ini = SizeState(InitExt);
        switch (ref.req.tag) {default: EXIT
        break; case(SizeReq): return 0;
        break; case(LockReq):
        break; case(BothReq):
        break; case(NullReq): return 0;
        break; case(ExclReq): if (ref.max == max || max == ini) return 0;}
        if (lnk) {lnk->ptr = this; lnk->loc = loc;}
        ref.lst.ptr = ptr; ref.lst.loc = lst;
        ref.nxt.ptr = 0; ref.nxt.loc = Relocs;
        return &ref.nxt;
    }
    Loc &get(Reloc loc) {
        if ((int)loc < 0 || (int)loc >= Relocs) EXIT
        return ploc[loc];
    }
    Bnd get(Unl &unl, int idx);
    int get(Quality tag);
    virtual void unsize(Loc &loc, SmartState log) EXIT
    virtual void resize(Loc &loc, SmartState log) EXIT
    virtual VkFence setup(Loc &loc, SmartState log) EXIT
    virtual void upset(Loc &loc, SmartState log) EXIT
    virtual BindState *getBind(SmartState log) EXIT
    virtual VkImage getImage() EXIT
    virtual VkSwapchainKHR getSwapchain() EXIT
    virtual VkSemaphore getAcquireSem() EXIT
    virtual VkSemaphore getPresentSem() EXIT
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

struct Onl {
    int hdl; Quality key; int val;
};
struct Res {
    BaseState *resrc;
    bool reuse;
};
struct StackState {
    static const int descrs = 4; // maximum descriptor sets in use
    static const int micros = Micros;
    static const int frames = 2; // prevent blocking on resources
    static const int images = 10; // fragment shader textures
    static const int piercs = 1; // fragment shader feedback
    static const int instrs = 40; // maximum steps in a binded submit
    static const int resrcs = 2; // resource classification greediness
    static const int handls = 4; // maximum number of classifications
    virtual void wait() = 0;
    virtual void qualify(int hdl, Quality key, int val) = 0;
    virtual bool compare(Onl one, Onl oth) = 0;
    virtual void show(int hdl, Quality key, int val, char **str) = 0;
    virtual Res newold(int hdl, Quality key, int val) = 0;
    virtual Res newbuf(int hdl, Quality key, int val) = 0;
    virtual Res oldbuf(int hdl, Quality key, int val) = 0;
    virtual Res getbuf(int hdl, Quality key, int val) = 0;
    virtual BaseState *idxbuf(int i) = 0;
    virtual BaseState *newbuf() = 0;
    virtual BaseState *oldbuf() = 0;
    virtual void post() = 0;
    virtual void advance(int hdl, Quality key, int val) = 0;
    virtual void advance(int i) = 0;
    virtual void advance() = 0;
    virtual int buftag(int i, Quality t) = 0;
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
    StackState() {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::index = 0;
    }
    StackState(VkBufferUsageFlags flags) {
        StackState::self = this;
        StackState::debug = 0;
        StackState::micro = 0;
        StackState::index = 0;
        StackState::flags = flags;        
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

template <class State, Resrc Type, int Size> struct ArrayState : public StackState {
    SafeState safe;
    bool excl;
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
        safe(1), excl(false), idx(0) {
    }
    ArrayState(VkBufferUsageFlags flags) : StackState(flags), safe(1), idx(0) {
    }
    ArrayState() : safe(1), idx(0) {
    }
    void wait() /*override*/ {
        safe.wait();
        excl = true;
    }
    void qualify(int hdl, Quality key, int val) /*override*/ { // set current tags
        if (!excl) EXIT
        tag.qualify(hdl,key,val);
    }
    bool compare(Onl one, Onl oth) /*override*/ {
        if (!excl) EXIT
        return (tag.get(one.hdl,one.key,one.val) == tag.get(oth.hdl,oth.key,oth.val));
    }
    void show(int hdl, Quality key, int val, char **str) /*override*/ {
        if (!excl) EXIT
        tag.show(hdl,key,val,str);
    }
    Res newold(int hdl, Quality key, int val) /*override*/ {
        if (!excl) EXIT
        auto idx = tag.newold(hdl,key,val);
        return {&state[idx.resrc],idx.reuse};
    }
    Res newbuf(int hdl, Quality key, int val) /*override*/ {
        if (!excl) EXIT
        auto idx = tag.newbuf(hdl,key,val);
        return {&state[idx.resrc],idx.reuse};
    }
    Res oldbuf(int hdl, Quality key, int val) /*override*/ {
        if (!excl) EXIT
        auto idx = tag.oldbuf(hdl,key,val);
        return {&state[idx.resrc],idx.reuse};
    }
    Res getbuf(int hdl, Quality key, int val) /*override*/ {
        if (!excl) EXIT
        auto idx = tag.getbuf(hdl,key,val);
        return {&state[idx.resrc],idx.reuse};
    }
    BaseState *idxbuf(int i) /*override*/ {
        if (!excl) EXIT
        if (i < 0 || i >= Size) EXIT
        return &state[i];
    }
    BaseState *newbuf() /*override*/ {
        if (!excl) EXIT
        return &state[idx];
    }
    BaseState *oldbuf() /*override*/ {
        if (!excl) EXIT
        return &state[(idx+1)%Size];
    }
    void post() /*override*/ {
        excl = false;
        safe.post();
    }
    void advance(int hdl, Quality key, int val) /*override*/ {
        // make oldbuf into newbuf
        safe.wait();
        auto idx = tag.oldbuf(hdl,key,val);
        tag.remove(idx.resrc);
        if (tag.insert(hdl,key,val)!=idx.resrc) EXIT
        safe.post();
    }
    void advance(int i) /*override*/ {
        if (i < 0 || i >= Size) EXIT
        safe.wait();
        idx = i;
        safe.post();
    }
    void advance() /*override*/ {
        safe.wait();
        idx = (idx+1)%Size;
        safe.post();
    }
    int buftag(int i, Quality t) /*override*/ {
        if (i < 0 || i >= Size || t < 0 || t >= Qualitys) EXIT
        safe.wait();
        int val = tag.get(i,t);
        safe.post();
        return val;
    }
    const char *bufnam() /*override*/ {
        switch (Type) {
        default: EXIT
        case (SwapRes): return "SwapRes";
        case (PipeRes): return "PipeRes";
        case (IndexRes): return "IndexRes";
        case (BringupRes): return "BringupRes";
        case (ImageRes): return "ImageRes";
        case (PierceRes): return "PierceRes";
        case (RelateRes): return "RelateRes";
        case (ColorRes): return "ColorRes";
        case (UniformRes): return "UniformRes";
        case (MatrixRes): return "MatrixRes";
        case (TriangleRes): return "TriangleRes";
        case (NumericRes): return "NumericRes";
        case (VertexRes): return "VertexRes";
        case (BasisRes): return "BasisRes";
        case (ChainRes): return "ChainRes";
        case (DrawRes): return "DrawRes";
        case (BindRes): return "BindRes";}
        return 0;
    }
};
int BaseState::index()
{
    return StackState::index++;
}
int BaseState::count()
{
    return StackState::debug++;
}
const char *BaseState::bname()
{
    return item->bufnam();
}
void BaseState::advance(Adv &adv, SmartState log)
{
    item->advance(adv.hdl,adv.key,adv.val);
}
int BaseState::get(Quality tag)
{
    return item->buftag(indx,tag);
}

struct ConstState {
    decltype(MemoryIns__Memory__Int__Instr) *memins;
    decltype(MemoryIns__Memory__Int__Reloc) *memloc;
    decltype(MemoryIns__Memory__Int__Format) *memfmt;
    decltype(MemoryIns__Memory__Int__Resrc) *memres;
    decltype(MemoryIns__Memory__Int__Memory) *memmem;
    decltype(MemoryIns__Memory__Int__Micro) *memmic;
    decltype(MemoryIns__Memory__Int__Phase) *memphs;
    decltype(MemoryIns__Memory__Int__Quality) *memkey;
    decltype(MemoryIns__Memory__Int__Reuse) *memret;
    decltype(MemoryIns__Memory__Int__Default) *memdef;
    decltype(MemoryIns__Memory__Int__Int) *memval;
    decltype(ResrcIns__Resrc__Int__Instr) *resins;
    decltype(ResrcIns__Resrc__Int__Reloc) *resloc;
    decltype(ResrcIns__Resrc__Int__Format) *resfmt;
    decltype(ResrcIns__Resrc__Int__Resrc) *resres;
    decltype(ResrcIns__Resrc__Int__Memory) *resmem;
    decltype(ResrcIns__Resrc__Int__Micro) *resmic;
    decltype(ResrcIns__Resrc__Int__Phase) *resphs;
    decltype(ResrcIns__Resrc__Int__Quality) *reskey;
    decltype(ResrcIns__Resrc__Int__Reuse) *resret;
    decltype(ResrcIns__Resrc__Int__Default) *resdef;
    decltype(ResrcIns__Resrc__Int__Int) *resval;
    decltype(MicroIns__Micro__Int__Instr) *micins;
    decltype(MicroIns__Micro__Int__Reloc) *micloc;
    decltype(MicroIns__Micro__Int__Format) *micfmt;
    decltype(MicroIns__Micro__Int__Resrc) *micres;
    decltype(MicroIns__Micro__Int__Memory) *micmem;
    decltype(MicroIns__Micro__Int__Micro) *micmic;
    decltype(MicroIns__Micro__Int__Phase) *micphs;
    decltype(MicroIns__Micro__Int__Quality) *mickey;
    decltype(MicroIns__Micro__Int__Reuse) *micret;
    decltype(MicroIns__Micro__Int__Default) *micdef;
    decltype(MicroIns__Micro__Int__Int) *micval;
};

struct SaveState {
    BaseState *buf; bool sav; int psav, rsav, wsav; Resrc typ; int fst, fin;
    SaveState() : buf(0), sav(false), psav(0), rsav(0), wsav(0), typ(Resrcs), fst(0), fin(0) {}
};
struct Sav {
    SaveState *sav; Instr ins; Phase phs; int bnd;
};
struct Lst {
    Onl onl; Instr ins; int idx;
};
struct BindState : public BaseState {
    // each reserved instance of a resource type assumed to have different qualifiers
    bool atom[Resrcs];
    HeapState<SaveState,StackState::handls> bind[Resrcs];
    Lst last[Resrcs];
    int lock; bool excl;
    HeapState<Sav,StackState::instrs> resp;
    BindState() :
        BaseState("BindState",StackState::self),
        lock(0), excl(false) {
        for (int i = 0; i < Resrcs; i++) atom[i] = false;
    }
    BindState *getBind(SmartState log) override {
        safe.wait();
        if (excl) {log << "bind fail " << debug << '\n'; safe.post(); return 0;}
        log << debug << ":pass" << '\n';
        excl = true;
        safe.post();
        if (lock != 0) EXIT
        lock = 1;
        return this;
    }
    int vld(Resrc typ, Lst lst, StackState *src) { // same qualification as last
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (bind[typ].get() == 0) return false;
        bool found = false;
        switch (lst.ins) {default: break; case (IdxDerIns): case (IdxDeeIns): found = true;}
        switch (last[typ].ins) {default: break; case (IdxDerIns): case (IdxDeeIns): found = true;}
        if (found) return (last[typ].ins == lst.ins && last[typ].idx == lst.idx);
        return src->compare(last[typ].onl,lst.onl);
    }
    void set(Resrc typ, Lst lst) { // set qualification for subsequent vld
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        last[typ] = lst;
    }
    SaveState *get(Resrc typ) { // current resource of type
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        return &bind[typ].get(0);
    }
    SaveState *get(Resrc typ, int i, SmartState log) { // current next or first
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        SaveState *sav = &bind[typ].get(0);
        if (sav->fin < i || sav->fst > i) {bind[typ].get(1); sav = &bind[typ].get(0);}
        if (sav->fin < i || sav->fst > i) ERROR();
        return sav;
    }
    bool vld(Resrc typ) { // mark resource array as locked
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        bool tmp =  atom[typ]; atom[typ] = true;
        return tmp;
    }
    bool clr(Resrc typ) { // mark resource array as unlocked
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        bool tmp =  atom[typ]; atom[typ] = false;
        return tmp;
    }
    SaveState *add(Resrc typ, Phase ref, int bnd, BaseState *buf, int fst, Lst lst, SmartState log) { // add resource of type
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (ref < 0 || ref >= Phases) EXIT
        SaveState *sav = &bind[typ].add(1);
        sav->buf = buf; sav->typ = typ; sav->fst = fst;
        set(typ,lst);
        {char *st0 = 0; char *st1 = 0;
        showResrc(typ,&st0); showPhase(ref,&st1);
        log << (sav->buf?sav->buf->debug:"nil") << ":add " << st0 << " " << st1 << '\n';
        free(st0); free(st1);}
        return sav;
    }
    Sav &pre(Unl &unl, int idx) {
        if (!excl) EXIT
        if (unl.dee+unl.siz > resp.size()) EXIT
        if (idx < 0 || idx >= unl.siz) EXIT
        return resp[unl.dee+idx];
    }
    Bnd get(Unl &unl, int idx) {
        Sav &met = pre(unl,idx);
        SaveState *ptr = met.sav;
        return Bnd{ptr->buf,ptr->typ,met.phs,met.bnd,met.ins};
    }
    SaveState *sav(Resrc typ, int hdl) { // indexed resource of type
        if (!excl) EXIT
        if (typ < 0 || typ >= Resrcs) EXIT
        if (hdl < 0 || hdl >= bind[typ].get()) EXIT
        return &bind[typ][hdl];
    }
    bool push(SaveState *sav, Requ req, Unl unl, SmartState log) {
        // reserve depender and push to thread
        if (!excl) EXIT
        unl.der = sav;
        if (!sav->buf->push(this,req,unl,log)) return false;
        if (!sav->sav) lock += 1;
        sav->sav = true;
        sav->psav += 1;
        log << sav->buf->debug << ":push lock:" << lock << " psav:" << sav->psav << " rsav:" << sav->rsav << " wsav:" << sav->wsav << '\n';
        return true;
    }
    void push(SaveState *sav, Instr ins, Phase phs, int bnd, SmartState log) {
        // reserve dependee without push to thread
        resp<<Sav{sav,ins,phs,bnd};
    }
    void done(SaveState *sav, SmartState log) { // attempt depender release
        if (!excl) EXIT
        if (sav->psav <= 0) EXIT
        sav->psav -= 1;
        if (sav->psav == 0 && sav->rsav == 0 && sav->wsav == 0) {
        bind[sav->typ].clear();
        sav->sav = false; lock -= 1;}
        log << sav->buf->debug << ":done lock:" << lock << " psav:" << sav->psav << " rsav:" << sav->rsav << " wsav:" << sav->wsav << '\n';
    }
    void done(Resrc typ, SmartState log) { // depender upon fail
        done(get(typ),log);
    }
    void done(SmartState log) { // attempt this release
        if (!excl) EXIT
        log << debug << ":bind lock:" << lock  << '\n';
        if (lock == 1) {
        lock = 0; resp.clear();
        safe.wait(); excl = false; safe.post();}
    }
    void done(Unl unl, SmartState log) { // attempt dependee release
        if (!excl) EXIT
        if (unl.dee+unl.siz > resp.size()) EXIT
        for (int i = 0; i < unl.siz; i++) {
        switch (resp[unl.dee+i].ins) {default:
        break; case (RdlDeeIns): case (IdxDeeIns): rdec(resp[unl.dee+i].sav,log);
        break; case (WrlDeeIns): wdec(resp[unl.dee+i].sav,log);}}
        done(unl.der,log); done(log);
    }
    bool incr(SaveState *sav, bool elock, SmartState log) {
        if (!excl) EXIT
        if (!sav->buf->incr(elock,sav->psav,sav->rsav,sav->wsav)) {
        return false;}
        if (!sav->sav) lock += 1;
        sav->sav = true;
        (elock ? sav->wsav : sav->rsav) += 1;
        log << sav->buf->debug << ":incr lock:" << lock << " psav:" << sav->psav << " rsav:" << sav->rsav << " wsav:" << sav->wsav << '\n';
        return true;
    }
    void decr(SaveState *sav, bool elock, SmartState log) {
        if (!excl) EXIT
        if (lock <= 0 || !sav->sav) EXIT
        sav->buf->decr(elock);
        if ((elock ? sav->wsav : sav->rsav) <= 0) EXIT
        (elock ? sav->wsav : sav->rsav) -= 1;
        if (sav->psav == 0 && sav->rsav == 0 && sav->wsav == 0) {
        bind[sav->typ].clear();
        sav->sav = false; lock -= 1;}
        log << sav->buf->debug << ":decr lock:" << lock << " psav:" << sav->psav << " rsav:" << sav->rsav << " wsav:" << sav->wsav << '\n';
    }
    bool rinc(SaveState *sav, SmartState log) {
        return incr(sav,false,log);
    }
    bool winc(SaveState *sav, SmartState log) {
        return incr(sav,true,log);
    }
    void rdec(SaveState *sav, SmartState log) {
        decr(sav,false,log);
    }
    void wdec(SaveState *sav, SmartState log) {
        decr(sav,true,log);
    }
};
bool BaseState::check(SaveState *sav) {
    return (plock-sav->psav || rlock-sav->rsav || wlock-sav->wsav);
}
Bnd BaseState::get(Unl &unl, int idx) {
    return lock->get(unl,idx);
}
void BaseState::unlock(Unl &unl, SmartState log) {
    lock->done(unl,log);
}

struct Push {
    SmartState log;
    BaseState *base; Reloc loc;
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
        // std::cout << debug << std::endl;
    }
    ~ThreadState() {
        // std::cout << "~" << debug << std::endl;
    }
    void push(Push push) {
        push.fence = VK_NULL_HANDLE;
        safe.wait();
        before << push;
        safe.post();
        wake.post();
    }
    void push(SmartState log, BaseState *base, Reloc loc) {
        // enque resource to process on thread
        push({log,base,loc,0,0});
    }
    void push(SmartState log, Center *ptr, int sub) {
        // enque response for after enqued resource fences
        push({log,0,Relocs,ptr,sub});
    }
    bool stage() {
        while (1) {while (1) {
        safe.wait();
        if (before.size() == 0) {safe.post(); break;}
        Push push = before[0]; before.clear(1); safe.post();
        if (push.base) {
        Request tag = push.base->get(push.loc).req.tag;
        switch (tag) {
        default: EXIT
        break; case(SizeReq): push.fence = VK_NULL_HANDLE; push.base->baseres(push.loc,push.log);
        break; case(LockReq): push.fence = push.base->basesup(push.loc,push.log);
        break; case(BothReq): push.fence = push.base->basesiz(push.loc,push.log);
        break; case(NullReq): push.fence = VK_NULL_HANDLE; push.base->basenul(push.loc,push.log);
        break; case(ExclReq): push.fence = push.base->basexor(push.loc,push.log);}}
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
        if (push.ptr) centerDone(push.ptr,push.sub);
        change->wots(RegisterWake,1<<FnceMsk);}
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
    Reloc loc = Relocs; Reuse use = Reuses; Format fmt = Formats; Quality key = Qualitys;
    Resrc res = Resrcs; Memory mem = Memorys; Micro mic = Micros; Phase phs = Phases;
};
struct CopyState {
    ChangeState<Configure,Configures> *change;
    ThreadState *thread;
    StackState *stack[Resrcs];
    ConstState *array;
    int depth;
    CopyState(ChangeState<Configure,Configures> *change,
        ThreadState *thread, EnumState *stack, ConstState *ary) :
        change(change),
        thread(thread),
        stack{},
        array(ary),
        depth(0) {
        // std::cout << "CopyState" << std::endl;
        for (EnumState *i = stack; i->key != Resrcs; i++) this->stack[i->key] = i->val;
    }
    ~CopyState() {
        // std::cout << "~CopyState" << std::endl;
    }
    StackState *src(Resrc res) {
        // array of resources of given type
        if ((int)res < 0 || (int)res >= Resrcs) EXIT
        return stack[res];
    }
    int get(int *arg, int siz, int &idx, SmartState log, const char *str) {
        // next default or given argument
        log << "get:" << str << ":" << idx << ":" << arg[idx] << '\n';
        if (idx >= siz) {std::cerr << "not enough int arguments " << idx << ">=" << siz << std::endl; EXIT}
        int tmp = idx; idx += 1;
        return arg[tmp];
    }
    BaseState *get(Instr ins, Resrc res, int idx, int hdl, Quality key, int val) {
        // depender or dependee qualified resource
        switch (ins) {default: break;
        case(NewDerIns): case(NidDerIns):
        case(WrlDeeIns): case(WidDeeIns):
        case(RdlDeeIns): case(RidDeeIns): {
        auto ptr = src(res)->newbuf(hdl,key,val);
        if (ptr.reuse) ptr.resrc->finish();
        return ptr.resrc;}
        break; case(OldDerIns): case(OidDerIns):
        return src(res)->getbuf(hdl,key,val).resrc;
        break; case(GetDerIns): case(GidDerIns):
        return src(res)->oldbuf(hdl,key,val).resrc;
        break; case(IdxDerIns): case(IdxDeeIns):
        return src(res)->idxbuf(idx);}
        return 0;
    }
    SaveState *get(Instr ins, Resrc res, int idx, BindState *bnd, SmartState log) {
        switch (ins) {default: break; case(SetTagIns): case(MovTagIns): return 0;}
        return bnd->get(res,idx,log);
    }
    #define INS ins[i].ins
    #define RES ins[i].res.res
    #define FRC ins[i].res.frc
    #define PHS ins[i].res.phs
    #define BND ins[i].res.bnd
    #define KES ins[i].key.res
    #define HDL ins[i].key.hdl
    #define KEY ins[i].key.key
    #define VAL ins[i].key.val
    #define ONL ins[i].key.hdl,ins[i].key.key,ins[i].key.val
    #define LOC ins[i].req.loc
    #define REQ ins[i].req
    void push(HeapState<Inst,StackState::instrs> &ins, Center *ptr, int sub, Rsp rsp, SmartState log) {
        // four orderings, in same list: acquire reserve submit notify
        int num = ins.size(); // number that might be reserved
        bool goon = true; while (goon) {goon = false;
        log << "count depends" << '\n';
        int count = 0;
        for (int i = 0; i < num; i++) {
            switch (INS) {default:
            break; case(NewDerIns): case(OldDerIns): case(GetDerIns): count += 1;
            break; case(NidDerIns): case(OidDerIns): case(GidDerIns): case(IdxDerIns): count += 1;
            break; case(WrlDeeIns): case(RdlDeeIns): count += 1;
            break; case(WidDeeIns): case(RidDeeIns): case(IdxDeeIns): count += 1;}}
        log << "choose binding" << '\n';
        BindState *bind = 0; int min = 0;
        if (count > min) {
        stack[BindRes]->wait();
        bind = stack[BindRes]->getbuf(0,Qualitys,0).resrc->getBind(log);
        stack[BindRes]->post();}
        int lim = num; // number checked for reservation
        if (count > min && bind == 0) lim = -1;
        log << "reserve arrays" << '\n';
        for (int i = 0; i < num && i < lim; i++) {
            switch (INS) {default: if (!bind->vld(RES)) src(RES)->wait();
            break; case (SetTagIns): case (MovTagIns): if (!bind->vld(KES)) src(KES)->wait();}}
        log << "choose buffers" << '\n';
        for (int i = 0; i < num && i < lim; i++) {
            switch (INS) {default: {std::cerr << "invalid instruction" << std::endl; EXIT}
            break; case (SetTagIns): {
            {char *st0 = 0; char *st1 = 0; char *st2 = 0;
            showInstr(INS,&st0); showResrc(KES,&st1); showQuality(KEY,&st2);
            log << st0 << " " << st1 << " " << HDL << " " << st2 << " " << VAL << '\n';
            free(st0); free(st1); free(st2);}
            src(KES)->qualify(ONL);}
            break; case (MovTagIns): {
            {char *st0 = 0; char *st1 = 0; char *st2 = 0;
            showInstr(INS,&st0); showResrc(KES,&st1); showQuality(KEY,&st2);
            log << st0 << " " << st1 << " " << HDL << " " << st2 << " " << VAL << '\n';
            free(st0); free(st1); free(st2);}
            src(KES)->newold(ONL);
            bind->set(KES,Lst{Onl{ONL},INS,FRC});}
            break; case(NewDerIns): case(NidDerIns): case(OldDerIns): case(OidDerIns):
            case(GetDerIns): case(GidDerIns): case(IdxDerIns):
            case(WrlDeeIns): case(WidDeeIns): case(RdlDeeIns): case(RidDeeIns): case(IdxDeeIns): {
            Lst lst = Lst{Onl{ONL},INS,FRC}; bool cnd = bind->vld(RES,lst,src(RES));
            // note there is no way to refer to previous before intervening of a certain resource
            SaveState *sav = (cnd?bind->get(RES):bind->add(RES,PHS,BND,get(INS,RES,FRC,ONL),i,lst,log));
            {char *st0 = 0; showInstr(INS,&st0); log << sav->buf->debug << ":" << st0 << " " << i << " " << cnd << '\n'; free(st0);}
            sav->fin = i;}}}
        log << "release arrays" << '\n';
        for (int i = 0; i < num && i < lim; i++) {
            switch (INS) {default: if (bind->clr(RES)) src(RES)->post();
            break; case (SetTagIns): case (MovTagIns): if (bind->clr(KES)) src(KES)->post();}}
        log << "reserve chosen" << '\n';
        int resps = 0;
        for (int i = 0; i < num && i < lim; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);
            switch (INS) {default:
            break; case(NewDerIns): case(OldDerIns): case(GetDerIns):
            case(NidDerIns): case(OidDerIns): case(GidDerIns): case(IdxDerIns): {
            Unl unl = {.der=0,.dee=resps,.siz=0};
            for (int j = i+1; j < num; j++) switch (ins[j].ins) {default:
            break; case(NewDerIns): case(OldDerIns): case(GetDerIns):
            case(NidDerIns): case(OidDerIns): case(GidDerIns): case(IdxDerIns): j = num-1;
            break; case(WrlDeeIns): case(RdlDeeIns):
            case(WidDeeIns): case(RidDeeIns): case(IdxDeeIns): resps += 1; unl.siz += 1;}
            if (!bind->push(sav,REQ,unl,log)) lim = i;
            if (sav->fin == i && lim == num) {
            Adv adv = {PushAdv,ONL};
            switch (INS) {default: break; case(OldDerIns): case(GetDerIns): adv.adv = FnceAdv;}
            sav->buf->push(adv,log);}}
            break; case(WrlDeeIns): case(WidDeeIns): {
            if (!bind->winc(sav,log)) lim = i;}
            break; case(RdlDeeIns): case(RidDeeIns): case(IdxDeeIns): {
            if (!bind->rinc(sav,log)) lim = i;}}}
        if (lim == num) {
        log << "link list" << '\n';
        Lnk *lnk = 0; Reloc lst = Relocs; BaseState *bas = 0;
        for (int i = 0; i < num; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);
            switch(INS) {default:
            break; case(NewDerIns): case (OldDerIns): case (GetDerIns):
            case(NidDerIns): case(OidDerIns): case(GidDerIns): case (IdxDerIns): {
            Lnk *tmp = sav->buf->link(LOC,bas,lst,lnk);
            if (tmp) {lnk = tmp; bas = sav->buf; lst = LOC;}}}}
        for (int i = 0; i < num; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);
            switch(INS) {default:
            break; case(NewDerIns): case (OldDerIns): case (GetDerIns):
            case(NidDerIns): case(OidDerIns): case(GidDerIns): case (IdxDerIns): {
            Lnk *nxt = &sav->buf->ploc[LOC].nxt;
            Lnk *lst = &sav->buf->ploc[LOC].lst;
            log << "link " << lst->loc << "/" << (lst->ptr?lst->ptr->debug:"null") << "->" << LOC << "/" << sav->buf->debug << "->" << nxt->loc << "/" << (nxt->ptr?nxt->ptr->debug:"null") << '\n';}}}
        log << "record bindings" << '\n';
        for (int i = 0; i < num; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);
            switch (INS) {default:
            break; case (WrlDeeIns): case(RdlDeeIns):
            case (WidDeeIns): case (RidDeeIns): case(IdxDeeIns):
            if (bind) bind->push(sav,INS,PHS,BND,log);}}
        log << "submit buffers" << '\n';
        for (int i = 0; i < num; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);
            switch (INS) {default:
            break; case(NewDerIns): case(NidDerIns): {
            if (sav->fst == i) src(RES)->advance(ONL);
            log << "NewDerIns push " << sav->buf->debug << '\n';
            thread->push(log,sav->buf,LOC);}
            break; case(OldDerIns): case(GetDerIns): case(OidDerIns): case(GidDerIns): {
            log << "OldDerIns push " << sav->buf->debug << '\n';
            thread->push(log,sav->buf,LOC);}
            break; case(IdxDerIns): {
            if (sav->fst == i) src(RES)->advance(FRC);
            log << "IdxDerIns push " << sav->buf->debug << '\n';
            thread->push(log,sav->buf,LOC);}}}
        log << "notify pass" << '\n';
        ptr->slf = 0;
        switch (rsp) {default:
        break; case (RetRsp): case (RptRsp): log << "RptRsp " << ptr << '\n'; thread->push(log,ptr,sub);}
        if (bind) stack[BindRes]->advance(0,Qualitys,0);
        } else {
        log << "wrap handles " << num << ">" << lim << '\n';
        if (lim >= 0) for (int i = lim+1; i < num; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);}
        log << "release reserved " << num << ">" << lim << '\n';
        for (int i = 0; i < lim; i++) {
            SaveState *sav = get(INS,RES,i,bind,log);
            switch (INS) {default:
            break; case(NewDerIns): case(OldDerIns): case(GetDerIns):
            case(NidDerIns): case(OidDerIns): case(GidDerIns): case(IdxDerIns): {
            bind->done(RES,log);
            sav->buf->fail(log);}
            break; case(WrlDeeIns): case (WidDeeIns): {
            bind->wdec(sav,log);}
            break; case(RdlDeeIns): case (RidDeeIns): case(IdxDeeIns): {
            bind->rdec(sav,log);}}}
        if (bind) bind->done(log);
        log << "notify fail" << '\n';
        switch (rsp) {default:
        break; case (RptRsp): case (MptRsp): goon = true; vulkanWait();
        break; case (MltRsp): ptr->slf = -1;
        break; case (RetRsp): ptr->slf = -1; thread->push(log,ptr,sub);}}}
    }
    Requ request(Format frm, Reloc loc, Reuse use, void *val, int *arg, int siz, int &idx, SmartState log) {
        Requ req = {Requests,0,0,0,Extents,0,0,loc,use};
        switch (frm) {default: EXIT
        break; case (ImageFrm): // initial to texture,shadow
        req.tag = ExclReq; req.ext = FormExt; // ReformLoc
        req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        break; case (WonlyFrm): // texture,shadow to write,fill
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (RonlyFrm): // write,fill to texture,shadow
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        break; case (PierceFrm): // initial to render
        req.tag = ExclReq; req.ext = FormExt; // ReformLoc
        req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        break; case (PeekFrm): // render to read
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (SourceFrm): // read to render
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL; req.size = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        break; case (PokeFrm): // render to write,fill
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (DestFrm): // write,fill to render
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        break; case (RelateFrm): // initial to read
        req.tag = ExclReq; req.ext = FormExt; // ReformLoc
        req.base = VK_IMAGE_LAYOUT_UNDEFINED; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (RdwrFrm): // read to write,fill
        req.tag = BothReq; req.ext = FormExt; // BeforeLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break; case (WrrdFrm): // write,fill to read
        req.tag = BothReq; req.ext = FormExt; // AfterLoc
        req.base = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL; req.size = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        break; case (ExtentFrm):
        req.tag = SizeReq; req.ext = ExtentExt;
        req.base = get(arg,siz,idx,log,"ExtentFrm.base"); req.size = get(arg,siz,idx,log,"ExtentFrm.size");
        break; case (SizeFrm):
        req.tag = SizeReq; req.ext = IntExt;
        req.base = get(arg,siz,idx,log,"SizeFrm.base"); req.size = get(arg,siz,idx,log,"SizeFrm.size");
        break; case (WholeFrm):
        req.tag = BothReq; req.ext = IntExt; req.ptr = val;
        req.idx = get(arg,siz,idx,log,"WholeFrm.idx"); req.siz = get(arg,siz,idx,log,"WholeFrm.siz");
        req.base = req.idx; req.size = req.siz;
        break; case (LockFrm):
        req.tag = LockReq; req.ext = IntExt; req.ptr = val;
        req.idx = get(arg,siz,idx,log,"LockFrm.idx"); req.siz = get(arg,siz,idx,log,"LockFrm.siz");
        break; case (ResrcFrm):
        req.tag = SizeReq; req.ext = ResrcExt;
        req.base = get(arg,siz,idx,log,"ResrcFrm.base");
        break; case (PipeFrm):
        req.tag = SizeReq; req.ext = MicroExt;
        req.base = get(arg,siz,idx,log,"PipeFrm.base");
        break; case (MicroFrm):
        req.tag = BothReq; req.ext = MicroExt;
        req.idx = get(arg,siz,idx,log,"MicroFrm.idx"); req.siz = get(arg,siz,idx,log,"MicroFrm.siz");
        req.base = get(arg,siz,idx,log,"MicroFrm.base");
        break; case (NullFrm):
        req.tag = NullReq; req.ext = TrueExt;
        break; case (FalseFrm):
        req.tag = SizeReq; req.ext = FalseExt;
        break; case (TrueFrm):
        req.tag = SizeReq; req.ext = TrueExt;
        }
        return req;
    }
    Resv reserve(Instr ins, Resrc res, Phase phs, int *arg, int siz, int &idx, SmartState log) {
        Resv ret = {res,phs,0,0};
        switch (phs) {default:
        break; case (UniformPhs): ret.bnd = get(arg,siz,idx,log,"UniformPhs.bnd");
        break; case (StoragePhs): ret.bnd = get(arg,siz,idx,log,"StoragePhs.bnd");
        break; case (RelatePhs): ret.bnd = get(arg,siz,idx,log,"RelatePhs.bnd");
        break; case (SamplePhs): ret.bnd = get(arg,siz,idx,log,"SamplePhs.bnd");}
        if (ins == IdxDerIns || ins == IdxDeeIns) ret.frc = get(arg,siz,idx,log,"reserve.frc");
        return ret;
    }
    Qual qualify(Resrc res, Quality key, Reuse use, int *arg, int siz, int &idx, SmartState log) {
        Qual ret = {res,0,key,0};
        if (key != Qualitys && key != RuseQua) {
        ret.hdl = get(arg,siz,idx,log,"qualify.hdl");
        ret.val = get(arg,siz,idx,log,"qualify.val");}
        if (key == RuseQua) ret.val = use;
        return ret;
    }
    template <class Type> Inst instruct(HeapState<Arg,0> &dot, int i, Type typ, void *val, int *arg, int siz, int &idx, int &count, SmartState log) {
        Instr ins = dot[i].ins;
        switch (ins) {default:
        {char *st0 = 0; showResrc(dot[i].res,&st0);
        char *st1 = 0; showReloc(dot[i].loc,&st1);
        char *st2 = 0; showInstr(dot[i].ins,&st2);
        char *st3 = 0; showReuse(dot[i].use,&st3);
        char *st4 = 0; showPhase(dot[i].phs,&st4);
        log << st2 << " " << st0 << " " << st4 << " " << st1 << " " << st3 << '\n';
        free(st0); free(st1); free(st2); free(st3); free(st4);}
        break; case(ResIncIns):
        {char *st0 = 0; showResrc(dot[i].res,&st0);
        log << "include " << st0 << '\n'; free(st0);}
        break; case(MemIncIns):
        {char *st0 = 0; showMemory(dot[i].mem,&st0);
        log << "include " << st0 << '\n'; free(st0);}
        break; case(MicIncIns):
        {char *st0 = 0; showMicro(dot[i].mic,&st0);
        log << "include " << st0 << '\n'; free(st0);}}
        switch (ins) {default: EXIT
        break; case(NewDerIns): case(OldDerIns): case(GetDerIns): {
        Requ req = request(dot[i].fmt,dot[i].loc,dot[i].use,val,arg,siz,idx,log);
        Resv res = reserve(dot[i].ins,dot[i].res,dot[i].phs,arg,siz,idx,log);
        Qual key = qualify(dot[i].res,Qualitys,Reuses,arg,siz,idx,log);
        return Inst{.ins=ins,.req=req,.res=res,.key=key};}
        break; case(NidDerIns): case(OidDerIns): case(GidDerIns): {
        Requ req = request(dot[i].fmt,dot[i].loc,dot[i].use,val,arg,siz,idx,log);
        Resv res = reserve(dot[i].ins,dot[i].res,dot[i].phs,arg,siz,idx,log);
        Qual key = qualify(dot[i].res,dot[i].key,dot[i].use,arg,siz,idx,log);
        return Inst{.ins=ins,.req=req,.res=res,.key=key};}
        break; case(IdxDerIns): {
        Requ req = request(dot[i].fmt,dot[i].loc,dot[i].use,val,arg,siz,idx,log);
        Resv res = reserve(dot[i].ins,dot[i].res,dot[i].phs,arg,siz,idx,log);
        Qual key = qualify(dot[i].res,Qualitys,Reuses,arg,siz,idx,log);
        return Inst{.ins=ins,.req=req,.res=res,.key=key};}
        break; case(WrlDeeIns): case(RdlDeeIns): {
        Resv res = reserve(dot[i].ins,dot[i].res,dot[i].phs,arg,siz,idx,log);
        Qual key = qualify(dot[i].res,Qualitys,Reuses,arg,siz,idx,log);
        return Inst{.ins=ins,.res=res,.key=key};}
        break; case(WidDeeIns): case(RidDeeIns): {
        Resv res = reserve(dot[i].ins,dot[i].res,dot[i].phs,arg,siz,idx,log);
        Qual key = qualify(dot[i].res,dot[i].key,dot[i].use,arg,siz,idx,log);
        return Inst{.ins=ins,.res=res,.key=key};}
        break; case(IdxDeeIns): {
        Resv res = reserve(dot[i].ins,dot[i].res,dot[i].phs,arg,siz,idx,log);
        Qual key = qualify(dot[i].res,Qualitys,Reuses,arg,siz,idx,log);
        return Inst{.ins=ins,.res=res,.key=key};}
        break; case(SetTagIns): case(MovTagIns): {
        Qual key = qualify(dot[i].res,dot[i].key,dot[i].use,arg,siz,idx,log);
        return Inst{.ins=ins,.key=key};}
        break; case(ResIncIns): return Inst{.ins=ins,.inc=Incl{.res=dot[i].res}};
        break; case(MemIncIns): return Inst{.ins=ins,.inc=Incl{.mem=dot[i].mem}};
        break; case(MicIncIns): return Inst{.ins=ins,.inc=Incl{.mic=dot[i].mic}};}
        return Inst{.ins=Instrs};
    }
    template <class Type, class Fnc, class Arg> bool builtin(Type &sav, Type &arg, Fnc fnc, Arg typ, int i, Type inv, SmartState log) {
        Type val = (fnc&&fnc(typ)?fnc(typ)(i):inv);
        arg = val;
        if (arg == inv) arg = sav; else sav = arg;
        return (val != inv);
    }
    bool iterate(Memory typ, int sub, Arg &sav, Arg &dot, ConstState *ary, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {OldDerIns,MiddleLoc,Reuses,WholeFrm,Qualitys,Resrcs,Memorys,Micros,DrawPhs};
        if (builtin(sav.ins,dot.ins,ary->memins,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ary->memloc,typ,sub,Relocs,log)) done = false;
        if (builtin(sav.use,dot.use,ary->memret,typ,sub,Reuses,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ary->memfmt,typ,sub,Formats,log)) done = false;
        if (builtin(sav.key,dot.key,ary->memkey,typ,sub,Qualitys,log)) done = false;
        if (builtin(sav.res,dot.res,ary->memres,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.mem,dot.mem,ary->memmem,typ,sub,Memorys,log)) done = false;
        if (builtin(sav.mic,dot.mic,ary->memmic,typ,sub,Micros,log)) done = false;
        if (builtin(sav.phs,dot.phs,ary->memphs,typ,sub,Phases,log)) done = false;
        /*char *db0 = 0; showResrc(dot.res,&db0);
        char *db1 = 0; showInstr(dot.ins,&db1);
        char *db2 = 0; showMemory(typ,&db2);
        char *db3 = 0; showReloc(dot.loc,&db3);
        char *db4 = 0; showPhase(dot.phs,&db4);
        log << "memory " << done << " " << db2 << " " << db3 << " " << db1 << " " << db0 << " " << db4 << '\n';
        free(db0); free(db1); free(db2); free(db3); free(db4);*/
        return !done;
    }
    bool iterate(Resrc typ, int sub, Arg &sav, Arg &dot, ConstState *ary, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {OldDerIns,ResizeLoc,Reuses,SizeFrm,Qualitys,Resrcs,Memorys,Micros,DrawPhs};
        if (builtin(sav.ins,dot.ins,ary->resins,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ary->resloc,typ,sub,Relocs,log)) done = false;
        if (builtin(sav.use,dot.use,ary->resret,typ,sub,Reuses,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ary->resfmt,typ,sub,Formats,log)) done = false;
        if (builtin(sav.key,dot.key,ary->reskey,typ,sub,Qualitys,log)) done = false;
        if (builtin(sav.res,dot.res,ary->resres,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.mem,dot.mem,ary->resmem,typ,sub,Memorys,log)) done = false;
        if (builtin(sav.mic,dot.mic,ary->resmic,typ,sub,Micros,log)) done = false;
        if (builtin(sav.phs,dot.phs,ary->resphs,typ,sub,Phases,log)) done = false;
        /*char *db0 = 0; showResrc(dot.res,&db0);
        char *db1 = 0; showInstr(dot.ins,&db1);
        char *db2 = 0; showResrc(typ,&db2);
        char *db3 = 0; showReloc(dot.loc,&db3);
        char *db4 = 0; showPhase(dot.phs,&db4);
        log << "resrc " << done << " " << db2 << " " << db3 << " " << db1 << " " << db0 << " " << db4 << '\n';
        free(db0); free(db1); free(db2); free(db3); free(db4);*/
        return !done;
    }
    bool iterate(Micro typ, int sub, Arg &sav, Arg &dot, ConstState *ary, SmartState log) {
        bool done = true;
        if (sub == 0) sav = {NewDerIns,ResizeLoc,Reuses,SizeFrm,Qualitys,Resrcs,Memorys,Micros,DrawPhs};
        if (builtin(sav.ins,dot.ins,ary->micins,typ,sub,Instrs,log)) done = false;
        if (builtin(sav.loc,dot.loc,ary->micloc,typ,sub,Relocs,log)) done = false;
        if (builtin(sav.use,dot.use,ary->micret,typ,sub,Reuses,log)) done = false;
        if (builtin(sav.fmt,dot.fmt,ary->micfmt,typ,sub,Formats,log)) done = false;
        if (builtin(sav.key,dot.key,ary->mickey,typ,sub,Qualitys,log)) done = false;
        if (builtin(sav.res,dot.res,ary->micres,typ,sub,Resrcs,log)) done = false;
        if (builtin(sav.mem,dot.mem,ary->micmem,typ,sub,Memorys,log)) done = false;
        if (builtin(sav.mic,dot.mic,ary->micmic,typ,sub,Micros,log)) done = false;
        if (builtin(sav.phs,dot.phs,ary->micphs,typ,sub,Phases,log)) done = false;
        /*char *db0 = 0; showResrc(dot.res,&db0);
        char *db1 = 0; showInstr(dot.ins,&db1);
        char *db2 = 0; showMicro(typ,&db2);
        char *db3 = 0; showReloc(dot.loc,&db3);
        char *db4 = 0; showPhase(dot.phs,&db4);
        log << "micro " << done << " " << db2 << " " << db3 << " " << db1 << " " << db0 << " " << db4 << '\n';
        free(db0); free(db1); free(db2); free(db3); free(db4);*/
        return !done;
    }
    void showType(Memory typ, char **str) {
        showMemory(typ,str);
    }
    void showType(Resrc typ, char **str) {
        showResrc(typ,str);
    }
    void showType(Micro typ, char **str) {
        showMicro(typ,str);
    }
    template <class Type> void push(HeapState<Inst,StackState::instrs> &lst, Type typ, void *val, int *arg, int siz, int &idx, int ary, SmartState log) {
        int count = 0; Arg sav; Arg tmp; HeapState<Arg,0> dot;
        {char *st0 = 0; showType(typ,&st0); log << st0 << ":push " << " alt:" << ary << " depth:" << depth << '\n'; free(st0);}
        for (int i = 0; iterate(typ,i,sav,tmp,&array[ary],log); i++) dot << tmp;
        for (int i = 0; i < dot.size(); i++) {
        Inst ins = instruct(dot,i,typ,val,arg,siz,idx,count,log);
        switch (ins.ins) {default: lst << ins;
        break; case (ResIncIns): depth += 1; push(lst,ins.inc.res,val,arg,siz,idx,get(arg,siz,idx,log,"ResIncIns.ary"),log); depth -= 1;
        break; case (MemIncIns): depth += 1; push(lst,ins.inc.mem,val,arg,siz,idx,get(arg,siz,idx,log,"MemIncIns.ary"),log); depth -= 1;
        break; case (MicIncIns): depth += 1; push(lst,ins.inc.mic,val,arg,siz,idx,get(arg,siz,idx,log,"MicIncIns.ary"),log); depth -= 1;}}
        {char *st0 = 0; showType(typ,&st0); log << st0 << ":done alt:" << ary << " depth:" << depth << '\n'; free(st0);}
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
        int tot = 0; int lim = size(typ,ary);
        if (siz && sze) {
            tot = lim;
            // maximum of number of fill values and force index
            for (int i = 0; i < siz; i++)
            if (arg[i] >= tot) tot = arg[i]+1;}
        // maximum of number of fill values and number of packed force values
        else if (sze) tot = sze;
        // otherwise number of fill values
        if (tot < lim) tot = lim;
        int vlu[tot];
        // initialize with defaults
        for (int i = 0; i < tot; i++) {
            // ignore siz sze since TrivDef fill value is an immediate value
            if (i < lim && dflt(typ,i,ary) == TrivDef) vlu[i] = fill(typ,i,ary);
            else vlu[i] = 0;}
        // copy from given
        if (siz) for (int i = 0; i < tot; i++)
            // ignore GiveDef if there is nothing proferred
            if (i < lim && dflt(typ,i,ary) == GiveDef) {
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
            if (i < lim && dflt(typ,i,ary) == BackDef) {
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
    void push(Memory mem, void *dat, int idx, int siz, int wid, int hei, Center *ptr, int sub, Rsp rsp, int ary, SmartState log) {
        int mval[] = {
        wid,hei, // OldDerIns ExtentFrm
        idx,siz}; // OldDerIns WholeFrm
        int msiz = sizeof(mval)/sizeof(int); int midx = 0;
        push(mem,dat,mval,0,msiz,0,midx,ptr,sub,rsp,ary,log);
    }
    void push(Center *ptr, int sub, Rsp rsp, int ary, SmartState log) {
        switch (ptr->mem) {default: {
        int mod = centerMod(ptr); int idx = ptr->idx*mod; int siz = ptr->siz*mod;
        int val[] = {idx,siz}; int aiz = sizeof(val)/sizeof(int); int adx = 0;
        switch (ptr->mem) {default: EXIT
        break; case (Indexz): push(ptr->mem,(void*)ptr->ind,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Bringupz): push(ptr->mem,(void*)ptr->ver,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Uniformz): push(ptr->mem,(void*)ptr->uni,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Matrixz): push(ptr->mem,(void*)ptr->mat,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Trianglez): push(ptr->mem,(void*)ptr->tri,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Numericz): push(ptr->mem,(void*)ptr->num,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Vertexz): push(ptr->mem,(void*)ptr->vtx,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);
        break; case (Basisz): push(ptr->mem,(void*)ptr->bas,val,0,aiz,0,adx,ptr,sub,rsp,ary,log);}}
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
        break; case (Imagez): push(ptr->mem,(void*)datxVoidz(0,ptr->img[0].dat),ptr->idx,
            datxVoids(ptr->img[0].dat),ptr->img[0].wid,ptr->img[0].hei,ptr,sub,rsp,ary,log);
        break; case (Getintz): case (Getoldz): case (Setintz): case (Setoldz):
            push(ptr->mem,(void*)ptr->uns,ptr->idx,ptr->siz,
            change->read(UniformWid),change->read(UniformHei),ptr,sub,rsp,ary,log);
        break; case (Vectorz):
            push(ptr->mem,(void*)ptr->uns,ptr->idx*4,ptr->siz*4,
            change->read(UniformWid),change->read(UniformHei),ptr,sub,rsp,ary,log);}
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
        imageFormat(StackState::imageFormat[SrgbFrm]),
        depthFormat(StackState::depthFormat),
        renderPass(StackState::renderPass[SrgbFrm]),
        memProperties(StackState::memProperties) {
    }
    ~SwapState() {
        reset(SmartState());
    }
    VkSwapchainKHR getSwapchain() override {return swapChain;}
    VkFramebuffer getFramebuffer(int i) override {return framebuffers[i];}
    VkExtent2D getExtent() override {return capabilities.currentExtent;}
    void resize(Loc &loc, SmartState log) override {
        capabilities = findCapabilities(window,surface,physical);
        change->poke(UniformWid,getExtent().width); change->write(UniformHei,getExtent().height);
        // std::cout << "extent " << getExtent().width << "/" << getExtent().height << std::endl;
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
    Micro micro;
    VkDescriptorPool descriptorPool;
    VkDescriptorSetLayout descriptorSetLayout;
    VkPipelineLayout pipelineLayout;
    VkPipeline pipeline;
    PipeState() :
        BaseState("PipeState",StackState::self),
        device(StackState::device),
        renderPass(StackState::renderPass[MicroRender__Micro__Render((Micro)StackState::micro)]),
        micro((Micro)StackState::micro++),
        descriptorPool(createDescriptorPool(StackState::device,StackState::descrs)),
        descriptorSetLayout(createDescriptorSetLayout(StackState::device,micro)),
        pipelineLayout(createPipelineLayout(StackState::device,descriptorSetLayout)),
        pipeline(createGraphicsPipeline(StackState::device,getRenderPass(),pipelineLayout,micro)) {
    }
    ~PipeState() {
        vkDestroyPipeline(device, pipeline, nullptr);
        vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
        vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);
        vkDestroyDescriptorPool(device, descriptorPool, nullptr);
        // std::cout << "~" << debug << std::endl;
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
    static VkDescriptorSetLayout createDescriptorSetLayout(VkDevice device, Micro micro);
    static VkPipelineLayout createPipelineLayout(VkDevice device, VkDescriptorSetLayout descriptorSetLayout);
    static VkPipeline createGraphicsPipeline(VkDevice device, VkRenderPass renderPass, VkPipelineLayout pipelineLayout, Micro micro);
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
        range = loc.max.size;
        VkDeviceSize bufferSize = loc.max.size;
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
        int tmp = loc.req.idx - loc.max.base;
        if (tmp < 0 || loc.req.siz < 0 || tmp+loc.req.siz > loc.max.size) EXIT
        memcpy((void*)((char*)mapped+tmp), loc.req.ptr, loc.req.siz);
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
        log << "resize " << debug << " " << loc.max << '\n';
        range = loc.max.size;
        VkDeviceSize bufferSize = loc.max.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_DST_BIT | flags, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, memProperties, buffer, memory);
        loc.commandBuffer = createCommandBuffer(device,commandPool);
        loc.syn.fen = createFence(device);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkDestroyFence(device, loc.syn.fen, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &loc.commandBuffer);
        vkFreeMemory(device, memory, nullptr);
        vkDestroyBuffer(device, buffer, nullptr);
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << '\n';
        int tmp = loc.req.idx - loc.max.base;
        if (tmp < 0 || loc.req.siz < 0 || tmp+loc.req.siz > loc.max.size) EXIT
        VkDeviceSize bufferSize = loc.max.size;
        createBuffer(device, physical, bufferSize, VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
        memProperties, stagingBuffer, stagingBufferMemory);
        void* data; vkMapMemory(device, stagingBufferMemory, 0, bufferSize, 0, &data);
        memcpy((void*)((char*)data+tmp),loc.req.ptr,loc.req.siz);
        vkResetCommandBuffer(loc.commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        vkResetFences(device, 1, &loc.syn.fen);
        copyBuffer(device, graphics, stagingBuffer, buffer, bufferSize, loc.commandBuffer, loc.syn.fen,VK_NULL_HANDLE,VK_NULL_HANDLE);
        return loc.syn.fen;
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
    // ReformLoc format for use as texture
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
    static Render vulkanRender(Reuse i) {
        switch (i) {default: EXIT
        break; case (TexUse): case (ColUse): return SrgbFrm;
        break; case (FdbUse): case (PieUse): return UintFrm;
        break; case (GetUse): case (SetUse): case (DptUse): return SfloatFrm;}
        return Renders;
    }
    static bool isr(Reuse i) {
        switch (i) {default: EXIT
        break; case (TexUse): case (SetUse): return false;
        break; case (FdbUse): case (GetUse): case (PieUse): case (DptUse): case (ColUse): return true;}
        return false;
    }
    static const char *vulkanDebug(VkImageLayout img) {
        switch (img) {default: EXIT
        break; case (VK_IMAGE_LAYOUT_UNDEFINED): return "VK_IMAGE_LAYOUT_UNDEFINED";
        break; case (VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL): return "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL";
        break; case (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL): return "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL";
        break; case (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL): return "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL";VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        break; case (VK_IMAGE_LAYOUT_PRESENT_SRC_KHR): return "VK_IMAGE_LAYOUT_PRESENT_SRC_KHR";VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;}
        return 0;
    }
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << " location:" << *loc << " quality value:" << get(RuseQua) << "/" << loc->use << '\n';
        if (*loc == ResizeLoc) {
        if (&loc != &ploc[*loc]) EXIT
        int texWidth = loc.max.extent.width;
        int texHeight = loc.max.extent.height;
        extent = loc.max.extent;
        VkImageUsageFlagBits flags;
        VkFormat forms = PhysicalState::vulkanFormat(vulkanRender(loc->use));
        if (loc->use == TexUse) {
        flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_SAMPLED_BIT | (int)VK_IMAGE_USAGE_TRANSFER_DST_BIT);}
        if (loc->use != TexUse) {
        flags = (VkImageUsageFlagBits)((int)VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | (int)VK_IMAGE_USAGE_TRANSFER_SRC_BIT | (int)VK_IMAGE_USAGE_TRANSFER_DST_BIT);}
        createImage(device, physical, texWidth, texHeight, forms, flags, memProperties, /*output*/ image, imageMemory);
        imageView = createImageView(device, image, forms, VK_IMAGE_ASPECT_COLOR_BIT);
        if (loc->use == TexUse) {
        textureSampler = createTextureSampler(device,properties);}
        if (loc->use != TexUse) {
        createImage(device, physical, loc.max.extent.width, loc.max.extent.height, depthFormat, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, memProperties,/*output*/ depthImage, depthMemory);
        depthImageView = createImageView(device, depthImage, depthFormat, VK_IMAGE_ASPECT_DEPTH_BIT);
        createFramebuffer(device,loc.max.extent,renderPass[vulkanRender(loc->use)],imageView,depthImageView,framebuffer);}}
        if (*loc == ReformLoc) loc.commandBuffer = createCommandBuffer(device,commandPool); 
        if (*loc == BeforeLoc) loc.commandBuffer = createCommandBuffer(device,commandPool);
        if (*loc == MiddleLoc) loc.commandBuffer = createCommandBuffer(device,commandPool);
        if (*loc == AfterLoc) loc.commandBuffer = createCommandBuffer(device,commandPool);
        loc.syn.sem = createSemaphore(device); // TODO as needed
        loc.syn.fen = createFence(device); // TODO as needed
    }
    void unsize(Loc &loc, SmartState log) override {
        log << "unsize " << debug << " location:" << *loc << '\n';
        vkDestroyFence(device, loc.syn.fen, nullptr); // TODO as needed
        vkDestroySemaphore(device, loc.syn.sem, nullptr); // TODO as needed
        if (*loc == AfterLoc) vkFreeCommandBuffers(device, commandPool, 1, &loc.commandBuffer);
        if (*loc == MiddleLoc) vkFreeCommandBuffers(device, commandPool, 1, &loc.commandBuffer);
        if (*loc == BeforeLoc) vkFreeCommandBuffers(device, commandPool, 1, &loc.commandBuffer);
        if (*loc == ReformLoc) vkFreeCommandBuffers(device, commandPool, 1, &loc.commandBuffer);
        if (*loc == ResizeLoc) {
        if (&loc != &ploc[*loc]) EXIT
        if (loc->use != TexUse) {
        vkDestroyFramebuffer(device, framebuffer, nullptr);
        vkDestroyImageView(device, depthImageView, nullptr);
        vkDestroyImage(device, depthImage, nullptr);
        vkFreeMemory(device, depthMemory, nullptr);}
        if (loc->use == TexUse) {
        vkDestroySampler(device, textureSampler, nullptr);}
        vkDestroyImageView(device, imageView, nullptr);
        vkDestroyImage(device, image, nullptr);
        vkFreeMemory(device, imageMemory, nullptr);}
    }
    VkFence setup(Loc &loc, SmartState log) override {
        VkFence fence = (loc.nxt.ptr==0?loc.syn.fen:VK_NULL_HANDLE);
        VkSemaphore before = (loc.lst.ptr!=0?loc.lst.ptr->get(loc.lst.loc).syn.sem:VK_NULL_HANDLE);
        VkSemaphore after = (loc.nxt.ptr!=0?loc.syn.sem:VK_NULL_HANDLE);
        log << "setup " << debug << " location:" << *loc << " quality value:" << get(RuseQua) << "/" << loc->use << " before:" << before << " after:" << after << " fence:" << fence << '\n'; slog.clr();
        VkFormat forms = PhysicalState::vulkanFormat(vulkanRender(loc->use));
        if (fence != VK_NULL_HANDLE) vkResetFences(device, 1, &fence);
        if (*loc == ReformLoc) {
        vkResetCommandBuffer(loc.commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        log << "setup ReformLoc " << vulkanDebug(loc.max.src) << "/" << before << "->" << vulkanDebug(loc.max.dst) << "/" << after << '\n';
        transitionImageLayout(device, graphics, loc.commandBuffer, getImage(), before, after, fence, forms, loc.max.src, loc.max.dst);}
        if (*loc == BeforeLoc) {
        vkResetCommandBuffer(loc.commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        log << "setup BeforeLoc " << vulkanDebug(loc.max.src) << "/" << before << "->" << vulkanDebug(loc.max.dst) << "/" << after << '\n';
        transitionImageLayout(device, graphics, loc.commandBuffer, getImage(), before, after, fence, forms, loc.max.src, loc.max.dst);}
        if (*loc == AfterLoc) {
        vkResetCommandBuffer(loc.commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        log << "setup AfterLoc " << vulkanDebug(loc.max.src) << "/" << before << "->" << vulkanDebug(loc.max.dst) << "/" << after << '\n';
        transitionImageLayout(device, graphics, loc.commandBuffer, getImage(), before, after, fence, forms, loc.max.src, loc.max.dst);}
        if (*loc == MiddleLoc) {
        Loc &got = get(ResizeLoc);
        int texWidth = got.max.extent.width;
        int texHeight = got.max.extent.height;
        VkDeviceSize imageSize = texWidth*texHeight*4;
        createBuffer(device, physical, imageSize, (isr(loc->use) ? VK_BUFFER_USAGE_TRANSFER_DST_BIT : VK_BUFFER_USAGE_TRANSFER_SRC_BIT), VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, memProperties, loc.stagingBuffer, loc.stagingBufferMemory);
        void* data; if (!isr(loc->use)) {
        vkMapMemory(device, loc.stagingBufferMemory, 0, imageSize, 0, &data);}
        if (loc->use == TexUse) {
        memcpy(data, loc.req.ptr, loc.req.siz);}
        else if (!isr(loc->use)) {
        // TODO allow widths other than 4 by interpreting idx and siz as bytes
        memcpy((void*)((char*)data + loc.req.idx*4), loc.req.ptr, loc.req.siz*4);}
        vkResetCommandBuffer(loc.commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        copyTextureImage(device, graphics, memProperties, getImage(),0,0,texWidth,texHeight, before, after, loc.stagingBuffer, loc.commandBuffer, isr(loc->use));}
        return fence;
    }
    void upset(Loc &loc, SmartState log) override {
        log << "upset " << debug << " location:" << *loc << '\n';
        if (*loc == MiddleLoc) {
        Loc &got = get(ResizeLoc);
        int texWidth = got.max.extent.width;
        int texHeight = got.max.extent.height;
        VkDeviceSize imageSize = texWidth*texHeight*4;
        if (isr(loc->use)) {
        void* data; vkMapMemory(device, loc.stagingBufferMemory, 0, imageSize, 0, &data);
        // TODO allow widths other than 4 by interpreting idx and siz as bytes
        memcpy((void*)loc.req.ptr, (void*)((char*)data + loc.req.idx*4), loc.req.siz*4);}
        vkUnmapMemory(device, loc.stagingBufferMemory);
        vkDestroyBuffer(device, loc.stagingBuffer, nullptr);
        vkFreeMemory(device, loc.stagingBufferMemory, nullptr);}
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
    Reloc imageLoc;
    VkFramebuffer framebuffer;
    VkSemaphore acquire;
    ChainState() :
        BaseState("ChainState",StackState::self),
        device(StackState::device),
        present(StackState::present),
        change(StackState::change) {}
    ~ChainState() {
        reset(SmartState());
    }
    VkSemaphore getAcquireSem() override {return acquire;}
    VkSemaphore getPresentSem() override {return get(imageLoc).syn.sem;}
    VkFramebuffer getFramebuffer() override {return framebuffer;}
    void resize(Loc &loc, SmartState log) override {
        log << "resize " << debug << '\n';
        if (*loc == BeforeLoc) {
        for (int i = 0; i < Relocs; i++) get((Reloc)i).syn.sem = createSemaphore(device); // TODO as needed
        acquire = createSemaphore(device);}
    }
    void unsize(Loc &loc, SmartState log) override {
        if (*loc == BeforeLoc) {
        vkDestroySemaphore(device, acquire, nullptr);
        for (int i = 0; i < Relocs; i++) vkDestroySemaphore(device, get((Reloc)i).syn.sem, nullptr);} // TODO as needed
        log << "usize " << debug << '\n';
    }
    VkFence setup(Loc &loc, SmartState log) override {
        BaseState *swp = 0;
        for (int i = 0; i < loc.unl.siz; i++) {Bnd bnd = get(loc.unl,i);
        if (bnd.phs == SwapPhs && swp) EXIT
        if (bnd.phs == SwapPhs) swp = bnd.buf;}
        log << "setup " << debug << '\n';
        if (*loc == BeforeLoc) {
        VkResult result = vkAcquireNextImageKHR(device,
        swp->getSwapchain(), UINT64_MAX, acquire, VK_NULL_HANDLE, &imageIndex);
        imageLoc = (Reloc)imageIndex;
        if (imageLoc < 0 || imageLoc >= Relocs) EXIT
        if (result == VK_ERROR_OUT_OF_DATE_KHR) change->wots(RegisterWake,1<<SizeMsk);
        else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) EXIT
        framebuffer = swp->getFramebuffer(imageIndex);}
        if (*loc == AfterLoc) {
        // prior depender must interpret location as swap chain semaphore
        if (!presentFrame(present,swp->getSwapchain(),imageIndex,get(imageLoc).syn.sem))
        change->wots(RegisterWake,1<<SizeMsk);}
        // TODO presentFrame might block and signals no fence
        // TODO perhaps run on separate thread and allow block on progress in that thread instead of on fence
        return VK_NULL_HANDLE; // no resources to release, use NullReq
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
    DrawState() :
        BaseState("DrawState",StackState::self),
        device(StackState::device),
        graphics(StackState::graphics),
        commandPool(StackState::commandPool),
        frames(StackState::frames) {
        for (int i = 0; i < Relocs; i++) get((Reloc)i).syn.sem = createSemaphore(device); // TODO as needed
    }
    ~DrawState() {
        for (int i = 0; i < Relocs; i++) vkDestroySemaphore(device, get((Reloc)i).syn.sem, nullptr); // TODO as needed
        reset(SmartState());
    }
    void resize(Loc &loc, SmartState log) override {
        BaseState *pip = 0;
        for (int i = 0; i < loc.unl.siz; i++) {Bnd bnd = get(loc.unl,i);
        if (bnd.phs == PipePhs && pip) EXIT
        if (bnd.phs == PipePhs) pip = bnd.buf;}
        log << "resize " << debug << " " << pip->debug << '\n';
        descriptorPool = pip->getDescriptorPool();
        descriptorLayout = pip->getDescriptorSetLayout();
        descriptorSet = createDescriptorSet(device,descriptorPool,descriptorLayout,frames);
        loc.commandBuffer = createCommandBuffer(device,commandPool);
        loc.syn.fen = createFence(device);
    }
    void unsize(Loc &loc, SmartState log) override {
        vkDestroyFence(device, loc.syn.fen, nullptr);
        vkFreeCommandBuffers(device, commandPool, 1, &loc.commandBuffer);
        vkFreeDescriptorSets(device,descriptorPool,1,&descriptorSet);
        log << "unsize " << debug << '\n';
    }
    VkFence setup(Loc &loc, SmartState log) override {
        log << "setup " << debug << '\n';
        if (loc.req.ptr != 0 || loc.req.idx != 0) EXIT
        if (loc.max.tag != MicroExt) EXIT
        VkFence fence = (loc.nxt.ptr==0?loc.syn.fen:VK_NULL_HANDLE);
        VkSemaphore before = (loc.lst.ptr!=0?loc.lst.ptr->get(loc.lst.loc).syn.sem:VK_NULL_HANDLE);
        VkSemaphore after = (loc.nxt.ptr!=0?loc.syn.sem:VK_NULL_HANDLE);
        vkResetFences(device, 1, &loc.syn.fen);
        vkResetCommandBuffer(loc.commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);
        BaseState *pipePtr, *swapPtr, *framePtr, *indexPtr, *fetchPtr;
        pipePtr = swapPtr = framePtr = indexPtr = fetchPtr = 0;
        for (int i = 0; i < loc.unl.siz; i++) {
        Bnd bnd = get(loc.unl,i);
        {char *st0 = 0; char *st1 = 0; char *st2 = 0;
        showPhase(bnd.phs,&st0); showResrc(bnd.typ,&st1); showInstr(bnd.ins,&st2);
        log << "setup " << st0 << " " << st1 << " " << st2 << " " << bnd.buf->debug << '\n';
        free(st0); free(st1); free(st2);}
        switch (bnd.phs) {default: EXIT
        break; case(PipePhs): if (pipePtr) EXIT; pipePtr = bnd.buf;
        break; case(SwapPhs): if (swapPtr) EXIT; swapPtr = bnd.buf;
        break; case(FramePhs): if (framePtr) EXIT; framePtr = bnd.buf;
        break; case(RenderPhs): if (swapPtr || framePtr) EXIT; swapPtr = framePtr = bnd.buf;
        break; case(IndexPhs): if (indexPtr) EXIT; indexPtr = bnd.buf;
        break; case(FetchPhs): if (fetchPtr) EXIT; fetchPtr = bnd.buf;
        break; case(UniformPhs): {BaseState *ptr = bnd.buf;
        updateUniformDescriptor(device,ptr->getBuffer(),ptr->getRange(),bnd.bnd,descriptorSet);}
        break; case(StoragePhs): {BaseState *ptr = bnd.buf;
        updateStorageDescriptor(device,ptr->getBuffer(),ptr->getRange(),bnd.bnd,descriptorSet);}
        break; case(RelatePhs): {BaseState *ptr = bnd.buf;
        updateStorageDescriptor(device,ptr->getBuffer(),ptr->getRange(),bnd.bnd,descriptorSet);}
        break; case(SamplePhs): {BaseState *ptr = bnd.buf;
        updateTextureDescriptor(device,ptr->getImageView(),ptr->getTextureSampler(),bnd.bnd,descriptorSet);}}}
        if (!pipePtr || !swapPtr || !framePtr) EXIT
        log << "record " << debug << " " << framePtr->debug << '\n';
        recordCommandBuffer(loc.commandBuffer,pipePtr->getRenderPass(),descriptorSet,swapPtr->getExtent(),loc.req.siz,framePtr->getFramebuffer(),pipePtr->getPipeline(),pipePtr->getPipelineLayout(),(fetchPtr?fetchPtr->getBuffer():VK_NULL_HANDLE),(indexPtr?indexPtr->getBuffer():VK_NULL_HANDLE));
        VkSemaphore acquire = (framePtr != swapPtr ? framePtr->getAcquireSem() : VK_NULL_HANDLE);
        VkSemaphore release = (framePtr != swapPtr ? framePtr->getPresentSem() : VK_NULL_HANDLE);
        drawFrame(loc.commandBuffer,graphics,loc.req.ptr,loc.req.idx,loc.req.siz,acquire,release,fence,before,after);
        return fence;
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
    static void recordCommandBuffer(VkCommandBuffer commandBuffer, VkRenderPass renderPass, VkDescriptorSet descriptorSet, VkExtent2D renderArea, uint32_t indices, VkFramebuffer framebuffer, VkPipeline graphicsPipeline, VkPipelineLayout pipelineLayout, VkBuffer vertexBuffer, VkBuffer indexBuffer);
    static void drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, void *ptr, int loc, int siz, VkSemaphore acquire, VkSemaphore release, VkFence fence, VkSemaphore before, VkSemaphore after);
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
    ArrayState<ImageState,PierceRes,StackState::piercs> pierceState;
    ArrayState<ImageState,RelateRes,StackState::frames> relateState;
    ArrayState<ImageState,ColorRes,StackState::frames> colorState;
    ArrayState<UniformState,UniformRes,StackState::frames> uniformState;
    ArrayState<UniformState,MatrixRes,StackState::frames> matrixState;
    ArrayState<BufferState,TriangleRes,StackState::frames> triangleState;
    ArrayState<BufferState,NumericRes,StackState::frames> numericState;
    ArrayState<BufferState,VertexRes,StackState::frames> vertexState;
    ArrayState<BufferState,BasisRes,StackState::frames> basisState;
    ArrayState<ChainState,ChainRes,StackState::frames> chainState;
    ArrayState<DrawState,DrawRes,StackState::micros> drawState;
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
            {PierceRes,&pierceState},
            {RelateRes,&relateState},
            {ColorRes,&colorState},
            {UniformRes,&uniformState},
            {MatrixRes,&matrixState},
            {TriangleRes,&triangleState},
            {NumericRes,&numericState},
            {VertexRes,&vertexState},
            {BasisRes,&basisState},
            {ChainRes,&chainState},
            {DrawRes,&drawState},
            {BindRes,&bindState},
            {Resrcs,0}},
        constState{{
            MemoryIns__Memory__Int__Instr,
            MemoryIns__Memory__Int__Reloc,
            MemoryIns__Memory__Int__Format,
            MemoryIns__Memory__Int__Resrc,
            MemoryIns__Memory__Int__Memory,
            MemoryIns__Memory__Int__Micro,
            MemoryIns__Memory__Int__Phase,
            MemoryIns__Memory__Int__Quality,
            MemoryIns__Memory__Int__Reuse,
            MemoryIns__Memory__Int__Default,
            MemoryIns__Memory__Int__Int,
            ResrcIns__Resrc__Int__Instr,
            ResrcIns__Resrc__Int__Reloc,
            ResrcIns__Resrc__Int__Format,
            ResrcIns__Resrc__Int__Resrc,
            ResrcIns__Resrc__Int__Memory,
            ResrcIns__Resrc__Int__Micro,
            ResrcIns__Resrc__Int__Phase,
            ResrcIns__Resrc__Int__Quality,
            ResrcIns__Resrc__Int__Reuse,
            ResrcIns__Resrc__Int__Default,
            ResrcIns__Resrc__Int__Int,
            MicroIns__Micro__Int__Instr,
            MicroIns__Micro__Int__Reloc,
            MicroIns__Micro__Int__Format,
            MicroIns__Micro__Int__Resrc,
            MicroIns__Micro__Int__Memory,
            MicroIns__Micro__Int__Micro,
            MicroIns__Micro__Int__Phase,
            MicroIns__Micro__Int__Quality,
            MicroIns__Micro__Int__Reuse,
            MicroIns__Micro__Int__Default,
            MicroIns__Micro__Int__Int},{
            MemoryAlt__Memory__Int__Instr,
            MemoryAlt__Memory__Int__Reloc,
            MemoryAlt__Memory__Int__Format,
            MemoryAlt__Memory__Int__Resrc,
            MemoryAlt__Memory__Int__Memory,
            MemoryAlt__Memory__Int__Micro,
            MemoryAlt__Memory__Int__Phase,
            MemoryAlt__Memory__Int__Quality,
            MemoryAlt__Memory__Int__Reuse,
            MemoryAlt__Memory__Int__Default,
            MemoryAlt__Memory__Int__Int,
            ResrcAlt__Resrc__Int__Instr,
            ResrcAlt__Resrc__Int__Reloc,
            ResrcAlt__Resrc__Int__Format,
            ResrcAlt__Resrc__Int__Resrc,
            ResrcAlt__Resrc__Int__Memory,
            ResrcAlt__Resrc__Int__Micro,
            ResrcAlt__Resrc__Int__Phase,
            ResrcAlt__Resrc__Int__Quality,
            ResrcAlt__Resrc__Int__Reuse,
            ResrcAlt__Resrc__Int__Default,
            ResrcAlt__Resrc__Int__Int,
            MicroAlt__Micro__Int__Instr,
            MicroAlt__Micro__Int__Reloc,
            MicroAlt__Micro__Int__Format,
            MicroAlt__Micro__Int__Resrc,
            MicroAlt__Micro__Int__Memory,
            MicroAlt__Micro__Int__Micro,
            MicroAlt__Micro__Int__Phase,
            MicroAlt__Micro__Int__Quality,
            MicroAlt__Micro__Int__Reuse,
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
        indexState(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
        bringupState(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
        triangleState(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
        threadState(logicalState.device,&changeState),
        copyState(&changeState,&threadState,enumState,constState) {
        // std::cout << "MainState" << std::endl;
    }
    ~MainState() {
        // std::cout << "~MainState" << std::endl;
    }
};

MainState *mptr = 0;
// glfw callbacks
void glfwKeypress(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (key == GLFW_KEY_C && action == GLFW_PRESS && mods == GLFW_MOD_CONTROL) EXIT
}
void glfwResize(GLFWwindow* window, int width, int height) {
    mptr->changeState.poke(UniformWid,width);
    mptr->changeState.write(UniformHei,height);
    mptr->changeState.wots(RegisterWake,1<<SizeMsk);
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
int vulkanHnfo() {
    return mptr->changeState.hnfo();
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
    /*void *buffer[100];
    int size = backtrace(buffer,100);
    backtrace_symbols_fd(buffer,size,2);*/
}
void whereIsExit(int val, void *arg) {
    if (val < 0) *(int*)0=0;
}
void errorFunc(const char *str, int num, int idx) {
    std::cout << "errfnc called on " << idx << " in " << str << ": " << num << std::endl;
}
void sigintFunc(int sig) {
    slog.clr(); *(int*)0=0;
}

int main(int argc, const char **argv) {
    struct sigaction act;
    act.sa_handler = sigintFunc;
    if (sigaction(SIGINT,&act,0) < 0) ERROR();
    if (sigaction(SIGSEGV,&act,0) < 0) ERROR();
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
    main.changeState.write(ConstantPiercs,StackState::piercs);
    main.changeState.write(ConstantInstrs,StackState::instrs);
    main.changeState.write(ConstantResrcs,StackState::resrcs);
    main.changeState.write(ConstantHandls,StackState::handls);
    main.changeState.call(RegisterOpen,vulkanBack);
    main.changeState.call(RegisterWake,vulkanBack);
    main.callState.back(&main.threadState,FenceThd);
    planeInit(vulkanCopy,vulkanCall,vulkanFork,vulkanInfo,vulkanJnfo,vulkanKnfo,vulkanHnfo,vulkanCmnd,vulkanWait);
    // TODO move glfw functions to WindowState
    glfwSetKeyCallback(main.windowState.window,glfwKeypress);
    glfwSetWindowSizeCallback(main.windowState.window,glfwResize);
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
    if (messageSeverity != VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT)
    std::cout << "validation layer: " << pCallbackData->pMessage << " severity:0x" << std::hex << messageSeverity << std::dec << std::endl;
    // if (messageSeverity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) *(int*)0=0;
    // if (pCallbackData->messageIdNumber == 1180184443) *(int*)0=0;
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
    actualExtent.width = actualExtent.width;
    if (actualExtent.width < capabilities.minImageExtent.width) actualExtent.width = capabilities.minImageExtent.width;
    if (actualExtent.width > capabilities.maxImageExtent.width) actualExtent.width = capabilities.maxImageExtent.width;
    actualExtent.height = actualExtent.height;
    if (actualExtent.height < capabilities.minImageExtent.height) actualExtent.height = capabilities.minImageExtent.height;
    if (actualExtent.height > capabilities.maxImageExtent.height) actualExtent.height = capabilities.maxImageExtent.height;
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
VkDescriptorSetLayout PipeState::createDescriptorSetLayout(VkDevice device, Micro micro) {
    VkDescriptorSetLayout descriptorSetLayout;
    HeapState<VkDescriptorSetLayoutBinding> bindings;
    for (int i = 0; MicroBinding__Micro__Int__Phase(micro)(i) != Phases; i++) {
    Phase phs = MicroBinding__Micro__Int__Phase(micro)(i);
    int bnd = MicroBinding__Micro__Int__Int(micro)(i);
    /*{char *st0 = 0; char *st1 = 0;
    showMicro(micro,&st0); showPhase(phs,&st1);
    std::cerr << st0 << " " << st1 << bnd << std::endl;
    free(st0); free(st1);}*/
    switch (phs) {default:
    break; case (UniformPhs): {
    VkDescriptorSetLayoutBinding uboLayoutBinding{};
    uboLayoutBinding.binding = bnd;
    uboLayoutBinding.descriptorCount = 1;
    uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    uboLayoutBinding.pImmutableSamplers = nullptr;
    uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT|VK_SHADER_STAGE_FRAGMENT_BIT;
    bindings << uboLayoutBinding;}
    break; case (StoragePhs): {
    VkDescriptorSetLayoutBinding storageLayoutBinding{};
    storageLayoutBinding.binding = bnd;
    storageLayoutBinding.descriptorCount = 1;
    storageLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    storageLayoutBinding.pImmutableSamplers = nullptr;
    storageLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
    bindings << storageLayoutBinding;}
    break; case (RelatePhs): {
    VkDescriptorSetLayoutBinding relateLayoutBinding{};
    relateLayoutBinding.binding = bnd;
    relateLayoutBinding.descriptorCount = 1;
    relateLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    relateLayoutBinding.pImmutableSamplers = nullptr;
    relateLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
    bindings << relateLayoutBinding;}
    break; case (SamplePhs): {
    VkDescriptorSetLayoutBinding samplerLayoutBinding{};
    samplerLayoutBinding.binding = bnd;
    samplerLayoutBinding.descriptorCount = 1;
    samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    samplerLayoutBinding.pImmutableSamplers = nullptr;
    samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
    bindings << samplerLayoutBinding;}}}
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
    if (!file.is_open()) {std::cerr << "failed to open shader: " << filename << std::endl; exit(-1);}
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
    VkPipelineLayout pipelineLayout, Micro micro) {
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
    /*{char *st2 = 0; showMicro(micro,&st2);
    fprintf(stderr,"micro:%s\n",st2);}*/
    for (int i = 0; MicroBinding__Micro__Int__Phase(micro)(i) != Phases; i++) {
    switch (MicroBinding__Micro__Int__Phase(micro)(i)) {default:
    break; case (FetchPhs): {
    VkVertexInputBindingDescription bindingDescription{};
    bindingDescription.binding = MicroBinding__Micro__Int__Int(micro)(i);
    bindingDescription.stride = MicroStride__Micro__Int(micro);
    bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
    bindingDescriptions << bindingDescription;
    for (int j = 0; MicroFormat__Micro__Int__Render(micro)(j) != Renders; j++) {
    // fprintf(stderr,"render:%d\n",j);
    VkVertexInputAttributeDescription attributeDescription{};
    attributeDescription.binding = MicroBinding__Micro__Int__Int(micro)(i);
    attributeDescription.location = j;
    attributeDescription.format = PhysicalState::vulkanFormat(MicroFormat__Micro__Int__Render(micro)(j));
    attributeDescription.offset = MicroOffset__Micro__Int__Int(micro)(j);
    attributeDescriptions << attributeDescription;}}}}
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
    VkDescriptorSet descriptorSet, VkExtent2D renderArea, uint32_t indices,
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
    if (vertexBuffer!=VK_NULL_HANDLE) vkCmdBindVertexBuffers(commandBuffer, 0, 1, buffers, offsets);
    if (indexBuffer!=VK_NULL_HANDLE) vkCmdBindIndexBuffer(commandBuffer, indexBuffer, 0, VK_INDEX_TYPE_UINT16);
    vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
    if (vertexBuffer!=VK_NULL_HANDLE && indexBuffer!=VK_NULL_HANDLE) vkCmdDrawIndexed(commandBuffer, indices, 1, 0, 0, 0);
    else vkCmdDraw(commandBuffer,indices,indices/3,0,0);
    vkCmdEndRenderPass(commandBuffer);
    if (vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
    EXIT
}
void DrawState::drawFrame(VkCommandBuffer commandBuffer, VkQueue graphics, void *ptr, int loc, int siz,
    VkSemaphore acquire, VkSemaphore release, VkFence fence, VkSemaphore before, VkSemaphore after) {
    VkSubmitInfo submitInfo{};
    submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    VkSemaphore waitSemaphores[] = {acquire,before};
    VkPipelineStageFlags waitStages[] = {
    VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
    VK_PIPELINE_STAGE_ALL_COMMANDS_BIT};
    submitInfo.waitSemaphoreCount = 2;
    if (acquire == VK_NULL_HANDLE && before == VK_NULL_HANDLE) submitInfo.waitSemaphoreCount = 0;
    else if (acquire == VK_NULL_HANDLE) {waitSemaphores[0] = before; submitInfo.waitSemaphoreCount = 1;}
    else if (before == VK_NULL_HANDLE) submitInfo.waitSemaphoreCount = 1;
    submitInfo.pWaitSemaphores = waitSemaphores;
    submitInfo.pWaitDstStageMask = waitStages;
    VkCommandBuffer commandBuffers[] = {commandBuffer};
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &commandBuffer;
    VkSemaphore signalSemaphores[] = {release,after};
    submitInfo.signalSemaphoreCount = 2;
    if (release == VK_NULL_HANDLE && after == VK_NULL_HANDLE) submitInfo.signalSemaphoreCount = 0;
    else if (release == VK_NULL_HANDLE) {signalSemaphores[0] = after; submitInfo.signalSemaphoreCount = 1;}
    else if (after == VK_NULL_HANDLE) submitInfo.signalSemaphoreCount = 1;
    submitInfo.pSignalSemaphores = signalSemaphores;
    if (vkQueueSubmit(graphics, 1, &submitInfo, fence) != VK_SUCCESS) EXIT
}
