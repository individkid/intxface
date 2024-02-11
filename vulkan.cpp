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
#include <array>
#include <optional>
#include <set>
#include <queue>
#include <functional>
#include <pthread.h>
#include <semaphore.h>

#define NANOSECONDS (10^9)

extern "C" {
    #include "type.h"
    #include "plane.h"
    // TODO link with plane.c
    void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw) {init();}
    void planeAddarg(const char *str) {}
    int planeInfo(enum Configure cfg) {return 0;}
    void planeSafe(enum Proc proc, enum Wait wait, enum Configure hint) {}
    void planeMain() {}
    void planeReady(struct Pierce *pierce) {}
    // following are called from plane.c
    void vulkanInit(); // init
    void vulkanDma(struct Center *center); // dma
    void vulkanSafe(); // safe
    void vulkanMain(enum Proc proc, enum Wait wait); // main
    int vulkanInfo(enum Configure query); // info
    void vulkanDraw(enum Micro shader, int base, int limit); // draw
}

// TODO replace by type.h
struct Input {
    glm::vec2 pos;
    glm::vec3 color;

    static VkVertexInputBindingDescription getBindingDescription() {
        VkVertexInputBindingDescription bindingDescription{};
        bindingDescription.binding = 0;
        bindingDescription.stride = sizeof(Input);
        bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

        return bindingDescription;
    }

    static std::array<VkVertexInputAttributeDescription, 2> getAttributeDescriptions() {
        std::array<VkVertexInputAttributeDescription, 2> attributeDescriptions{};

        attributeDescriptions[0].binding = 0;
        attributeDescriptions[0].location = 0;
        attributeDescriptions[0].format = VK_FORMAT_R32G32_SFLOAT;
        attributeDescriptions[0].offset = offsetof(Input, pos);

        attributeDescriptions[1].binding = 0;
        attributeDescriptions[1].location = 1;
        attributeDescriptions[1].format = VK_FORMAT_R32G32B32_SFLOAT;
        attributeDescriptions[1].offset = offsetof(Input, color);

        return attributeDescriptions;
    }
};
struct UniformBufferObject {
    alignas(16) glm::mat4 model;
    alignas(16) glm::mat4 view;
    alignas(16) glm::mat4 proj;
};
// TODO move following to planer.lua
const std::vector<Input> vertices = {
    {{-0.5f, -0.5f}, {1.0f, 0.0f, 0.0f}},
    {{0.5f, -0.5f}, {0.0f, 1.0f, 0.0f}},
    {{0.5f, 0.5f}, {0.0f, 0.0f, 1.0f}},
    {{-0.5f, 0.5f}, {1.0f, 1.0f, 1.0f}}
};

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

VkResult CreateDebugUtilsMessengerEXT(VkInstance instance, const VkDebugUtilsMessengerCreateInfoEXT* pCreateInfo, const VkAllocationCallbacks* pAllocator, VkDebugUtilsMessengerEXT* pDebugMessenger) {
    auto func = (PFN_vkCreateDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
    if (func != nullptr) {
        return func(instance, pCreateInfo, pAllocator, pDebugMessenger);
    } else {
        return VK_ERROR_EXTENSION_NOT_PRESENT;
    }
}
void DestroyDebugUtilsMessengerEXT(VkInstance instance, VkDebugUtilsMessengerEXT debugMessenger, const VkAllocationCallbacks* pAllocator) {
    auto func = (PFN_vkDestroyDebugUtilsMessengerEXT) vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
    if (func != nullptr) {
        func(instance, debugMessenger, pAllocator);
    }
}
VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity, VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData) {
    std::cerr << "validation layer: " << pCallbackData->pMessage << std::endl;
    return VK_FALSE;
}

// TODO move the following to anonymous in SwapState and FrameState
VkSwapchainKHR createSwapChain(VkPhysicalDevice physicalDevice, VkDevice device, VkSurfaceKHR surface,
    VkSurfaceFormatKHR surfaceFormat, VkPresentModeKHR presentMode, VkExtent2D swapChainExtent,
    VkSurfaceCapabilitiesKHR surfaceCapabilities, uint32_t minImageCount, uint32_t graphicid, uint32_t presentid) {

    VkSwapchainKHR swapChain;

    VkSwapchainCreateInfoKHR createInfo{};
    createInfo.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
    createInfo.surface = surface;

    createInfo.minImageCount = minImageCount;
    createInfo.imageFormat = surfaceFormat.format;
    createInfo.imageColorSpace = surfaceFormat.colorSpace;
    createInfo.imageExtent = swapChainExtent;
    createInfo.imageArrayLayers = 1;
    createInfo.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;


    if (graphicid != presentid) {
        uint32_t indices[2] = {
            graphicid,
            presentid,
        };
        createInfo.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
        createInfo.queueFamilyIndexCount = 2;
        createInfo.pQueueFamilyIndices = indices;
    } else {
        createInfo.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    }

    createInfo.preTransform = surfaceCapabilities.currentTransform;
    createInfo.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
    createInfo.presentMode = presentMode;
    createInfo.clipped = VK_TRUE;

    if (vkCreateSwapchainKHR(device, &createInfo, nullptr, &swapChain) != VK_SUCCESS) {
        throw std::runtime_error("failed to create swap chain!");
    }

    return swapChain;
}
VkImageView createImageView(VkDevice device, VkFormat swapChainImageFormat, VkImage swapChainImage) {
    VkImageView swapChainImageView;
    VkImageViewCreateInfo createInfo{};
    createInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    createInfo.image = swapChainImage;
    createInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
    createInfo.format = swapChainImageFormat;
    createInfo.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
    createInfo.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
    createInfo.subresourceRange.baseMipLevel = 0;
    createInfo.subresourceRange.levelCount = 1;
    createInfo.subresourceRange.baseArrayLayer = 0;
    createInfo.subresourceRange.layerCount = 1;
    if (vkCreateImageView(device, &createInfo, nullptr, &swapChainImageView) != VK_SUCCESS)
        throw std::runtime_error("failed to create image views!");
    return swapChainImageView;
}
VkFramebuffer createFramebuffer(VkDevice device, VkExtent2D swapChainExtent, VkImageView swapChainImageView, VkRenderPass renderPass) {
    VkFramebuffer swapChainFramebuffer;
    VkImageView attachments[] = {
        swapChainImageView
    };
    VkFramebufferCreateInfo framebufferInfo{};
    framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    framebufferInfo.renderPass = renderPass;
    framebufferInfo.attachmentCount = 1;
    framebufferInfo.pAttachments = attachments;
    framebufferInfo.width = swapChainExtent.width;
    framebufferInfo.height = swapChainExtent.height;
    framebufferInfo.layers = 1;
    if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &swapChainFramebuffer) != VK_SUCCESS) {
        throw std::runtime_error("failed to create framebuffer!");
    }
    return swapChainFramebuffer;
}
VkExtent2D chooseSwapExtent(GLFWwindow* window, const VkSurfaceCapabilitiesKHR& capabilities) {
    if (capabilities.currentExtent.width != std::numeric_limits<uint32_t>::max()) {
        return capabilities.currentExtent;
    } else {
        int width, height;
        glfwGetFramebufferSize(window, &width, &height);

        VkExtent2D actualExtent = {
            static_cast<uint32_t>(width),
            static_cast<uint32_t>(height)
        };

        actualExtent.width = std::clamp(actualExtent.width, capabilities.minImageExtent.width, capabilities.maxImageExtent.width);
        actualExtent.height = std::clamp(actualExtent.height, capabilities.minImageExtent.height, capabilities.maxImageExtent.height);

        return actualExtent;
    }
}

// TODO move following to anonymous in DrawState
VkShaderModule createShaderModule(VkDevice device, const std::vector<char>& code) {
    VkShaderModuleCreateInfo createInfo{};
    createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    createInfo.codeSize = code.size();
    createInfo.pCode = reinterpret_cast<const uint32_t*>(code.data());
    VkShaderModule shaderModule;
    if (vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule) != VK_SUCCESS)
        throw std::runtime_error("failed to create shader module!");
    return shaderModule;
}
std::vector<char> readFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::ate | std::ios::binary);
    if (!file.is_open())
        throw std::runtime_error("failed to open file!");
    size_t fileSize = (size_t) file.tellg();
    std::vector<char> buffer(fileSize);
    file.seekg(0);
    file.read(buffer.data(), fileSize);
    file.close();
    return buffer;
}

// TODO move to anonymous in FetchState etc
uint32_t findMemoryType(VkPhysicalDevice physicalDevice, uint32_t typeFilter, VkMemoryPropertyFlags properties) {
    VkPhysicalDeviceMemoryProperties memProperties;
    vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProperties);

    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++) {
        if ((typeFilter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & properties) == properties) {
            return i;
        }
    }

    throw std::runtime_error("failed to find suitable memory type!");
}
VkDescriptorSet createDescriptorSet(VkDevice device, VkBuffer uniformBuffer,
    VkDescriptorSetLayout descriptorSetLayout, VkDescriptorPool descriptorPool) {
    VkDescriptorSetAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
    allocInfo.descriptorPool = descriptorPool;
    allocInfo.descriptorSetCount = static_cast<uint32_t>(1);
    allocInfo.pSetLayouts = &descriptorSetLayout;

    VkDescriptorSet descriptor;
    if (vkAllocateDescriptorSets(device, &allocInfo, &descriptor) != VK_SUCCESS) {
        throw std::runtime_error("failed to allocate descriptor sets!");
    }

    VkDescriptorBufferInfo bufferInfo{};
    bufferInfo.buffer = uniformBuffer;
    bufferInfo.offset = 0;
    bufferInfo.range = sizeof(UniformBufferObject);

    VkWriteDescriptorSet descriptorWrite{};
    descriptorWrite.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    descriptorWrite.dstSet = descriptor;
    descriptorWrite.dstBinding = 0;
    descriptorWrite.dstArrayElement = 0;
    descriptorWrite.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    descriptorWrite.descriptorCount = 1;
    descriptorWrite.pBufferInfo = &bufferInfo;

    vkUpdateDescriptorSets(device, 1, &descriptorWrite, 0, nullptr);
    return descriptor;
}
VkCommandBuffer createCommandBuffer(VkDevice device, VkCommandPool commandPool) {
    VkCommandBuffer commandBuffer;
    VkCommandBufferAllocateInfo allocInfo{};
    allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    allocInfo.commandPool = commandPool;
    allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    allocInfo.commandBufferCount = (uint32_t)1;
    if (vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer) != VK_SUCCESS)
        throw std::runtime_error("failed to allocate command buffers!");
    return commandBuffer;
}
VkSemaphore createSemaphore(VkDevice device) {
    VkSemaphore semaphore;
    VkSemaphoreCreateInfo semaphoreInfo{};
    semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    if (vkCreateSemaphore(device, &semaphoreInfo, nullptr, &semaphore) != VK_SUCCESS) {
        throw std::runtime_error("failed to create semaphore!");
    }
    return semaphore;
}
VkFence createFence(VkDevice device) {
    VkFence fence;
    VkFenceCreateInfo fenceInfo{};
    fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;
    if (vkCreateFence(device, &fenceInfo, nullptr, &fence) != VK_SUCCESS) {
        throw std::runtime_error("failed to create fence!");
    }
    return fence;
}

struct InitState {
    VkInstance instance;
    bool valid;
    VkDebugUtilsMessengerEXT debug;
    InitState(bool enable, const std::vector<const char*> layers) {
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
                        if (strcmp(name, properties.layerName) == 0) {
                            found = true;
                            break;}}
                    if (!found) return false;}
                return true;
            } (layers))
                throw std::runtime_error("validation layers requested, but not available!");
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
            if (vkCreateInstance(&info, nullptr, &instance) != VK_SUCCESS)
                throw std::runtime_error("failed to create instance!");
            return instance;
        } (enable,info,layers);
        if (valid = enable) debug = [](VkInstance instance, VkDebugUtilsMessengerCreateInfoEXT info) {
            VkDebugUtilsMessengerEXT debug;
            if (CreateDebugUtilsMessengerEXT(instance, &info, nullptr, &debug) != VK_SUCCESS)
                throw std::runtime_error("failed to set up debug messenger!");
            return debug;
        } (instance,info);
    }
    ~InitState() {
        if (valid) DestroyDebugUtilsMessengerEXT(instance, debug, nullptr);
        valid = false;
        vkDestroyInstance(instance, nullptr);
        glfwTerminate();
    }
};

struct MainState;
void framebufferResized(GLFWwindow* window, int width, int height);
void keyPressed(GLFWwindow* window, int key, int scancode, int action, int mods);
void mouseClicked(GLFWwindow* window, int button, int action, int mods);
void mouseMoved(GLFWwindow* window, double xpos, double ypos);
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
    OpenState(VkInstance instance, uint32_t WIDTH, uint32_t HEIGHT, MainState *mainState) {
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
        VkSurfaceCapabilitiesKHR surfaceCapabilities;
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical, surface, &surfaceCapabilities);
        minimum = surfaceCapabilities.minImageCount + 1;
        if (surfaceCapabilities.maxImageCount > 0 && minimum > surfaceCapabilities.maxImageCount)
            minimum = surfaceCapabilities.maxImageCount;
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
    VkQueue graphic;
    VkQueue present;
    VkRenderPass render;
    VkDescriptorSetLayout descriptor;
    VkPipelineLayout layout;
    VkPipeline pipeline;
    VkCommandPool command;
    VkDescriptorPool pool;
    DeviceState(VkPhysicalDevice physical, uint32_t graphicid, uint32_t presentid, VkFormat image,
        std::vector<const char*> layers, std::vector<const char*> extensions, bool enable, int MAX_BUFFERS_AVAILABLE) {
        device = [](VkPhysicalDevice physical, uint32_t graphicid, uint32_t presentid,
            const std::vector<const char*> layers, const std::vector<const char*> extensions, bool enable) {
            VkDevice device;
            std::vector<VkDeviceQueueCreateInfo> infos;
            float priority = 1.0f;
            {
                VkDeviceQueueCreateInfo info{};
                info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
                info.queueFamilyIndex = graphicid;
                info.queueCount = 1;
                info.pQueuePriorities = &priority;
                infos.push_back(info);}
            if (presentid != graphicid) {
                VkDeviceQueueCreateInfo info{};
                info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
                info.queueFamilyIndex = presentid;
                info.queueCount = 1;
                info.pQueuePriorities = &priority;
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
            else
                info.enabledLayerCount = 0;
            if (vkCreateDevice(physical, &info, nullptr, &device) != VK_SUCCESS)
                throw std::runtime_error("failed to create logical device!");
            return device;
        } (physical,graphicid,presentid,layers,extensions,enable);
        vkGetDeviceQueue(device, graphicid, 0, &graphic);
        vkGetDeviceQueue(device, presentid, 0, &present);
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
        descriptor = [](VkDevice device) {
            VkDescriptorSetLayoutBinding binding{};
            binding.binding = 0;
            binding.descriptorCount = 1;
            binding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
            binding.pImmutableSamplers = nullptr;
            binding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
            VkDescriptorSetLayoutCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
            info.bindingCount = 1;
            info.pBindings = &binding;
            VkDescriptorSetLayout descriptor;
            if (vkCreateDescriptorSetLayout(device, &info, nullptr, &descriptor) != VK_SUCCESS)
                throw std::runtime_error("failed to create descriptor set layout!");
            return descriptor;
        } (device);
        layout = [](VkDevice device, VkDescriptorSetLayout descriptor) {
            VkPipelineLayoutCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
            info.setLayoutCount = 1;
            info.pSetLayouts = &descriptor;
            VkPipelineLayout layout;
            if (vkCreatePipelineLayout(device, &info, nullptr, &layout) != VK_SUCCESS)
                throw std::runtime_error("failed to create pipeline layout!");
            return layout;
        } (device,descriptor);
        pipeline = [](VkDevice device, VkRenderPass render, VkPipelineLayout layout, const char *vertex, const char *fragment) {
            auto vcode = readFile(vertex);
            auto fcode = readFile(fragment);
            VkShaderModule vmodule = createShaderModule(device,vcode);
            VkShaderModule fmodule = createShaderModule(device,fcode);
            VkPipelineShaderStageCreateInfo vinfo{};
            vinfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
            vinfo.stage = VK_SHADER_STAGE_VERTEX_BIT;
            vinfo.module = vmodule;
            vinfo.pName = "main";
            VkPipelineShaderStageCreateInfo finfo{};
            finfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
            finfo.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
            finfo.module = fmodule;
            finfo.pName = "main";
            VkPipelineShaderStageCreateInfo stages[] = {vinfo, finfo};
            VkPipelineVertexInputStateCreateInfo input{};
            input.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
            auto bindingDescription = Input::getBindingDescription();
            auto attributeDescriptions = Input::getAttributeDescriptions();
            input.vertexBindingDescriptionCount = 1;
            input.vertexAttributeDescriptionCount = static_cast<uint32_t>(attributeDescriptions.size());
            input.pVertexBindingDescriptions = &bindingDescription;
            input.pVertexAttributeDescriptions = attributeDescriptions.data();
            VkPipelineInputAssemblyStateCreateInfo assembly{};
            assembly.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
            assembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
            assembly.primitiveRestartEnable = VK_FALSE;
            VkPipelineViewportStateCreateInfo viewport{};
            viewport.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
            viewport.viewportCount = 1;
            viewport.scissorCount = 1;
            VkPipelineRasterizationStateCreateInfo rasterize{};
            rasterize.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
            rasterize.depthClampEnable = VK_FALSE;
            rasterize.rasterizerDiscardEnable = VK_FALSE;
            rasterize.polygonMode = VK_POLYGON_MODE_FILL;
            rasterize.lineWidth = 1.0f;
            rasterize.cullMode = VK_CULL_MODE_BACK_BIT;
            rasterize.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
            rasterize.depthBiasEnable = VK_FALSE;
            VkPipelineMultisampleStateCreateInfo multisample{};
            multisample.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
            multisample.sampleShadingEnable = VK_FALSE;
            multisample.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
            VkPipelineColorBlendAttachmentState attachment{};
            attachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
                VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
            attachment.blendEnable = VK_FALSE;
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
            std::vector<VkDynamicState> states = {
                VK_DYNAMIC_STATE_VIEWPORT,
                VK_DYNAMIC_STATE_SCISSOR
            };
            VkPipelineDynamicStateCreateInfo state{};
            state.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
            state.dynamicStateCount = static_cast<uint32_t>(states.size());
            state.pDynamicStates = states.data();
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
            VkPipeline pipeline;
            if (vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &info, nullptr, &pipeline) != VK_SUCCESS)
                throw std::runtime_error("failed to create graphic pipeline!");
            vkDestroyShaderModule(device, fmodule, nullptr);
            vkDestroyShaderModule(device, vmodule, nullptr);
            return pipeline;
        } (device,render,layout,"vulkan.vsv","vulkan.fsv");
        command = [](VkDevice device, uint32_t graphicid) {
            VkCommandPoolCreateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
            info.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
            info.queueFamilyIndex = graphicid;
            VkCommandPool command;
            if (vkCreateCommandPool(device, &info, nullptr, &command) != VK_SUCCESS)
                throw std::runtime_error("failed to create graphic command pool!");
            return command;
        } (device, graphicid);
        pool = [](VkDevice device, int uniforms, int stores) {
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
        vkDestroyDescriptorPool(device, pool, nullptr);
        vkDestroyCommandPool(device, command, nullptr);
        vkDestroyPipeline(device, pipeline, nullptr);
        vkDestroyPipelineLayout(device, layout, nullptr);
        vkDestroyDescriptorSetLayout(device, descriptor, nullptr);
        vkDestroyRenderPass(device, render, nullptr);
        vkDestroyDevice(device, nullptr);
    }
};

struct ThreadState {
    VkDevice device;
    sem_t protect;
    sem_t semaphore;
    pthread_t thread;
    bool finish;
    std::queue<std::function<VkFence()>> setup;
    std::queue<VkFence> fence;
    std::queue<int> order;
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
                arg->fence.push(arg->setup.front()());
                arg->setup.pop();}
            if (arg->fence.empty()) {
                if (sem_post(&arg->protect) != 0) throw std::runtime_error("cannot post to protect!");
                if (sem_wait(&arg->semaphore) != 0) throw std::runtime_error("cannot wait for semaphore!");
                if (sem_wait(&arg->protect) != 0) throw std::runtime_error("cannot wait for protect!");}
            else {
                if (sem_post(&arg->protect) != 0) throw std::runtime_error("cannot post to protect!");
                VkResult result = vkWaitForFences(arg->device,1,&arg->fence.front(),VK_FALSE,NANOSECONDS);
                glfwPostEmptyEvent();
                if (sem_wait(&arg->protect) != 0) throw std::runtime_error("cannot wait for protect!");
                if (result != VK_SUCCESS && result != VK_TIMEOUT) throw std::runtime_error("cannot wait for fence!");
                if (result == VK_SUCCESS) {arg->lookup.erase(arg->order.front()); arg->order.pop(); arg->fence.pop();}}}
        return 0;
    }
    bool clear(int given) {
        if (sem_wait(&protect) != 0) throw std::runtime_error("cannot wait for protect!");
        bool done = (lookup.find(given) == lookup.end());
        if (sem_post(&protect) != 0) throw std::runtime_error("cannot post to protect!");
        return done;
    }
    std::function<bool()> push(std::function<void()> given) {
        if (sem_wait(&protect) != 0) throw std::runtime_error("cannot wait for protect!");
        if (fence.empty() && sem_post(&semaphore) != 0) throw std::runtime_error("cannot post to semaphore!");
        given();
        int local = seqnum++;
        order.push(local);
        lookup.insert(local);
        if (fence.size()+setup.size() != order.size()) throw std::runtime_error("cannot push seqnum!");
        if (order.size() != lookup.size()) throw std::runtime_error("cannot insert seqnum!");
        std::function<bool()> done = [this,local](){return this->clear(local);};
        if (sem_post(&protect) != 0) throw std::runtime_error("cannot post to protect!");
        return done;        
    }
    std::function<bool()> push(VkFence given) {
        return push([this,given](){this->fence.push(given);});
    }
    std::function<bool()> push(std::function<VkFence()> given) {
        return push([this,given](){this->setup.push(given);});
    }
};

template<class Buffer> struct BufferQueue {
    std::queue<Buffer*> pool;
    std::queue<Buffer*> running; std::queue<std::function<bool()>> toready;
    Buffer* ready; std::queue<std::function<bool()>> toinuse;
    std::queue<Buffer*> inuse; std::queue<std::function<bool()>> topool;
    int count; int limit;
    BufferQueue(int limit) {
        ready = 0;
        count = 0;
        this->limit = limit;
    }
    ~BufferQueue() {
        while (!pool.empty()) {delete pool.front(); pool.pop();}
        while (!running.empty()) {delete running.front(); running.pop();}
        if (ready) delete ready;
        while (!inuse.empty()) {delete inuse.front(); inuse.pop();}
    }
    void clr() {
        while (!pool.empty()) {delete pool.front(); pool.pop();}
    }
    bool tst() {
        while (!topool.empty() && topool.front()() && !inuse.front()) {
            inuse.pop(); topool.pop();}
        if (!topool.empty() && topool.front()()) return true;
        if (count < limit) return true;
        return false;
    }
    Buffer *set(std::function<Buffer*()> make, std::function<void(Buffer*)> setup, std::function<std::function<bool()>(Buffer*)> done) {
        while (!topool.empty() && topool.front()()) {
            if (inuse.front()) pool.push(inuse.front());
            inuse.pop(); topool.pop();}
        if (pool.empty()) {pool.push(make()); count++;}
        Buffer *ptr = pool.front(); pool.pop();
        setup(ptr); running.push(ptr); toready.push(done(ptr));
        return ptr;
    }
    bool vld() {
        if (ready) return true;
        if (running.empty()) return false;
        if (toready.front()()) return true;
        return false;
    }
    Buffer *get(std::function<bool()> done) {
        if (ready && !toinuse.empty() && !toready.empty() && toready.front()()) {
            while (toinuse.size() > 1) {inuse.push(0); topool.push(toinuse.front()); toinuse.pop();}
            inuse.push(ready); topool.push(toinuse.front()); toinuse.pop(); ready = 0;}
        while (!toready.empty() && toready.front()()) {
            if (ready) pool.push(ready);
            ready = running.front(); running.pop(); toready.pop();}
        toinuse.push(done);
        return ready;
    }
};

// if (loc+siz > fetchSize) fetchQueue->clr();
// fetchQueue->set([...](){return new BufferState(...);},[loc,siz,ptr](BufferState*fetch, VkFence fence){fetch->setup(loc,siz,ptr,fence);});
// fetchQueue->get().buffer;
enum BufferTag {FetchBuf,ChangeBuf,StoreBuf,TestBuf};
struct BufferState {
    VkDevice device;
    VkQueue graphic;
    VkCommandPool pool;
    VkDeviceSize size;
    VkBuffer staging;
    VkDeviceMemory wasted;
    VkBuffer buffer;
    VkDeviceMemory memory;
    void *mapped;
    VkCommandBuffer command;
    VkDescriptorSet descriptor;
    VkFence fence;
    BufferTag tag;
    BufferState(VkPhysicalDevice physical, VkDevice device, VkQueue graphic, VkCommandPool pool,
        int size, BufferTag tag,
        VkDescriptorSetLayout layout = VK_NULL_HANDLE, VkDescriptorPool dpool = VK_NULL_HANDLE) {
        this->device = device;
        this->graphic = graphic;
        this->pool = pool;
        this->size = size;
        this->tag = tag;
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
        wasted = [](VkPhysicalDevice physical, VkDevice device, VkBuffer buffer, VkMemoryPropertyFlags properties) {
            VkMemoryRequirements requirements;
            vkGetBufferMemoryRequirements(device, buffer, &requirements);
            VkMemoryAllocateInfo info{};
            info.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
            info.allocationSize = requirements.size;
            info.memoryTypeIndex = findMemoryType(physical, requirements.memoryTypeBits, properties);
            VkDeviceMemory memory;
            if (vkAllocateMemory(device, &info, nullptr, &memory) != VK_SUCCESS)
                throw std::runtime_error("failed to allocate buffer memory!");
            return memory;
        } (physical,device,staging,VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
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
            VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT:
            VK_BUFFER_USAGE_TRANSFER_DST_BIT|VK_BUFFER_USAGE_VERTEX_BUFFER_BIT);
        memory = [](VkPhysicalDevice physical, VkDevice device, VkBuffer buffer, VkMemoryPropertyFlags properties) {
            VkMemoryRequirements requirements;
            vkGetBufferMemoryRequirements(device, buffer, &requirements);
            VkMemoryAllocateInfo allocInfo{};
            allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
            allocInfo.allocationSize = requirements.size;
            allocInfo.memoryTypeIndex = findMemoryType(physical, requirements.memoryTypeBits, properties);
            VkDeviceMemory memory;
            if (vkAllocateMemory(device, &allocInfo, nullptr, &memory) != VK_SUCCESS)
                throw std::runtime_error("failed to allocate buffer memory!");
            return memory;
        } (physical,device,buffer,tag==ChangeBuf?
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT|VK_MEMORY_PROPERTY_HOST_COHERENT_BIT:
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
        vkBindBufferMemory(device, buffer, memory, 0);
        vkMapMemory(device, tag==ChangeBuf?memory:wasted, 0, size, 0, &mapped);}
        command = createCommandBuffer(device,pool);
        if (tag == ChangeBuf || tag == StoreBuf) {
        descriptor = createDescriptorSet(device,buffer,layout,dpool);}
        fence = createFence(device);
    }
    ~BufferState() {
        vkDestroyFence(device,fence,0);
        vkFreeCommandBuffers(device, pool, 1, &command);
        if (tag == FetchBuf || tag == ChangeBuf || tag == StoreBuf) {
        vkDestroyBuffer(device, buffer, nullptr);
        vkFreeMemory(device, memory, nullptr);}
        if (tag == FetchBuf || tag == StoreBuf) {
        vkDestroyBuffer(device, staging, nullptr);
        vkFreeMemory(device, wasted, nullptr);}
    }
    VkResult setup(int loc, int siz, const void *ptr) {
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
        return result;
    }
    void test(VkExtent2D swapChainExtent) {
        static auto startTime = std::chrono::high_resolution_clock::now();
        auto currentTime = std::chrono::high_resolution_clock::now();
        float time = std::chrono::duration<float, std::chrono::seconds::period>(currentTime - startTime).count();
        UniformBufferObject ubo{};
        ubo.model = glm::rotate(glm::mat4(1.0f), time * glm::radians(90.0f), glm::vec3(0.0f, 0.0f, 1.0f));
        ubo.view = glm::lookAt(glm::vec3(2.0f, 2.0f, 2.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f));
        ubo.proj = glm::perspective(glm::radians(45.0f), swapChainExtent.width / (float) swapChainExtent.height, 0.1f, 10.0f);
        ubo.proj[1][1] *= -1;
        memcpy(mapped, &ubo, sizeof(ubo));
    }
};

struct DrawState {
    VkDevice device;
    VkRenderPass render;
    VkPipeline pipeline;
    VkPipelineLayout layout;
    VkQueue graphic;
    VkQueue present;
    VkSemaphore imageAvailableSemaphore;
    VkSemaphore renderFinishedSemaphore;
    VkCommandBuffer commandBuffer;
    VkFence fence;
    DrawState(VkDevice device, VkCommandPool commandPool, VkRenderPass render,
        VkPipeline pipeline, VkPipelineLayout layout, VkQueue graphic, VkQueue present) {
        this->device = device;
        this->render = render;
        this->pipeline = pipeline;
        this->layout = layout;
        this->graphic = graphic;
        this->present = present;
        imageAvailableSemaphore = createSemaphore(device);
        renderFinishedSemaphore = createSemaphore(device);
        commandBuffer = createCommandBuffer(device,commandPool);
        fence = createFence(device);
    }
    ~DrawState() {
        vkDestroyFence(device,fence,0);
        vkDestroySemaphore(device, renderFinishedSemaphore, nullptr);
        vkDestroySemaphore(device, imageAvailableSemaphore, nullptr);
    }
    VkResult setup(VkExtent2D swapChainExtent, VkSwapchainKHR swapChain, std::vector<VkFramebuffer> &swapChainFramebuffers,
        VkDescriptorSet descriptor, VkBuffer vertexBuffer, uint32_t size) {
        VkResult result;
        uint32_t imageIndex;
        result = vkAcquireNextImageKHR(device, swapChain, UINT64_MAX, imageAvailableSemaphore, VK_NULL_HANDLE, &imageIndex);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) {
            return result;
        } else if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) {
            throw std::runtime_error("failed to acquire swap chain image!");
        }

        vkResetFences(device, 1, &fence);
        vkResetCommandBuffer(commandBuffer, /*VkCommandBufferResetFlagBits*/ 0);

        VkCommandBufferBeginInfo beginInfo{};
        beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        if (vkBeginCommandBuffer(commandBuffer, &beginInfo) != VK_SUCCESS) {
            throw std::runtime_error("failed to begin recording command buffer!");
        }
        [](VkRenderPass render, VkFramebuffer framebuffer,
            VkExtent2D swapChainExtent, VkCommandBuffer commandBuffer) {
            VkRenderPassBeginInfo info{};
            info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
            info.renderPass = render;
            info.framebuffer = framebuffer;
            info.renderArea.offset = {0, 0};
            info.renderArea.extent = swapChainExtent;
            VkClearValue clearColor = {{{0.0f, 0.0f, 0.0f, 1.0f}}};
            info.clearValueCount = 1;
            info.pClearValues = &clearColor;
            vkCmdBeginRenderPass(commandBuffer, &info, VK_SUBPASS_CONTENTS_INLINE);
        } (render,swapChainFramebuffers[imageIndex],swapChainExtent,commandBuffer);

        vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline);

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

        VkBuffer vertexBuffers[] = {vertexBuffer};
        VkDeviceSize offsets[] = {0};
        vkCmdBindVertexBuffers(commandBuffer, 0, 1, vertexBuffers, offsets);
        vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, layout, 0, 1, &descriptor, 0, nullptr);

        vkCmdDraw(commandBuffer, size, 1, 0, 0);
        vkCmdEndRenderPass(commandBuffer);
        if (vkEndCommandBuffer(commandBuffer) != VK_SUCCESS) {
            throw std::runtime_error("failed to record command buffer!");
        }

        VkSubmitInfo submitInfo{};
        submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
        VkSemaphore waitSemaphores[] = {imageAvailableSemaphore};
        VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
        submitInfo.waitSemaphoreCount = 1;
        submitInfo.pWaitSemaphores = waitSemaphores;
        submitInfo.pWaitDstStageMask = waitStages;
        submitInfo.commandBufferCount = 1;
        submitInfo.pCommandBuffers = &commandBuffer;
        VkSemaphore signalSemaphores[] = {renderFinishedSemaphore};
        submitInfo.signalSemaphoreCount = 1;
        submitInfo.pSignalSemaphores = signalSemaphores;
        if (vkQueueSubmit(graphic, 1, &submitInfo, fence) != VK_SUCCESS) {
            throw std::runtime_error("failed to submit draw command buffer!");
        }

        VkPresentInfoKHR presentInfo{};
        presentInfo.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
        presentInfo.waitSemaphoreCount = 1;
        presentInfo.pWaitSemaphores = signalSemaphores;
        VkSwapchainKHR swapChains[] = {swapChain};
        presentInfo.swapchainCount = 1;
        presentInfo.pSwapchains = swapChains;
        presentInfo.pImageIndices = &imageIndex;
        result = vkQueuePresentKHR(present, &presentInfo);
        if (result != VK_SUCCESS && result != VK_ERROR_OUT_OF_DATE_KHR)
            throw std::runtime_error("device lost on wait for fence!");
        return result;
    }
};

struct SwapState {
    VkDevice device;
    VkExtent2D extent;
    VkSwapchainKHR swapChain;
    uint32_t count;
    std::vector<VkImage> swapChainImages;
    std::vector<VkImageView> swapChainImageViews;
    std::vector<VkFramebuffer> swapChainFramebuffers;
    SwapState(GLFWwindow* window, VkPhysicalDevice physical, VkDevice device, VkSurfaceKHR surface,
        VkFormat image, VkSurfaceFormatKHR format, VkPresentModeKHR mode, VkRenderPass pass,
        uint32_t minimum, uint32_t graphicid, uint32_t presentid) {
        this->device = device;
        int width = 0, height = 0;
        glfwGetFramebufferSize(window, &width, &height);
        while (width == 0 || height == 0) {
            glfwWaitEvents();
            glfwGetFramebufferSize(window, &width, &height);
        }
        VkSurfaceCapabilitiesKHR capabilities;
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR(physical, surface, &capabilities);
        extent = chooseSwapExtent(window,capabilities);
        swapChain = createSwapChain(physical,device,surface,format,mode, extent,capabilities,minimum,graphicid,presentid);
        vkGetSwapchainImagesKHR(device, swapChain, &count, nullptr);
        swapChainImages.resize(count);
        swapChainImageViews.resize(count);
        swapChainFramebuffers.resize(count);
        vkGetSwapchainImagesKHR(device, swapChain, &count, swapChainImages.data());
        for (int i = 0; i < count; i++) swapChainImageViews[i] = createImageView(device,image,swapChainImages[i]);
        for (int i = 0; i < count; i++) swapChainFramebuffers[i] = createFramebuffer(device,extent,swapChainImageViews[i],pass);
    }
    ~SwapState() {
        vkDeviceWaitIdle(device);
        for (int i = 0; i < count; i++) vkDestroyFramebuffer(device, swapChainFramebuffers[i], nullptr);
        for (int i = 0; i < count; i++) vkDestroyImageView(device, swapChainImageViews[i], nullptr);
        vkDestroySwapchainKHR(device, swapChain, nullptr);
    }
};

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
    struct Center *center;
    struct InitState *initState;
    struct OpenState* openState;
    struct PhysicalState* physicalState;
    struct DeviceState* logicalState;
    const uint32_t WIDTH = 800;
    const uint32_t HEIGHT = 600;
    const int MAX_FRAMES_IN_FLIGHT = 2;
    const int MAX_BUFFERS_AVAILABLE = 7; // TODO collective limit for BufferQueue
    const std::vector<const char*> deviceExtensions = {
        VK_KHR_SWAPCHAIN_EXTENSION_NAME
    };
    const std::vector<const char*> validationLayers = {
        "VK_LAYER_KHRONOS_validation"
    };
    #ifdef NDEBUG
    const bool enableValidationLayers = false;
    #else
    const bool enableValidationLayers = true;
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
    .center = 0,
    .initState = 0,
    .openState = 0,
    .physicalState = 0,
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

void vulkanInit() {
    for (int arg = 0; arg < mainState.argc; arg++) planeAddarg(mainState.argv[arg]);
    mainState.initState = new InitState(mainState.enableValidationLayers,mainState.validationLayers);
}
void vulkanDma(struct Center *center) {
    // TODO call set in BufferQueue from lookup of Memory
}
void vulkanSafe() {
    glfwPostEmptyEvent();
}
void vulkanMain(enum Proc proc, enum Wait wait) {
    // TODO trust plane.c to call this
}
int vulkanInfo(enum Configure query) {
    return 0; // TODO
}
void vulkanDraw(enum Micro shader, int base, int limit) {
    // TODO mainState.callDraw
}

int main(int argc, char **argv) {
    mainState.argc = argc;
    mainState.argv = argv;
    try {
        planeInit(vulkanInit,vulkanDma,vulkanSafe,vulkanMain,vulkanInfo,vulkanDraw);
        VkInstance instance = mainState.initState->instance;
        uint32_t WIDTH = mainState.WIDTH;
        uint32_t HEIGHT = mainState.HEIGHT;
        int MAX_FRAMES_IN_FLIGHT = mainState.MAX_FRAMES_IN_FLIGHT;
        int MAX_BUFFERS_AVAILABLE = mainState.MAX_BUFFERS_AVAILABLE;
        const std::vector<const char*> deviceExtensions = mainState.deviceExtensions;
        std::vector<const char*> validationLayers = mainState.validationLayers;
        bool enableValidationLayers = mainState.enableValidationLayers;

        // TODO move following to vulkanMain
        mainState.openState = new OpenState(instance,WIDTH,HEIGHT,&mainState);
        GLFWwindow* window = mainState.openState->window;
        VkSurfaceKHR surface = mainState.openState->surface;

        mainState.physicalState = new PhysicalState(instance,surface,deviceExtensions);
        VkPhysicalDevice physicalDevice = mainState.physicalState->physical;
        uint32_t graphicIndex = mainState.physicalState->graphicid;
        uint32_t presentIndex = mainState.physicalState->presentid;
        uint32_t minImageCount = mainState.physicalState->minimum;
        VkSurfaceFormatKHR surfaceFormat = mainState.physicalState->format;
        VkPresentModeKHR presentMode = mainState.physicalState->mode;
        VkFormat swapChainImageFormat = mainState.physicalState->image;

        mainState.logicalState = new DeviceState(physicalDevice,graphicIndex,presentIndex,swapChainImageFormat,
            validationLayers,deviceExtensions,enableValidationLayers,MAX_BUFFERS_AVAILABLE*Memorys);
        VkDevice device = mainState.logicalState->device;
        VkQueue graphicQueue = mainState.logicalState->graphic;
        VkQueue presentQueue = mainState.logicalState->present;
        VkRenderPass renderPass = mainState.logicalState->render;
        VkDescriptorSetLayout descriptorSetLayout = mainState.logicalState->descriptor;
        VkPipelineLayout pipelineLayout = mainState.logicalState->layout;
        VkPipeline graphicPipeline = mainState.logicalState->pipeline;
        VkCommandPool commandPool = mainState.logicalState->command;
        VkDescriptorPool descriptorPool = mainState.logicalState->pool;

        struct ThreadState *threadState = new ThreadState(device);
        struct ThreadState *fenceState = new ThreadState(device);

        BufferQueue<BufferState> *fetchQueue = new BufferQueue<BufferState>(MAX_BUFFERS_AVAILABLE);
        BufferQueue<BufferState> *changeQueue = new BufferQueue<BufferState>(MAX_BUFFERS_AVAILABLE);
        BufferQueue<DrawState> *drawQueue = new BufferQueue<DrawState>(MAX_FRAMES_IN_FLIGHT);

        std::vector<DrawState*> drawState(MAX_FRAMES_IN_FLIGHT);
        for (int i = 0; i < drawState.size(); i++)
            drawState[i] = new DrawState(device,commandPool,renderPass,graphicPipeline,pipelineLayout,graphicQueue,presentQueue);
        std::vector<VkSemaphore> imageAvailableSemaphores(MAX_FRAMES_IN_FLIGHT);
        for (int i = 0; i < imageAvailableSemaphores.size(); i++)
            imageAvailableSemaphores[i] = drawState[i]->imageAvailableSemaphore;
        std::vector<VkSemaphore> renderFinishedSemaphores(MAX_FRAMES_IN_FLIGHT);
        for (int i = 0; i < renderFinishedSemaphores.size(); i++)
            renderFinishedSemaphores[i] = drawState[i]->renderFinishedSemaphore;
        std::vector<VkFence> fences(MAX_FRAMES_IN_FLIGHT);
        for (int i = 0; i < fences.size(); i++)
            fences[i] = drawState[i]->fence;
        std::vector<VkCommandBuffer> commandBuffers(MAX_FRAMES_IN_FLIGHT);
        for (int i = 0; i < commandBuffers.size(); i++)
            commandBuffers[i] = drawState[i]->commandBuffer;

        struct SwapState *swapState = 0;
        VkExtent2D swapChainExtent;
        VkSwapchainKHR swapChain;
        std::vector<VkFramebuffer> swapChainFramebuffers;
        uint32_t currentFrame = 0; // TODO change to drawQueue
        while (!mainState.escapePressed || !mainState.enterPressed) {
            glfwWaitEventsTimeout(0.01);
            if (mainState.framebufferResized) {
                mainState.framebufferResized = false;
                if (swapState) delete swapState;
                swapState = 0;}
            if (!swapState) {
                swapState = new SwapState(window,physicalDevice,device,surface,
                    swapChainImageFormat,surfaceFormat,presentMode,renderPass,
                    minImageCount,graphicIndex,presentIndex);
                swapChainExtent = swapState->extent;
                swapChain = swapState->swapChain;
                swapChainFramebuffers.resize(swapState->count);
                for (int i = 0; i < swapState->count; i++) swapChainFramebuffers[i] = swapState->swapChainFramebuffers[i];
            }
            if (mainState.callOnce) {
                if (!fetchQueue->tst()) continue;
                mainState.callOnce = false;
                fetchQueue->set([physicalDevice, device, graphicQueue, commandPool](){
                    return new BufferState(physicalDevice, device, graphicQueue, commandPool,
                    sizeof(vertices[0]) * vertices.size(), FetchBuf);},
                    [](BufferState*buffer){buffer->setup(0,sizeof(vertices[0])*vertices.size(),vertices.data());},
                    [threadState](BufferState*buffer){return threadState->push(buffer->fence);});
            }
            if (mainState.callDma) {
                if (!changeQueue->tst()) continue;
                mainState.callDma = false;
                changeQueue->set([physicalDevice,device,graphicQueue,commandPool,
                    descriptorSetLayout,descriptorPool,swapChainExtent](){
                    return new BufferState(physicalDevice,device,graphicQueue,commandPool,
                    sizeof(UniformBufferObject),ChangeBuf,descriptorSetLayout,descriptorPool);},
                    [swapChainExtent](BufferState*buffer){buffer->test(swapChainExtent);},
                    [threadState](BufferState*buffer){return threadState->push(buffer->fence);});
            }
            if (mainState.callDraw) {
                if (!fetchQueue->vld()) continue;
                if (!changeQueue->vld()) continue;
                VkFence fence = fences[currentFrame];
                std::function<bool()> done = [device,fence](){
                    VkResult result = vkWaitForFences(device, 1, &fence, VK_TRUE, NANOSECONDS);
                    if (result != VK_SUCCESS && result != VK_TIMEOUT)
                        throw std::runtime_error("failed to wait for fences!");
                    return (result == VK_SUCCESS);};
                if (!done()) continue;
                if (drawState[currentFrame]->setup(swapChainExtent,swapChain,swapChainFramebuffers,
                    changeQueue->get(done)->descriptor,fetchQueue->get([](){return false;})->buffer,
                    static_cast<uint32_t>(vertices.size())) == VK_ERROR_OUT_OF_DATE_KHR) {
                    mainState.framebufferResized = true;}
                currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT;
                mainState.callDma = true;
            }
        }

        vkDeviceWaitIdle(device);
        delete swapState;
        for (size_t i = 0; i < drawState.size(); i++) delete drawState[i];
        delete fenceState;
        delete threadState;
        delete changeQueue;
        delete fetchQueue;
        delete mainState.logicalState;
        delete mainState.physicalState;
        delete mainState.openState;
        delete mainState.initState;
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
