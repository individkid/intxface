/*
*    metal.m
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define GLFW_INCLUDE_NONE
#define GLFW_EXPOSE_NATIVE_COCOA
#include <GLFW/glfw3.h>
#include <GLFW/glfw3native.h>
#import <Metal/Metal.h>
#import <QuartzCore/QuartzCore.h>
#include "plane.h"

extern struct GLFWwindow* window;
id<MTLDevice> device = 0;
id<MTLCommandQueue> cmdque = 0;
NSWindow *nswin = 0;
CAMetalLayer *layer = 0;
MTLRenderPassDescriptor *render = 0;

void metalDma()
{
	switch (cb.client->mem) {
	case (Corner): /*TODO*/ break;
	case (Triangle): /*TODO*/ break;
	case (Range): ERROR(cb.err,-1);
	case (Basis): /*TODO*/ break;
	case (Subject): /*TODO*/ break;
	case (Object): /*TODO*/ break;
	case (Feature): /*TODO*/ break;
	case (Feather): /*TODO*/ break;
	case (Arrow): /*TODO*/ break;
	case (Cloud): /*TODO*/ break;
	case (Hand): /*TODO*/ break;
	case (Tag): /*TODO*/ break;
	case (Face): ERROR(cb.err,-1);
	case (User): ERROR(cb.err,-1);
	default: ERROR(exiterr,-1);}
}

void metalGet()
{
	/*TODO*/
}

void metalFunc()
{
	/*TODO*/
}

int metalFull()
{
	if (cb.client)
	for (int i = 0; i < cb.client->len; i++)
	switch (cb.client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Rmw2): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): /*TODO*/ break;
	case (Port): break;
	default: ERROR(cb.err,-1);}
	return 0;
}

void metalDraw()
{
	for (int i = 0; i < cb.client->len; i++)
	switch (cb.client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Rmw2): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): metalDma(); break;
	case (Dma1): metalGet(); break;
	case (Draw): metalFunc(); break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void metalDone()
{
	[layer release];
	[cmdque release];
	[device release];
}

int metalInit()
{
	return 0;

	cb.full = metalFull;
	cb.draw = metalDraw;
	cb.done = metalDone;

	device = MTLCreateSystemDefaultDevice();;
	cmdque = [device newCommandQueue];
	nswin = glfwGetCocoaWindow(window);
	layer = [CAMetalLayer layer];
	layer.device = device;
	layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
	nswin.contentView.layer = layer;
	nswin.contentView.wantsLayer = YES;
	render = [MTLRenderPassDescriptor new];

	int width, height;
	glfwGetFramebufferSize(window, &width, &height);
	layer.drawableSize = CGSizeMake(width, height);

	id<CAMetalDrawable> drawable = [layer nextDrawable];
	id<MTLCommandBuffer> commandBuffer = [cmdque commandBuffer];
	render.colorAttachments[0].clearColor = MTLClearColorMake(1.00f,1.00f,1.00f,1.00f);
	render.colorAttachments[0].texture = drawable.texture;
	render.colorAttachments[0].loadAction = MTLLoadActionClear;
	render.colorAttachments[0].storeAction = MTLStoreActionDontCare;
	id <MTLRenderCommandEncoder> renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:render];
	[renderEncoder endEncoding];
	[commandBuffer presentDrawable:drawable];
	[commandBuffer commit];

	return 1;
}
