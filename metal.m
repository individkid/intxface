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

#include "plane.h"
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>
#import <Foundation/Foundation.h>

id<MTLDevice> device = 0;
MTKView *view = 0;

void metalErr(const char *str, int num, int arg)
{
}

void metalPos(int *xloc, int *yloc)
{
	// TODO get window position
}

void metalSize(int *width, int *height)
{
	// TODO get window size
}

void metalCall()
{
	// TODO call appDelegate run
	cb.done();
	// TODO release pointers
}

int metalInit(int argc, char **argv)
{
	return 0;
	cb.err = metalErr;
	cb.pos = metalPos;
	cb.size = metalSize;
	cb.call = metalCall;
	int width, height;
	cb.size(&width,&height);
	device = MTLCreateSystemDefaultDevice();
	view = [[MTKView alloc] initWithFrame: CGRectMake(0,0,width,height) device:device];
}
