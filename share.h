/*
*    share.h
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

#include "face.h"
#include "base.h"
#include "type.h"
#include "metic.h"

struct Callback {
	// share.c : plane.sw
	void (*err)(const char *str, int num, int arg);
	void (*pos)(int *xloc, int *yloc);
	void (*size)(int *width, int *height);
	// plane.sw : share.c
	void (*key)(int key);
	void (*move)(double xpos, double ypos);
	void (*roll)(double xoffset, double yoffset);
	void (*click)(int isright);
	int (*full)(); // plane.sw : plane.sw
	void (*draw)(); // plane.sw : plane.sw
	void (*proc)(); // plane.sw : share.c
	void (*prod)(); // plane.sw : share.c
	int (*read)(); // plane.sw : share.c
	void (*call)(); // plane.sw : plane.sw
	void (*wake)(); // share.c : plane.sw
	void (*done)(); // plane.sw : plane.sw
	int hub; // from other processes
	int tub; // from displayCall
	int zub; // from threadDone
	int esc;
	struct Client *client;
	struct Client *state[Memorys];
	void *refer[Memorys];
};

extern struct Callback cb;
extern struct GLFWwindow* glfw;
#ifndef NOID
extern id cocoa;
#endif

void exiterr(const char *str, int num, int arg);
int displayInit(const char *name);
void windowInit();
void displayDone();
void constructVector(float *point, float *plane, int versor, float *basis);
void transformVector(float *point, float *matrix);
int normalVector(float *normal, float *point);
int solveVector(float *pierce, float *point, float *normal, float *feather);
int pierceVector(float *pierce, float *point, float *normal, float *feather, float *arrow);
int intersectVector(float *point, float *plane, int *versor, float *basis);
void planeInit(int argc);
void threadInit();
void threadDone();
void callError();
