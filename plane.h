/*
*    plane.h
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

#define GL_SILENCE_DEPRECATION
#include <GLFW/glfw3.h>
#include "face.h"
#include "base.h"
#include "type.h"
#include "metic.h"

struct Callback {
	void (*err)(const char *str, int num, int arg);
	void (*pos)(int *xloc, int *yloc);
	void (*size)(int *width, int *height);
	void (*key)(int key);
	void (*move)(double xpos, double ypos);
	void (*roll)(double xoffset, double yoffset);
	void (*click)(int isright);
	int (*full)();
	int (*read)();
	void (*proc)();
	void (*draw)();
	void (*prod)();
	void (*call)();
	void (*swap)();
	void (*done)();
};

extern int esc;
extern struct Client *client;
extern struct Client *state[Memorys];
extern void *refer[Memorys];
extern struct Callback cb;

void exiterr(const char *str, int num, int arg);
int displayInit(int argc, char **argv);
int metalInit(int argc, char **argv);
int vulkanInit();
int openglInit();
int modelInit();
void constructVector(float *point, float *plane, int versor, float *basis);
void transformVector(float *point, float *matrix);
int normalVector(float *normal, float *point);
int solveVector(float *pierce, float *point, float *normal, float *feather);
int pierceVector(float *pierce, float *point, float *normal, float *feather, float *arrow);
int intersectVector(float *point, float *plane, int *versor, float *basis);
