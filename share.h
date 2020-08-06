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
	void (*err)(const char *str, int num, int arg);
	// kvm to share
	void (*move)(double xpos, double ypos);
	void (*roll)(double xoffset, double yoffset);
	void (*click)(int isright);
	void (*size)(double width, double height);
	void (*write)(struct Vector *point, struct Vector *normal);
	// share to kvm
	void (*warp)(double xpos, double ypos);
	int (*full)(); // loop to kvm
	void (*draw)(); // loop to kvm
	void (*proc)(); // loop to share
	void (*prod)(); // loop to share
	int (*read)(); // loop to share
	void (*call)(); // main to loop
	void (*wake)(); // share to loop
	void (*done)(); // main to kvm
	int hub;
	int tub;
	int zub;
	int esc;
	struct Client *client;
	struct Client *state[Memorys];
};

extern struct Callback cb;

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
