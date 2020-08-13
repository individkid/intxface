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
	void (*drag)(double xpos, double ypos);
	void (*write)(struct Vector *point, struct Vector *normal, int object);
	// share to kvm
	void (*warp)(double xpos, double ypos);
	void (*dma)(enum Memory mem);
	void (*draw)();
	// loop to kvm
	int (*full)();
	// loop to share
	void (*proc)();
	int (*read)();
	// main to loop
	void (*start)();
	void (*call)();
	// share to loop
	void (*wake)();
	// main to kvm
	void (*done)();
	int hub;
	int tub;
	int zub;
	int esc;
	struct Client *state[Memorys];
};

extern struct Callback cb;

void novoid();
int nofalse();
void nowarp(double xpos, double ypos);
void nodma(enum Memory mem);
void nosize(double width, double height);
void nodrag(double xpos, double ypos);
void nomove(double xpos, double ypos);
void noroll(double xoffset, double yoffset);
void noclick(int isright);
void nowrite(struct Vector *point, struct Vector *normal, int object);
void shareArg(const char *arg);
void shareInit();
void shareDone();
void threadInit();
void threadDone();
