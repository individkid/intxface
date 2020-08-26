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

enum Shader {
	Display,Track,Shaders
};
enum Config {
	PictureMinX,PictureMinY,PictureMinZ,
	PictureWide,PictureHigh,PictureDeep,
	DefaultWide,DefaultHigh,
	DefaultDeep,DefaultLong,DefaultStop,
	ScreenWide,ScreenHigh,
};

struct Callback {
	void (*err)(const char *str, int num, int arg);
	// kvm to share; read from state; write to pipe
	void (*move)(double xpos, double ypos);
	void (*roll)(double xoffset, double yoffset);
	void (*click)(int isright);
	void (*drag)(double xmid, double ymid, double xmax, double ymax);
	void (*write)(struct Vector *point, struct Vector *normal, int object);
	// share to kvm; read from state; write to gpu
	double (*conf)(enum Config config);
	void (*warp)(double xpos, double ypos);
	void (*dma)(enum Memory mem, int idx, int siz);
	void (*draw)(enum Shader shader);
	// loop to kvm
	int (*full)();
	// loop to share; read from pipe; write to state
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

void shareArg(const char *arg);
void shareInit();
void shareDone();
void threadInit();
void threadDone();
void clientFacet(enum Memory mem, int idx, int siz, int len, struct Facet *ptr, enum Function *fnc);
void clientVertex(enum Memory mem, int idx, int siz, int len, struct Vertex *ptr, enum Function *fnc);
void clientInt(enum Memory mem, int idx, int siz, int len, int *ptr, enum Function *fnc);
void clientArray(enum Memory mem, int idx, int siz, int len, struct Array *ptr, enum Function *fnc);
void clientLinear(enum Memory mem, int idx, int siz, int len, struct Linear *ptr, enum Function *fnc);
void clientAffine(enum Memory mem, int idx, int siz, int len, struct Affine *ptr, enum Function *fnc);
void clientVector(enum Memory mem, int idx, int siz, int len, struct Vector *ptr, enum Function *fnc);
void clientMode(enum Memory mem, int idx, int siz, int len, struct Mode *ptr, enum Function *fnc);
