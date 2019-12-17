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

#include <GLFW/glfw3.h>
#include "face.h"
#include "base.h"
#include "type.h"

#ifndef EXTERN
#define EXTERN extern
#endif
#define VERTEX(FIELD) ((void*)&(((struct Vertex *)0)->FIELD))

struct Affine {
	float view[4][4]; // all polytopes at once
	float tope[NUMFILE][4][4]; // individual polytopes
	float face[4][4]; // individual plane
};

EXTERN int esc;
EXTERN GLFWwindow* window;
EXTERN int vertexBufferChanged;
EXTERN int elementBufferChanged;
EXTERN int uniformBufferChanged[Specials];
EXTERN int nextBufferChanged[Specials];
EXTERN int firstBufferChanged;
EXTERN float basis[3][3][3];
EXTERN struct Affine affine;
EXTERN int vertices;
EXTERN struct Vertex *vertex;
EXTERN int facets;
EXTERN struct Facet *facet;
EXTERN float feather[3];
EXTERN float arrow[3];
EXTERN float cloud[NUMFEND][3];
EXTERN int tag;
EXTERN int tope;
EXTERN int plane;
// TODO feather arrow and feedback

void huberr(const char *str, int num, int arg);
void exiterr(const char *str, int num, int arg);
void windowInit(int argc, char **argv);
void windowDestroy();
int metalInit();
void metalDraw();
void metalDestroy();
int vulkanInit();
void vulkanDraw();
void vulkanDestroy();
int openglInit();
void openglDraw();
void openglDestroy();
