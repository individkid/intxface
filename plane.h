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

#include "type.h"

struct Affine {
	float view[4][4]; // all polytopes at once
	float tope[NUMFILE][4][4]; // individual polytopes
	float face[4][4]; // individual plane
};
#define VERTEX(FIELD) ((void*)&(((struct Vertex *)0)->FIELD))

extern int esc;
extern GLFWwindow* window;
extern int vertexBufferChanged;
extern int elementBufferChanged;
// TODO uniformBufferChanged;
extern float basis[3][3][3];
extern struct Affine affine;
extern int vertices;
extern struct Vertex *vertex;
extern int facets;
extern struct Facet *facet;
extern int tag;
extern int plane;
// TODO feather arrow and feedback

void huberr(const char *str, int num, int arg);
void exiterr(const char *str, int num, int arg);
