/*
*    opengl.sl
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

#version 330 core

layout (std140) uniform Uniform
{
	mat3 basis[3];
	mat4 subject;
	mat4 object[NUMFILE];
	mat4 feature;
	vec3 feather;
	vec3 arrow;
	vec3 cloud[NUMFEND];
	int hand;
	int tag;
};

#ifdef VERTEX
layout (location=0) in ivec3 tag;
layout (location=1) in vec3 plane[3];
layout (location=4) in ivec3 versor;
layout (location=5) in vec2 coord[3];
layout (location=8) in vec4 color[3];
layout (location=11) in ivec3 texid;
layout (location=12) in ivec3 facid;
layout (location=13) in int matid;
#endif
