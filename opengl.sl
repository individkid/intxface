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
