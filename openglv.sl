/*
*    openglv.sl
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

#define VERTEX
#include "opengl.sl"

#ifdef DISPLAY
out vec2 VertCoord;
out int VertIndex;
#endif
out vec4 VertColor;

void main()
{
	vec3 point = intersectVector(plane,versor,basis);
	int index = 0; for (int i = 0; i < 3; i++) if (tag == tags[i]) index = i;
	point = transformVector(point,subject);
	point = transformVector(point,object[matid]);
	if (facid[index] == hand) point = transformVector(point,feature);
	gl_Position = vec4(point.x, point.y, point.z, 1.0);
#ifdef DISPLAY
	VertCoord = coord[index];
	VertIndex = texid[index];
	VertColor = color[index];
#endif
#ifdef TRACK
	VertColor = vec4(facid[index],facid[index],facid[index],1.0);
#endif
}
