/*
*    openglf.sl
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

#include "opengl.sl"

out vec4 FragColor;
#ifdef TRACK
in vec4 GeomColor;
#endif
#ifdef DISPLAY
in vec4 VertColor;
in vec2 VertCoord;
flat in int VertIndex;
uniform sampler2D image[16];
#endif

void main()
{
#ifdef TRACK
	FragColor = GeomColor;
#endif
#ifdef DISPLAY
	FragColor = VertColor;
	if (VertIndex >= 0 && VertIndex < 16)
	FragColor *= texture(image[VertIndex],VertCoord);
#endif
}
