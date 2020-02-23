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
#ifdef STREAM
in vec4 VertColor;
in vec2 VertCoord;
uniform sampler2D image;
#endif
#ifdef DISPLAY
in vec4 VertColor;
#endif

void main()
{
#ifdef TRACK
	FragColor = GeomColor;
#endif
#ifdef STREAM
	FragColor = VertColor;
	FragColor *= texture(image,VertCoord);
#endif
#ifdef DISPLAY
	FragColor = VertColor;
#endif
}
