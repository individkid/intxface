/*
*    model.c
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

#include <GL/glew.h>
#include "plane.h"

int modelInit()
{
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	return 1;
}

void modelDma()
{
	switch (client->mem) {
	case (Corner): /*TODO*/ break;
	case (Triangle): /*TODO*/ break;
	case (Basis): /*TODO*/ break;
	case (Subject): /*TODO*/ break;
	case (Object): /*TODO*/ break;
	case (Feature): /*TODO*/ break;
	case (Feather): /*TODO*/ break;
	case (Arrow): /*TODO*/ break;
	case (Cloud): /*TODO*/ break;
	case (MMatrix): ERROR(huberr,-1);
	case (MClick): ERROR(huberr,-1);
	case (MMove): ERROR(huberr,-1);
	case (MRoll): ERROR(huberr,-1);
	case (Fixed): ERROR(huberr,-1);
	case (Moved): ERROR(huberr,-1);
	case (Rolled): ERROR(huberr,-1);
	case (Face): /*TODO*/ break;
	case (Tope): /*TODO*/ break;
	case (Tag): /*TODO*/ break;
	default: ERROR(exiterr,-1);}
}

int modelCheck()
{
	return 0;
}

void modelGet()
{
	// TODO
}

void modelFunc()
{
	glClear(GL_COLOR_BUFFER_BIT);
	// TODO
	glfwSwapBuffers(window);
}

void modelDraw()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): modelDma(); break;
	case (Dma1): modelGet(); break;
	case (Draw): modelFunc(); break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void modelDone()
{
}
