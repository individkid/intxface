/*
*    opengl.c
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

GLuint vertexArray = 0;
GLuint programID = 0;
GLuint vertexBuffer = 0;
GLuint elementBuffer = 0;

GLuint loadShaders(const char *vs, const char *fs)
{
	return 0;
}

int openglInit()
{
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS); 
	glGenVertexArrays(1, &vertexArray);
	glBindVertexArray(vertexArray);
	programID = loadShaders("plane.vs","plane.fs");
	glGenBuffers(1, &vertexBuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
	GLuint index = 0;
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(tag[0]));
	for (int i = 0; i < 3; i++)
	glVertexAttribPointer(index++,3,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(plane[i][0]));
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(versor[0]));
	for (int i = 0; i < 3; i++)
	glVertexAttribPointer(index++,2,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(coord[i][0]));
	for (int i = 0; i < 3; i++)
	glVertexAttribPointer(index++,4,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(coord[i][0]));
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(texid[0]));
	glVertexAttribIPointer(index++,3,GL_INT,sizeof(struct Vertex),VERTEX(facid[0]));
	glVertexAttribIPointer(index++,1,GL_INT,sizeof(struct Vertex),VERTEX(matid));
	for (int i = 0; i < index; i++)
	glEnableVertexAttribArray(i);
	glBindVertexArray(0);
	for (int i = 0; i < index; i++)
	glDisableVertexAttribArray(i);
	glGenBuffers(1, &elementBuffer);
	return 1;
}

void openglDraw()
{
	if (vertexBufferChanged) {
	glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(struct Vertex)*vertices, vertex, GL_STATIC_DRAW);}
	if (elementBufferChanged) {
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementBuffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(struct Facet)*facets, facet, GL_STATIC_DRAW);}
	if (vertexBufferChanged || elementBufferChanged) {
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programID);
	glBindVertexArray(vertexArray);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementBuffer);
	glDrawElements(GL_TRIANGLES,facets,GL_UNSIGNED_INT,(void*)0);
	glfwSwapBuffers(window);}
}

void openglDestroy()
{
	glDeleteBuffers(1, &elementBuffer);
	glDeleteBuffers(1, &vertexBuffer);
	glDeleteVertexArrays(1, &vertexArray);
	glDeleteProgram(programID);
}
