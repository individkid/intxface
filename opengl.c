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

GLuint arrayId = 0;
GLuint programId = 0;
GLuint vertexId = 0;
GLuint elementId = 0;
GLuint uniformId[Specials] = {0};

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
	glGenVertexArrays(1, &arrayId);
	glBindVertexArray(arrayId);
	programId = loadShaders("opengl.vs","opengl.fs");
	glGenBuffers(1, &vertexId);
	glBindBuffer(GL_ARRAY_BUFFER, vertexId);
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
	glGenBuffers(1, &elementId);
	uniformId[Basis] = glGetUniformLocation(programId, "basis");
	uniformId[Subject] = glGetUniformLocation(programId, "subject");
	uniformId[Object] = glGetUniformLocation(programId, "object");
	uniformId[Feature] = glGetUniformLocation(programId, "feature");
	uniformId[Feather] = glGetUniformLocation(programId, "feather");
	uniformId[Arrow] = glGetUniformLocation(programId, "arrow");
	uniformId[Cloud] = glGetUniformLocation(programId, "cloud");
	uniformId[Pass] = glGetUniformLocation(programId, "pass");
	uniformId[Tool] = glGetUniformLocation(programId, "tool");
	return 1;
}

void openglDraw()
{
	if (vertexBufferChanged) {
	glBindBuffer(GL_ARRAY_BUFFER, vertexId);
	glBufferData(GL_ARRAY_BUFFER, sizeof(struct Vertex)*vertices, vertex, GL_STATIC_DRAW);}
	if (elementBufferChanged) {
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementId);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(struct Facet)*facets, facet, GL_STATIC_DRAW);}
	for (enum Special i = firstBufferChanged; uniformBufferChanged[i]; i = nextBufferChanged[i]) switch (i) {
	case (Basis): glUniformMatrix3fv(uniformId[Basis], 3, GL_FALSE, &basis[0][0][0]); break;
	case (Subject): glUniformMatrix4fv(uniformId[Subject], 1, GL_FALSE, &affine.view[0][0]); break;
	case (Object): glUniformMatrix4fv(uniformId[Object], 1, GL_FALSE, &affine.tope[tope][0][0]); break;
	case (Feature): glUniformMatrix4fv(uniformId[Feature], 1, GL_FALSE, &affine.view[0][0]); break;
	case (Feather): glUniform3fv(uniformId[Feather], 1, &feather[0]); break;
	case (Arrow): glUniform3fv(uniformId[Arrow], 1, &arrow[0]); break;
	case (Cloud): glUniform3fv(uniformId[Cloud], NUMFEND, &cloud[0][0]); break;
	case (Pass): glUniform1i(uniformId[Pass], tag); break;
	case (Tool): glUniform1i(uniformId[Tool], plane); break;
	default: ERROR(exiterr,-1);}
	if (vertexBufferChanged || elementBufferChanged || uniformBufferChanged[firstBufferChanged]) {
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programId);
	glBindVertexArray(arrayId);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementId);
	glDrawElements(GL_TRIANGLES,facets,GL_UNSIGNED_INT,(void*)0);
	glfwSwapBuffers(window);}
}

void openglDestroy()
{
	glDeleteBuffers(1, &elementId);
	glDeleteBuffers(1, &arrayId);
	glDeleteVertexArrays(1, &arrayId);
	glDeleteProgram(programId);
}
