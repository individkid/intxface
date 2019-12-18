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
GLuint *uniformId[Memorys] = {0};

GLuint loadShaders(const char *vs, const char *fs)
{
	return 0;
}

GLuint *uniformIdent(const char *name, int size)
{
	GLuint *ptr = 0;
	if (size) ptr = malloc(size*sizeof(GLuint));
	else ptr = malloc(sizeof(GLuint));
	if (!ptr) ERROR(exiterr,-1);
	if (size) for (int i = 0; i < size; i++) {
		char *str = 0;
		if (asprintf(&str,"%s[%d]",name,i) < 0) ERROR(exiterr,-1);
		ptr[i] = glGetUniformLocation(programId,str);
		free(str);
	} else ptr[0] = glGetUniformLocation(programId,name);
	return ptr;
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
	uniformId[Basis] = uniformIdent("basis",3);
	uniformId[Subject] = uniformIdent("subject",0);
	uniformId[Object] = uniformIdent("object",NUMFILE);
	uniformId[Feature] = uniformIdent("feature",0);
	uniformId[Feather] = uniformIdent("feather",0);
	uniformId[Arrow] = uniformIdent("arrow",0);
	uniformId[Cloud] = uniformIdent("cloud",NUMFEND);
	uniformId[Face] = uniformIdent("face",0);
	uniformId[Tope] = uniformIdent("tope",0);
	uniformId[Tag] = uniformIdent("tag",0);
	return 1;
}

int openglCheck()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Check): {
	// TODO return 0 if query not finished
	break;}
	case (Rdma): {
	// TODO transfer from gpu to state[client->mem]
	break;}
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save0): break;
	case (Save1): break;
	case (Dma): break;
	case (Report): break;
	case (Render): break;
	default: ERROR(exiterr,-1);}
	return 1;
}

void openglDma()
{
	switch (client->mem) {
	case (Corner):
	glBindBuffer(GL_ARRAY_BUFFER, vertexId);
	// TODO use client->idx as index for glBufferSubData
	glBufferData(GL_ARRAY_BUFFER, sizeof(*client->corner)*client->siz, client->corner, GL_STATIC_DRAW);
	break;
	case (Triangle): 
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementId);
	// TODO use client->idx as index for glBufferSubData
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(*client->triangle)*client->siz, client->triangle, GL_STATIC_DRAW);
	break;
	case (Basis): glUniformMatrix3fv(uniformId[Basis][0], 3, GL_FALSE, &client->basis->val[0][0]); break;
	case (Subject): glUniformMatrix4fv(uniformId[Subject][0], 1, GL_FALSE, &client->subject->val[0][0]); break;
	case (Object): glUniformMatrix4fv(uniformId[Object][client->idx], client->siz, GL_FALSE, &client->object->val[0][0]); break;
	case (Feature): glUniformMatrix4fv(uniformId[Feature][0], 1, GL_FALSE, &client->feature->val[0][0]); break;
	case (Feather): glUniform3fv(uniformId[Feather][0], 1, &client->feather->val[0]); break;
	case (Arrow): glUniform3fv(uniformId[Arrow][0], 1, &client->arrow->val[0]); break;
	case (Cloud): glUniform3fv(uniformId[Cloud][client->idx], client->siz, &client->cloud->val[0]); break;
	case (MMatrix): ERROR(huberr,-1);
	case (MClick): ERROR(huberr,-1);
	case (MMove): ERROR(huberr,-1);
	case (MRoll): ERROR(huberr,-1);
	case (Fixed): ERROR(huberr,-1);
	case (Moved): ERROR(huberr,-1);
	case (Rolled): ERROR(huberr,-1);
	case (Face): glUniform1i(uniformId[Face][0], client->face); break;
	case (Tope): glUniform1i(uniformId[Tope][0], client->tope); break;
	case (Tag): glUniform1i(uniformId[Tag][0], client->tag); break;
	default: ERROR(exiterr,-1);}
}

void openglRender()
{
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programId);
	glBindVertexArray(arrayId);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementId);
	glDrawElements(GL_TRIANGLES,state[Triangle]->siz*3,GL_UNSIGNED_INT,(void*)0);
	glfwSwapBuffers(window);
}

void openglDraw()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Check): break;
	case (Rdma): break;
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save0): break;
	case (Save1): break;
	case (Dma): openglDma(); break;
	case (Report): break;
	case (Render): openglRender(); break;
	default: ERROR(exiterr,-1);}
}

void openglDone()
{
	glDeleteBuffers(1, &elementId);
	glDeleteBuffers(1, &arrayId);
	glDeleteVertexArrays(1, &arrayId);
	glDeleteProgram(programId);
}
