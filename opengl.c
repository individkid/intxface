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

GLuint programId = 0;
GLuint arrayId[NUMCNTX] = {0};
GLuint vertexId[NUMCNTX] = {0};
GLuint elementId[NUMCNTX] = {0};
GLuint uniformId[NUMCNTX] = {0};
int *offset[Memorys] = {0}; // offset into uniform buffer
int *init[Memorys] = {0}; // all invalid
int *first[Memorys] = {0}; // first invalid
int *next[NUMCNTX][Memorys] = {0}; // next invalid
int *last[NUMCNTX][Memorys] = {0}; // last invalid
GLsync fence[NUMCNTX] = {0}; // test to disuse context
int head = 0; // current not inuse context
int tail = 0; // next inuse (if != head) context to disuse

GLuint loadShaders(const char *vs, const char *fs)
{
	return 0;
}

/*
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
*/

int openglInit()
{
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	programId = loadShaders("opengl.vs","opengl.fs");
	for (int context = 0; context < NUMCNTX; context++) {
	glGenVertexArrays(1, &arrayId[context]);
	glBindVertexArray(arrayId[context]);
	glGenBuffers(1, &vertexId[context]);
	glBindBuffer(GL_ARRAY_BUFFER, vertexId[context]);
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
	glGenBuffers(1, &elementId[context]);}
	/*
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
	*/
	return 1;
}

/*
#define INDEXED(ENUM,FIELD,IDENT,BUFFER) \
	glBindBuffer(BUFFER, IDENT); \
	if (client->siz < state[ENUM]->siz) \
	glBufferSubData(BUFFER, sizeof(*client->FIELD)*client->idx, sizeof(*client->FIELD)*client->siz, client->FIELD); \
	else \
	glBufferData(GL_ARRAY_BUFFER, sizeof(*client->FIELD)*client->siz, client->FIELD, GL_STATIC_DRAW);
*/
void openglDma()
{
	switch (client->mem) {
	case (Corner): /*INDEXED(Corner,corner,vertexId,GL_ARRAY_BUFFER);*/ break;
	case (Triangle): /*INDEXED(Triangle,triangle,elementId,GL_ELEMENT_ARRAY_BUFFER);*/ break;
	case (Basis): /*glUniformMatrix3fv(uniformId[Basis][0], 3, GL_FALSE, &client->basis->val[0][0]);*/ break;
	case (Subject): /*glUniformMatrix4fv(uniformId[Subject][0], 1, GL_FALSE, &client->subject->val[0][0]);*/ break;
	case (Object): /*glUniformMatrix4fv(uniformId[Object][client->idx], client->siz, GL_FALSE, &client->object->val[0][0]);*/ break;
	case (Feature): /*glUniformMatrix4fv(uniformId[Feature][0], 1, GL_FALSE, &client->feature->val[0][0]);*/ break;
	case (Feather): /*glUniform3fv(uniformId[Feather][0], 1, &client->feather->val[0]);*/ break;
	case (Arrow): /*glUniform3fv(uniformId[Arrow][0], 1, &client->arrow->val[0]);*/ break;
	case (Cloud): /*glUniform3fv(uniformId[Cloud][client->idx], client->siz, &client->cloud->val[0]);*/ break;
	case (MMatrix): ERROR(huberr,-1);
	case (MClick): ERROR(huberr,-1);
	case (MMove): ERROR(huberr,-1);
	case (MRoll): ERROR(huberr,-1);
	case (Fixed): ERROR(huberr,-1);
	case (Moved): ERROR(huberr,-1);
	case (Rolled): ERROR(huberr,-1);
	case (Face): /*glUniform1i(uniformId[Face][0], client->face);*/ break;
	case (Tope): /*glUniform1i(uniformId[Tope][0], client->tope);*/ break;
	case (Tag): /*glUniform1i(uniformId[Tag][0], client->tag);*/ break;
	case (Scratch): ERROR(huberr,-1);
	default: ERROR(exiterr,-1);}
}

int openglCheck()
{
	// TODO check fence[tail] to advance tail
	int found = 0;
	for (int i = 0; i < client->len && !found; i++) {
	if (client->fnc[i] == Draw) found = 1;}
	return (found && ((head + 1) % NUMCNTX) == tail);
}

void openglGet()
{
	// TODO
}

void openglFunc()
{
	// TODO for each invalid buffer, dma from last and remember valid
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programId);
	// TODO bind uniformId[head]
	glBindVertexArray(arrayId[head]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementId[head]);
	glDrawElements(GL_TRIANGLES,state[Triangle]->siz*3,GL_UNSIGNED_INT,(void*)0);
	fence[head] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE,0);
	glfwSwapBuffers(window);
	head = (head + 1) % NUMCNTX;
}

void openglDraw()
{
	// TODO check fence[tail] to advance tail
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): openglDma(); break;
	case (Dma1): openglGet(); break;
	case (Draw): openglFunc(); break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void openglDone()
{
	for (int context = 0; context < NUMCNTX; context++) {
	glDeleteBuffers(1, &elementId[context]);
	glDeleteBuffers(1, &vertexId[context]);
	glDeleteVertexArrays(1, &arrayId[context]);}
	glDeleteProgram(programId);
}
