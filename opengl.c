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
GLuint blockId = 0;
GLuint arrayId[NUMCNTX] = {0};
GLuint vertexId[NUMCNTX] = {0};
GLuint elementId[NUMCNTX] = {0};
GLuint uniformId[NUMCNTX] = {0};
int offsets[Memorys] = {0};
int *offset[Memorys] = {0}; // offset into uniform buffer
struct Pend {enum Memory mem;
int idx; int siz; int len; 
void *buf; GLuint hdl; GLuint tgt;
} pend[NUMCNTX][NUMPEND] = {0};
int pead[NUMCNTX] = {0};
int pail[NUMCNTX] = {0};
GLsync fence[NUMCNTX] = {0}; // test to disuse context
int head = 0; // current not inuse context
int tail = 0; // next inuse (if != head) context to disuse
void *unpack = 0;

GLuint openglLoad(const char *vs, const char *fs)
{
	return 0;
}

int *openglOffset(int num, int siz, int *tot)
{
	if (num > 1 && siz < 4) siz = 4;
	int *res = malloc(num*sizeof(int));
	for (int i = 0; i < num; i++) {
	res[i] = *tot; *tot += siz*4;}
	return res;
}

int openglInit()
{
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	programId = openglLoad("opengl.vs","opengl.fs");
	blockId = glGetUniformBlockIndex(programId,"Uniform");
	glUniformBlockBinding(programId,blockId,0);
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
	glGenBuffers(1, &elementId[context]);
	glGenBuffers(1, &uniformId[context]);
	int total = 0;
	offset[Basis] = openglOffset(3*3,3,&total);
	offset[Subject] = openglOffset(4,4,&total);
	offset[Object] = openglOffset(NUMFILE*4,4,&total);
	offset[Feature] = openglOffset(4,4,&total);
	offset[Feather] = openglOffset(1,3,&total);
	offset[Arrow] = openglOffset(1,3,&total);
	offset[Cloud] = openglOffset(NUMFEND,3,&total);
	offset[Face] = openglOffset(1,1,&total);
	offset[Tope] = openglOffset(1,1,&total);
	offset[Tag] = openglOffset(1,1,&total);
	glBindBuffer(GL_UNIFORM_BUFFER, uniformId[context]);
	glBufferData(GL_UNIFORM_BUFFER, total, 0, GL_STATIC_DRAW);
	glBindBuffer(GL_UNIFORM_BUFFER, 0);
	glBindBufferRange(GL_UNIFORM_BUFFER, 0, uniformId[context], 0, total);}
	return 1;
}

void openglPendee(enum Memory mem, int idx, int siz, int len, void *buf, GLuint hdl, GLuint tgt)
{
}

void openglPender(struct Pend *ptr)
{
	openglPendee(ptr->mem,ptr->idx,ptr->siz,ptr->len,ptr->buf,ptr->hdl,ptr->tgt);
}

void openglPending(struct Pend *ptr, enum Memory mem, int idx, int siz, int len, void *buf, GLuint hdl, GLuint tgt)
{
	ptr->mem=mem;ptr->idx=idx;ptr->siz=siz;ptr->len=len;ptr->buf=buf;ptr->hdl=hdl;ptr->tgt=tgt;
}

void openglBuffer(enum Memory mem, int idx, int siz, int len, void *buf, GLuint *hdl, GLuint tgt)
{
	for (int ctx = 0; ctx < NUMCNTX; ctx++) if (ctx != head) {
	int found = 0; for (int pnd = pead[ctx]; pnd != pail[ctx] && !found; pnd = (pnd+1)%NUMPEND)
	if (pend[ctx][pnd].mem == mem && pend[ctx][pnd].idx == idx && pend[ctx][pnd].siz == siz) found = 1;
	if (!found) {if ((pail[ctx]+1)%NUMPEND == pead[ctx]) {
	openglPender(&pend[ctx][pead[ctx]]); pead[ctx] = (pead[ctx]+1)%NUMPEND;}
	openglPending(&pend[ctx][pail[ctx]],mem,idx,siz,len,buf,hdl[ctx],tgt); pail[ctx] = (pail[ctx]+1)%NUMPEND;}}
	struct Pend tmp; openglPending(&tmp,mem,idx,siz,len,buf,hdl[head],tgt); openglPender(&tmp);
}

void openglDma()
{
	switch (client->mem) {
	case (Corner): openglBuffer(Corner,client->idx,client->siz,sizeof(struct Vertex),&client->corner[0],vertexId,GL_ARRAY_BUFFER); break;
	case (Triangle): openglBuffer(Triangle,client->idx,client->siz,sizeof(struct Facet),&client->triangle[0],elementId,GL_ELEMENT_ARRAY_BUFFER); break;
	case (Basis): openglBuffer(Basis,client->idx,client->siz*3,3,&client->basis->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Subject): openglBuffer(Subject,0,4,4,&client->subject->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Object): openglBuffer(Object,client->idx,client->siz*4,4,&client->object->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Feature): openglBuffer(Feature,0,4,4,&client->feature->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Feather): openglBuffer(Feather,0,1,3,&client->feather->val[0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Arrow): openglBuffer(Arrow,0,1,3,&client->arrow->val[0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Cloud): openglBuffer(Cloud,client->idx,client->siz,3,&client->cloud->val[0],uniformId,GL_UNIFORM_BUFFER); break;
	case (MMatrix): ERROR(huberr,-1);
	case (MClick): ERROR(huberr,-1);
	case (MMove): ERROR(huberr,-1);
	case (MRoll): ERROR(huberr,-1);
	case (Fixed): ERROR(huberr,-1);
	case (Moved): ERROR(huberr,-1);
	case (Rolled): ERROR(huberr,-1);
	case (Face): openglBuffer(Face,0,1,1,&client->face,uniformId,GL_UNIFORM_BUFFER); break;
	case (Tope): openglBuffer(Tope,0,1,1,&client->tope,uniformId,GL_UNIFORM_BUFFER); break;
	case (Tag): openglBuffer(Tag,0,1,1,&client->tag,uniformId,GL_UNIFORM_BUFFER); break;
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
	// TODO read from ubo in first valid context before tail
}

void openglFunc()
{
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programId);
	glBindBufferBase(GL_UNIFORM_BUFFER,0,uniformId[head]);
	glBindVertexArray(arrayId[head]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementId[head]);
	glDrawElements(GL_TRIANGLES,state[Triangle]->siz*3,GL_UNSIGNED_INT,(void*)0);
	fence[head] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE,0);
	glfwSwapBuffers(window);
	head = (head + 1) % NUMCNTX;
	while (pail[head] == pead[head]) {
	openglPender(&pend[head][pead[head]]);
	pead[head] = (pead[head]+1)%NUMPEND;}
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
