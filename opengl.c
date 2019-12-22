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
int *offset[Memorys] = {0};
int total[NUMCNTX][Memorys] = {0};
struct Pend {int idx; int siz; int len; int bas; int *tot; void *buf; GLuint hdl; GLuint tgt;};
struct Pend pend[NUMCNTX][NUMPEND] = {0};
int pead[NUMCNTX] = {0};
int pail[NUMCNTX] = {0};
GLsync fence[NUMCNTX] = {0};
int head = 0;
int tail = 0;

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
	int uniform = 0;
	offset[Basis] = openglOffset(3*3,3,&uniform);
	offset[Subject] = openglOffset(4,4,&uniform);
	offset[Object] = openglOffset(NUMFILE*4,4,&uniform);
	offset[Feature] = openglOffset(4,4,&uniform);
	offset[Feather] = openglOffset(1,3,&uniform);
	offset[Arrow] = openglOffset(1,3,&uniform);
	offset[Cloud] = openglOffset(NUMFEND,3,&uniform);
	offset[Face] = openglOffset(1,1,&uniform);
	offset[Tope] = openglOffset(1,1,&uniform);
	offset[Tag] = openglOffset(1,1,&uniform);
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
	glBindBuffer(GL_UNIFORM_BUFFER, uniformId[context]);
	glBufferData(GL_UNIFORM_BUFFER, uniform, 0, GL_STATIC_DRAW);
	glBindBuffer(GL_UNIFORM_BUFFER, 0);
	glBindBufferRange(GL_UNIFORM_BUFFER, 0, uniformId[context], 0, uniform);}
	return 1;
}

void openglPendee(int idx, int siz, int len, int bas, int *tot, void *buf, GLuint hdl, GLuint tgt)
{
	glBindBuffer(tgt, hdl);
	if (tot && *tot == 0) glBufferData(tgt, (*tot=idx+siz)*len, 0, GL_STATIC_DRAW);
	if (tot && idx == 0 && siz > *tot) glBufferData(tgt, (*tot=(idx+siz))*len, 0, GL_STATIC_DRAW);
	if (tot && idx+siz > *tot) {
	char buffer[*tot]; glGetBufferSubData(tgt, 0, (*tot)*len, buffer);
	glBufferData(tgt, (idx+siz)*len, 0, GL_STATIC_DRAW);
	glBufferSubData(tgt, 0, (*tot)*len, buffer);
	*tot = idx+siz;}
	if (tot) glBufferSubData(tgt, idx*len, siz*len, buf);
	else glBufferSubData(tgt, bas+idx*len, siz*len, buf);
	glBindBuffer(tgt, 0);
}

void openglPender(struct Pend *ptr)
{
	openglPendee(ptr->idx,ptr->siz,ptr->len,ptr->bas,ptr->tot,ptr->buf,ptr->hdl,ptr->tgt);
}

void openglPending(struct Pend *ptr, int idx, int siz, int len, int bas, int *tot, void *buf, GLuint hdl, GLuint tgt)
{
	ptr->idx=idx;ptr->siz=siz;ptr->len=len;ptr->bas=bas;ptr->tot=tot;ptr->buf=buf;ptr->hdl=hdl;ptr->tgt=tgt;
}

int openglPendant(struct Pend *ptr, int idx, int siz, int len, int bas, int *tot, void *buf, GLuint hdl, GLuint tgt)
{
	return (ptr->bas == bas && ptr->tot == tot && ptr->idx == idx && ptr->siz == siz);
}

void openglBuffer(int idx, int siz, int len, int bas, int *tot, void *buf, GLuint *hdl, GLuint tgt)
{
	for (int ctx = 0; ctx < NUMCNTX; ctx++) if (ctx != head) {
	int found = 0; for (int pnd = pead[ctx]; pnd != pail[ctx] && !found; pnd = (pnd+1)%NUMPEND)
	if (openglPendant(&pend[ctx][pnd],idx,siz,len,bas,tot,buf,hdl[ctx],tgt)) found = 1;
	if (!found) {if ((pail[ctx]+1)%NUMPEND == pead[ctx]) {
	openglPender(&pend[ctx][pead[ctx]]); pead[ctx] = (pead[ctx]+1)%NUMPEND;}
	openglPending(&pend[ctx][pail[ctx]],idx,siz,len,bas,tot,buf,hdl[ctx],tgt); pail[ctx] = (pail[ctx]+1)%NUMPEND;}}
	struct Pend tmp; openglPending(&tmp,idx,siz,len,bas,tot,buf,hdl[head],tgt); openglPender(&tmp);
}

void openglDma()
{
	switch (client->mem) {
	case (Corner): openglBuffer(client->idx,client->siz,sizeof(struct Vertex),0,&total[head][Corner],&state[Corner]->corner[0],vertexId,GL_ARRAY_BUFFER); break;
	case (Triangle): openglBuffer(client->idx,client->siz,sizeof(struct Facet),0,&total[head][Triangle],&state[Triangle]->triangle[0],elementId,GL_ELEMENT_ARRAY_BUFFER); break;
	case (Basis): openglBuffer(client->idx,client->siz*3,3,offset[Basis][client->idx],0,&state[Basis]->basis->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Subject): openglBuffer(0,4,4,offset[Subject][0],0,&state[Subject]->subject->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Object): openglBuffer(client->idx,client->siz*4,4,offset[Object][client->idx],0,&state[Object]->object->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Feature): openglBuffer(0,4,4,offset[Feature][0],0,&state[Feature]->feature->val[0][0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Feather): openglBuffer(0,1,3,offset[Feather][0],0,&state[Feather]->feather->val[0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Arrow): openglBuffer(0,1,3,offset[Arrow][0],0,&state[Arrow]->arrow->val[0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Cloud): openglBuffer(client->idx,client->siz,3,offset[Cloud][client->idx],0,&state[Cloud]->cloud->val[0],uniformId,GL_UNIFORM_BUFFER); break;
	case (Face): openglBuffer(0,1,1,offset[Face][0],0,&state[Face]->face,uniformId,GL_UNIFORM_BUFFER); break;
	case (Tope): openglBuffer(0,1,1,offset[Tope][0],0,&state[Tope]->tope,uniformId,GL_UNIFORM_BUFFER); break;
	case (Tag): openglBuffer(0,1,1,offset[Tag][0],0,&state[Tag]->tag,uniformId,GL_UNIFORM_BUFFER); break;
	case (Mode0): ERROR(huberr,-1);
	case (Mode1): ERROR(huberr,-1);
	case (Mode2): ERROR(huberr,-1);
	case (Mode3): ERROR(huberr,-1);
	case (Fixed): ERROR(huberr,-1);
	case (Moved): ERROR(huberr,-1);
	case (Rolled): ERROR(huberr,-1);
	default: ERROR(exiterr,-1);}
}

int openglFull()
{
	while (tail != head) {
	GLint val;
	GLsizei len;
	glGetSynciv(fence[tail],GL_SYNC_STATUS,1,&len,&val);
	if (val == GL_UNSIGNALED) break;
	tail = (tail+1)%NUMCNTX;}
	int found = 0;
	for (int i = 0; i < client->len && !found; i++) {
	if (client->fnc[i] == Draw) found = 1;}
	return (found && ((head + 1) % NUMCNTX) == tail);
}

void openglGet()
{
	// TODO read from transform feedback buffer in first valid context before tail
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
