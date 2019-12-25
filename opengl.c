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

#define VERTEX(FIELD) ((void*)&(((struct Vertex *)0)->FIELD))
GLuint programId[Shaders] = {0};
GLuint blockId[Shaders] = {0};
GLuint arrayId[NUMCNTX] = {0};
GLuint vertexId[NUMCNTX] = {0};
GLuint elementId[NUMCNTX] = {0};
GLuint uniformId[NUMCNTX] = {0};
int size[NUMCNTX][Memorys] = {0};
int base[Memorys] = {0};
int unit[Memorys] = {0};
struct Pend {int idx; int cnt; int cpu; int gpu; int bas; int *siz; void **buf; GLuint hdl; GLuint tgt;};
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

int openglInit()
{
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	for (int shader = 0; shader < Shaders; shader++) {
	char *vertex = 0; char *fragment = 0;
	if (asprintf(&vertex,"opengl%d.vs",shader) < 0) ERROR(exiterr,-1);
	if (asprintf(&fragment,"opengl%d.fs",shader) < 0) ERROR(exiterr,-1);
	programId[shader] = openglLoad(vertex,fragment);
	free(vertex); free(fragment);
	blockId[shader] = glGetUniformBlockIndex(programId[shader],"Uniform");
	glUniformBlockBinding(programId[shader],blockId[shader],0);}
	int total = 0;
	unit[Basis] = 3*4*4; base[Basis] = total; total += unit[Basis]*3;
	unit[Subject] = 4*4*4; base[Subject] = total; total += unit[Subject];
	unit[Object] = 4*4*4; base[Object] = total; total += unit[Object]*NUMFILE;
	unit[Feature] = 4*4*4; base[Feature] = total; total += unit[Feature];
	unit[Feather] = 4*4; base[Feather] = total; total += unit[Feather];
	unit[Arrow] = 4*4; base[Arrow] = total; total += unit[Arrow];
	unit[Cloud] = 4*4; base[Cloud] = total; total += unit[Cloud]*NUMFEND;
	unit[Face] = 4; base[Face] = total; total += unit[Face];
	unit[Tag] = 4; base[Tag] = total; total += unit[Tag];
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
	glBufferData(GL_UNIFORM_BUFFER, total, 0, GL_STATIC_DRAW);
	glBindBufferRange(GL_UNIFORM_BUFFER, 0, uniformId[context], 0, total);}
	return 1;
}

void *openglBuffed(int idx, int len, void *buf)
{
	char *ptr = buf;
	ptr += idx*len;
	return ptr;
}

void openglPendee(int idx, int cnt, int cpu, int gpu, int bas, int *siz, void **buf, GLuint hdl, GLuint tgt)
{
	glBindBuffer(tgt, hdl);
	if (siz && *siz == 0) glBufferData(tgt, (*siz=idx+cnt)*gpu, 0, GL_STATIC_DRAW);
	if (siz && idx == 0 && cnt > *siz) glBufferData(tgt, (*siz=(idx+cnt))*gpu, 0, GL_STATIC_DRAW);
	if (siz && idx+cnt > *siz) {
	char buffer[*siz]; glGetBufferSubData(tgt, 0, (*siz)*gpu, buffer);
	glBufferData(tgt, (idx+cnt)*gpu, 0, GL_STATIC_DRAW);
	glBufferSubData(tgt, 0, (*siz)*gpu, buffer);
	*siz = idx+cnt;}
	if (siz && cpu == gpu) glBufferSubData(tgt, idx*gpu, cnt*gpu, *buf);
	if (siz && cpu != gpu) for (int i = 0; i < cnt; i++)
	glBufferSubData(tgt, (idx+i)*gpu, cpu, openglBuffed(i,cpu,*buf));
	if (siz == 0 && cpu == gpu) glBufferSubData(tgt, bas+idx*gpu, cnt*gpu, *buf);
	if (siz == 0 && cpu != gpu) for (int i = 0; i < cnt; i++)
	glBufferSubData(tgt, bas+(idx+i)*gpu, cpu, openglBuffed(i,cpu,*buf));
	glBindBuffer(tgt, 0);
}

void openglPender(struct Pend *ptr)
{
	openglPendee(ptr->idx,ptr->cnt,ptr->cpu,ptr->gpu,ptr->bas,ptr->siz,ptr->buf,ptr->hdl,ptr->tgt);
}

void openglPending(struct Pend *ptr, int idx, int cnt, int cpu, int gpu, int bas, int *siz, void **buf, GLuint hdl, GLuint tgt)
{
	ptr->idx=idx;ptr->cnt=cnt;ptr->cpu=cpu;ptr->gpu=gpu;ptr->bas=bas;ptr->siz=siz;ptr->buf=buf;ptr->hdl=hdl;ptr->tgt=tgt;
}

int openglPendant(struct Pend *ptr, int idx, int cnt, int cpu, int gpu, int bas, int *siz, void **buf, GLuint hdl, GLuint tgt)
{
	return (ptr->bas == bas && ptr->siz == siz && ptr->idx == idx && ptr->cnt == cnt);
}

void openglBuffer(int idx, int cnt, int cpu, int gpu, int bas, int *siz, void **buf, GLuint *hdl, GLuint tgt)
{
	for (int ctx = 0; ctx < NUMCNTX; ctx++) if (ctx != head) {
	int found = 0; for (int pnd = pead[ctx]; pnd != pail[ctx] && !found; pnd = (pnd+1)%NUMPEND)
	if (openglPendant(&pend[ctx][pnd],idx,cnt,cpu,gpu,bas,siz,buf,hdl[ctx],tgt)) found = 1;
	if (!found) {if ((pail[ctx]+1)%NUMPEND == pead[ctx]) {
	openglPender(&pend[ctx][pead[ctx]]); pead[ctx] = (pead[ctx]+1)%NUMPEND;}
	openglPending(&pend[ctx][pail[ctx]],idx,cnt,cpu,gpu,bas,siz,buf,hdl[ctx],tgt); pail[ctx] = (pail[ctx]+1)%NUMPEND;}}
	struct Pend tmp; openglPending(&tmp,idx,cnt,cpu,gpu,bas,siz,buf,hdl[head],tgt); openglPender(&tmp);
}

void openglDma()
{
	switch (client->mem) {
	case (Corner): openglBuffer(client->idx,client->siz,sizeof(struct Vertex),sizeof(struct Vertex),0,&size[head][Corner],&refer[Corner],vertexId,GL_ARRAY_BUFFER); break;
	case (Triangle): openglBuffer(client->idx,client->siz,sizeof(struct Facet),sizeof(struct Facet),0,&size[head][Triangle],&refer[Triangle],elementId,GL_ELEMENT_ARRAY_BUFFER); break;
	case (Range): ERROR(huberr,-1);
	case (Basis): openglBuffer(client->idx,client->siz,sizeof(struct Linear),unit[Basis],base[Basis],0,&refer[Basis],uniformId,GL_UNIFORM_BUFFER); break;
	case (Subject): openglBuffer(0,1,sizeof(struct Affine),unit[Subject],base[Subject],0,&refer[Subject],uniformId,GL_UNIFORM_BUFFER); break;
	case (Object): openglBuffer(client->idx,client->siz,sizeof(struct Affine),unit[Object],base[Object],0,&refer[Object],uniformId,GL_UNIFORM_BUFFER); break;
	case (Feature): openglBuffer(0,1,sizeof(struct Affine),unit[Feature],base[Feature],0,&refer[Feature],uniformId,GL_UNIFORM_BUFFER); break;
	case (Feather): openglBuffer(0,1,sizeof(struct Vector),unit[Feather],base[Feather],0,&refer[Feather],uniformId,GL_UNIFORM_BUFFER); break;
	case (Arrow): openglBuffer(0,1,sizeof(struct Vector),unit[Arrow],base[Arrow],0,&refer[Arrow],uniformId,GL_UNIFORM_BUFFER); break;
	case (Cloud): openglBuffer(client->idx,client->siz,sizeof(struct Vector),unit[Cloud],base[Cloud],0,&refer[Cloud],uniformId,GL_UNIFORM_BUFFER); break;
	case (Face): openglBuffer(0,1,sizeof(int),unit[Face],base[Face],0,&refer[Face],uniformId,GL_UNIFORM_BUFFER); break;
	case (Tag): openglBuffer(0,1,sizeof(int),unit[Tag],base[Tag],0,&refer[Tag],uniformId,GL_UNIFORM_BUFFER); break;
	case (User): ERROR(huberr,-1);
	case (Collect): ERROR(huberr,-1);
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
	if (client)
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): if (((head + 1) % NUMCNTX) == tail) return 1; else break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
	return 0;
}

void openglBuffee(int idx, int siz, int len, void *buf, GLuint hdl, GLuint tgt)
{
	glBindBuffer(tgt, hdl);
	glGetBufferSubData(tgt, idx*len, siz*len, buf);
	glBindBuffer(tgt, 0);
}

void openglGet()
{
	float color = 0.0;
	glReadBuffer(GL_AUX0);
	glReadPixels(0,0,1,1,GL_RED,GL_FLOAT,&color);
	state[Face]->face = color;
}

void openglFunc()
{
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programId[state[User]->user->shader]);
	glBindVertexArray(arrayId[head]);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,elementId[head]);
	glBindBufferBase(GL_UNIFORM_BUFFER,0,uniformId[head]);
	// TODO depending on state[Shader] render to invisible framebuffer instead
	//  then depth replacement finds closest pierce point
	//  and color is plane identifier
	for (int i = 0; i < state[Range]->siz; i++) {
	void *buf = openglBuffed(state[Range]->range[i].idx,sizeof(struct Facet),0);
	state[Tag]->tag = state[Range]->range[i].tag;
	openglPendee(0,1,sizeof(int),unit[Tag],base[Tag],0,&refer[Tag],uniformId[head],GL_UNIFORM_BUFFER);
	glDrawElements(GL_TRIANGLES,state[Range]->range[i].siz*3,GL_UNSIGNED_INT,buf);}
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
	for (int shader = 0; shader < Shaders; shader++)
	glDeleteProgram(programId[shader]);
}
