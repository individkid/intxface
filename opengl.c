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

#define GL_SILENCE_DEPRECATION
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#define NOID
#include "share.h"
#include "contain.h"

struct Pend {int idx; int cnt; int cpu; int gpu; int bas; int *siz; void **buf; GLuint hdl; GLuint tgt;};
typedef void (*initGLuintCast)(int sub, GLuint* box);
typedef void (*doneGLuintCast)(GLuint* box);
void putInt(struct QueInt *buf, int sub, int *box);
int *getInt(struct QueInt *buf, int sub, int *box);
int sizInt(struct QueInt *buf);
void putGLuint(struct QueGLuint *buf, int sub, GLuint *box);
GLuint *getGLuint(struct QueGLuint *buf, int sub, GLuint *box);
int sizGLuint(struct QueGLuint *buf);
void putGLsync(struct QueGLsync *buf, int sub, GLsync *box);
GLsync *getGLsync(struct QueGLsync *buf, int sub, GLsync *box);
int sizGLsync(struct QueGLsync *buf);
void putPend(struct QuePend *buf, int sub, struct Pend *box);
struct Pend *getPend(struct QuePend *buf, int sub, struct Pend *box);
int sizPend(struct QuePend *buf);

struct Queue {
	struct QueInt size;
	struct QueGLuint ident;
	struct QueInt smart;
	struct QuePend pend;
	int inuse;
	int limit;
	int flex;
};
struct Format {
	int base;
	int unit;
};

GLuint programId[Shaders] = {0};
GLuint blockId[Shaders] = {0};
struct Queue arrayQue = {0};
struct Queue vertexQue = {0};
struct Queue elementQue = {0};
struct Queue uniformQue = {0}; struct Format format[Memorys] = {0}; int total = 0;
struct Queue imageQue = {0};
struct QueGLsync fence = {0}; int limit = 0;

void *openglBufferF(int idx, int len, void *buf)
{
	char *ptr = buf;
	ptr += idx*len;
	return ptr;
}

void openglBuffer(int idx, int cnt, int cpu, int gpu, int bas, int *siz, void **buf, GLuint hdl, GLuint tgt)
{
	glBindBuffer(tgt, hdl);
	if (siz && *siz == 0) glBufferData(tgt, (*siz=idx+cnt)*gpu, 0, GL_STATIC_DRAW);
	if (siz && idx == 0 && cnt > *siz) glBufferData(tgt, (*siz=cnt)*gpu, 0, GL_STATIC_DRAW);
	if (siz && idx+cnt > *siz) {
	char buffer[*siz]; glGetBufferSubData(tgt, 0, (*siz)*gpu, buffer);
	glBufferData(tgt, (idx+cnt)*gpu, 0, GL_STATIC_DRAW);
	glBufferSubData(tgt, 0, (*siz)*gpu, buffer);
	*siz = idx+cnt;}
	if (siz && cpu == gpu) glBufferSubData(tgt, idx*gpu, cnt*gpu, *buf);
	if (siz && cpu != gpu) for (int i = 0; i < cnt; i++)
	glBufferSubData(tgt, (idx+i)*gpu, cpu, openglBufferF(i,cpu,*buf));
	if (siz == 0 && cpu == gpu) glBufferSubData(tgt, bas+idx*gpu, cnt*gpu, *buf);
	if (siz == 0 && cpu != gpu) for (int i = 0; i < cnt; i++)
	glBufferSubData(tgt, bas+(idx+i)*gpu, cpu, openglBufferF(i,cpu,*buf));
	glBindBuffer(tgt, 0);
}

void initArray(int sub, GLuint *box)
{
	getGLuint(&vertexQue.ident,sub,0);
	glGenVertexArrays(1,box);
}

#define VERTEX(FIELD) \
	((void*)&(((struct Vertex *)0)->FIELD))
#define INTEGER(SIZE,FIELD) \
	glVertexAttribIPointer(index++,SIZE,GL_INT,sizeof(struct Vertex),VERTEX(FIELD))
#define FLOATER(SIZE,FIELD) \
	glVertexAttribPointer(index++,SIZE,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(FIELD))
void initVertex(int sub, GLuint *box)
{
	glGenBuffers(1,box);
	glBindVertexArray(*getGLuint(&arrayQue.ident,sub,0));
	glBindBuffer(GL_ARRAY_BUFFER,*box);
	GLuint index = 0;
	INTEGER(3,tag[0]); // location=0 ivec3
	for (int i = 0; i < 3; i++) FLOATER(3,plane[i][0]); // location=1 vec3[3]
	INTEGER(3,versor[0]); // location=4 ivec3
	for (int i = 0; i < 3; i++) FLOATER(2,coord[i][0]); // location=5 vec2[3]
	for (int i = 0; i < 3; i++) FLOATER(4,color[i][0]); // location=8 vec4[3]
	INTEGER(3,facid[0]); // location=11 ivec3
	INTEGER(1,matid); // location=12 int
	for (int i = 0; i < index; i++)
	glEnableVertexAttribArray(i);
	glBindVertexArray(0);
	for (int i = 0; i < index; i++)
	glDisableVertexAttribArray(i);
	openglBuffer(0,cb.state[Corner]->siz,sizeof(struct Vertex),sizeof(struct Vertex),0,getInt(&vertexQue.size,sub,0),&cb.refer[Corner],*box,GL_ARRAY_BUFFER);
}

void initElement(int sub, GLuint *box)
{
	glGenBuffers(1,box);
	openglBuffer(0,cb.state[Triangle]->siz,sizeof(struct Facet),sizeof(struct Facet),0,getInt(&elementQue.size,sub,0),&cb.refer[Triangle],*box,GL_ELEMENT_ARRAY_BUFFER);
}

void initUniform(int sub, GLuint *box)
{
	glGenBuffers(1,box);
	glBindBuffer(GL_UNIFORM_BUFFER,*box);
	glBufferData(GL_UNIFORM_BUFFER,total,0,GL_STATIC_DRAW);
	glBindBufferRange(GL_UNIFORM_BUFFER,0,*box,0,total);
	openglBuffer(0,cb.state[Basis]->siz,sizeof(struct Linear),format[Basis].unit,format[Basis].base,0,&cb.refer[Basis],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Affine),format[Subject].unit,format[Subject].base,0,&cb.refer[Subject],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,cb.state[Object]->siz,sizeof(struct Affine),format[Object].unit,format[Object].base,0,&cb.refer[Object],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Affine),format[Feature].unit,format[Feature].base,0,&cb.refer[Feature],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Vector),format[Feather].unit,format[Feather].base,0,&cb.refer[Feather],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Vector),format[Arrow].unit,format[Arrow].base,0,&cb.refer[Arrow],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,cb.state[Cloud]->siz,sizeof(struct Vector),format[Cloud].unit,format[Cloud].base,0,&cb.refer[Cloud],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(int),format[Hand].unit,format[Hand].base,0,&cb.refer[Hand],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(int),format[Tag].unit,format[Tag].base,0,&cb.refer[Tag],*getGLuint(&uniformQue.ident,sub,0),GL_UNIFORM_BUFFER);
}

void initImage(int sub, GLuint *box)
{
	// TODO
}

void doneArray(GLuint *box)
{
	glDeleteVertexArrays(1,box);
}

void doneBuffer(GLuint *box)
{
	glDeleteBuffers(1,box);
}

void doneImage(GLuint *box)
{
	// TODO
}

GLuint queueDraw(struct Queue *que)
{
	int sub = que->inuse;
	getInt(&que->size,sub,0);
	if (*getInt(&que->smart,sub,0) == 0) que->inuse += 1;
	*getInt(&que->smart,sub,0) += 1;
	return *getGLuint(&que->ident,sub,0);
}

void queueSync(struct Queue *que)
{
	*getInt(&que->smart,0,0) -= 1;
	if (*getInt(&que->smart,0,0) > 0) return;
	int sub = sizGLuint(&que->ident);
	if (sub-que->inuse < que->flex) {
	GLuint ident = *getGLuint(&que->ident,0,0);
	putInt(&que->size,sub,getInt(&que->size,0,0));
	putGLuint(&que->ident,sub,&ident);
	putInt(&que->smart,sub,getInt(&que->smart,0,0));
	while (sizPend(&que->pend) > 0) {
	struct Pend *pend = getPend(&que->pend,0,0);
	openglBuffer(pend->idx,pend->cnt,pend->cpu,pend->gpu,pend->bas,pend->siz,pend->buf,pend->hdl,pend->tgt);
	putPend(&que->pend,0,0);}}
	putInt(&que->size,0,0);
	putGLuint(&que->ident,0,0);
	putInt(&que->smart,0,0);
	que->inuse -= 1;
}

void queueBufferF(struct Queue *que)
{
	if (sizGLuint(&que->ident) == que->inuse) {
	getInt(&que->size,que->inuse,0);
	getGLuint(&que->ident,que->inuse,0);
	getInt(&que->smart,que->inuse,0);}
}

void queueBufferG(int idx, int cnt, int cpu, int gpu, int bas, int sub, void **buf, struct Queue *que, GLuint tgt)
{
	openglBuffer(idx,cnt,cpu,gpu,bas,(tgt==GL_UNIFORM_BUFFER?0:getInt(&que->size,sub,0)),buf,*getGLuint(&que->ident,sub,0),tgt);
}

void queueBufferH(int idx, int cnt, int cpu, int gpu, int bas, int sub, void **buf, struct Queue *que, GLuint tgt)
{
	struct Pend pend = {0};
	pend.idx=idx,pend.cnt=cnt,pend.cpu=cpu,pend.gpu=gpu,pend.bas=bas;
	pend.siz=(tgt==GL_UNIFORM_BUFFER?0:getInt(&que->size,sub,0));
	pend.buf=buf;pend.hdl=*getGLuint(&que->ident,sub,0);pend.tgt=tgt;
	putPend(&que->pend,sizPend(&que->pend),&pend);
}

void queueBuffer(int idx, int cnt, int cpu, int gpu, int bas, void **buf, struct Queue *que, GLuint tgt)
{
	queueBufferF(que);
	queueBufferG(idx,cnt,cpu,gpu,bas,que->inuse,buf,que,tgt);
}

void queueBuffers(int idx, int cnt, int cpu, int gpu, int bas, void **buf, struct Queue *que, GLuint tgt)
{
	queueBufferF(que);
	for (int sub = 0; sub < que->inuse; sub++)
	queueBufferH(idx,cnt,cpu,gpu,bas,sub,buf,que,tgt);
	for (int sub = que->inuse; sub < sizGLuint(&que->ident); sub++)
	queueBufferG(idx,cnt,cpu,gpu,bas,sub,buf,que,tgt);
}

void openglDma()
{
	switch (cb.client->mem) {
	case (Corner): queueBuffers(cb.client->idx,cb.client->siz,sizeof(struct Vertex),sizeof(struct Vertex),0,&cb.refer[Corner],&vertexQue,GL_ARRAY_BUFFER); break;
	case (Triangle): queueBuffers(cb.client->idx,cb.client->siz,sizeof(struct Facet),sizeof(struct Facet),0,&cb.refer[Triangle],&elementQue,GL_ELEMENT_ARRAY_BUFFER); break;
	case (Range): ERROR(cb.err,-1);
	case (Basis): queueBuffers(cb.client->idx,cb.client->siz,sizeof(struct Linear),format[Basis].unit,format[Basis].base,&cb.refer[Basis],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Subject): queueBuffers(0,1,sizeof(struct Affine),format[Subject].unit,format[Subject].base,&cb.refer[Subject],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Object): queueBuffers(cb.client->idx,cb.client->siz,sizeof(struct Affine),format[Object].unit,format[Object].base,&cb.refer[Object],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Feature): queueBuffers(0,1,sizeof(struct Affine),format[Feature].unit,format[Feature].base,&cb.refer[Feature],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Feather): queueBuffers(0,1,sizeof(struct Vector),format[Feather].unit,format[Feather].base,&cb.refer[Feather],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Arrow): queueBuffers(0,1,sizeof(struct Vector),format[Arrow].unit,format[Arrow].base,&cb.refer[Arrow],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Cloud): queueBuffers(cb.client->idx,cb.client->siz,sizeof(struct Vector),format[Cloud].unit,format[Cloud].base,&cb.refer[Cloud],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Hand): queueBuffers(0,1,sizeof(int),format[Hand].unit,format[Hand].base,&cb.refer[Hand],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Tag): queueBuffers(0,1,sizeof(int),format[Tag].unit,format[Tag].base,&cb.refer[Tag],&uniformQue,GL_UNIFORM_BUFFER); break;
	case (Face): ERROR(cb.err,-1);
	case (User): ERROR(cb.err,-1);
	case (Image): /*TODO*/ break;
	default: ERROR(exiterr,-1);}
}

void openglGet()
{
	float color = 0.0;
	glReadPixels(0,0,1,1,GL_RED,GL_FLOAT,&color);
	cb.state[Face]->face = color;
}

void openglFunc()
{
	glClear(GL_COLOR_BUFFER_BIT);
	glUseProgram(programId[cb.state[User]->user->shader]);
	glBindVertexArray(queueDraw(&arrayQue)); queueDraw(&vertexQue);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,queueDraw(&elementQue));
	for (int i = 0; i < cb.state[Range]->siz; i++) {
	cb.state[Tag]->tag = cb.state[Range]->range[i].tag;
	queueBuffer(0,1,sizeof(int),format[Tag].unit,format[Tag].base,&cb.refer[Tag],&uniformQue,GL_UNIFORM_BUFFER);
	glBindBufferBase(GL_UNIFORM_BUFFER,0,queueDraw(&uniformQue));
	if (cb.state[User]->user->shader == Stream) {
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D,queueDraw(&imageQue));}
	void *buf = openglBufferF(cb.state[Range]->range[i].idx,sizeof(struct Facet),0);
	glDrawElements(GL_TRIANGLES,cb.state[Range]->range[i].siz*3,GL_UNSIGNED_INT,buf);
	GLsync temp = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE,0);
	putGLsync(&fence,sizGLsync(&fence),&temp);}
	if (cb.state[User]->user->shader == Display || cb.state[User]->user->shader == Stream) cb.swap();
}

int openglFull()
{
	while (sizGLsync(&fence) > 0) {
	GLint value = 0;
	glGetSynciv(*getGLsync(&fence,0,0),GL_SYNC_STATUS,sizeof(GLint),0,&value);
	if (value != GL_SIGNALED) break;
	GLsync temp = *getGLsync(&fence,0,0);
	putGLsync(&fence,0,0);
	glDeleteSync(temp);
	queueSync(&arrayQue);
	queueSync(&vertexQue);
	queueSync(&elementQue);
	queueSync(&uniformQue);
	queueSync(&imageQue);}
	if (cb.client)
	for (int i = 0; i < cb.client->len; i++)
	switch (cb.client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Rmw2): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): switch (cb.client->mem) {
	case (Corner):
	if (sizGLuint(&vertexQue.ident) == vertexQue.inuse &&
	vertexQue.inuse == vertexQue.limit) return 1; break;
	case (Triangle):
	if (sizGLuint(&elementQue.ident) == elementQue.inuse &&
	elementQue.inuse == elementQue.limit) return 1; break;
	case (Range): ERROR(cb.err,-1);
	case (Basis): case (Subject): case (Object): case (Feature):
	case (Feather): case (Arrow): case (Cloud): case (Hand): case (Tag):
	if (sizGLuint(&uniformQue.ident) == uniformQue.inuse &&
	uniformQue.inuse == uniformQue.limit) return 1; break;
	case (Face): ERROR(cb.err,-1);
	case (User): ERROR(cb.err,-1);
	case (Image):
	if (sizGLuint(&imageQue.ident) == imageQue.inuse &&
	imageQue.inuse == imageQue.limit) return 1; break;
	default: ERROR(exiterr,-1);} break;
	case (Dma1): break;
	case (Draw):
	if (sizGLsync(&fence) == limit) return 1; break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
	return 0;
}

void openglDraw()
{
	for (int i = 0; i < cb.client->len; i++)
	switch (cb.client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Rmw2): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): openglDma(); break;
	case (Dma1): openglGet(); break;
	case (Draw): openglFunc(); break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void queueDone(struct QueGLuint *que)
{
	for (int sub = 0; sub < sizGLuint(que); sub++) {
	GLuint ident = *getGLuint(que,sub,0);
	glDeleteBuffers(1,&ident);
	putGLuint(que,sub,0);}
}

void openglDone()
{
	queueDone(&uniformQue.ident);
	queueDone(&elementQue.ident);
	queueDone(&vertexQue.ident);
	queueDone(&arrayQue.ident);
	for (int shader = 0; shader < Shaders; shader++)
	glDeleteProgram(programId[shader]);
}

void openglShader(GLuint i, GLenum j, const char *file, const char *def)
{
	int stream[2] = {0};
	pipe(stream);
	if (fork() == 0) {
	const char *args[] = {"lua","opengl.lua",file,def,0};
	close(stream[0]);
	dup2(stream[1], STDOUT_FILENO);
	execvp(args[0], (char * const*)args);
	exit(-1);}
	close(stream[1]);
	char *buf[1] = {0};
	int len[1] = {0};
	int siz[1] = {0};
	int nread = 0;
	buf[0] = malloc(siz[0]=BUFSIZE*CMDSIZE);
	while ((nread = read(stream[0],buf[0]+len[0],BUFSIZE)) != 0)
	if ((len[0]+=nread)>siz[0]-BUFSIZE)
	buf[0] = realloc(buf[0],siz[0]+=BUFSIZE*CMDSIZE);
	GLuint k = glCreateShader(j);
	glShaderSource(k,1,(const char *const *)buf,len);
	glCompileShader(k);
	free(buf[0]);
	GLint stat = 0;
	glGetShaderiv(k,GL_COMPILE_STATUS,&stat);
	if (stat == GL_FALSE) {
	GLint max = 0;
	glGetShaderiv(k,GL_INFO_LOG_LENGTH,&max);
	char log[max];
	glGetShaderInfoLog(k,max,&max,log);
	printf("file(%s) log(%s)\n",file,log);}
	glAttachShader(i,k);
	glDeleteShader(k);
}

GLuint openglLoad(const char *name, const char *vs, const char *gs, const char *fs)
{
	GLuint retval = glCreateProgram();
	openglShader(retval,GL_VERTEX_SHADER,vs,name);
	if (gs) openglShader(retval,GL_GEOMETRY_SHADER,gs,name);
	openglShader(retval,GL_FRAGMENT_SHADER,fs,name);
	glLinkProgram(retval);
	GLint stat = 0;
	glGetProgramiv(retval,GL_LINK_STATUS,(int*)&stat);
	if (stat == GL_FALSE) {
	GLint max = 0;
	glGetProgramiv(retval,GL_INFO_LOG_LENGTH,&max);
	char log[max];
	glGetProgramInfoLog(retval,max,&max,log);
	if (gs) printf("vs(%s) gs(%s) fs(%s) log(%s)\n",vs,gs,fs,log);
	else printf("vs(%s) fs(%s) log(%s)\n",vs,fs,log);}
	return retval;
}

void openglAlign(int *total, int *base, int *unit, int elem, int count, int size)
{
	int rem = *total%elem;
	if (rem) *total += (elem-rem);
	*unit = elem*count;
	*base = *total;
	*total += *unit*size;
}

void queueInit(initGLuintCast init, doneGLuintCast done, int limit, int flex, struct Queue *que)
{
	que->ident.init = (initArrGLuint)init;
	que->ident.done = (doneArrGLuint)done;
	que->limit = limit;
	que->flex = flex;
}

int openglInit()
{
	printf("unsigned(%d) GLuint(%d) GLsync(%d) GLfloat(%d)\n",(int)sizeof(unsigned),(int)sizeof(GLuint),(int)sizeof(GLsync),(int)sizeof(GLfloat));
	cb.full = openglFull;
	cb.draw = openglDraw;
	cb.done = openglDone;
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(1.00f,1.00f,0.00f,1.00f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	const char *name[] = {"PROXIMITY","DISPLAY","STREAM","TRACK"};
	const char *geom[] = {"openglg.sl",0,0,"openglg.sl"};
	for (int shader = 0; shader < Shaders; shader++) {
	programId[shader] = openglLoad(name[shader],"openglv.sl",geom[shader],"openglf.sl");
	blockId[shader] = glGetUniformBlockIndex(programId[shader],"Uniform");
	glUniformBlockBinding(programId[shader],blockId[shader],0);}
	total = 0;
	openglAlign(&total,&format[Basis].base,&format[Basis].unit,4*4,3,3);
	openglAlign(&total,&format[Subject].base,&format[Subject].unit,4*4,4,1);
	openglAlign(&total,&format[Object].base,&format[Object].unit,4*4,4,NUMFILE);
	openglAlign(&total,&format[Feature].base,&format[Feature].unit,4*4,4,1);
	openglAlign(&total,&format[Feather].base,&format[Feather].unit,4*4,1,1);
	openglAlign(&total,&format[Arrow].base,&format[Arrow].unit,4*4,1,1);
	openglAlign(&total,&format[Cloud].base,&format[Cloud].unit,4*4,1,NUMFEND);
	openglAlign(&total,&format[Hand].base,&format[Hand].unit,4,1,1);
	openglAlign(&total,&format[Tag].base,&format[Tag].unit,4,1,1);
	queueInit(initArray,doneArray,NUMCNTX,NUMFLEX,&arrayQue);
	queueInit(initVertex,doneBuffer,NUMCNTX,NUMFLEX,&vertexQue);
	queueInit(initElement,doneBuffer,NUMCNTX,NUMFLEX,&elementQue);
	queueInit(initUniform,doneBuffer,NUMCNTX,NUMFLEX,&uniformQue);
	queueInit(initImage,doneImage,NUMCNTX,NUMFLEX,&imageQue);
	limit = NUMCNTX;
	glClear(GL_COLOR_BUFFER_BIT);
	cb.swap();
	return 1;
}
