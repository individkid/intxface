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
#include "plane.h"
#include "contain.h"

typedef GLuint gluint;
typedef GLsync glsync;
void putInt(struct QueInt *buf, int sub, int *box);
int getInt(struct QueInt *buf, int sub, int *box);
int sizInt(struct QueInt *buf);
VALUEBOX(int,Int);
void putGluint(struct QueGluint *buf, int sub, gluint *box);
gluint getGluint(struct QueGluint *buf, int sub, gluint *box);
int sizGluint(struct QueGluint *buf);
VALUEBOX(gluint,Gluint);
void putGlsync(struct QueGlsync *buf, int sub, glsync *box);
glsync getGlsync(struct QueGlsync *buf, int sub, glsync *box);
int sizGlsync(struct QueGlsync *buf);
VALUEBOX(glsync,Glsync)
struct Queue {
	struct QueInt size;
	struct QueGluint ident;
	struct QueInt smart;
	int inuse;
};

GLuint programId[Shaders] = {0};
GLuint blockId[Shaders] = {0};
struct Queue arrayQue = {0};
struct Queue vertexQue = {0};
struct Queue elementQue = {0};
struct Queue uniformQue = {0};
struct Queue imageQue = {0};
struct QueGlsync fence = {0};
int base[Memorys] = {0};
int unit[Memorys] = {0};
int flex[Memorys] = {0};
int total = 0;

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

void queueArray(int sub, GLuint *box)
{
	getGluint(&vertexQue.ident,sub,0);
	glGenVertexArrays(1,box);
}

#define VERTEX(FIELD) \
	((void*)&(((struct Vertex *)0)->FIELD))
#define INTEGER(SIZE,FIELD) \
	glVertexAttribIPointer(index++,SIZE,GL_INT,sizeof(struct Vertex),VERTEX(FIELD))
#define FLOATER(SIZE,FIELD) \
	glVertexAttribPointer(index++,SIZE,GL_FLOAT,GL_FALSE,sizeof(struct Vertex),VERTEX(FIELD))
void queueVertex(int sub, GLuint *box)
{
	glGenBuffers(1,box);
	glBindVertexArray(getGluint(&arrayQue.ident,sub,0));
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
	getInt(&vertexQue.size,sub,0);
}

void queueElement(int sub, GLuint *box)
{
	glGenBuffers(1,box);
	getInt(&elementQue.size,sub,0);
}

void queueUniform(int sub, GLuint *box)
{
	glGenBuffers(1,box);
	glBindBuffer(GL_UNIFORM_BUFFER,*box);
	glBufferData(GL_UNIFORM_BUFFER,total,0,GL_STATIC_DRAW);
	glBindBufferRange(GL_UNIFORM_BUFFER,0,*box,0,total);
	openglBuffer(0,cb.state[Basis]->siz,sizeof(struct Linear),unit[Basis],base[Basis],0,&cb.refer[Basis],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Affine),unit[Subject],base[Subject],0,&cb.refer[Subject],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,cb.state[Object]->siz,sizeof(struct Affine),unit[Object],base[Object],0,&cb.refer[Object],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Affine),unit[Feature],base[Feature],0,&cb.refer[Feature],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Vector),unit[Feather],base[Feather],0,&cb.refer[Feather],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(struct Vector),unit[Arrow],base[Arrow],0,&cb.refer[Arrow],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,cb.state[Cloud]->siz,sizeof(struct Vector),unit[Cloud],base[Cloud],0,&cb.refer[Cloud],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(int),unit[Hand],base[Hand],0,&cb.refer[Hand],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
	openglBuffer(0,1,sizeof(int),unit[Tag],base[Tag],0,&cb.refer[Tag],peekGluint(&uniformQue.ident,sub),GL_UNIFORM_BUFFER);
}

void queueImage(int sub, GLuint *box)
{
	// TODO
}

int *queueSize(struct Queue *que)
{
	// TODO
	return 0;
}

GLuint queueDma(struct Queue *que)
{
	// TODO
	return 0;
}

GLuint queueDraw(struct Queue *que)
{
	// TODO
	return 0;
}

void queueSync(struct Queue *que)
{
	// TODO
}

void openglDma()
{
	switch (cb.client->mem) {
	case (Corner): openglBuffer(cb.client->idx,cb.client->siz,sizeof(struct Vertex),sizeof(struct Vertex),0,queueSize(&vertexQue),&cb.refer[Corner],queueDma(&vertexQue),GL_ARRAY_BUFFER); break;
	case (Triangle): openglBuffer(cb.client->idx,cb.client->siz,sizeof(struct Facet),sizeof(struct Facet),0,queueSize(&elementQue),&cb.refer[Triangle],queueDma(&elementQue),GL_ELEMENT_ARRAY_BUFFER); break;
	case (Range): ERROR(cb.err,-1);
	case (Basis): openglBuffer(cb.client->idx,cb.client->siz,sizeof(struct Linear),unit[Basis],base[Basis],0,&cb.refer[Basis],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Subject): openglBuffer(0,1,sizeof(struct Affine),unit[Subject],base[Subject],0,&cb.refer[Subject],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Object): openglBuffer(cb.client->idx,cb.client->siz,sizeof(struct Affine),unit[Object],base[Object],0,&cb.refer[Object],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Feature): openglBuffer(0,1,sizeof(struct Affine),unit[Feature],base[Feature],0,&cb.refer[Feature],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Feather): openglBuffer(0,1,sizeof(struct Vector),unit[Feather],base[Feather],0,&cb.refer[Feather],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Arrow): openglBuffer(0,1,sizeof(struct Vector),unit[Arrow],base[Arrow],0,&cb.refer[Arrow],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Cloud): openglBuffer(cb.client->idx,cb.client->siz,sizeof(struct Vector),unit[Cloud],base[Cloud],0,&cb.refer[Cloud],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Hand): openglBuffer(0,1,sizeof(int),unit[Hand],base[Hand],0,&cb.refer[Hand],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
	case (Tag): openglBuffer(0,1,sizeof(int),unit[Tag],base[Tag],0,&cb.refer[Tag],queueDma(&uniformQue),GL_UNIFORM_BUFFER); break;
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
	openglBuffer(0,1,sizeof(int),unit[Tag],base[Tag],0,&cb.refer[Tag],queueDma(&uniformQue),GL_UNIFORM_BUFFER);
	glBindBufferBase(GL_UNIFORM_BUFFER,0,queueDraw(&uniformQue));
	if (cb.state[User]->user->shader == Stream) {
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D,queueDraw(&imageQue));}
	void *buf = openglBufferF(cb.state[Range]->range[i].idx,sizeof(struct Facet),0);
	glDrawElements(GL_TRIANGLES,cb.state[Range]->range[i].siz*3,GL_UNSIGNED_INT,buf);
	enqueGlsync(&fence,glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE,0));}
	if (cb.state[User]->user->shader == Display || cb.state[User]->user->shader == Stream) cb.swap();
}

int openglFull()
{
	while (sizGlsync(&fence) > 0) {
	GLint value = 0;
	glGetSynciv(headGlsync(&fence),GL_SYNC_STATUS,sizeof(GLint),0,&value);
	if (value != GL_SIGNALED) break;
	glDeleteSync(dequeGlsync(&fence));
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
	case (Dma0): /*TODO return 1 if could no more of flex*/ break;
	case (Dma1): break;
	case (Draw): /*TODO return 1 if too many of sync*/ break;
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

void openglDone()
{
	for (int sub = 0; sub < sizGluint(&uniformQue.ident); sub++) {
		GLuint ident = getGluint(&uniformQue.ident,sub,0);
		glDeleteBuffers(1,&ident);
		putGluint(&uniformQue.ident,sub,0);}
	for (int sub = 0; sub < sizGluint(&elementQue.ident); sub++) {
		GLuint ident = getGluint(&elementQue.ident,sub,0);
		glDeleteBuffers(1,&ident);
		putGluint(&elementQue.ident,sub,0);}
	for (int sub = 0; sub < sizGluint(&vertexQue.ident); sub++) {
		GLuint ident = getGluint(&vertexQue.ident,sub,0);
		glDeleteBuffers(1,&ident);
		putGluint(&vertexQue.ident,sub,0);}
	for (int sub = 0; sub < sizGluint(&arrayQue.ident); sub++) {
		GLuint ident = getGluint(&arrayQue.ident,sub,0);
		glDeleteBuffers(1,&ident);
		putGluint(&arrayQue.ident,sub,0);}
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

GLuint openglLoad(const char *vs, const char *gs, const char *fs)
{
	GLuint retval = glCreateProgram();
	const char *def = (gs ? "TRACK" : "DISPLAY"); // TODO check for def of STREAM
	openglShader(retval,GL_VERTEX_SHADER,vs,def);
	if (gs) openglShader(retval,GL_GEOMETRY_SHADER,gs,def);
	openglShader(retval,GL_FRAGMENT_SHADER,fs,def);
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

int openglInit()
{
	printf("unsigned(%d) GLuint(%d) GLsync(%d) GLfloat(%d)\n",(int)sizeof(unsigned),(int)sizeof(GLuint),(int)sizeof(GLsync),(int)sizeof(GLfloat));
	cb.full = openglFull;
	cb.draw = openglDraw;
	cb.done = openglDone;
	if (glewInit() != GLEW_OK) ERROR(exiterr,-1);
	glClearColor(1.00f,1.00f,1.00f,1.00f);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	for (int shader = 0; shader < Shaders; shader++) {
	const char *geom = (shader == Track ? "openglg.sl" : 0);
	programId[shader] = openglLoad("openglv.sl",geom,"openglf.sl");
	blockId[shader] = glGetUniformBlockIndex(programId[shader],"Uniform");
	glUniformBlockBinding(programId[shader],blockId[shader],0);}
	total = 0;
	openglAlign(&total,&base[Basis],&unit[Basis],4*4,3,3);
	openglAlign(&total,&base[Subject],&unit[Subject],4*4,4,1);
	openglAlign(&total,&base[Object],&unit[Object],4*4,4,NUMFILE);
	openglAlign(&total,&base[Feature],&unit[Feature],4*4,4,1);
	openglAlign(&total,&base[Feather],&unit[Feather],4*4,1,1);
	openglAlign(&total,&base[Arrow],&unit[Arrow],4*4,1,1);
	openglAlign(&total,&base[Cloud],&unit[Cloud],4*4,1,NUMFEND);
	openglAlign(&total,&base[Hand],&unit[Hand],4,1,1);
	openglAlign(&total,&base[Tag],&unit[Tag],4,1,1);
	arrayQue.ident.func = (funcGluint)queueArray;
	vertexQue.ident.func = (funcGluint)queueVertex;
	elementQue.ident.func = (funcGluint)queueElement;
	uniformQue.ident.func = (funcGluint)queueUniform;
	imageQue.ident.func = (funcGluint)queueImage;
	glClear(GL_COLOR_BUFFER_BIT);
	cb.swap();
	return 1;
}
