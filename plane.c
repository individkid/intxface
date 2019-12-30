/*
*    plane.c
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

#define EXTERN
#include "plane.h"
#include <pthread.h>
#include <sys/ioctl.h>

enum API {None, Metal, Vulkan, Opengl, Model};
int vld = 0;
int sub = 0;
int hub = 0;
int tub = 0;
int zub = 0;
int esc = 0;
jmp_buf jmpbuf = {0};
pthread_mutex_t mutex = {0};
pthread_cond_t cond = {0};
pthread_t pthread = {0};
GLFWwindow *window = 0;
struct Client *client = 0;
struct Client *state[Memorys] = {0};
struct Client *saved[Memorys] = {0};
void *refer[Memorys] = {0};
enum API api = 0;
int full = 0;

void huberr(const char *str, int num, int arg)
{
	longjmp(jmpbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	glfwTerminate();
	printf("exiterr (%s) (%d)\n",str,num); fflush(stdout);
	exit(arg);
}

void constructVector(float *point, float *plane, int versor, float *basis)
{
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	point[i*3+j] = basis[versor*9+i*3+j];
	for (int i = 0; i < 3; i++)
	point[i*3+versor] = plane[i];
}

void transformVector(float *point, float *matrix)
{
	float temp[4]; copyvec(temp,point,3); temp[3] = 1.0;
	jumpmat(temp,matrix,4);
	copyvec(point,temp,3);
}

int normalVector(float *normal, float *point)
{
	float neg[3];
	float leg0[3];
	float leg1[3];
	scalevec(copyvec(neg,point,3),-1.0,3);
	plusvec(copyvec(leg0,point+3,3),neg,3);
	plusvec(copyvec(leg1,point+6,3),neg,3);
	if (!normvec(crossvec(copyvec(normal,leg0,3),leg1),3)) return 0;
	return 1;
}

int solveVector(float *pierce, float *point, float *normal, float *feather)
{
	// point+(feather-point-normal/((feather-point)*normal))
	plusvec(scalevec(copyvec(pierce,point,3),-1.0,3),feather,3);
	float denom = dotvec(pierce,normal,3);
	if (fabs(denom) < 1.0 && 1.0 > fabs(INVALID*denom)) return 0;
	float delta[3]; scalevec(copyvec(delta,normal,3),-1.0/denom,3);
	plusvec(plusvec(pierce,delta,3),point,3);
	return 1;
}

int pierceVector(float *pierce, float *point, float *normal, float *point0, float *point1)
{
	// feather+(arrow-feather)*z(arrow-p(arrow))/(z(arrow-p(arrow))+z(feather-p(feather)))
	float solve0[3]; if (!solveVector(solve0,point,normal,point0)) return 0;
	float solve1[3]; if (!solveVector(solve1,point,normal,point1)) return 0;
	float diff0 = solve0[2]-point0[2];
	float diff1 = solve1[2]-point1[2];
	float denom = diff0-diff1;
	float ratio;
	if (fabs(denom) < 1.0 && fabs(diff0) > fabs(INVALID*denom)) return 0;
	else ratio = diff0/denom;
	plusvec(scalevec(plusvec(scalevec(copyvec(pierce,point0,3),-1.0,3),point1,3),ratio,3),point0,3);
	return 1;
}

int intersectVector(float *point, float *plane, int *versor, float *basis)
{
	float corner[27];
	for (int i = 0; i < 3; i++) constructVector(&corner[i*9],&plane[i],versor[i],basis);
	float normal[9];
	for (int i = 0; i < 3; i++) if (!normalVector(&normal[i*3],&corner[i*9])) return 0;
	float pierce0[3];
	float pierce1[3];
	for (int i = 0; i < 3; i++) {
	int i0 = i; int i1 = (i+1)%3; int i2 = (i+2)%3;
	int a0 = i0*3; int a1 = i1*3; int a2 = i2*3;
	int b0 = i0*9; int b1 = i1*9; int b2 = i2*9;
	for (int j = 0; j < 3; j++) {
	int j0 = j; int j1 = (j+1)%3; int j2 = (j+2)%3;
	int c0 = b2+j0; int c1 = b2+j1; int c2 = b2+j2;
	if (!pierceVector(&pierce0[0],&corner[b1],&normal[a1],&corner[c0],&corner[c1])) continue;
	if (!pierceVector(&pierce1[0],&corner[b1],&normal[a1],&corner[c0],&corner[c2])) continue;
	if (pierceVector(point,&corner[b0],&normal[a0],&pierce0[0],&pierce1[0])) return 1;}}
	return 0;
}

int callread(int argc)
{
	int res = 0;
	if (argc == 4) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (vld) {readClient(client,sub); vld = 0; res = 1;
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);}
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}
	return res;
}

float *clientMat(struct Client *client, int idx)
{
	switch (client->mem) {
	case (Subject): return &state[Subject]->subject[0].val[0][0];
	case (Object): return &state[Object]->object[idx].val[0][0];
	case (Feature): return &state[Feature]->feature[0].val[0][0];
	default: ERROR(exiterr,-1);}
	return 0;
}

void clientRmw0()
{
	// state[idx] = client[0]*saved[idx]
	float *stat = clientMat(state[client->mem],client->idx);
	float *save = clientMat(saved[client->mem],client->idx);
	float *give = clientMat(client,0);
	copymat(stat,timesmat(save,give,4),4);
}

void clientRmw1()
{
	// A = B*C
	// A' = B*C'
	// B = A/C
	// A' = (A/C)*C'
	// saved[idx] = 1/saved[idx]
	// state[idx] = state[idx]*saved[idx]
	// saved[idx] = client[0]
	// state[idx] = state[idx]*client[0]
	float *save = clientMat(saved[client->mem],client->idx); invmat(save,4);
	float *stat = clientMat(state[client->mem],client->idx); timesmat(stat,save,4);
	float *give = clientMat(client,0); copymat(save,give,4); timesmat(stat,give,4);
}

void clientRmw2()
{
	// A = B*C
	// A = B'*C'
	// A = B*B'*D
	// A = B'*B*D
	// C = B'*D
	// C' = B*D
	// D = (1/B')*C
	// C' = B*(1/B')*C
	// C = saved[idx]
	// B = client[1]
	// B' = client[0]
	float *save = clientMat(saved[client->mem],client->idx);
	float *give0 = clientMat(client,0);
	float *give1 = clientMat(client,1);
	float inv[16]; invmat(copymat(inv,give0,4),4);
	jumpmat(jumpmat(save,inv,4),give1,4);
}

#define INDEXED(ENUM,FIELD) \
	if (client->mem == ENUM && ptr[client->mem] && client->siz < ptr[client->mem]->siz) \
	{memcpy(&ptr[ENUM]->FIELD[client->idx],client->FIELD,client->siz*sizeof(*client->FIELD)); return;}
void clientCopy(struct Client **ptr)
{
	INDEXED(Corner,corner);
	INDEXED(Triangle,triangle);
	INDEXED(Range,range);
	INDEXED(Basis,basis);
	INDEXED(Object,object);
	INDEXED(Cloud,cloud);
	allocClient(&ptr[client->mem],0); ptr[client->mem] = client;
}

void clientRefer()
{
	switch (client->mem) {
	case (Corner): refer[Corner] = &state[Corner]->corner[0];
	case (Triangle): refer[Triangle] = &state[Triangle]->triangle[0];
	case (Basis): refer[Basis] = &state[Basis]->basis[0];
	case (Subject): refer[Subject] = &state[Subject]->subject[0];
	case (Object): refer[Object] = &state[Object]->object[0];
	case (Feature): refer[Feature] = &state[Feature]->feature[0];
	case (Feather): refer[Feather] = &state[Feather]->feather[0];
	case (Arrow): refer[Arrow] = &state[Arrow]->arrow[0];
	case (Cloud): refer[Cloud] = &state[Cloud]->cloud[0];
	case (Hand): refer[Hand] = &state[Hand]->face;
	case (Tag): refer[Tag] = &state[Tag]->tag;
	default: break;}
}

void process()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): clientRmw0(); break;
	case (Rmw1): clientRmw1(); break;
	case (Rmw2): clientRmw2(); break;
	case (Copy): clientCopy(state); clientRefer(); break;
	case (Save): clientCopy(saved); break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void produce()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): break;
	case (Port): {
	struct Metric metric = {0};
	metric.src = Plane;
	metric.plane = state[client->mem];
	writeMetric(&metric,hub);
	break;}
	default: ERROR(exiterr,-1);}
}

void *thread(void *arg)
{
	int tmp = 0;
	int gon = 1;
	while (gon) {
	for (tmp = waitAny(); tmp >= 0 && gon; tmp = waitAny()) {
	if (tmp == zub) gon = 0; else if (tmp >= 0) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	sub = tmp; vld = 1; glfwPostEmptyEvent();
	if (pthread_cond_wait(&cond,&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}}}
	return 0;
}

void threadInit(int argc, char **argv)
{
	if (argc == 4) {sub = -1;
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((zub = openPipe()) < 0) ERROR(exiterr,-1);
	if ((tub = openPipe()) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub); bothJump(huberr,zub); bothJump(huberr,tub);
	if (pthread_mutex_init(&mutex,0) != 0) ERROR(exiterr,-1);
	if (pthread_cond_init(&cond,0) != 0) ERROR(exiterr,-1);
	if (pthread_create(&pthread,0,thread,0) != 0) ERROR(exiterr,-1);}
}

void threadDone(int argc)
{
	if (argc == 4) {writeInt(1,zub);
	if (pthread_join(pthread,0) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_destroy(&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_cond_destroy(&cond) != 0) ERROR(exiterr,-1);}
}

int main(int argc, char **argv)
{
	struct ttysize ts;
	ioctl(0, TIOCGSIZE, &ts);
	printf("uint32_t(%d) int(%d) GL_INT(%d) float(%d) GL_FLOAT(%d) lines(%d) columns(%d)\n",
	(int)sizeof(uint32_t),(int)sizeof(int),(int)sizeof(GL_INT),(int)sizeof(float),(int)sizeof(GL_FLOAT),
	ts.ts_lines,ts.ts_cols);

	threadInit(argc,argv);
	windowInit(argc,argv);
	if (metalInit()) api = Metal;
	else if (vulkanInit()) api = Vulkan;
	else if (openglInit()) api = Opengl;
	else if (modelInit()) api = Model;
	else api = None;

	while (esc < 2 && !glfwWindowShouldClose(window))
	if (setjmp(jmpbuf) == 0)
	while(esc < 2 && !glfwWindowShouldClose(window)) {
	switch (api) {
	case (None): break;
	case (Metal): full = metalFull(); break;
	case (Vulkan): full = vulkanFull(); break;
	case (Opengl): full = openglFull(); break;
	case (Model): full = modelFull(); break;}
	if (full) {
	glfwWaitEventsTimeout(1000.0*NANO2SEC);
	continue;}

	if (callread(argc)) {
	process();
	switch (api) {
	case (None): break;
	case (Metal): metalDraw(); break;
	case (Vulkan): vulkanDraw(); break;
	case (Opengl): openglDraw(); break;
	case (Model): modelDraw(); break;}
	produce();
	glfwPollEvents();
	continue;}
	glfwWaitEvents();}

	switch (api) {
	case (None): break;
	case (Metal): metalDone(); break;
	case (Vulkan): vulkanDone(); break;
	case (Opengl):  openglDone(); break;
	case (Model):  modelDone(); break;}
	windowDone();
	threadDone(argc);

	return 0;
}
