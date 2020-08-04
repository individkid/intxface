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

#include "share.h"
#include <pthread.h>

int vld = 0;
int sub = 0;
pthread_mutex_t mutex = {0};
pthread_cond_t cond = {0};
pthread_t pthread = {0};
struct Client *saved[Memorys] = {0};
struct Callback cb = {0};
float xmove = 0.0;
float ymove = 0.0;
float offset = 0.0;
int toggle = 0;
float vector[3] = {0};
float matrix[16] = {0};
float piemat[16] = {0};
float normat[16] = {0};
int object = 0;

void exiterr(const char *str, int num, int arg)
{
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
	// point+(feather-point-normal*((feather-point)*normal))
	plusvec(scalevec(copyvec(pierce,point,3),-1.0,3),feather,3);
	float portion = dotvec(pierce,normal,3);
	float delta[3]; scalevec(copyvec(delta,normal,3),-portion,3);
	plusvec(plusvec(pierce,delta,3),point,3);
	return 1;
}

int pierceVector(float *pierce, float *point, float *normal, float *point0, float *point1)
{
	// feather+(arrow-feather)*z(feather-p(feather))/(z(feather-p(feather))-z(arrow-p(arrow)))
	float solve0[3]; if (!solveVector(solve0,point,normal,point0)) return 0;
	float solve1[3]; if (!solveVector(solve1,point,normal,point1)) return 0;
	float diff0 = solve0[2]-point0[2];
	float diff1 = solve1[2]-point1[2];
	float denom = diff0-diff1;
	if (fabs(denom) < 1.0 && fabs(diff0) > fabs(INVALID*denom)) return 0;
	float ratio = diff0/denom;
	plusvec(scalevec(plusvec(scalevec(copyvec(pierce,point0,3),-1.0,3),point1,3),ratio,3),point0,3);
	return 1;
}

int intersectVector(float *point, float *plane, int *versor, float *basis)
{
	for (int i = 0; i < 3; i++) {
	float plane0[3][3]; constructVector(&plane0[0][0],&plane[i*3],versor[i],basis);
	float normal0[3]; if (!normalVector(&normal0[0],&plane0[0][0])) continue;
	for (int j = 1; j < 3; j++) {
	float plane1[3][3]; constructVector(&plane1[0][0],&plane[((i+j)%3)*3],versor[(i+j)%3],basis);
	float plane2[3][3]; constructVector(&plane2[0][0],&plane[((i+j+1)%3)*3],versor[(i+j+1)%3],basis);
	float normal2[3]; if (!normalVector(&normal2[0],&plane2[0][0])) continue;
	for (int k = 0; k < 3; k++) {
	float pierce0[3]; if (!pierceVector(&pierce0[0],&plane0[0][0],&normal0[0],&plane1[(k+0)%3][0],&plane1[(k+1)%3][0])) continue;
	float pierce1[3]; if (!pierceVector(&pierce1[0],&plane0[0][0],&normal0[0],&plane1[(k+0)%3][0],&plane1[(k+2)%3][0])) continue;
	if (!pierceVector(&point[0],&plane2[0][0],&normal2[0],&pierce0[0],&pierce1[0])) continue;
	return 1;}}}
	return 0;
}

void longitudeMatrix(float *result, float *vector)
{
	float vec[2];
	identmat(result,4);
	if (!normvec(copyvec(vec,vector,2),2)) return;
	result[0*4+0] = vec[1];
	result[0*4+1] = vec[0];
	result[1*4+0] = -vec[0];
	result[1*4+1] = vec[1];
}

void latitudeMatrix(float *result, float *vector)
{
	float vec[2]; copyvec(vec,vector,2);
	vec[0] = sqrtf(dotvec(vec,vec,2)); vec[1] = vector[2];
	identmat(result,4);
	if (!normvec(vec,2)) return;
	result[0*4+0] = vec[1];
	result[0*4+2] = -vec[0];
	result[2*4+0] = vec[0];
	result[2*4+2] = vec[1];
}

void translateMatrix(float *result, float *vector)
{
	copyvec(identmat(result,4)+12,vector,3);
}

void angleMatrix(float *result, float angle)
{
	float vec[2]; vec[0] = sinf(angle); vec[1] = cosf(angle);
	identmat(result,4);
	result[0*4+0] = vec[1];
	result[0*4+1] = vec[0];
	result[1*4+0] = -vec[0];
	result[1*4+1] = vec[1];
}

void lengthMatrix(float *result, float length)
{
	identmat(result,4);
	result[3*4+2] = length;
}

void scaleMatrix(float *result, float scale)
{
	scale = logf(scale);
	identmat(result,4);
	for (int i = 0; i < 3; i++)
	result[i*4+i] = scale;
}

void normalMatrix(float *result, float *normal)
{
	float lon[16]; longitudeMatrix(lon,normal);
	float lat[16]; latitudeMatrix(lat,normal);
	timesmat(copymat(result,lat,4),lon,4);
}

void fixedMatrix(float *result, float *pierce)
{
	translateMatrix(result,pierce);
}

void offsetVector(float *result)
{
	struct Mode *user = cb.state[User]->user;
	float cur[3]; cur[0] = xmove; cur[1] = ymove; cur[2] = -1.0;
	float pix[3]; pix[2] = -1.0;
	for (int i = 0; i < 2; i++) pix[i] = vector[i];
	plusvec(copyvec(result,cur,3),scalevec(pix,-1.0,3),3);
}

void transformMatrix(float *result)
{
	struct Mode *user = cb.state[User]->user;
	switch (user->move) {
	case (Rotate): { // rotate about fixed pierce point
	float vec[3]; offsetVector(vec);
	float lon[16]; longitudeMatrix(lon,vec);
	latitudeMatrix(result,vec);
	float mat[16]; jumpmat(copymat(mat,piemat,4),lon,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Slide): { // translate parallel to fixed facet
	float vec[3]; offsetVector(vec);
	translateMatrix(result,vec);
	float mat[16]; copymat(mat,normat,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Slate): { // translate parallel to picture plane
	float vec[3]; offsetVector(vec);
	translateMatrix(result,vec);
	break;}
	default: ERROR(cb.err,-1);}
}

void composeMatrix(float *result)
{
	float mat[16];
	struct Mode *user = cb.state[User]->user;
	switch (user->roll) {
	case (Cylinder): { // rotate with rotated fixed axis
	angleMatrix(result,offset);
	float mat[16]; jumpmat(copymat(mat,piemat,4),matrix,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Clock): { // rotate with fixed normal to picture plane
	angleMatrix(result,offset);
	float mat[16]; copymat(mat,piemat,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Compass): { // rotate with fixed normal to facet
	angleMatrix(result,offset);
	float mat[16]; jumpmat(jumpmat(copymat(mat,piemat,4),normat,4),matrix,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Normal): { // translate with fixed normal to facet
	lengthMatrix(result,offset);
	float mat[16]; copymat(mat,normat,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Balloon): { // scale with fixed pierce point
	scaleMatrix(result,offset);
	float mat[16]; copymat(mat,piemat,4);
	float inv[16]; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	// TODO compose with identity if roll is Focal
	default: ERROR(cb.err,-1);}
}

void calculateGlobal()
{
	vector[0] = xmove; vector[1] = ymove; vector[2] = -1.0; offset = 0.0;
	float norvec[3];
	float pievec[3];
	// TODO get norvec and pievec from what pierce shader reported in Dma1
	normalMatrix(normat,norvec);
	fixedMatrix(piemat,pievec);
	identmat(matrix,4);
	toggle = 0;
}

enum Memory assignAffine(struct Client *client, struct Affine *affine)
{
	switch (cb.state[User]->user->matrix) {
	case (Global): cb.client->subject = affine; cb.client->idx = 0; return Subject;
	case (Several): cb.client->object = affine; cb.client->idx = object; return Object;
	case (Single): cb.client->feature = affine; cb.client->idx = 0; return Feature;
	default: ERROR(cb.err,-1);}
	return Memorys;
}

#define REJECT(MEM,FIELD,IDX) \
	mem = MEM; \
	cb.client->idx = IDX; \
	src = &cb.state[MEM]->FIELD[cb.client->idx]; \
	cb.client->FIELD = affine;
enum Memory copyAffine(struct Client *client, struct Affine *affine)
{
	struct Affine *src;
	enum Memory mem;
	switch (cb.state[User]->user->matrix) {
	case (Global): REJECT(Subject,subject,0); break;
	case (Several): REJECT(Object,object,object); break;
	case (Single): REJECT(Feature,feature,0); break;
	default: ERROR(cb.err,-1);}
	memcpy(&affine->val[0][0],src,sizeof(struct Affine));
	return mem;
}

enum Memory copyUser(struct Client *client, struct Mode *user)
{
	struct Mode *src = cb.state[User]->user;
	cb.client->user = user;
	memcpy(user,src,sizeof(struct Mode));
	return User;
}

float *clientMat(struct Client *client, int idx)
{
	switch (cb.client->mem) {
	case (Subject): return &cb.state[Subject]->subject[0].val[0][0];
	case (Object): return &cb.state[Object]->object[idx].val[0][0];
	case (Feature): return &cb.state[Feature]->feature[0].val[0][0];
	default: ERROR(exiterr,-1);}
	return 0;
}

void clientRmw0()
{
	// cb.state[idx] = client[0]*saved[idx]
	float *stat = clientMat(cb.state[cb.client->mem],cb.client->idx);
	float *save = clientMat(saved[cb.client->mem],cb.client->idx);
	float *give = clientMat(cb.client,0);
	copymat(stat,timesmat(save,give,4),4);
}

void clientRmw1()
{
	// A = B*C
	// A' = B*C'
	// B = A/C
	// A' = (A/C)*C'
	// saved[idx] = 1/saved[idx]
	// cb.state[idx] = cb.state[idx]*saved[idx]
	// saved[idx] = client[0]
	// cb.state[idx] = cb.state[idx]*client[0]
	float *save = clientMat(saved[cb.client->mem],cb.client->idx); invmat(save,4);
	float *stat = clientMat(cb.state[cb.client->mem],cb.client->idx); timesmat(stat,save,4);
	float *give = clientMat(cb.client,0); copymat(save,give,4); timesmat(stat,give,4);
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
	float *save = clientMat(saved[cb.client->mem],cb.client->idx);
	float *give0 = clientMat(cb.client,0);
	float *give1 = clientMat(cb.client,1);
	float inv[16]; invmat(copymat(inv,give0,4),4);
	jumpmat(jumpmat(save,inv,4),give1,4);
}

#define INDEXED(ENUM,FIELD) \
	if (cb.client->mem == ENUM && ptr[ENUM] && cb.client->siz < ptr[ENUM]->siz) \
	{memcpy(&ptr[ENUM]->FIELD[cb.client->idx],cb.client->FIELD,cb.client->siz*sizeof(*cb.client->FIELD)); return;}
void clientCopy(struct Client **ptr)
{
	INDEXED(Corner,corner);
	INDEXED(Triangle,triangle);
	INDEXED(Range,range);
	INDEXED(Basis,basis);
	INDEXED(Object,object);
	INDEXED(Cloud,cloud);
	allocClient(&ptr[cb.client->mem],0); ptr[cb.client->mem] = cb.client;
}

void windowProc()
{
	for (int i = 0; i < cb.client->len; i++)
	switch (cb.client->fnc[i]) {
	case (Rmw0): clientRmw0(); break;
	case (Rmw1): clientRmw1(); break;
	case (Rmw2): clientRmw2(); break;
	case (Copy): clientCopy(cb.state); break;
	case (Save): clientCopy(saved); break;
	case (Dma0): break;
	case (Dma1): break; // TODO	read feather for pierce point
	case (Draw): break;
	case (Port): break;
	default: ERROR(exiterr,-1);}
}

void windowProd()
{
	for (int i = 0; i < cb.client->len; i++)
	switch (cb.client->fnc[i]) {
	case (Rmw0): break;
	case (Rmw1): break;
	case (Rmw2): break;
	case (Copy): break;
	case (Save): break;
	case (Dma0): break;
	case (Dma1): break;
	case (Draw): break;
	case (Port): {
	struct Metric metric = {0};
	metric.src = Plane;
	metric.plane = cb.state[cb.client->mem];
	writeMetric(&metric,cb.hub);
	break;}
	default: ERROR(exiterr,-1);}
}

void *thread(void *arg)
{
	int tmp = 0;
	int gon = 1;
	while (gon) {
	for (tmp = waitAny(); tmp >= 0 && gon; tmp = waitAny()) {
	printf("thread(%d) cb.hub(%d) cb.tub(%d) cb.zub(%d)\n",tmp,cb.hub,cb.tub,cb.zub);
	if (tmp == cb.zub) gon = 0; else if (tmp >= 0) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	sub = tmp; vld = 1; cb.wake();
	if (pthread_cond_wait(&cond,&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);}}}
	return 0;
}

int windowRead()
{
	int res = 0;
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (vld) {readClient(cb.client,sub); vld = 0; res = 1;
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);}
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);
	return res;
}

void windowMove(double xpos, double ypos)
{
	xmove = xpos; ymove = ypos;
	if (cb.state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = cb.state[User]->user;
	if (user->click == Transform) {
	struct Client client;
	struct Affine affine[2];
	enum Function function[3];
	enum Function rmw = (toggle ? Rmw2 : Rmw0);
	int size = (toggle ? 2 : 1); toggle = 0;
	transformMatrix(&affine[0].val[0][0]);
	if (size > 1) composeMatrix(&affine[1].val[0][0]);
	function[0] = rmw; function[1] = Dma0; function[2] = Draw;
	client.mem = assignAffine(&client,&affine[0]);
	client.fnc = function; client.len = 3; client.siz = size;
	writeClient(&client,cb.tub);} else {
	struct Client client;
	struct Vector vector;
	enum Function function[3];
	vector.val[0] = xmove; vector.val[1] = ymove; vector.val[2] = -1.0;
	function[0] = Copy; function[1] = Dma1; function[2] = Draw;
	client.feather = &vector; client.idx = 0; client.mem = Feather;
	client.fnc = function; client.len = 3; client.siz = 1;
	writeClient(&client,cb.tub);}
}

void windowRoll(double xoffset, double yoffset)
{
	offset += yoffset*ANGLE;
	if (cb.state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = cb.state[User]->user;
	if (user->click == Transform) {
	// TODO instead of assignAffine, if user->roll is Focal
	// TODO assignArrow using recorded window size.
	struct Client client;
	struct Affine affine[2];
	enum Function function[3];
	enum Function rmw = (toggle ? Rmw0 : Rmw2);
	int size = (toggle ? 1 : 2); toggle = 1;
	if (size > 1) {transformMatrix(&affine[1].val[0][0]);
	copymat(matrix,&affine[1].val[0][0],4);}
	composeMatrix(&affine[0].val[0][0]);
	function[0] = rmw; function[1] = Dma0; function[2] = Draw;
	client.mem = assignAffine(&client,&affine[0]);
	client.fnc = function; client.len = 3; client.siz = size;
	writeClient(&client,cb.tub);}
}

void windowClick(int isright)
{
	calculateGlobal();
	struct Client client;
	struct Affine affine;
	struct Mode user;
	enum Function function[2];
	client.fnc = function;
	client.fnc[0] = Save; client.fnc[1] = Port;
	client.mem = copyAffine(&client,&affine);
	client.len = 2; client.siz = 1;
	writeClient(&client,cb.tub);
	client.fnc[0] = Copy;
	client.mem = copyUser(&client,&user);
	if (user.click == Transform) {
	if (isright) {
	user.click = Suspend; user.shader = Track;} else {
	user.click = Complete; user.shader = Track;}}
	if (user.click == Suspend) {
	user.click = Transform; user.shader = Display;
	if (isright)
	cb.warp(vector[0],vector[1]);}
	if (user.click == Complete) {
	if (!isright) {
	user.click = Transform; user.shader = Display;}}
	client.len = 1; client.siz = 1;
	writeClient(&client,cb.tub);
}

void windowSize(double width, double height)
{
	// TODO record width and height for use in windowRoll
	windowRoll(0.0,0.0);
}

void novoid()
{
}

int nofalse()
{
	return 0;
}

void nowarp(double xpos, double ypos)
{
	printf("warp %f %f\n",xpos,ypos);
}

void nosize(double width, double height)
{
	printf("size %f %f\n",width,height);
}

void nomove(double xpos, double ypos)
{
	printf("move %f %f\n",xpos,ypos);
}

void noroll(double xoffset, double yoffset)
{
	printf("roll %f %f\n",xoffset,yoffset);
}

void noclick(int isright)
{
	printf("click %d\n",isright);
}

void planeInit(int argc)
{
	cb.err = exiterr;
	cb.move = (argc == 4 ? windowMove : nomove);
	cb.roll = (argc == 4 ? windowRoll : noroll);
	cb.click = (argc == 4 ? windowClick : noclick);
	cb.size = (argc == 4 ? windowSize : nosize);
	cb.warp = nowarp;
	cb.full = nofalse;
	cb.draw = novoid;
	cb.proc = windowProc;
	cb.prod = windowProd;
	cb.read = windowRead;
	cb.call = novoid;
	cb.wake = novoid;
	cb.done = novoid;
}

void threadInit()
{
	if (pthread_mutex_init(&mutex,0) != 0) callError();
	if (pthread_cond_init(&cond,0) != 0) callError();
	if (pthread_create(&pthread,0,thread,0) != 0) callError();
}

void threadDone()
{
	if (pthread_join(pthread,0) != 0) callError();
	if (pthread_mutex_destroy(&mutex) != 0) callError();
	if (pthread_cond_destroy(&cond) != 0) callError();
}

void callError()
{
	ERROR(exiterr,-1);
}
