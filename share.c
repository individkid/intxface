/*
*    share.c
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
struct Client *client = 0;
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
float norvec[3];
float pievec[3];
int object = 0;
float render[2][3];
float pierce[2][3];

void exiterr(const char *str, int num, int arg)
{
	printf("exiterr (%s) (%d)\n",str,num); fflush(stdout);
	exit(arg);
}

double getconf(const char *str)
{
	if (strcmp(str,"WINWIDE") == 0) return WINWIDE;
	if (strcmp(str,"WINHIGH") == 0) return WINHIGH;
	if (strcmp(str,"WINDEEP") == 0) return WINDEEP;
	return 0;
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
	case (Focal): {
	identmat(result,4);
	break;}
	case (Picture): {
	identmat(result,4);
	break;}
	default: ERROR(cb.err,-1);}
}

enum Memory assignAffine(struct Client *client, struct Affine *affine)
{
	switch (cb.state[User]->user->matrix) {
	case (Global): client->subject = affine; client->idx = 0; return Subject;
	case (Several): client->object = affine; client->idx = object; return Object;
	case (Single): client->feature = affine; client->idx = 0; return Feature;
	default: ERROR(cb.err,-1);}
	return Memorys;
}

#define REJECT(MEM,FIELD,IDX) \
	mem = MEM; \
	client->idx = IDX; \
	src = &cb.state[MEM]->FIELD[client->idx]; \
	client->FIELD = affine;
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
	client->user = user;
	memcpy(user,src,sizeof(struct Mode));
	return User;
}

float *shareMat(struct Client *client, int idx)
{
	switch (client->mem) {
	case (Subject): return &cb.state[Subject]->subject[0].val[0][0];
	case (Object): return &cb.state[Object]->object[idx].val[0][0];
	case (Feature): return &cb.state[Feature]->feature[0].val[0][0];
	default: ERROR(exiterr,-1);}
	return 0;
}

void shareRmw0()
{
	// cb.state[idx] = client[0]*saved[idx]
	float *stat = shareMat(cb.state[client->mem],client->idx);
	float *save = shareMat(saved[client->mem],client->idx);
	float *give = shareMat(client,0);
	copymat(stat,timesmat(save,give,4),4);
}

void shareRmw1()
{
	// A = B*C
	// A' = B*C'
	// B = A/C
	// A' = (A/C)*C'
	// saved[idx] = 1/saved[idx]
	// cb.state[idx] = cb.state[idx]*saved[idx]
	// saved[idx] = client[0]
	// cb.state[idx] = cb.state[idx]*client[0]
	float *save = shareMat(saved[client->mem],client->idx); invmat(save,4);
	float *stat = shareMat(cb.state[client->mem],client->idx); timesmat(stat,save,4);
	float *give = shareMat(client,0); copymat(save,give,4); timesmat(stat,give,4);
}

void shareRmw2()
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
	float *save = shareMat(saved[client->mem],client->idx);
	float *give0 = shareMat(client,0);
	float *give1 = shareMat(client,1);
	float inv[16]; invmat(copymat(inv,give0,4),4);
	jumpmat(jumpmat(save,inv,4),give1,4);
}

#define INDEXED(ENUM,FIELD) \
	if (client->mem == ENUM) {\
	if (!ptr[ENUM] || client->idx+client->siz > ptr[ENUM]->siz) \
	{allocClient(&ptr[ENUM],1); \
	void *mem = malloc((client->idx+client->siz)*sizeof(*client->FIELD)); \
	memcpy(mem,ptr[ENUM]->FIELD,ptr[ENUM]->siz*sizeof(*client->FIELD)); \
	ptr[ENUM]->FIELD = mem; \
	ptr[ENUM]->siz = client->idx+client->siz;} \
	memcpy(&ptr[ENUM]->FIELD[client->idx],client->FIELD,client->siz*sizeof(*client->FIELD)); \
	return;}
void shareCopy(struct Client **ptr)
{
	INDEXED(Triangle,triangle);
	INDEXED(Corner,corner);
	INDEXED(Frame,frame);
	INDEXED(Base,base);
	INDEXED(Range,range);
	INDEXED(Active,active);
	INDEXED(Basis,basis);
	INDEXED(Subject,subject);
	INDEXED(Object,object);
	INDEXED(Feature,feature);
	INDEXED(Render,render);
	INDEXED(Pierce,pierce);
	INDEXED(Cloud,cloud);
	INDEXED(User,user);
}

void sharePierce()
{
	memcpy(pievec,client->pierce[0].val,sizeof(client->pierce[0].val));
	memcpy(norvec,client->pierce[1].val,sizeof(client->pierce[1].val));
	object = client->idx;
}

void shareMetric()
{
	struct Metric metric = {0};
	metric.src = Plane;
	metric.plane = cb.state[client->mem];
	writeMetric(&metric,cb.hub);
}

void shareRender()
{
	enum Function function[2];
	function[0] = Copy; function[1] = Draw;
	struct Vector vector[2];
	for (int i = 0; i < 3; i++) {
	vector[0].val[i] = render[0][i];
	vector[1].val[i] = render[1][i];}
	struct Client client;
	client.mem = Render;
	client.len = 2;
	client.fnc = function;
	client.idx = 0;
	client.siz = 1;
	client.render = vector;
	writeClient(&client,cb.tub);
}

void shareProc()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): shareRmw0(); break;
	case (Rmw1): shareRmw1(); break;
	case (Rmw2): shareRmw2(); break;
	case (Copy): shareCopy(cb.state); break;
	case (Save): shareCopy(saved); break;
	case (Dma0): cb.dma(client->mem); break;
	case (Dma1): sharePierce(); break;
	case (Draw): cb.draw(); break;
	case (Port): shareMetric(); break;
	default: ERROR(exiterr,-1);}
}

int shareRead()
{
	int res = 0;
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (vld) {
	if (!client) allocClient(&client,1);
	readClient(client,sub); vld = 0; res = 1;
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);}
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);
	return res;
}

void shareWrite(struct Vector *point, struct Vector *normal, int object)
{
	enum Function function[1];
	function[0] = Dma1;
	struct Vector pierce[2];
	for (int i = 0; i < 3; i++) {
	pierce[0].val[i] = point->val[i];
	pierce[1].val[i] = normal->val[i];}
	struct Client client;
	client.mem = Pierce;
	client.len = 1;
	client.fnc = function;
	client.idx = object;
	client.siz = 0;
	client.pierce = pierce;
	writeClient(&client,cb.tub);
}

void shareMove(double xpos, double ypos)
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
	enum Function function[2];
	function[0] = Copy; function[1] = Draw;
	struct Vector vector[2];
	for (int i = 0; i < 3; i++) {
	vector[0].val[i] = pierce[0][i];
	vector[1].val[i] = pierce[1][i];}
	client.pierce = vector; client.idx = 0; client.mem = Pierce;
	client.fnc = function; client.len = 2; client.siz = 2;
	writeClient(&client,cb.tub);}
}

void shareRoll(double xoffset, double yoffset)
{
	offset += yoffset*ANGLE;
	if (cb.state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = cb.state[User]->user;
	if (user->click == Transform && user->roll == Focal) {
	if (render[0][2]+yoffset*LENGTH > WINDEEP) render[0][2] += yoffset*LENGTH;
	shareRender();}
	else if (user->click == Transform && user->roll == Picture) {
	if (render[1][2]+yoffset*LENGTH > WINDEEP) render[1][2] += yoffset*LENGTH;
	shareRender();}
	else if (user->click == Transform) {
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

void shareClick(int isright)
{
	vector[0] = xmove; vector[1] = ymove; vector[2] = -1.0; offset = 0.0;
	normalMatrix(normat,norvec);
	fixedMatrix(piemat,pievec);
	identmat(matrix,4);
	toggle = 0;
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

void shareSize(double width, double height)
{
	render[1][0] = width;
	render[1][1] = height;
	shareRender();
}

void shareDrag(double xpos, double ypos)
{
	render[0][0] = xpos+render[1][0]/2.0;
	render[0][1] = ypos+render[1][1]/2.0;
	shareRender();
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

void nodma(enum Memory mem)
{
	printf("dma %d\n",mem);
}

void nosize(double width, double height)
{
	if (cb.esc == 1) printf("size %f %f\n",width,height);
}

void nodrag(double xpos, double ypos)
{
	if (cb.esc == 1) printf("drag %f %f\n",xpos,ypos);
}

void nomove(double xpos, double ypos)
{
	if (cb.esc == 1) printf("move %f %f\n",xpos,ypos);
}

void noroll(double xoffset, double yoffset)
{
	if (cb.esc == 1) printf("roll %f %f\n",xoffset,yoffset);
}

void noclick(int isright)
{
	printf("click %d\n",isright);
}

void shareInit(int argc)
{
	cb.err = exiterr;
	cb.conf = getconf;
	cb.move = (argc == 4 ? shareMove : nomove);
	cb.roll = (argc == 4 ? shareRoll : noroll);
	cb.click = (argc == 4 ? shareClick : noclick);
	cb.size = (argc == 4 ? shareSize : nosize);
	cb.drag = (argc == 4 ? shareDrag : nodrag);
	cb.write = shareWrite;
	cb.warp = nowarp;
	cb.dma = nodma;
	cb.draw = novoid;
	cb.full = nofalse;
	cb.proc = shareProc;
	cb.read = shareRead;
	cb.call = novoid;
	cb.wake = novoid;
	cb.done = novoid;
}

void shareDone()
{
}

void *threadCall(void *arg)
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

void threadInit()
{
	if (pthread_mutex_init(&mutex,0) != 0) ERROR(exiterr,-1);
	if (pthread_cond_init(&cond,0) != 0) ERROR(exiterr,-1);
	if (pthread_create(&pthread,0,threadCall,0) != 0) ERROR(exiterr,-1);
}

void threadDone()
{
	writeInt(1,cb.zub);
	if (pthread_join(pthread,0) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_destroy(&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_cond_destroy(&cond) != 0) ERROR(exiterr,-1);
}
