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
int toggle = 0;
float xmove = 0.0;
float ymove = 0.0;
float vector[3] = {0};
float offset = 0.0;
float matrix[16] = {0};
float piemat[16] = {0};
float normat[16] = {0};
float pievec[3];
float norvec[3];
int object = 0;
float render[3][3];

void exiterr(const char *str, int num, int arg)
{
	printf("exiterr (%s) (%d)\n",str,num); fflush(stdout);
	exit(arg);
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
	default: {
	identmat(result,4);
	break;}}
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
	default: {
	identmat(result,4);
	break;}}
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

#define COPYAFFINE(MEM,FIELD,IDX) \
	mem = MEM; \
	client->idx = IDX; \
	src = &cb.state[MEM]->FIELD[client->idx]; \
	client->FIELD = affine;
enum Memory copyAffine(struct Client *client, struct Affine *affine)
{
	struct Affine *src;
	enum Memory mem;
	switch (cb.state[User]->user->matrix) {
	case (Global): COPYAFFINE(Subject,subject,0); break;
	case (Several): COPYAFFINE(Object,object,object); break;
	case (Single): COPYAFFINE(Feature,feature,0); break;
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
	client.siz = 2;
	client.pierce = pierce;
	writeClient(&client,cb.tub);
}

void shareRender()
{
	struct Mode *user = cb.state[User]->user;
	enum Function function[3];
	function[0] = Copy; function[1] = Dma0; function[2] = Gpu0;
	struct Vector vector[2];
	for (int i = 0; i < 3; i++) {
	vector[0].val[i] = render[0][i];
	vector[1].val[i] = render[1][i];}
	struct Client client;
	client.mem = Render;
	client.len = 3;
	client.fnc = function;
	client.idx = 0;
	client.siz = 2;
	client.render = vector;
	writeClient(&client,cb.tub);
}

void shareDrag(double xpos, double ypos, double width, double height)
{
	double xhalf = width/2.0;
	double yhalf = height/2.0;
	render[0][0] = xpos+xhalf;
	render[0][1] = ypos+yhalf;
	render[1][0] = xpos+width;
	render[1][1] = ypos+height;
	shareRender();
}

void shareRoll(double xoffset, double yoffset)
{
	if (yoffset == 0.0) return;
	offset += yoffset*ANGLE;
	if (cb.state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = cb.state[User]->user;
	float dif = yoffset*LENGTH;
	if (user->click == Transform && user->roll == Focal) {
	if (render[0][2]+dif > render[1][2]+MINDEEP)
	render[0][2] += dif;
	shareRender();}
	else if (user->click == Transform && user->roll == Picture) {
	if (render[0][2] > render[1][2]+dif+MINDEEP)
	render[1][2] += dif;
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
	function[0] = rmw; function[1] = Dma0; function[2] = Gpu0;
	client.mem = assignAffine(&client,&affine[0]);
	client.fnc = function; client.len = 3; client.siz = size;
	writeClient(&client,cb.tub);}
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
	function[0] = rmw; function[1] = Dma0; function[2] = Gpu0;
	client.mem = assignAffine(&client,&affine[0]);
	client.fnc = function; client.len = 3; client.siz = size;
	writeClient(&client,cb.tub);} else {
	struct Client client;
	enum Function function[3];
	function[0] = Copy; function[1] = Dma0; function[2] = Gpu1;
	struct Vector vector[2];
	vector[1].val[0] = vector[0].val[0] = render[0][0] + xmove;
	vector[1].val[1] = vector[0].val[1] = render[0][1] + ymove;
	vector[1].val[2] = render[1][2]; vector[0].val[2] = 0.0; 
	client.pierce = vector; client.idx = 0; client.mem = Pierce;
	client.fnc = function; client.len = 3; client.siz = 2;
	writeClient(&client,cb.tub);}
}

void shareClick(int isright)
{
	if (cb.state[User] == 0 || cb.state[User]->user == 0) ERROR(cb.err,-1);
	vector[0] = xmove; vector[1] = ymove; vector[2] = -1.0; offset = 0.0;
	normalMatrix(normat,norvec);
	fixedMatrix(piemat,pievec);
	identmat(matrix,4);
	toggle = 0;
	struct Client client;
	struct Affine affine;
	struct Mode user = *cb.state[User]->user;
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
	user.click = Suspend;} else {
	user.click = Complete;}}
	else if (user.click == Suspend) {
	user.click = Transform;
	if (isright)
	cb.warp(vector[0],vector[1]);}
	else if (user.click == Complete) {
	if (!isright) {
	user.click = Transform;}}
	client.len = 1; client.siz = 1;
	writeClient(&client,cb.tub);
}

float *procMat(struct Client *client, int idx)
{
	switch (client->mem) {
	case (Subject): return &client->subject[idx].val[0][0];
	case (Object): return &client->object[idx].val[0][0];
	case (Feature): return &client->feature[idx].val[0][0];
	default: ERROR(cb.err,-1);}
	return 0;
}

void procRmw0() // continuation of move or roll
{
	if (cb.state[client->mem] == 0) ERROR(cb.err,-1);
	if (saved[client->mem] == 0) ERROR(cb.err,-1);
	// A = B*C
	// cb.state[idx] = client[0]*saved[idx]
	float *stat = procMat(cb.state[client->mem],client->idx);
	float *save = procMat(saved[client->mem],client->idx);
	float *give = procMat(client,0);
	copymat(stat,timesmat(save,give,4),4);
}

void procRmw1() // from outside parallel since last Rmw1 or Save
{
	if (cb.state[client->mem] == 0) ERROR(cb.err,-1);
	if (saved[client->mem] == 0) ERROR(cb.err,-1);
	// A = B*C
	// A' = B*C'
	// B = A/C
	// A' = (A/C)*C'
	// saved[idx] = 1/saved[idx]
	// cb.state[idx] = cb.state[idx]*saved[idx]
	// saved[idx] = client[0]
	// cb.state[idx] = cb.state[idx]*client[0]
	float *save = procMat(saved[client->mem],client->idx); invmat(save,4);
	float *stat = procMat(cb.state[client->mem],client->idx); timesmat(stat,save,4);
	float *give = procMat(client,0); copymat(save,give,4); timesmat(stat,give,4);
}

void procRmw2() // transition between move and roll
{
	if (saved[client->mem] == 0) ERROR(cb.err,-1);
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
	float *save = procMat(saved[client->mem],client->idx);
	float *give0 = procMat(client,0);
	float *give1 = procMat(client,1);
	float inv[16]; invmat(copymat(inv,give0,4),4);
	jumpmat(jumpmat(save,inv,4),give1,4);
}

#define PROCCOPY(ENUM,FIELD,TYPE) \
	if (client->mem == ENUM) {\
	if (!ptr[ENUM]) \
	{allocClient(&ptr[ENUM],1); \
	ptr[ENUM]->mem = ENUM;} \
	if (client->idx+client->siz > ptr[ENUM]->siz) { \
	void *mem = ptr[ENUM]->FIELD; \
	alloc##TYPE(&ptr[ENUM]->FIELD,client->idx+client->siz); \
	if (mem) { \
	memcpy(ptr[ENUM]->FIELD,mem,ptr[ENUM]->siz*sizeof(*client->FIELD));} \
	ptr[ENUM]->siz = client->idx+client->siz;} \
	memcpy(&ptr[ENUM]->FIELD[client->idx],client->FIELD,client->siz*sizeof(*client->FIELD)); \
	/*if (ENUM == Render) printf("Copy Render %d %d\n",client->idx,client->siz);*/ \
	return;}
void procCopy(struct Client **ptr)
{
	PROCCOPY(Triangle,triangle,Facet);
	PROCCOPY(Corner,corner,Vertex);
	PROCCOPY(Frame,frame,Int);
	PROCCOPY(Base,base,Int);
	PROCCOPY(Range,range,Array);
	PROCCOPY(Active,active,Array);
	PROCCOPY(Basis,basis,Linear);
	PROCCOPY(Subject,subject,Affine);
	PROCCOPY(Object,object,Affine);
	PROCCOPY(Feature,feature,Affine);
	PROCCOPY(Render,render,Vector);
	PROCCOPY(Pierce,pierce,Vector);
	PROCCOPY(Cloud,cloud,Vector);
	PROCCOPY(User,user,Mode);
	ERROR(cb.err,-1);
}

void procPierce()
{
	memcpy(pievec,client->pierce[0].val,sizeof(pievec));
	memcpy(norvec,client->pierce[1].val,sizeof(norvec));
	object = client->idx;
}

void procMetric()
{
	if (cb.state[client->mem] == 0) NOTICE(cb.err,-1);
	struct Metric metric = {0};
	metric.src = Plane;
	metric.plane = cb.state[client->mem];
	if (cb.hub >= 0) {
	writeMetric(&metric,cb.hub);}
}

void shareProc()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): if (cb.esc && 0) printf("Rmw0\n"); procRmw0(); break;
	case (Rmw1): if (cb.esc && 0) printf("Rmw1\n"); procRmw1(); break;
	case (Rmw2): if (cb.esc && 0) printf("Rmw2\n"); procRmw2(); break;
	case (Copy): if (cb.esc && 0) printf("Copy\n"); procCopy(cb.state); break;
	case (Save): if (cb.esc && 0) printf("Save\n"); procCopy(saved); break;
	case (Dma0): if (cb.esc && 0) printf("Dma0\n"); cb.dma(client->mem,client->idx,1); break;
	case (Dma1): if (cb.esc && 0) printf("Dma1\n"); procPierce(); break;
	case (Dma2): if (cb.esc && 0) printf("Dma2\n"); cb.dma(client->mem,client->idx,client->siz); break;
	case (Gpu0): if (cb.esc && 0) printf("Gpu0\n"); cb.draw(Display); break;
	case (Gpu1): if (cb.esc && 0) printf("Gpu1\n"); cb.draw(Track); break;
	case (Port): if (cb.esc && 0) printf("Port\n"); procMetric(); break;
	default: ERROR(cb.err,-1);}
}

int shareRead()
{
	int res = 0;
	if (pthread_mutex_lock(&mutex) != 0) ERROR(cb.err,-1);
	if (vld) {
	if (!client) allocClient(&client,1);
	readClient(client,sub); vld = 0; res = 1;
	if (pthread_cond_signal(&cond) != 0) ERROR(cb.err,-1);}
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(cb.err,-1);
	return res;
}

int argc = 0;
char **argv = 0;
void shareArg(const char *arg)
{
	argc++;
	argv = realloc(argv,argc*sizeof(*argv));
	argv[argc-1] = malloc(strlen(arg)+1);
	strcpy(argv[argc-1],arg);
}

void shareInit()
{
	cb.err = exiterr;
	cb.move = shareMove;
	cb.roll = shareRoll;
	cb.click = shareClick;
	cb.drag = shareDrag;
	cb.write = shareWrite;
	cb.proc = shareProc;
	cb.read = shareRead;
	if (argc == 4) {
	cb.hub = pipeInit(argv[1],argv[2]);
	if (cb.hub < 0) {
	ERROR(cb.err,-1);}
	bothJump(cb.err,cb.hub);}
	else cb.hub = -1;
	cb.zub = openPipe();
	cb.tub = openPipe();
	if (cb.zub < 0 || cb.tub < 0) {
	ERROR(cb.err,-1);}
	bothJump(cb.err,cb.zub);
	bothJump(cb.err,cb.tub);
	cb.esc = 0;
    struct Client client = {0};
    enum Function function[2] = {0}; client.fnc = function;
    client.len = 1; client.siz = 0; function[0] = Copy;
    for (client.mem = 0; client.mem < Memorys; client.mem++) {
	writeClient(&client,cb.tub);}
    struct Mode mode = {0}; mode.matrix = Global;
    mode.click = Complete; mode.move = Moves; mode.roll = Rolls;
    client.siz = 1; client.user = &mode; client.mem = User;
	writeClient(&client,cb.tub);
	struct Affine affine = {0}; identmat(&affine.val[0][0],4);
	client.subject = &affine; client.mem = Subject;
	writeClient(&client,cb.tub);
	client.feature = &affine; client.mem = Feature;
	writeClient(&client,cb.tub);
	struct Linear linear = {0};
	linear.val[1][1] = linear.val[2][2] = 1.0;
	linear.val[4][2] = linear.val[5][0] = 1.0;
	linear.val[7][0] = linear.val[8][1] = 1.0;
	client.basis = &linear; client.mem = Basis;
	writeClient(&client,cb.tub);
	double xpos = cb.conf(PictureMinX);
	double ypos = cb.conf(PictureMinY);
	double width = xpos+cb.conf(PictureMaxX);
	double height = ypos+cb.conf(PictureMaxY);
	double xmax = cb.conf(ScreenMaxX);
	double ymax = cb.conf(ScreenMaxY);
	double xhalf = width/2.0;
	double yhalf = height/2.0;
	render[0][0] = xpos+xhalf;
	render[0][1] = ypos+yhalf;
	render[0][2] = 2*WINDEEP;
	render[1][0] = xpos+width;
	render[1][1] = ypos+height;
	render[1][2] = WINDEEP;
	render[2][0] = xmax;
	render[2][1] = ymax;
	render[2][2] = 0.0;
	struct Vector vector[2] = {0};
	for (int i = 0; i < 2; i++) {
	for (int j = 0; j < 2; j++) {
	vector[i].val[j] = render[i][j];}}
	client.siz = 2; client.render = vector; client.mem = Render;
	writeClient(&client,cb.tub);
	vector[1].val[0] = vector[0].val[0] = render[0][0];
	vector[1].val[1] = vector[0].val[1] = render[0][1];
	vector[1].val[2] = render[1][2]; vector[0].val[2] = 0.0; 
	client.pierce = vector; client.mem = Pierce;
	writeClient(&client,cb.tub);
	function[0] = Gpu0; function[1] = Gpu1;
	client.len = 2; client.siz = 0;
	writeClient(&client,cb.tub);
}

void shareDone()
{
	printf("shareDone\n");
}

void *threadCall(void *arg)
{
	int tmp = 0;
	int gon = 1;
	while (gon) {
	for (tmp = waitAny(); tmp >= 0 && gon; tmp = waitAny()) {
	if (tmp == cb.zub) gon = 0; else if (tmp >= 0) {
	if (pthread_mutex_lock(&mutex) != 0) ERROR(cb.err,-1);
	sub = tmp; vld = 1; cb.wake();
	if (pthread_cond_wait(&cond,&mutex) != 0) ERROR(cb.err,-1);
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(cb.err,-1);}}}
	return 0;
}

void threadInit()
{
	if (pthread_mutex_init(&mutex,0) != 0) ERROR(cb.err,-1);
	if (pthread_cond_init(&cond,0) != 0) ERROR(cb.err,-1);
	if (pthread_create(&pthread,0,threadCall,0) != 0) ERROR(cb.err,-1);
}

void threadDone()
{
	writeInt(1,cb.zub);
	while (shareRead());
	if (pthread_join(pthread,0) != 0) ERROR(cb.err,-1);
	if (pthread_mutex_destroy(&mutex) != 0) ERROR(cb.err,-1);
	if (pthread_cond_destroy(&cond) != 0) ERROR(cb.err,-1);
	printf("threadDone\n");
}
