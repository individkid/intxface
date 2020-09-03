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
#include <stdarg.h>

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
float pievec[3] = {0};
float norvec[3] = {0};
int object = 0;
float render[3][3] = {0};

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
	result[0*4+0] = vec[0];
	result[0*4+1] = vec[1];
	result[1*4+0] = -vec[1];
	result[1*4+1] = vec[0];
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
	float cur[3]; cur[0] = xmove; cur[1] = ymove;
	cur[2] = vector[2];
	float pix[3]; pix[2] = 0.0;
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
	float mat[16]; timesmat(copymat(mat,piemat,4),lon,4);
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

enum Memory shareMemory(enum Matrix matrix)
{
	switch (matrix) {
	case (Global): return Subject;
	case (Several): return Object;
	case (Single): return Feature;
	default: ERROR(cb.err,-1);}
	return Memorys;
}

int shareIndex(enum Matrix matrix)
{
	switch (matrix) {
	case (Global): return 0;
	case (Several): return object;
	case (Single): return 0;
	default: ERROR(cb.err,-1);}
	return 0;
}

struct Affine *shareAffine(enum Matrix matrix)
{
	switch (matrix) {
	case (Global): return cb.state[Subject]->subject;
	case (Several): return cb.state[Object]->object+object;
	case (Single): return cb.state[Feature]->feature;
	default: ERROR(cb.err,-1);}
	return 0;
}

enum Click shareMachine(enum Click click, int isright)
{
	if (click == Transform && isright) {click = Suspend;}
	else if (click == Transform) {click = Complete;}
	else if (click == Suspend) {click = Transform;}
	else if (click == Complete && !isright) {click = Transform;}
	return click;
}

#define SHARECLIENT0(REC,PTR) \
	REC.fnc = PTR; \
	REC.mem = mem; \
	REC.idx = idx; \
	REC.siz = siz; \
	REC.len = len; \
    switch (mem) {
#define SHARECLIENT1(MEM,REC,FLD,PTR) \
	case (MEM): REC.FLD = PTR; break;
#define SHARECLIENT2 \
	default: ERROR(cb.err,-1);}
void shareClient(enum Memory mem, int idx, int siz, int len, ...)
{
    va_list args;
    va_start(args, len);
	enum Function function[len];
	struct Client client;
	SHARECLIENT0(client,function);
	SHARECLIENT1(Triangle,client,triangle,va_arg(args,struct Facet *));
	SHARECLIENT1(Corner,client,corner,va_arg(args,struct Vertex *));
	SHARECLIENT1(Frame,client,frame,va_arg(args,struct Index *));
	SHARECLIENT1(Base,client,base,va_arg(args,int *));
	SHARECLIENT1(Range,client,range,va_arg(args,struct Array *));
	SHARECLIENT1(Active,client,active,va_arg(args,struct Array *));
	SHARECLIENT1(Basis,client,basis,va_arg(args,struct Linear *));
	SHARECLIENT1(Subject,client,subject,va_arg(args,struct Affine *));
	SHARECLIENT1(Object,client,object,va_arg(args,struct Affine *));
	SHARECLIENT1(Feature,client,feature,va_arg(args,struct Affine *));
	SHARECLIENT1(Render,client,render,va_arg(args,struct Vector *));
	SHARECLIENT1(Pierce,client,pierce,va_arg(args,struct Vector *));
	SHARECLIENT1(Cloud,client,cloud,va_arg(args,struct Vector *));
	SHARECLIENT1(User,client,user,va_arg(args,struct Mode *));
	SHARECLIENT1(Process,client,process,va_arg(args,struct Client *));
	SHARECLIENT2;
	for (int i = 0; i < len; i++) {
	function[i] = va_arg(args,enum Function);}
	writeClient(&client,cb.tub);
    va_end(args);
}
#define SHARECLIENS(TYP,NAM,MEM,FLD) \
void client##NAM(enum Memory mem, int idx, int siz, int len, TYP *ptr, enum Function *fnc) \
{ \
	struct Client client; \
	SHARECLIENT0(client,fnc); \
	SHARECLIENT1(MEM,client,FLD,ptr); \
	SHARECLIENT2; \
	writeClient(&client,cb.tub); \
}
#define SHARECLIENSS(TYP,NAM,MEM0,FLD0,MEM1,FLD1) \
void client##NAM(enum Memory mem, int idx, int siz, int len, TYP *ptr, enum Function *fnc) \
{ \
	struct Client client; \
	SHARECLIENT0(client,fnc); \
	SHARECLIENT1(MEM0,client,FLD0,ptr); \
	SHARECLIENT1(MEM1,client,FLD1,ptr); \
	SHARECLIENT2; \
	writeClient(&client,cb.tub); \
}
#define SHARECLIENSSS(TYP,NAM,MEM0,FLD0,MEM1,FLD1,MEM2,FLD2) \
void client##NAM(enum Memory mem, int idx, int siz, int len, TYP *ptr, enum Function *fnc) \
{ \
	struct Client client; \
	SHARECLIENT0(client,fnc); \
	SHARECLIENT1(MEM0,client,FLD0,ptr); \
	SHARECLIENT1(MEM1,client,FLD1,ptr); \
	SHARECLIENT1(MEM2,client,FLD2,ptr); \
	SHARECLIENT2; \
	writeClient(&client,cb.tub); \
}
SHARECLIENS(struct Facet,Facet,Triangle,triangle)
SHARECLIENS(struct Vertex,Vertex,Corner,corner)
SHARECLIENS(struct Index,Index,Frame,frame)
SHARECLIENS(int,Int,Base,base)
SHARECLIENSS(struct Array,Array,Range,range,Active,active)
SHARECLIENS(struct Linear,Linear,Basis,basis)
SHARECLIENSSS(struct Affine,Affine,Subject,subject,Object,object,Feature,feature)
SHARECLIENSSS(struct Vector,Vector,Render,render,Pierce,pierce,Cloud,cloud)
SHARECLIENS(struct Mode,Mode,User,user)
SHARECLIENS(struct Client,Client,Process,process)
#define SHARECLIENT(TYP,NAM,MEM,FLD) \
void atomic##NAM(enum Memory mem, int idx, int siz, int len, TYP *ptr, enum Function *fnc, \
	int num, struct Client *ary, void (*func)(int num, struct Client *ptr)) \
{ \
	struct Client client[num+1]; \
	memcpy(client,ary,num*sizeof(struct Client)); \
	SHARECLIENT0(client[num],fnc); \
	SHARECLIENT1(MEM,client[num],FLD,ptr); \
	SHARECLIENT2; \
	func(num+1,client); \
}
#define SHARECLIENTT(TYP,NAM,MEM0,FLD0,MEM1,FLD1) \
void atomic##NAM(enum Memory mem, int idx, int siz, int len, TYP *ptr, enum Function *fnc, \
	int num, struct Client *ary, void (*func)(int num, struct Client *ptr)) \
{ \
	struct Client client[num+1]; \
	memcpy(client,ary,num*sizeof(struct Client)); \
	SHARECLIENT0(client[num],fnc); \
	SHARECLIENT1(MEM0,client[num],FLD0,ptr); \
	SHARECLIENT1(MEM1,client[num],FLD1,ptr); \
	SHARECLIENT2; \
	func(num+1,client); \
}
#define SHARECLIENTTT(TYP,NAM,MEM0,FLD0,MEM1,FLD1,MEM2,FLD2) \
void atomic##NAM(enum Memory mem, int idx, int siz, int len, TYP *ptr, enum Function *fnc, \
	int num, struct Client *ary, void (*func)(int num, struct Client *ptr)) \
{ \
	struct Client client[num+1]; \
	memcpy(client,ary,num*sizeof(struct Client)); \
	SHARECLIENT0(client[num],fnc); \
	SHARECLIENT1(MEM0,client[num],FLD0,ptr); \
	SHARECLIENT1(MEM1,client[num],FLD1,ptr); \
	SHARECLIENT1(MEM2,client[num],FLD2,ptr); \
	SHARECLIENT2; \
	func(num+1,client); \
}
SHARECLIENT(struct Facet,Facet,Triangle,triangle)
SHARECLIENT(struct Vertex,Vertex,Corner,corner)
SHARECLIENT(struct Index,Index,Frame,frame)
SHARECLIENT(int,Int,Base,base)
SHARECLIENTT(struct Array,Array,Range,range,Active,active)
SHARECLIENT(struct Linear,Linear,Basis,basis)
SHARECLIENTTT(struct Affine,Affine,Subject,subject,Object,object,Feature,feature)
SHARECLIENTTT(struct Vector,Vector,Render,render,Pierce,pierce,Cloud,cloud)
SHARECLIENT(struct Mode,Mode,User,user)
SHARECLIENT(struct Client,Client,Process,process)

void shareWrite(struct Vector *point, struct Vector *normal, int object)
{
	struct Vector vector[2];
	for (int i = 0; i < 3; i++) {
	vector[0].val[i] = point->val[i];
	vector[1].val[i] = normal->val[i];}
	shareClient(Pierce,object,2,1,vector,Dma1);
}

void shareRender()
{
	struct Vector vector[2];
	for (int i = 0; i < 3; i++) {
	vector[0].val[i] = render[0][i];
	vector[1].val[i] = render[1][i];}
	shareClient(Render,0,2,3,vector,Copy,Dma0,Gpu0);
}

void sharePierce()
{
	struct Vector vector[2];
	vector[1].val[0] = vector[0].val[0] = xmove - render[1][0];
	vector[1].val[1] = vector[0].val[1] = ymove - render[1][1];
	vector[1].val[2] = 0.0; vector[0].val[2] = render[1][2];
	float ratio = (vector[1].val[2]-render[0][2])/(render[1][2]-render[0][2]);
	vector[1].val[0] *= ratio;
	vector[1].val[1] *= ratio;
	shareClient(Pierce,0,2,3,vector,Copy,Dma0,Gpu1);
}

void shareDrag(double xmid, double ymid, double xmax, double ymax)
{
	render[0][0] = xmid;
	render[0][1] = ymid;
	render[1][0] = xmax;
	render[1][1] = ymax;
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
	if (render[0][2]+dif > render[1][2]+cb.conf(DefaultStop))
	render[0][2] += dif;
	shareRender();}
	else if (user->click == Transform && user->roll == Picture) {
	if (render[0][2] > render[1][2]+dif+cb.conf(DefaultStop))
	render[1][2] += dif;
	shareRender();}
	if (user->click == Transform) {
	struct Affine affine[2]; toggle = 1;
	transformMatrix(&affine[1].val[0][0]);
	copymat(matrix,&affine[1].val[0][0],4);
	composeMatrix(&affine[0].val[0][0]);
	timesmat(&affine[0].val[0][0],&affine[1].val[0][0],4);
	shareClient(shareMemory(user->matrix),shareIndex(user->matrix),1,3,affine,Rmw0,Dma0,Gpu0);}
}

void shareMove(double xpos, double ypos)
{
	xmove = xpos; ymove = ypos;
	if (cb.state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = cb.state[User]->user;
	if (user->click == Transform) {
	struct Affine affine[2]; toggle = 1;
	transformMatrix(&affine[1].val[0][0]);
	copymat(matrix,&affine[1].val[0][0],4);
	composeMatrix(&affine[0].val[0][0]);
	timesmat(&affine[0].val[0][0],&affine[1].val[0][0],4);
	shareClient(shareMemory(user->matrix),shareIndex(user->matrix),1,3,affine,Rmw0,Dma0,Gpu0);}
	else sharePierce();
}

void shareClick(int isright)
{
	if (cb.state[User] == 0 || cb.state[User]->user == 0) ERROR(cb.err,-1);
	struct Mode user = *cb.state[User]->user;
	if (user.click == Suspend && isright) cb.warp(vector[0],vector[1]);
	vector[0] = xmove; vector[1] = ymove; vector[2] = cb.conf(LeverDeep);
	normalMatrix(normat,norvec);
	fixedMatrix(piemat,pievec);
	identmat(matrix,4);
	offset = 0.0;
	toggle = 0;
	shareClient(shareMemory(user.matrix),shareIndex(user.matrix),1,2,shareAffine(user.matrix),Save,Port);
	user.click = shareMachine(user.click,isright);
	shareClient(User,0,1,1,&user,Copy);
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
	jumpmat(copymat(stat,give,4),save,4);
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
	// B*C = B'*C'
	// C' = (1/B')*B*C
	// C = saved[idx]
	// B = client[1]
	// B' = client[0]
	float *save = procMat(saved[client->mem],client->idx);
	float *give0 = procMat(client,0);
	float *give1 = procMat(client,1);
	float inv[16]; invmat(copymat(inv,give0,4),4);
	jumpmat(jumpmat(save,give1,4),inv,4);
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
	return;}
void procCopy(struct Client **ptr)
{
	PROCCOPY(Triangle,triangle,Facet);
	PROCCOPY(Corner,corner,Vertex);
	PROCCOPY(Frame,frame,Index);
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
	PROCCOPY(Process,process,Client);
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

void shareProc();
void procAtom()
{
	struct Client *ptr = client;
	for (int i = 0; i < ptr->siz; i++)
	if (ptr->idx+i < cb.state[Process]->siz) {
	client = &cb.state[Process]->process[ptr->idx+i];
	shareProc();}
	client = ptr;
}

void shareProc()
{
	for (int i = 0; i < client->len; i++)
	switch (client->fnc[i]) {
	case (Rmw0): procRmw0(); break;
	case (Rmw1): procRmw1(); break;
	case (Rmw2): procRmw2(); break;
	case (Copy): procCopy(cb.state); break;
	case (Save): procCopy(saved); break;
	case (Dma0): cb.dma(client->mem,client->idx,1); break;
	case (Dma1): procPierce(); break;
	case (Dma2): cb.dma(client->mem,client->idx,client->siz); break;
	case (Gpu0): cb.draw(Display); break;
	case (Gpu1): cb.draw(Track); break;
	case (Port): procMetric(); break;
	case (Atom): procAtom(); break;
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
    for (enum Memory mem = 0; mem < Memorys; mem++) {
    shareClient(mem,0,0,1,0,Copy);}
    struct Mode mode = {0}; mode.matrix = Global;
    mode.click = Complete; mode.move = Rotate; mode.roll = Cylinder;
    shareClient(User,0,1,2,&mode,Copy,Dma0);
	struct Affine affine = {0}; identmat(&affine.val[0][0],4);
	shareClient(Subject,0,1,2,&affine,Copy,Dma0);
	shareClient(Object,0,1,2,&affine,Copy,Dma0);
	shareClient(Feature,0,1,2,&affine,Copy,Dma0);
	double wide = cb.conf(DefaultWide); double high = cb.conf(DefaultHigh);
	double xpos = -wide/2.0; double ypos = -high/2.0; double zpos = 0.0;
	double deep = cb.conf(DefaultDeep); double leng = cb.conf(DefaultLong);
	double xmax = cb.conf(ScreenWide); double ymax = cb.conf(ScreenHigh);
	double xhalf = wide/2.0; double yhalf = high/2.0;
	struct Linear linear = {0};
	linear.val[1][1] = linear.val[2][2] = xhalf/2.0;
	linear.val[4][2] = linear.val[5][0] = xhalf/2.0;
	linear.val[7][0] = linear.val[8][1] = xhalf/2.0;
	shareClient(Basis,0,1,2,&linear,Copy,Dma0);
	render[0][0] = xpos+xhalf; render[0][1] = ypos+yhalf; render[0][2] = zpos+leng;
	render[1][0] = xpos+wide; render[1][1] = ypos+high; render[1][2] = zpos+deep;
	render[2][0] = xmax; render[2][1] = ymax; render[2][2] = 0.0;
	sharePierce(); shareRender();
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
