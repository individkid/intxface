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

#include "plane.h"
#include <pthread.h>
#include <sys/ioctl.h>
#include <CoreGraphics/CoreGraphics.h>

int vld = 0;
int sub = 0;
int hub = 0;
int tub = 0;
int zub = 0;
int esc = 0;
pthread_mutex_t mutex = {0};
pthread_cond_t cond = {0};
pthread_t pthread = {0};
struct Client *client = 0;
struct Client *state[Memorys] = {0};
struct Client *saved[Memorys] = {0};
void *refer[Memorys] = {0};
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
	struct Mode *user = state[User]->user;
	float cur[3]; cur[0] = xmove; cur[1] = ymove; cur[2] = -1.0;
	float pix[3]; pix[2] = -1.0;
	for (int i = 0; i < 2; i++) pix[i] = vector[i];
	plusvec(copyvec(result,cur,3),scalevec(pix,-1.0,3),3);
}

void transformMatrix(float *result)
{
	struct Mode *user = state[User]->user;
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
	struct Mode *user = state[User]->user;
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
	default: ERROR(cb.err,-1);}
}

void calculateGlobal()
{
	vector[0] = xmove; vector[1] = ymove; vector[2] = -1.0; offset = 0.0;
	int face = state[Face]->face;
	int found = state[Range]->siz;
	for (int i = 0; i < found; i++) {
	struct Array *range = state[Range]->range;
	if (face < range[i].idx+range[i].siz && range[i].idx <= face) found = i;}
	float norvec[3];
	float pievec[3];
	if (found == state[Range]->siz) {
	object = 0;
	unitvec(norvec,3,2);
	zerovec(pievec,3);} else {
	int tag = state[Range]->range[found].tag;
	struct Facet *facet = &state[Triangle]->triangle[face];
	struct Vertex *vertex[3];
	for (int i = 0; i < 3; i++) vertex[i] = &state[Corner]->corner[facet->vtxid[i]];
	float plane[3];
	int versor;
	int done = 0;
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	if (vertex[i]->tag[j] == tag) {
	if (done == 0) for (int k = 0; k < 3; k++) plane[k] = vertex[i]->plane[j][k];
	if (done == 0) versor = vertex[i]->versor[j];
	if (done && object != vertex[i]->matid) ERROR(cb.err,-1);
	if (!done) object = vertex[i]->matid;
	done++;}
	if (done != 3) ERROR(cb.err,-1);
	float point[3][3];
	constructVector(&point[0][0],&plane[0],versor,&state[Basis]->basis[0].val[0][0]);
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Subject]->subject->val[0][0]);
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Object]->object[object].val[0][0]);
	if (face==state[Hand]->hand)
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Feature]->feature->val[0][0]);
	normalVector(norvec,&point[0][0]);
	float other[3]; plusvec(copyvec(other,&state[Feather]->feather->val[0],3),&state[Arrow]->arrow->val[0],3);
	pierceVector(pievec,&point[0][0],norvec,&state[Feather]->feather->val[0],other);}
	normalMatrix(normat,norvec);
	fixedMatrix(piemat,pievec);
	identmat(matrix,4);
	toggle = 0;
}

enum Memory assignAffine(struct Client *client, struct Affine *affine)
{
	switch (state[User]->user->matrix) {
	case (Global): client->subject = affine; client->idx = 0; return Subject;
	case (Several): client->object = affine; client->idx = object; return Object;
	case (Single): client->feature = affine; client->idx = 0; return Feature;
	default: ERROR(cb.err,-1);}
	return Memorys;
}

#define REJECT(MEM,FIELD,IDX) \
	mem = MEM; \
	client->idx = IDX; \
	src = &state[MEM]->FIELD[client->idx]; \
	client->FIELD = affine;
enum Memory copyAffine(struct Client *client, struct Affine *affine)
{
	struct Affine *src;
	enum Memory mem;
	switch (state[User]->user->matrix) {
	case (Global): REJECT(Subject,subject,0); break;
	case (Several): REJECT(Object,object,object); break;
	case (Single): REJECT(Feature,feature,0); break;
	default: ERROR(cb.err,-1);}
	memcpy(&affine->val[0][0],src,sizeof(struct Affine));
	return mem;
}

enum Memory copyUser(struct Client *client, struct Mode *user)
{
	struct Mode *src = state[User]->user;
	client->user = user;
	memcpy(user,src,sizeof(struct Mode));
	return User;
}

int callread()
{
	int res = 0;
	if (pthread_mutex_lock(&mutex) != 0) ERROR(exiterr,-1);
	if (vld) {readClient(client,sub); vld = 0; res = 1;
	if (pthread_cond_signal(&cond) != 0) ERROR(exiterr,-1);}
	if (pthread_mutex_unlock(&mutex) != 0) ERROR(exiterr,-1);
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

void windowWarp(double xpos, double ypos)
{
    int xloc, yloc;
    cb.pos(&xloc,&yloc);
    struct CGPoint point; point.x = xloc+xpos; point.y = yloc+ypos;
    CGWarpMouseCursorPosition(point);
}

void windowPos(int *xloc, int *yloc)
{
    *xloc = *yloc = 0;
}

void windowSize(int *width, int *height)
{
	*width = *height = 0;
}

void windowKey(int key)
{
	printf("GLFW key %d\n",key);
	if (key == 256) {if (esc == 0) esc = 1;}
	else if (key == 257) {if (esc == 1) esc = 2;}
	else esc = 0;
}

void windowMove(double xpos, double ypos)
{
	xmove = xpos; ymove = ypos;
	if (state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = state[User]->user;
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
	writeClient(&client,tub);} else {
	struct Client client;
	struct Vector vector;
	enum Function function[3];
	vector.val[0] = xmove; vector.val[1] = ymove; vector.val[2] = -1.0;
	function[0] = Copy; function[1] = Dma1; function[2] = Draw;
	client.feather = &vector; client.idx = 0; client.mem = Feather;
	client.fnc = function; client.len = 3; client.siz = 1;
	writeClient(&client,tub);}
}

void windowRoll(double xoffset, double yoffset)
{
	offset += yoffset*ANGLE;
	if (state[User] == 0) ERROR(cb.err,-1);
	struct Mode *user = state[User]->user;
	if (user->click == Transform) {
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
	writeClient(&client,tub);}
}

void windowClick(int isright)
{
	calculateGlobal();
	struct Client client;
	struct Affine affine;
	struct Mode user;
	enum Function function[2];
	function[0] = Save; function[1] = Port;
	client.mem = copyAffine(&client,&affine);
	client.fnc = function; client.len = 2; client.siz = 1;
	writeClient(&client,tub);
	function[0] = Copy;
	client.mem = copyUser(&client,&user);
	if (user.click == Transform) {
	if (isright) {
	user.click = Suspend; user.shader = Track;} else {
	user.click = Complete; user.shader = Track;}}
	if (user.click == Suspend) {
	user.click = Transform; user.shader = Display;
	if (isright)
	windowWarp(vector[0],vector[1]);}
	if (user.click == Complete) {
	if (!isright) {
	user.click = Transform; user.shader = Display;}}
	client.fnc = function; client.len = 1; client.siz = 1;
	writeClient(&client,tub);
}

void nocall()
{
}

int callfalse()
{
	return 0;
}

int main(int argc, char **argv)
{
	struct ttysize ts;
	ioctl(0, TIOCGSIZE, &ts);
	printf("uint32_t(%d) int(%d) GL_INT(%d) float(%d) GL_FLOAT(%d) lines(%d) columns(%d)\n",
	(int)sizeof(uint32_t),(int)sizeof(int),(int)sizeof(GL_INT),(int)sizeof(float),(int)sizeof(GL_FLOAT),
	ts.ts_lines,ts.ts_cols);

	cb.err = exiterr;
	cb.pos = windowPos;
	cb.size = windowSize;
	cb.key = windowKey;
	cb.move = windowMove;
	cb.roll = windowRoll;
	cb.click = windowClick;
	cb.full = callfalse;
	cb.read = (argc == 4 ? callread : callfalse);
	cb.proc = process;
	cb.draw = nocall;
	cb.prod = produce;
	cb.call = nocall;
	cb.swap = nocall;
	cb.done = nocall;

	if (argc == 4) {sub = -1;
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	if ((zub = openPipe()) < 0) ERROR(exiterr,-1);
	if ((tub = openPipe()) < 0) ERROR(exiterr,-1);
	if (pthread_mutex_init(&mutex,0) != 0) ERROR(exiterr,-1);
	if (pthread_cond_init(&cond,0) != 0) ERROR(exiterr,-1);
	if (pthread_create(&pthread,0,thread,0) != 0) ERROR(exiterr,-1);}

	if (metalInit(argc,argv) ||
	(displayInit(argc,argv) &&
	(vulkanInit() ||
	openglInit() ||
	modelInit()))) {
	if (argc == 4) {
	bothJump(cb.err,hub);
	bothJump(cb.err,zub);
	bothJump(cb.err,tub);}
	cb.call();}

	if (argc == 4) {writeInt(1,zub);
	if (pthread_join(pthread,0) != 0) ERROR(exiterr,-1);
	if (pthread_mutex_destroy(&mutex) != 0) ERROR(exiterr,-1);
	if (pthread_cond_destroy(&cond) != 0) ERROR(exiterr,-1);}

	return 0;
}
