/*
*    glfw.c
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
#include <CoreGraphics/CoreGraphics.h>

float xmove = 0.0;
float ymove = 0.0;
float offset = 0.0;
int toggle = 0;
float vector[3] = {0};
float matrix[16] = {0};
float piemat[16] = {0};
float normat[16] = {0};
int tope = 0;

void longitudeMatrix(float *result, float *vector)
{
}

void latitudeMatrix(float *result, float *vector)
{
}

void translateMatrix(float *result, float *vector)
{
	copyvec(identmat(result,4)+12,vector,3);
}

void angleMatrix(float *result, float angle)
{
}

void lengthMatrix(float *result, float angle)
{
}

void scaleMatrix(float *result, float angle)
{
}

void normalMatrix(float *result, float *normal)
{
}

void fixedMatrix(float *result, float *pierce)
{
}

void offsetVector(float *result)
{
	struct Mode *user = state[User]->user;
	float cur[3] = {0}; cur[0] = xmove; cur[1] = ymove; cur[2] = -1.0;
	float pix[3] = {0}; pix[2] = -1.0;
	for (int i = 0; i < 2; i++) pix[i] = vector[i];
	plusvec(copyvec(result,cur,3),scalevec(pix,-1.0,3),3);
}

void transformMatrix(float *result)
{
	struct Mode *user = state[User]->user;
	switch (user->move) {
	case (Rotate): { // rotate about fixed pierce point
	float vec[3] = {0}; offsetVector(vec);
	float lon[16] = {0}; longitudeMatrix(lon,vec);
	latitudeMatrix(result,vec);
	float mat[16] = {0}; jumpmat(copymat(mat,piemat,4),lon,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Slide): { // translate parallel to fixed facet
	float vec[3] = {0}; offsetVector(vec);
	translateMatrix(result,vec);
	float mat[16] = {0}; copymat(mat,normat,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Slate): { // translate parallel to picture plane
	float vec[3] = {0}; offsetVector(vec);
	translateMatrix(result,vec);
	break;}
	default: ERROR(huberr,-1);}
}

void composeMatrix(float *result)
{
	float mat[16] = {0};
	struct Mode *user = state[User]->user;
	switch (user->roll) {
	case (Cylinder): { // rotate with rotated fixed axis
	angleMatrix(result,offset);
	float mat[16] = {0}; jumpmat(copymat(mat,piemat,4),matrix,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Clock): { // rotate with fixed normal to picture plane
	angleMatrix(result,offset);
	float mat[16] = {0}; copymat(mat,piemat,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Compass): { // rotate with fixed normal to facet
	angleMatrix(result,offset);
	float mat[16] = {0}; jumpmat(jumpmat(copymat(mat,piemat,4),normat,4),matrix,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Normal): { // translate with fixed normal to facet
	lengthMatrix(result,offset);
	float mat[16] = {0}; copymat(mat,normat,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	case (Balloon): { // scale with fixed pierce point
	scaleMatrix(result,offset);
	float mat[16] = {0}; copymat(mat,piemat,4);
	float inv[16] = {0}; invmat(copymat(inv,mat,4),4);
	timesmat(jumpmat(result,mat,4),inv,4);
	break;}
	default: ERROR(huberr,-1);}
}

void constructVector(float *point, float *plane, int *versor, float *basis)
{
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	for (int k = 0; k < 3; k++)
	point[i*9+j*3+k] = basis[versor[i]*9+j*3+k];
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	point[i*9+j*3+versor[i]] = plane[i*3+j];
}

void transformVector(float *point, float *matrix)
{
	jumpmat(point,matrix,3);
}

void normalVector(float *normal, float *point)
{
	float neg[3] = {0};
	float leg0[3] = {0};
	float leg1[3] = {0};
	scalevec(copyvec(neg,point,3),-1.0,3);
	plusvec(copyvec(leg0,point+3,3),neg,3);
	plusvec(copyvec(leg1,point+6,3),neg,3);
	normvec(crossvec(copyvec(normal,leg0,3),leg1),3);
}

void pierceVector(float *pierce, float *normal, float *feather, float *arrow)
{
	float delta[3] = {0};
	scalevec(copyvec(delta,arrow,3),dotvec(arrow,normal,3),3);
	plusvec(copyvec(pierce,feather,3),delta,3);
}

void calculateGlobal()
{
	vector[0] = xmove; vector[1] = ymove; vector[2] = -1.0; offset = 0.0;
	int face = state[Face]->face;
	int found = state[Range]->siz;
	for (int i = 0; i < found; i++) {
	struct Array *range = state[Range]->range;
	if (face < range[i].idx+range[i].siz && range[i].idx <= face) found = i;}
	if (found == state[Range]->siz) {
	// TODO use original piere normal tope face
	return;}
	int tag = state[Range]->range[found].tag;
	struct Facet *facet = &state[Triangle]->triangle[face];
	struct Vertex *vertex[3] = {0};
	for (int i = 0; i < 3; i++) vertex[i] = &state[Corner]->corner[facet->vtxid[i]];
	float plane[3][3] = {0};
	int versor[3] = {0};
	int done = 0;
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	if (vertex[i]->tag[j] == tag) {
	for (int k = 0; k < 3; k++) plane[done][k] = vertex[i]->plane[j][k];
	versor[done] = vertex[i]->versor[j];
	if (done && tope != vertex[i]->matid) ERROR(huberr,-1);
	if (!done) tope = vertex[i]->matid;
	done++;}
	if (done != 3) ERROR(huberr,-1);
	float point[3][3] = {0};
	constructVector(&point[0][0],&plane[0][0],&versor[0],&state[Basis]->basis[0].val[0][0]);
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Subject]->subject->val[0][0]);
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Object]->object[tope].val[0][0]);
	if (face==state[Hand]->hand)
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Feature]->feature->val[0][0]);
	float norvec[3] = {0}; normalVector(norvec,&point[0][0]);
	float pievec[3] = {0}; pierceVector(pievec,norvec,&point[0][0],&state[Arrow]->arrow->val[0]);
	normalMatrix(normat,norvec);
	fixedMatrix(piemat,pievec);
	identmat(matrix,4);
	toggle = 0;
}

enum Memory assignAffine(struct Client *client, struct Affine *affine)
{
	switch (state[User]->user->matrix) {
	case (Global): client->subject = affine; client->idx = 0; return Subject;
	case (Several): client->object = affine; client->idx = tope; return Object;
	case (Single): client->feature = affine; client->idx = 0; return Feature;
	default: ERROR(huberr,-1);}
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
	case (Several): REJECT(Object,object,tope); break;
	case (Single): REJECT(Feature,feature,0); break;
	default: ERROR(huberr,-1);}
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

void warpCursor(struct GLFWwindow* ptr, double xpos, double ypos)
{
    int xloc, yloc;
    glfwGetWindowPos(window,&xloc,&yloc);
    struct CGPoint point; point.x = xloc+xpos; point.y = yloc+ypos;
    CGWarpMouseCursorPosition(point);
}

void displayKey(struct GLFWwindow* ptr, int key, int scancode, int action, int mods)
{
	if (action == 1) printf("GLFW key %d %d %d %d\n",key,scancode,action,mods);
	if (key == 256 && action == 1) {if (esc == 0) esc = 1;}
	else if (key == 257 && action == 1) {if (esc == 1) esc = 2;}
	else if (action == 1) esc = 0;
}

void displayMove(struct GLFWwindow* ptr, double xpos, double ypos)
{
	xmove = xpos; ymove = ypos;
	if (state[User] == 0) ERROR(huberr,-1);
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

void displayRoll(struct GLFWwindow* ptr, double xoffset, double yoffset)
{
	offset += yoffset*ANGLE;
	if (state[User] == 0) ERROR(huberr,-1);
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

void displayClick(struct GLFWwindow* ptr, int button, int action, int mods)
{
	calculateGlobal();
	struct Client client;
	struct Affine affine;
	enum Function function[2];
	struct Mode user;
	function[0] = Save; function[1] = Port;
	client.mem = copyAffine(&client,&affine);
	client.fnc = function; client.len = 2; client.siz = 1;
	writeClient(&client,tub);
	function[0] = Copy;
	client.mem = copyUser(&client,&user);
	if (action == GLFW_PRESS && user.click == Transform) {
	if (button == GLFW_MOUSE_BUTTON_RIGHT) user.click = Suspend;
	else user.click = Complete;}
	if (action == GLFW_PRESS && user.click == Suspend) {
	user.click = Transform;
	if (button == GLFW_MOUSE_BUTTON_RIGHT)
	warpCursor(ptr,vector[0],vector[1]);}
	if (action == GLFW_PRESS && user.click == Complete) {
	if (button == GLFW_MOUSE_BUTTON_LEFT) user.click = Transform;}
	client.fnc = function; client.len = 1; client.siz = 1;
	writeClient(&client,tub);
}

void windowInit(int argc, char **argv)
{
	glfwInit();
	glfwWindowHint(GLFW_SAMPLES, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE); // To make MacOS happy; should not be needed
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);
	const char *name = (argc == 4 ? argv[3] : argv[0]);
	if ((window = glfwCreateWindow(WINWIDE, WINHIGH, name, 0, 0)) == 0) ERROR(exiterr,-1);
	glfwSetKeyCallback(window, displayKey);
	glfwSetCursorPosCallback(window, displayMove);
	glfwSetScrollCallback(window, displayRoll);
	glfwSetMouseButtonCallback(window, displayClick);
	glfwMakeContextCurrent(window);
}

void windowDone()
{
	glfwDestroyWindow(window);
	glfwTerminate();
}
