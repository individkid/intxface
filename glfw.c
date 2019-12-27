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
float matrix[16] = {0};
float inverse[16] = {0};

void rotateMatrix(float *result, float *pierce, float *pixel, float *cursor)
{
	float neg[3] = {0}; scalevec(copyvec(neg,pierce,3),-1.0,3);
	float arm0[3] = {0}; normvec(plusvec(copyvec(arm0,pixel,3),neg,3),3);
	float arm1[3] = {0}; normvec(plusvec(copyvec(arm1,cursor,3),neg,3),3);
	float leg0[2] = {0}; leg0[0] = sqrtf(dotvec(arm1,arm1,2)); leg0[1] = arm1[2]; normvec(leg0,2);
	float leg1[2] = {0}; normvec(copyvec(leg1,arm1,2),2);
	float ang0 = asinf(leg0[0]); // angle in the yz plane
	float ang1 = asinf(leg1[0]); // angle in the xy plane
	float mat0[9] = {0}; identmat(mat0,3);
	mat0[4] = cosf(ang0); mat0[5] = -sinf(ang0);
	mat0[7] = sinf(ang0); mat0[8] = cosf(ang0);
	float mat1[9] = {0}; identmat(mat1,3);
	mat1[0] = cosf(ang1); mat1[1] = -sinf(ang1);
	mat1[3] = sinf(ang1); mat1[4] = cosf(ang1);
	float mat2[9] = {0}; invmat(copymat(mat2,mat1,3),3);
	timesmat(timesmat(mat2,mat0,3),mat1,3); // mat = (1/A)*B*A
	float rot0[16] = {0}; identmat(rot0,4); copyary(rot0,mat2,3,4,9);
	float rot1[16] = {0}; identmat(rot1,4); copyvec(rot1+12,pierce,3);
	float rot2[16] = {0}; invmat(copymat(rot2,rot1,4),4);
	timesmat(timesmat(rot2,rot0,4),rot1,4); // rot = (1/A)*B*A
	copymat(result,rot2,4);
}

void slideMatrix(float *result, float *normal, float *pixel, float *cursor)
{
	float neg[3] = {0}; scalevec(copyvec(neg,pixel,3),-1.0,3);
	float vec[3] = {0}; plusvec(copyvec(vec,cursor,3),neg,3);
	plusvec(vec,scalevec(copyvec(neg,normal,3),-dotvec(vec,normal,3),3),3);
	copyvec(identmat(result,4)+12,vec,3);
}

void slateMatrix(float *result, float *pixel, float *cursor)
{
	float neg[3] = {0}; scalevec(copyvec(neg,pixel,3),-1.0,3);
	float vec[3] = {0}; plusvec(copyvec(vec,cursor,3),neg,3);
	copyvec(identmat(result,4)+12,vec,3);
}

void cylinderMatrix(float *result, float roller)
{
}

void clockMatrix(float *result, float roller)
{
}

void compassMatrix(float *result, float roller)
{
}

void normalMatrix(float *result, float roller)
{
}

void balloonMatrix(float *result, float roller)
{
}

void transformMatrix(float *result)
{
	struct Mode *user = state[User]->user;
	float cur[3] = {0}; cur[0] = xmove; cur[1] = ymove; cur[2] = -1.0;
	float pix[3] = {0}; pix[2] = -1.0;
	for (int i = 0; i < 2; i++) pix[i] = user->cursor.val[i];
	switch (user->move) {
	case (Rotate): { // rotate about fixed pierce point
	float pie[3] = {0};
	for (int i = 0; i < 3; i++) pie[i] = user->pierce.val[i];
	rotateMatrix(result,pie,pix,cur);
	break;}
	case (Slide): { // translate parallel to fixed facet
	float nor[3] = {0};
	for (int i = 0; i < 3; i++) nor[i] = user->normal.val[i];
	slideMatrix(result,nor,pix,cur);
	break;}
	case (Slate): // translate parallel to picture plane
	slateMatrix(result,pix,cur);
	break;
	default: ERROR(huberr,-1);}
}

void composeMatrix(float *result)
{
	struct Mode *user = state[User]->user;
	switch (user->roll) {
	case (Cylinder): // rotate with rotated fixed axis
	cylinderMatrix(result,offset); break;
	case (Clock): // rotate with fixed normal to picture plane
	clockMatrix(result,offset); break;
	case (Compass): // rotate with fixed normal to facet
	compassMatrix(result,offset); break;
	case (Normal): // translate with fixed normal to facet
	normalMatrix(result,offset); break;
	case (Balloon): // scale with fixed pierce point
	balloonMatrix(result,offset); break;
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

void assignAffine(struct Client *client, struct Affine *matrix)
{
	struct Mode *user = state[User]->user;
	switch (user->matrix) {
	case (Global): client->mem = Subject; client->subject = matrix; break;
	case (Several): client->mem = Object; client->object = matrix;
	client->idx = user->tope; break;
	case (Single): client->mem = Feature; client->feature = matrix; break;
	default: ERROR(huberr,-1);}
}

#define REJECT(MEM,FIELD,IDX) \
	client->mem = MEM; \
	client->idx = IDX; \
	src = &state[MEM]->FIELD[client->idx]; \
	client->FIELD = matrix;
void copyAffine(struct Client *client, struct Affine *matrix)
{
	struct Mode *user = state[User]->user;
	struct Affine *src = 0;
	if (state[User] == 0) ERROR(huberr,-1);
	switch (user->matrix) {
	case (Global): REJECT(Subject,subject,0); break;
	case (Several): REJECT(Object,object,user->tope); break;
	case (Single): REJECT(Feature,feature,0); break;
	default: ERROR(huberr,-1);}
	memcpy(matrix,src,sizeof(struct Affine));
}

void copyUser(struct Client *client, struct Mode *user)
{
	client->mem = User; *(client->user = user) = *(state[User]->user);
	user->cursor.val[0] = xmove; user->cursor.val[1] = ymove; offset = 0.0;
	user->face = state[Face]->face;
	int found = state[Range]->siz;
	for (int i = 0; i < found; i++) {
	struct Array *range = state[Range]->range;
	if (user->face < range[i].idx+range[i].siz && range[i].idx <= user->face) found = i;}
	if (found == state[Range]->siz) {
	// TODO use original piere normal tope face
	return;}
	int tag = state[Range]->range[found].tag;
	struct Facet *facet = &state[Triangle]->triangle[user->face];
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
	if (done && user->tope != vertex[i]->matid) ERROR(huberr,-1);
	if (!done) user->tope = vertex[i]->matid;
	done++;}
	if (done != 3) ERROR(huberr,-1);
	float point[3][3] = {0};
	constructVector(&point[0][0],&plane[0][0],&versor[0],&state[Basis]->basis[0].val[0][0]);
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Subject]->subject->val[0][0]);
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Object]->object[user->tope].val[0][0]);
	if (user->face==state[Hand]->hand)
	for (int i = 0; i < 3; i++)
	transformVector(&point[i][0],&state[Feature]->feature->val[0][0]);
	normalVector(&user->normal.val[0],&point[0][0]);
	pierceVector(&user->pierce.val[0],&user->normal.val[0],
	&state[Feather]->feather->val[0],&state[Arrow]->arrow->val[0]);
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
	enum Function rmw = (toggle ? Rmw2 : Rmw0);
	int size = (toggle ? 2 : 1); toggle = 0;
	struct Affine *matrix = 0; allocAffine(&matrix,size);
	transformMatrix(&matrix[0].val[0][0]);
	if (size > 1) composeMatrix(&matrix[1].val[0][0]);
	struct Client client = {0}; assignAffine(&client,matrix); client.siz = 1; client.len = 3;
	allocFunction(&client.fnc,3); client.fnc[0] = rmw; client.fnc[1] = Dma0; client.fnc[2] = Draw;
	writeClient(&client,tub); freeClient(&client);} else {
	struct Client client = {0}; client.mem = Memorys; client.siz = 0; client.len = 2;
	allocFunction(&client.fnc,2); client.fnc[0] = Dma1; client.fnc[1] = Draw;
	writeClient(&client,tub); freeClient(&client);}
}

void displayRoll(struct GLFWwindow* ptr, double xoffset, double yoffset)
{
	offset += yoffset*ANGLE;
	if (!toggle) transformMatrix(matrix);
	if (!toggle) invmat(copymat(inverse,matrix,4),4);
}

void displayWarp(struct GLFWwindow* ptr, double xpos, double ypos)
{
    int xloc, yloc;
    glfwGetWindowPos(window,&xloc,&yloc);
    struct CGPoint point; point.x = xloc+xpos; point.y = yloc+ypos;
    CGWarpMouseCursorPosition(point);
}

void displayClick(struct GLFWwindow* ptr, int button, int action, int mods)
{
	struct Affine *matrix = 0; allocAffine(&matrix,1); 
	struct Client client = {0}; copyAffine(&client,matrix); client.siz = 1; client.len = 2;
	allocFunction(&client.fnc,2); client.fnc[0] = Save; client.fnc[1] = Port;
	writeClient(&client,tub); freeClient(&client);
	struct Mode *user = 0; allocMode(&user,1);
	copyUser(&client,user); client.siz = 1; client.len = 1;
	allocFunction(&client.fnc,1); client.fnc[0] = Copy;
	if (action == GLFW_PRESS && user->click == Transform) {
	if (button == GLFW_MOUSE_BUTTON_RIGHT) user->click = Suspend;
	else user->click = Complete;}
	if (action == GLFW_PRESS && user->click == Suspend) {
	user->click = Transform;
	if (button == GLFW_MOUSE_BUTTON_RIGHT)
	displayWarp(ptr,user->cursor.val[0],user->cursor.val[1]);}
	if (action == GLFW_PRESS && user->click == Complete) {
	if (button == GLFW_MOUSE_BUTTON_LEFT) user->click = Transform;}
	writeClient(&client,tub); freeClient(&client);
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
