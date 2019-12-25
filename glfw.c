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

void tangentMatrix(float *result, float *normal, float *pixel, float *cursor)
{
}

void translateMatrix(float *result, float *pixel, float *cursor)
{
	float neg[3] = {0}; scalevec(copyvec(neg,pixel,3),-1.0,3);
	float vec[3] = {0}; plusvec(copyvec(vec,cursor,3),neg,3);
	copyvec(identmat(result,4)+12,vec,3);
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
	struct Client client = {0};
	struct Affine *mat = 0;
	float *res = 0;
	float pie[3] = {0};
	float nor[3] = {0};
	float pix[3] = {0};
	float cur[3] = {0};
	if (state[Mode1] == 0) ERROR(huberr,-1);
	if (state[Mode1]->click == Transform) {
	if (state[Mode2] == 0 || state[Mode0] == 0) ERROR(huberr,-1);
	allocAffine(&mat,1); res = &mat->val[0][0];
	if (state[Pixel] == 0) ERROR(huberr,-1);
 	for (int i = 0; i < 2; i++) pix[i] = state[Pixel]->pixel->val[i]; pix[2] = -1.0;
	cur[0] = xpos; cur[1] = ypos; cur[2] = -1.0;
	switch (state[Mode2]->move) {
	case (Rotate):
	if (state[Pierce] == 0) ERROR(huberr,-1);
 	for (int i = 0; i < 3; i++) pie[i] = state[Pierce]->pierce->val[i];
	rotateMatrix(res,pie,pix,cur);
	break;
	case (Slide):
	if (state[Normal] == 0) ERROR(huberr,-1);
 	for (int i = 0; i < 3; i++) nor[i] = state[Normal]->normal->val[i];
	tangentMatrix(res,nor,pix,cur);
	break;
	case (Slate):
	translateMatrix(res,pix,cur);
	break;
	default: ERROR(huberr,-1);}
	switch (state[Mode0]->matrix) {
	case (Global): client.mem = Subject; client.subject = mat; break;
	case (Several): client.mem = Object; client.object = mat; client.idx = state[Tope]->tope; break;
	case (Single): client.mem = Feature; client.feature = mat; break;
	default: ERROR(huberr,-1);}
	client.siz = 1; client.len = 3; allocFunction(&client.fnc,3);
	client.fnc[0] = Rmw0; client.fnc[1] = Dma0; client.fnc[2] = Draw;
	writeClient(&client,tub);} else {
	// TODO call pierce shader and update state[Pierced]
	}
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
	glfwMakeContextCurrent(window);
}

void windowDone()
{
	glfwDestroyWindow(window);
	glfwTerminate();
}
