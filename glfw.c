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

/*
float *rotateMatrix(float *matrix, float *from, float *to)
{
	float minus[2]; scalevec(copyvec(minus,from,2),-1.0,2);
	float delta[2]; plusvec(copyvec(delta,to,2),minus,2);
	float angle[2]; angle[0] = -sqrtf(delta[0]*delta[0]+delta[1]*delta[1]); angle[1] = 1.0;
	float xmat[9]; float zmat[9]; float zinv[9];
	scalevec(angle,1.0/sqrtf(dotvec(angle,angle,2)),2);
	xmat[0] = 1.0; xmat[1] = xmat[2] = 0.0;
	xmat[3] = 0.0; xmat[4] = angle[1]; xmat[5] = -angle[0];
	xmat[6] = 0.0; xmat[7] = angle[0]; xmat[8] = angle[1];
	float denom = sqrtf(dotvec(delta,delta,2));
	if (absval(denom*INVALID) >= 1.0) {
	scalevec(delta,1.0/denom,2);
	zmat[0] = delta[1]; zmat[1] = -delta[0]; zmat[2] = 0.0;
	zmat[3] = delta[0]; zmat[4] = delta[1]; zmat[5] = 0.0;
	zmat[6] = 0.0; zmat[7] = 0.0; zmat[8] = 1.0;
	zinv[0] = delta[1]; zinv[1] = delta[0]; zinv[2] = 0.0;
	zinv[3] = -delta[0]; zinv[4] = delta[1]; zinv[5] = 0.0;
	zinv[6] = 0.0; zinv[7] = 0.0; zinv[8] = 1.0;
	return timesmat(timesmat(copymat(matrix,zmat,3),xmat,3),zinv,3);} else {
	return copymat(matrix,xmat,3);}
}

float *fixedMatrix(float *matrix)
{
	float before[16]; float after[16]; identmat(before,4); identmat(after,4);
	before[3] = -pointer->pierce[0]; before[7] = -pointer->pierce[1]; before[11] = -pointer->pierce[2];
	after[3] = pointer->pierce[0]; after[7] = pointer->pierce[1]; after[11] = pointer->pierce[2];
	return jumpmat(timesmat(matrix,before,4),after,4);
}

float *angleMatrix(float *matrix)
{
	float angle = ANGLE*pointer->roller;
	float sinval = sinf(angle); float cosval = cosf(angle);
	matrix[0] = cosval; matrix[1] = -sinval; matrix[2] = 0.0;
	matrix[3] = sinval; matrix[4] = cosval; matrix[5] = 0.0;
	matrix[6] = 0.0; matrix[7] = 0.0; matrix[8] = 1.0;
	return matrix;
}

float *tangentPoint(float *point)
{
	float *normal = pointer->normal;
	float *pierce = pointer->pierce;
	float minus[2]; scalevec(copyvec(minus,pierce,2),-1.0,2);
	float diff[2]; plusvec(copyvec(diff,point,2),minus,2);
	float abs = absval(normal[2]);
	float dot = dotvec(diff,normal,2);
	if (abs < 1.0 && absval(dot) > INVALID*abs) return zerovec(point,3);
	float proj[3]; copyvec(proj,diff,2); proj[2] = -dot/normal[2];
	float length = sqrtf(dotvec(diff,diff,2));
	float len = sqrtf(dotvec(proj,proj,3));
	if (len < 1.0 && length > INVALID*len) return zerovec(point,3);
	float adjust = length/len;
	return scalevec(copyvec(point,proj,3),adjust,3);
	// length = sqrt(diff[0]*diff[0]+diff[1]*diff[1])
	// proj[0]*normal[0] + proj[1]*normal[1] + proj[2]*normal[2] = 0
	// proj[0] = diff[0] proj[1] = diff[1]
	// adjust*sqrt(proj[0]*proj[0]+proj[1]*proj[1]+proj[2]*proj[2]) = length
	// point = adjust*proj
}

float *mouseMatrix(float *matrix)
{
	switch (mode.mouse) {
	case (RotateMode): {
	float inverse[9]; rotateMatrix(inverse,pointer->point,pointer->pierce);
	float rotate[9]; timesmat(rotateMatrix(rotate,pointer->pierce,pointer->cursor),inverse,3);
	identmat(matrix,4); copyary(matrix,rotate,3,4,9);
	return fixedMatrix(matrix);}
	case (TangentMode): {
	float point[3]; tangentPoint(copyvec(point,pointer->point,2));
	float cursor[3]; tangentPoint(copyvec(cursor,pointer->cursor,2));
	float translate[16]; identmat(translate,4); translate[3] = cursor[0]-point[0];
	translate[7] = cursor[1]-point[1]; translate[11] = cursor[2]-point[2];
	return copymat(matrix,translate,4);}
	case (TranslateMode): {
	float translate[16]; identmat(translate,4);
	translate[3] = pointer->cursor[0]-pointer->point[0];
	translate[7] = pointer->cursor[1]-pointer->point[1];
	translate[11] = 0.0;
	return copymat(matrix,translate,4);}
	default: displayError(mode.mouse,"invalid mode.mouse");}
	return matrix;
}

float *rollerMatrix(float *matrix)
{
	switch (mode.roller) {
	case (CylinderMode): {
	float inverse[9]; rotateMatrix(inverse,pointer->point,pointer->pierce);
	float rotate[9]; rotateMatrix(rotate,pointer->pierce,pointer->point);
	float zmat[9]; angleMatrix(zmat);
	timesmat(timesmat(rotate,zmat,3),inverse,3);
	identmat(matrix,4); copyary(matrix,rotate,3,4,9);
	return fixedMatrix(matrix);}
	case (ClockMode): {
	float zmat[9]; angleMatrix(zmat);
	identmat(matrix,4); copyary(matrix,zmat,3,4,9);
	return fixedMatrix(matrix);}
	case (NormalMode): {
	float zero[2]; zerovec(zero,2);
	int sub = 0; float min = INVALID;
	for (int i = 0; i < 3; i++) if (pointer->normal[i] < min) {sub = i; min = pointer->normal[i];}
	float norm[2]; int j = 0; for (int i = 0; i < 3; i++) if (i != sub) norm[j++] = pointer->normal[i];
	float inverse[9]; rotateMatrix(inverse,norm,zero);
	float rotate[9]; rotateMatrix(rotate,zero,norm);
	float zmat[9]; angleMatrix(zmat);
	timesmat(timesmat(rotate,zmat,3),inverse,3);
	identmat(matrix,4); copyary(matrix,rotate,3,4,9);
	return fixedMatrix(matrix);}
	case (ParallelMode): {
	float length = LENGTH*pointer->roller/sqrtf(dotvec(pointer->normal,pointer->normal,3));
	float offset[3]; scalevec(copyvec(offset,pointer->normal,3),length,3);
	float translate[16]; identmat(translate,4);
	translate[3] = offset[0]; translate[7] = offset[1]; translate[11] = offset[2];
	return copymat(matrix,translate,4);}
	case (ScaleMode): {
	scalevec(identmat(matrix,4),powf(BASE,pointer->roller),16);
	return fixedMatrix(matrix);}
	default: displayError(mode.roller,"invalid mode.roller");}
	return matrix;
}

void affineMatrix(int file, float *affine)
{
	switch (mode.target) {case(SessionMode): if (mode.toggle == 0)
	timesmat(timesmat(mouseMatrix(affine),matrix.session,4),matrix.polytope[file],4); else
	timesmat(timesmat(rollerMatrix(affine),matrix.session,4),matrix.polytope[file],4); break;
	case (PolytopeMode): if (file == matrix.file) {if (mode.toggle == 0)
	timesmat(timesmat(mouseMatrix(affine),matrix.polytope[file],4),matrix.session,4); else
	timesmat(timesmat(rollerMatrix(affine),matrix.polytope[file],4),matrix.session,4);} else
	timesmat(timesmat(identmat(affine,4),matrix.session,4),matrix.polytope[file],4); break;
	case (FacetMode): timesmat(timesmat(identmat(affine,4),matrix.session,4),matrix.polytope[file],4); break;
	default: displayError(mode.target,"invalid mode.target");}
}
*/

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
	struct Affine *mat = 0; allocAffine(&mat,1);
	if (state[Mode1] == 0) ERROR(huberr,-1);
	if (state[Mode1]->click == Transform) {
	if (state[Mode2] == 0 || state[Mode2] == 0) ERROR(huberr,-1);
	if (state[Pierced] == 0 || state[Moved] == 0) ERROR(huberr,-1);
	switch (state[Mode2]->move) {
	case (Rotate): /*rotateMatrix(&mat->val,state[Pierced]->val,state[Moved]->val,pos)*/ break;
	case (Tangent): /*tangentMatrix(&mat->val,state[Pierced]->val,state[Moved]->val,pos)*/ break;
	case (Translate): /*translateMatrix(&mat->val,state[Pierced]->val,state[Moved]->val,pos)*/ break;
	default: ERROR(huberr,-1);}
	switch (state[Mode0]->matrix) {
	case (Global): client.mem = Subject; client.subject = mat; break;
	case (Several): client.mem = Object; client.object = mat; client.idx = state[Tope]->tope; break;
	case (Single): client.mem = Feature; client.feature = mat; break;
	default: ERROR(huberr,-1);}
	client.siz = 1; client.len = 3; allocInt(&client.fnc,3);
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
