/*
*    model.c
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

int mub = 0;
int modesc = 0;
jmp_buf modjmp = {0};
pthread_t mthread = {0};
struct Client *accel[Memorys] = {0};
struct Client *pointer = 0;
int mface = 0;

void moderr(const char *str, int num, int arg)
{
	longjmp(modjmp,1);
}

void modelPrint(float *point, float *coord, float *color, int texid, int facid, int matid)
{
	// TODO intepolate color in point triangle, map onto ascii, and print ascii page instead of pixels
}

void constructVector(float *point, float *plane, int versor, float *basis);
void normalVector(float *normal, float *point);
int pierceVector(float *pierce, float *point, float *normal, float *feather, float *arrow);

int intersectVector(float *point, float *plane, int *versor, float *basis)
{
	// intersect three planes into one point
	float corner[27];
	for (int i = 0; i < 3; i++) constructVector(&corner[i*9],&plane[i],versor[i],basis);
	float normal[9];
	for (int i = 0; i < 3; i++) normalVector(&normal[i*3],&corner[i*9]);
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

void modelFunc(struct Array *range)
{
	float basis[27]; for (int i = 0; i < 3; i++) for (int j = 0; j < 3; j++) for (int k = 0; k < 3; k++)
	basis[i*9+j*3+k] = accel[Basis]->basis[i].val[j][k];
	for (int i = 0; i < range->siz; i++) {
		int sub[3]; for (int j = 0; j < 3; j++) sub[j] = accel[Triangle]->triangle[range->idx+i].vtxid[j];
		struct Vertex *ptr[3]; for (int j = 0; j < 3; j++) ptr[j] = &accel[Corner]->corner[sub[j]];
		float point[9]; float coord[6]; float color[8];
		int texid; int facid; int matid; float plane[3]; int versor;
		int skip = 0; for (int j = 0; j < 3; j++) {
			int found = 0; for (int k = 0; k < 3; k++) if (ptr[j]->tag[k] == range->tag) found = k;
			if (!intersectVector(&point[j*3],&ptr[j]->plane[0][0],ptr[j]->versor,basis)) {skip = 1; break;}
			for (int k = 0; k < 2; k++) coord[j*2+k] = ptr[j]->coord[found][k];
			for (int k = 0; k < 4; k++) color[j*4+k] = ptr[j]->color[found][k];
			if (j == 0) texid = ptr[j]->texid[found];
			else if (texid != ptr[j]->texid[found]) ERROR(exiterr,-1);
			if (j == 0) facid = ptr[j]->facid[found];
			else if (facid != ptr[j]->facid[found]) ERROR(exiterr,-1);
			if (j == 0) matid = ptr[j]->matid;
			else if (matid != ptr[j]->matid) ERROR(exiterr,-1);
			for (int k = 0; k < 3; k++)
				if (j == 0) plane[k] = ptr[j]->plane[found][k];
				else if (plane[k] != ptr[j]->plane[found][k]) ERROR(exiterr,-1);
			if (j == 0) versor = ptr[j]->versor[found];
			else if (versor != ptr[j]->versor[found]) ERROR(exiterr,-1);}
		if (!skip) modelPrint(point,coord,color,texid,facid,matid);}
}

#define INDEXED(ENUM,FIELD) \
	if (pointer->mem == ENUM && accel[ENUM] && pointer->siz < accel[ENUM]->siz) \
	memcpy(&accel[ENUM]->FIELD[pointer->idx],pointer->FIELD,pointer->siz*sizeof(*pointer->FIELD)); \
	else allocClient(&accel[ENUM],0); accel[ENUM] = pointer;
#define UNDEXED(ENUM) \
	allocClient(&accel[ENUM],0); accel[ENUM] = pointer;
void *model(void *arg)
{
	while (modesc == 0)
	if (setjmp(modjmp) == 0)
	while(modesc == 0) {
	allocClient(&pointer,1);
	readClient(pointer,mub);
	switch (pointer->mem) {
	case (Corner): INDEXED(Corner,corner); break;
	case (Triangle): INDEXED(Triangle,triangle); break;
	case (Range): for (int i = 0; i < pointer->siz; i++) modelFunc(&pointer->range[i]); break;
	case (Basis): INDEXED(Basis,basis); break;
	case (Subject): UNDEXED(Subject); break;
	case (Object): INDEXED(Object,object); break;
	case (Feature): UNDEXED(Feature); break;
	case (Feather): UNDEXED(Feather); break;
	case (Arrow): UNDEXED(Arrow); break;
	case (Cloud): INDEXED(Cloud,cloud); break;
	case (Hand): UNDEXED(Hand); break;
	case (Tag): UNDEXED(Tag); break;
	default: modesc = 1; break;}}
	return 0;
}

int modelInit()
{
	if ((mub = openPipe()) < 0) ERROR(exiterr,-1);
	bothJump(moderr,mub);
	if (pthread_create(&mthread,0,model,0) != 0) ERROR(exiterr,-1);
	return 1;
}

int modelFull()
{
	return 0;
}

void modelWrite(enum Memory memory)
{
	if (state[memory]->len) {
		allocFunction(&state[memory]->fnc,0);
		state[memory]->len = 0;}
	writeClient(state[memory],mub);
}

void modelDraw()
{
	while (pollPipe(mub)) {
		struct Client temp;
		readClient(&temp,mub);
		if (temp.mem == Face && state[Face])
			mface = temp.face;}
	for (int i = 0; i < client->len; i++) {
		if (client->fnc[i] == Dma0)
			modelWrite(client->mem);
		if (client->fnc[i] == Dma1)
			state[Face]->face = mface;
		if (client->fnc[i] == Draw)
			modelWrite(Range);}
}

void modelDone()
{
	struct Client temp;
	temp.len = 0; temp.siz = 0; temp.mem = Memorys;
	writeClient(&temp,mub);
	if (pthread_join(mthread,0) != 0) ERROR(exiterr,-1);
}
