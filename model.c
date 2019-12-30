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
#include <sys/ioctl.h>

int mub = 0;
int mesc = 0;
jmp_buf mjmp = {0};
pthread_t mthread = {0};
struct Client *accel[Memorys] = {0};
struct ttysize ts;
char *mpage;
float *mframe;

void moderr(const char *str, int num, int arg)
{
	longjmp(mjmp,1);
}

void modelClear()
{
	for (int i = 0; i < ts.ts_cols; i++)
	for (int j = 0; j < ts.ts_lines; j++) {
	mframe[i*ts.ts_cols+j] = INFINITE;
	mpage[i*ts.ts_cols+j] = ' ';}
}

void modelFrame(float *point, float *coord, float *color, int texid)
{
	float mat3[9];
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 2; j++)
	mat3[i*3+j] = point[i*3+j];
	for (int i = 0; i < 3; i++)
	mat3[i*3+2] = 1.0;
	invmat(mat3,3);
	float mat4[16];
	for (int i = 0; i < 3; i++)
	for (int j = 0; j < 3; j++)
	mat4[i*4+j] = point[i*3+j];
	for (int j = 0; j < 3; j++)
	mat4[12+j] = 0.0;
	for (int i = 0; i < 4; i++)
	mat4[i*4+3] = 1.0;
	invmat(mat4,4);
	float depth[3]; for (int i = 0; i < 3; i++) depth[i] = point[i*3+2];
	float shade[3]; for (int i = 0; i < 3; i++) shade[i] = color[i*4+3];
	float offset[2]; offset[0] = ts.ts_cols/-2.0; offset[1] = ts.ts_lines/-2.0;
	for (int i = 0; i < ts.ts_lines; i++)
	for (int j = 0; j < ts.ts_cols; j++) {
	float pos[3]; pos[0] = j*CHRHIGH; pos[1] = i*CHRWIDE; pos[2] = 1.0; plusvec(pos,offset,2);
	float vec[3]; copyvec(vec,pos,3); jumpvec(pos,mat3,3); vec[2] = dotvec(pos,depth,3);
	float ipos[4]; copyvec(ipos,vec,3); ipos[3] = 1.0; jumpvec(ipos,mat4,4);
	float ichr = dotvec(ipos,shade,3);
	char chr; if (ichr < 0.0) chr = '-'; else if (ichr >= 1.0) chr = '+'; else chr = '0'+(char)(ichr*10.0);
	if (mframe[i*ts.ts_cols+j] > vec[2]) {mframe[i*ts.ts_cols+j] = vec[2]; mpage[i*ts.ts_cols+j] = chr;}}
}

void modelPrint()
{
	for (int i = 0; i < ts.ts_lines; i++) {
	for (int j = 0; j < ts.ts_cols; j++)
	printf("%c",mpage[i*ts.ts_cols+j]);
	printf("\n");}
}

void modelTrack(float *point, int facid)
{
	float normal[3]; float other[3]; float pierce[3]; normalVector(normal,point);
	plusvec(copyvec(other,&accel[Feather]->feather->val[0],3),&accel[Arrow]->arrow->val[0],3);
	if (pierceVector(&pierce[0],point,normal,&accel[Feather]->feather->val[0],other)) {
	struct Client client = {0}; client.mem = Face; client.face = facid; writeClient(&client,mub);}
}

void modelFunc(struct Array *range)
{
	float basis[27]; for (int i = 0; i < 3; i++) for (int j = 0; j < 3; j++) for (int k = 0; k < 3; k++)
	basis[i*9+j*3+k] = accel[Basis]->basis[i].val[j][k];
	for (int i = 0; i < range->siz; i++) {
		int sub[3]; for (int j = 0; j < 3; j++) sub[j] = accel[Triangle]->triangle[range->idx+i].vtxid[j];
		struct Vertex *ptr[3]; for (int j = 0; j < 3; j++) ptr[j] = &accel[Corner]->corner[sub[j]];
		float point[9]; float coord[6]; float color[12];
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
		if (skip) continue;
		if (accel[User]->user->shader == Track) modelTrack(point,facid);
		else modelFrame(point,coord,color,texid);}
}

#define INDEXED(ENUM,FIELD) \
	if (client->mem == ENUM && accel[ENUM] && client->siz < accel[ENUM]->siz) \
	memcpy(&accel[ENUM]->FIELD[client->idx],client->FIELD,client->siz*sizeof(*client->FIELD)); \
	else allocClient(&accel[ENUM],0); accel[ENUM] = client;
#define UNDEXED(ENUM) \
	allocClient(&accel[ENUM],0); accel[ENUM] = client;
void *model(void *arg)
{
	struct Client *client = 0;
	struct Client *pend = 0;
	while (mesc == 0)
	if (setjmp(mjmp) == 0)
	while(mesc == 0) {
	while (pollPipe(mub)) {
	allocClient(&client,1);
	readClient(client,mub);
	switch (client->mem) {
	case (Corner): INDEXED(Corner,corner); break;
	case (Triangle): INDEXED(Triangle,triangle); break;
	case (Range): allocClient(&pend,0); pend = client; client = 0; break;
	case (Basis): INDEXED(Basis,basis); break;
	case (Subject): UNDEXED(Subject); break;
	case (Object): INDEXED(Object,object); break;
	case (Feature): UNDEXED(Feature); break;
	case (Feather): UNDEXED(Feather); break;
	case (Arrow): UNDEXED(Arrow); break;
	case (Cloud): INDEXED(Cloud,cloud); break;
	case (Hand): UNDEXED(Hand); break;
	case (Tag): UNDEXED(Tag); break;
	default: mesc = 1; break;}}
	if (pend) {
	if (accel[Basis] && accel[Triangle] && accel[Corner] &&
	accel[User] && accel[Feather] && accel[Arrow])
	modelClear();
	for (int i = 0; i < pend->siz; i++)
	modelFunc(&pend->range[i]);
	modelPrint();
	allocClient(&pend,0);}}
	return 0;
}

int modelInit()
{
	ioctl(0, TIOCGSIZE, &ts); ts.ts_cols--; ts.ts_lines--;
	mpage = malloc(sizeof(*mpage)*ts.ts_lines*ts.ts_cols);
	mframe = malloc(sizeof(*mframe)*ts.ts_lines*ts.ts_cols);
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
	int face = 0;
	while (pollPipe(mub)) {
		struct Client temp;
		readClient(&temp,mub);
		if (temp.mem == Face && state[Face])
			face = temp.face;}
	for (int i = 0; i < client->len; i++) {
		if (client->fnc[i] == Dma0)
			modelWrite(client->mem);
		if (client->fnc[i] == Dma1)
			state[Face]->face = face;
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
