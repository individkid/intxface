#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "luax.h"
#include "stlx.h"
#include "type.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int datxIrrcmp(const char *str);
int datxIrrexe(const char *str, int idx);
int datxIrrex(const char *lft, const char *rgt)
{
	return datxIrrexe(lft,datxIrrcmp(rgt));
}
int datxRegcmp(const char *str);
int datxRegexe(const char *str, int idx);
int datxRegex(const char *lft, const char *rgt)
{
	return datxRegexe(lft,datxRegcmp(rgt));
}

void *allocSafe(int val);
int hideCenter(struct Center *ptr, const char *str, int *len);
void showCenter(struct Center *ptr, char **str);
void allocCenter(struct Center **ptr, int siz);
void planeField(void **dst, void *src, void *fld, int num, int sub, int stp, int ftp);

int main()
{
	void *ptr = 0;
	datxInt(&ptr,5); if (datxInts(ptr) != 1) ERROR(); if (*datxIntz(0,ptr) != 5) ERROR();
	if (datxIrrex("abc",">abcdefg") != 1) ERROR();
	if (datxIrrex("bcd",">abcdefg") != 0) ERROR();
	if (datxIrrex("abcdef","abc=def") != 1) ERROR();
	if (datxIrrex("abcde","abc=def") != 0) ERROR();
	if (datxIrrex("def","|=abc^=def^^") != 1) ERROR();
	if (datxIrrex("abc","|=abc^=def^^") != 1) ERROR();
	if (datxIrrex("bcd","|=abc^=def^^") != 0) ERROR();
	if (datxIrrex("def","|=abc^=def") != 1) ERROR();
	if (datxIrrex("abc","|=abc^=def") != 1) ERROR();
	if (datxIrrex("bcd","|=abc^=def") != 0) ERROR();
	if (datxIrrex("abc","abc") != 1) ERROR();
	if (datxIrrex("abcghi","|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("defghi","|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("defgh","|=abc^=def^^ghi") != 0) ERROR();
	if (datxIrrex("876210abcghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("876543abcghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("876210defghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxIrrex("876543defghi","876|=543^=210^^|=abc^=def^^ghi") != 1) ERROR();
	if (datxRegex("abdef","ab[cd]ef") != 1) ERROR();
	if (datxRegex("abcef","ab[cd]ef") != 1) ERROR();
	if (datxRegex("abef","ab[cd]ef") != 0) ERROR();
	datxSingle();
	const char *st1 = "Kernel(saved:Matrix(mat[0]:0.0mat[1]:0.1mat[2]:0.2mat[3]:0.3mat[4]:1.0mat[5]:1.1mat[6]:1.2mat[7]:1.3mat[8]:2.0mat[9]:2.1mat[10]:2.2mat[11]:2.3mat[12]:3.0mat[13]:3.1mat[14]:3.2mat[15]:3.3)local:Matrix(mat[0]:0.0mat[1]:0.1mat[2]:0.2mat[3]:0.3mat[4]:1.0mat[5]:1.1mat[6]:1.2mat[7]:1.3mat[8]:2.0mat[9]:2.1mat[10]:2.2mat[11]:2.3mat[12]:3.0mat[13]:3.1mat[14]:3.2mat[15]:3.3)sent:Matrix(mat[0]:0.0mat[1]:0.1mat[2]:0.2mat[3]:0.3mat[4]:1.0mat[5]:1.1mat[6]:1.2mat[7]:1.3mat[8]:2.0mat[9]:2.1mat[10]:2.2mat[11]:2.3mat[12]:3.0mat[13]:3.1mat[14]:3.2mat[15]:3.3)global:Matrix(mat[0]:0.0mat[1]:0.1mat[2]:0.2mat[3]:0.3mat[4]:1.0mat[5]:1.1mat[6]:1.2mat[7]:1.3mat[8]:2.0mat[9]:2.1mat[10]:2.2mat[11]:2.3mat[12]:3.0mat[13]:3.1mat[14]:3.2mat[15]:3.3))";
	struct Kernel *ker = 0; allocKernel(&ker,1); int len = 0;
	if (hideKernel(ker,st1,&len) != 1 || len != strlen(st1)) ERROR();
	struct Extend *ext = 0; allocExtend(&ext,1);
	void *src = 0; void *fld = 0; void *dat = 0;
		writeCenter(ext->ptr,datxClr(0)); datxGet(0,&src); writeInt(1,datxClr(0)); datxGet(0,&fld);
		planeField(&dat, src, fld, identField(TYPECenter,"siz"), 0, TYPECenter, TYPEInt); readCenter(ext->ptr,datxPut(0,dat));
		if (ext->ptr->mem != 0 || ext->ptr->siz != 1)
	free(dat); free(src); free(fld); src = fld = dat = 0;
		writeCenter(ext->ptr,datxClr(0)); datxGet(0,&src); writeInt(Kernelz,datxClr(0)); datxGet(0,&fld);
		planeField(&dat, src, fld, identField(TYPECenter,"mem"), 0, TYPECenter, TYPEInt); readCenter(ext->ptr,datxPut(0,dat));
		if (ext->ptr->mem != Kernelz || ext->ptr->siz != 0)
	free(dat); free(src); free(fld); src = fld = dat = 0;
		writeCenter(ext->ptr,datxClr(0)); datxGet(0,&src); writeInt(2,datxClr(0)); datxGet(0,&fld);
		planeField(&dat, src, fld, identField(TYPECenter,"siz"), 0, TYPECenter, TYPEInt); readCenter(ext->ptr,datxPut(0,dat));
		if (ext->ptr->mem != Kernelz || ext->ptr->siz != 2)
	free(dat); free(src); free(fld); src = fld = dat = 0;
	if (ext->ptr->mem != Kernelz || ext->ptr->siz != 2) ERROR();
	float chk[16]; identmat(chk,4);
	struct Center *ctr = ext->ptr;
	for (int i = 0; i < 4; i++) {for (int j = 0; j < 4; j++) printf(" %01.1f",*matrc(ker->saved.mat,i,j,4)); printf("\n");}
	for (int i = 0; i < 4; i++) {for (int j = 0; j < 4; j++) printf(" %01.1f",*matrc(ctr->ker[0].saved.mat,i,j,4)); printf("\n");}
	for (int i = 0; i < 4; i++) {for (int j = 0; j < 4; j++) printf(" %01.1f",*matrc(chk,i,j,4)); printf("\n");}
	for (int i = 0; i < 4; i++) {for (int j = 0; j < 4; j++) printf(" %01.1f",*matrc(ctr->ker[1].saved.mat,i,j,4)); printf("\n");}
	for (int i = 0; i < 16; i++) if (ctr->ker[0].saved.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[0].local.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[0].sent.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[0].global.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[1].saved.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[1].local.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[1].sent.mat[i] != chk[i]) ERROR();
	for (int i = 0; i < 16; i++) if (ctr->ker[1].global.mat[i] != chk[i]) ERROR();
	return 0;
}