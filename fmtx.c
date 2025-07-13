#include <stdio.h>
#include <string.h>
#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>
void datxVoid(void **dat, int siz);
void *datxVoidz(int num, void *dat);
void fmtxStbi(void **ptr, int *wid, int *hei, int *cha, const char *str)
{
    stbi_uc* pixels = stbi_load(str, wid, hei, cha, STBI_rgb_alpha);
    if (!pixels) {fprintf(stderr,"failed to load texture image %s\n",str); exit(-1);}
    int siz = *wid**hei*4; datxVoid(ptr,siz); memcpy(datxVoidz(0,*ptr),pixels,siz); stbi_image_free(pixels);
}
