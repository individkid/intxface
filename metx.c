#include "metx.h"
#include <math.h>
#include <stdlib.h>
#include <stdarg.h>

#define INVALID 1.0e30

float *matrc(float *u, int r, int c, int n)
{
    return &u[c*n+r];
}

float dotvec(float *u, float *v, int n)
{
    float w = 0;
    for (int i = 0; i < n; i++) w += u[i]*v[i];
    return w;
}

float *plusvec(float *u, float *v, int n)
{
    for (int i = 0; i < n; i++) u[i] += v[i];
    return u;
}

float *scalevec(float *u, float s, int n)
{
    for (int i = 0; i < n; i++) u[i] *= s;
    return u;
}

float *jumpvec(float *u, float *v, int n)
{
    float w[n];
    for (int i = 0; i < n; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        u[i] = 0.0;
        for (int j = 0; j < n; j++) {
            u[i] += v[j*n+i]*w[j];}}
    return u;
}

float *zerovec(float *u, int n)
{
    for (int i = 0; i < n; i++) u[i] = 0.0;
    return u;
}

float *normvec(float *u, int n)
{
    float denom = sqrtf(dotvec(u,u,n));
    if (fabs(denom) < 1.0 && 1.0 > fabs(INVALID*denom)) return 0;
    return scalevec(u,1.0/denom,n);
}

float *unitvec(float *u, int n, int m)
{
    for (int i = 0; i < n; i++) u[i] = (i == m ? 1.0 : 0.0);
    return u;
}

float *timesvec(float *u, float *v, int n)
{
    float w[4];
    for (int i = 0; i < n; i++) {
        float t = 0.0;
        for (int j = 0; j < n; j++) {
            t += u[j] * v[j*n+i];}
        w[i] = t;}
    return copyvec(u,w,n);
}

// elements of a collumn iterated through before iterating through the next collumn
float *timesmat(float *u, float *v, int n)
{
    int m = n*n; float w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += w[k*n+j]*v[i*n+k];}}}
    return u;
}

float *jumpmat(float *u, float *v, int n)
{
    int m = n*n; float w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += v[k*n+j]*w[i*n+k];}}}
    return u;
}

float *argmat(float *u, int n, int m, ...)
{
    va_list args;
    va_start(args,m);
    for (int i = 0; i < m; i++) {
    timesmat(u,va_arg(args,float *),n);}
    va_end(args);
    return u;
}

float *identmat(float *u, int n)
{
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = (i == j ? 1.0 : 0.0);}}
    return u;
}

float *copyary(float *u, float *v, int duty, int stride, int start, int size)
{
    // set upper submatrix: duty > 0; stride > 0
    // set lower submatrix; duty < 0; stride > 0
    // duty is number to copy in chunk; negative for number to skip
    // stride is size of dest chunks; negative for size of src chunks
    // start is total number to skip; size is total number to copy
    float *w = u;
    int i = 0;
    int j = 0;
    int k = 0;
    if (duty == 0 || stride == 0 || start < 0 || size < 0) return 0;
    if (stride > 0) {
        v += start;}
    else {
        w += start;}
    while (i < size) {
        if (k == 0) {j = duty; k = stride;}
        if (stride > 0) {
            if (duty > 0) {
                if (j > 0) *w = v[i++];}
            else {
                if (j == 0) *w = v[i++];}
            w++;}
        else {
            if (duty > 0) {
                if (j > 0) w[i++] = *v;}
            else {
                if (j == 0) w[i++] = *v;}
            v++;}
        if (k > 0) k--;
        if (k < 0) k++;
        if (j > 0) j--;
        if (j < 0) j++;}
    return u;
}

float *copyvec(float *u, float *v, int n)
{
    for (int i = 0; i < n; i++) u[i] = v[i];
    return u;
}

float *copymat(float *u, float *v, int n)
{
    return copyvec(u,v,n*n);
}

float *compmat(float *u, float *v, int n)
{
    for (int i = 0; i < n*n; i++) if (u[i] != v[i]) return 0;
    return u;
}

float *crossmat(float *u)
{
    float x = u[0]; float y = u[1]; float z = u[2];
    u[0] =  0; u[3] = -z; u[6] =  y;
    u[1] =  z; u[4] =  0; u[7] = -x;
    u[2] = -y; u[5] =  x; u[8] =  0;
    return u;
}

float *crossvec(float *u, float *v)
{
    float w[9]; copyvec(w,u,3);
    return jumpvec(copyvec(u,v,3),crossmat(w),3);
}

float *submat(float *u, int i, int n)
{
    int m = n*n; int k = 0;
    for (int j = 0; j < m; j++) if (j/n!=i/n && j%n!=i%n) u[k++] = u[j];
    return u;
}

float *minmat(float *u, int n)
{
    int m = n*n; float v[m];
    for (int i = 0; i < m; i++) {
        float w[m];
        submat(copymat(w,u,n),i,n);
        v[i] = detmat(w,n-1);}
    return copymat(u,v,n);
}

float *cofmat(float *u, int n)
{
    int m = n*n;
    minmat(u,n);
    for (int i = 0; i < m; i++)
    u[i] = ((i/n+i%n)%2!=0?-u[i]:u[i]);
    return u;
}

float detmat(float *u, int n)
{
    if (n == 1) return *u;
    int m = n*n; float det = 0.0;
    for (int i = 0; i < n; i++) {
    float v[m];
    submat(copymat(v,u,n),i,n);
    float s = detmat(v,n-1);
    s *= u[i];
    det += ((i/n+i%n)%2!=0?-s:s);}
    return det;
}

float *xposmat(float *u, int n)
{
    int m = n*n; float v[m];
    for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
    v[i*n+j] = u[j*n+i];
    return copyvec(u,v,m);
}

float *adjmat(float *u, int n)
{
    return xposmat(cofmat(u,n),n);
}

float *invmat(float *u, int n)
{
    int m = n*n; float v[m];
    adjmat(copymat(v,u,n),n);
    float det = detmat(u,n);
    float lim = fabs(det*INVALID);
    for (int i = 0; i < m; i++) if (fabs(det)<1.0 && fabs(v[i])>lim) return 0;
    for (int i = 0; i < m; i++) u[i] = v[i]/det;
    return u;
}

float *crossvecs(float *u, int n)
{
    int m = n*n; int p = n-1; int q = p*n; float w[m];
    for (int i = q; i < m; i++) w[i] = 0.0;
    xposmat(copyvec(w,u,q),n);
    for (int i = 0; i < n; i++) {
    int j = (i+1)*n-1;
    float v[m];
    float s = detmat(submat(copyvec(v,u,m),j,n),n-1);
    w[i] = ((j/n+j%n)%2!=0?-s:s);}
    return copyvec(u,w,n);
}

float *tweakvec(float *u, float a, float b, int n)
{
    for (int i = 0; i < n; i++) u[i] = a+((b-a)*rand()/(float)RAND_MAX);
    return u;
}

float *anyvec(float *u, int n)
{
    int found = 0; float v[n]; float w[n];
    copyvec(v,u,n); copyvec(w,u,n);
    for (int i = 1; i < n; i++) {
    if (u[i] < v[i]) v[i] = u[i];
    if (u[i] > w[i]) w[i] = u[i];}
    for (int i = 1; i < n; i++)
    if (fabs(w[i]-v[i]) < fabs(w[found]-v[found])) found = i;
    u[found] += 1.0;
    return u;
}

float *orthovec(float *u, float *v, int n)
{
    float w[n]; copyvec(w,u,n);
    return plusvec(scalevec(copyvec(u,v,3),-dotvec(v,w,3),3),w,3);
}
