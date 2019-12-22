/*
*    arithmetic.h functions for operations on float arrays
*    Copyright (C) 2019  Paul Coelho
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

float dotvec(float *u, float *v, int n);
float *plusvec(float *u, float *v, int n);
float *scalevec(float *u, float s, int n);
float *jumpvec(float *u, float *v, int n);
float *zerovec(float *u, int n);
float *unitvec(float *u, int n, int m);
float *timesmat(float *u, float *v, int n);
float *jumpmat(float *u, float *v, int n);
float *identmat(float *u, int n);
float *copyary(float *u, float *v, int duty, int stride, int size);
float *copyvec(float *u, float *v, int n);
float *copymat(float *u, float *v, int n);
float *compmat(float *u, float *v, int n);
float *crossmat(float *u);
float *crossvec(float *u, float *v);
float detmat(float *u, int n);
float *adjmat(float *u, int n);
float *invmat(float *u, int n);
float *tweakvec(float *u, float a, float b, int n);
float absval(float a);
float *basearrow(float *u, float *v, int *i, float *b, int n);
float *arrowbase(float *u, float *v, int i, float *b, int n);
