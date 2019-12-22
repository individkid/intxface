/*
*    plane.h
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

#include <GLFW/glfw3.h>
#include "face.h"
#include "base.h"
#include "type.h"
#include "arithmetic.h"

#ifndef EXTERN
#define EXTERN extern
#endif

EXTERN int esc;
EXTERN GLFWwindow* window;
EXTERN struct Client *client;
EXTERN struct Client *state[Memorys];
EXTERN struct Client *saved[Memorys];

void huberr(const char *str, int num, int arg);
void exiterr(const char *str, int num, int arg);
void windowInit(int argc, char **argv);
void windowDone();
int metalInit();
int metalFull();
void metalDraw();
void metalDone();
int vulkanInit();
int vulkanFull();
void vulkanDraw();
void vulkanDone();
int openglInit();
int openglFull();
void openglDraw();
void openglDone();
int modelInit();
int modelFull();
void modelDraw();
void modelDone();
