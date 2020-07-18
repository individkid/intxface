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

#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include "share.h"

jmp_buf jmpbuf = {0};
struct GLFWwindow* window = 0;

void displayErr(const char *str, int num, int arg)
{
	longjmp(jmpbuf,1);
}

void displayPos(int *xloc, int *yloc)
{
    glfwGetWindowPos(window,xloc,yloc);
}

void displaySize(int *width, int *height)
{
	glfwGetWindowSize(window, width, height);
}

void displayKey(struct GLFWwindow* ptr, int key, int scancode, int action, int mods)
{
	if (action == 1) cb.key(key);
}

void displayMove(struct GLFWwindow* ptr, double xpos, double ypos)
{
	cb.move(xpos,ypos);
}

void displayRoll(struct GLFWwindow* ptr, double xoffset, double yoffset)
{
	cb.roll(xoffset,yoffset);
}

void displayClick(struct GLFWwindow* ptr, int button, int action, int mods)
{
	if (action == GLFW_PRESS && button == GLFW_MOUSE_BUTTON_LEFT) cb.click(0);
	if (action == GLFW_PRESS && button == GLFW_MOUSE_BUTTON_RIGHT) cb.click(1);
}

void displayCall()
{
	while (cb.esc < 2 && !glfwWindowShouldClose(window))
	if (setjmp(jmpbuf) == 0)
	while(cb.esc < 2 && !glfwWindowShouldClose(window)) {
	if (cb.full()) {
	glfwWaitEventsTimeout(1000.0*NANO2SEC);
	continue;}
	if (cb.read()) {
	cb.proc();
	cb.draw();
	cb.prod();
	glfwPollEvents();
	continue;}
	glfwWaitEvents();}
}

void displayDone()
{
	glfwDestroyWindow(window);
	glfwTerminate();
}

void displaySwap()
{
	glfwSwapBuffers(window);
}

int displayInit(const char *name)
{
	printf("uint32_t(%d) int(%d) float(%d)\n",(int)sizeof(uint32_t),(int)sizeof(int),(int)sizeof(float));
	cb.err = displayErr;
	cb.pos = displayPos;
	cb.size = displaySize;
	cb.call = displayCall;
	cb.swap = displaySwap;
	cb.wake = glfwPostEmptyEvent;
	glfwInit();
	glfwWindowHint(GLFW_SAMPLES, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE); // To make MacOS happy; should not be needed
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);
	if ((window = glfwCreateWindow(WINWIDE, WINHIGH, name, 0, 0)) == 0) ERROR(exiterr,-1);
	glfwSetKeyCallback(window,displayKey);
	glfwSetCursorPosCallback(window,displayMove);
	glfwSetScrollCallback(window,displayRoll);
	glfwSetMouseButtonCallback(window,displayClick);
	glfwMakeContextCurrent(window);
	return 1;
}
