#include "plane.h"
#include "face.h"
#include "metic.h"
#include "type.h"
#include "argx.h"
#include "share.h"
#include <stdlib.h>
#include <stdio.h>

struct Kernel {
	int valid; // optimized
	struct Matrix compose; // optimization
	struct Matrix maintain; // change to points
	struct Matrix written; // portion written
	struct Matrix towrite; // portion to write
	struct Vector fixed; // fixed point
	struct Vector start; // start axis
	struct Vector current; // current axis
	float angle; // roller delta
};
enum Atwill atwill = 0;
enum Action action = 0;
enum Transform xsform = 0;
enum Select select = 0;
enum Shader shader = 0;
struct Kernel subject = {0};
struct Kernel *object = {0};
int objects = 0;
struct Kernel facet = {0};
struct Ranje *range = {0};
int ranges = 0;

void planeAlize(float *dir, const float *vec)
{
}
void planeCross(float *axe, const float *fix, const float *pic, const float *cur)
{
}
void planeXtate(float *mat, const float *fix, const float *pic, const float *cur, float ang)
{
}
void planeXlate(float *mat, const float *fix, const float *pic, const float *cur, float ang)
{
}
void planeScale(float *mat, const float *fix, const float *pic, const float *cur, float ang)
{
}
void planeInit(vftype init, uftype dma, wftype draw)
{
	shareInit(); // from share.h
	// TODO set factory and script for face and loop
	init(); // this calls useArgument from argx.h
	runProgram(); // from argx.h
}
float planeConfig(enum Configure cfg)
{
	return 0.0;
}
void planeWake(int count)
{
}
void planeReady(struct Pierce *pierce, int size)
{
}
void planeEmpty()
{
}
