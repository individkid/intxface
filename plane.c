#include "plane.h"
#include "face.h"
#include "metic.h"
#include "type.h"
#include <stdlib.h>
#include <stdio.h>

struct Kernel {
	int valid; // optimized
	float compose[16]; // optimization
	float maintain[16]; // change to points
	float written[16]; // portion written
	float towrite[16]; // portion to write
	float fixed[3]; // fixed point
	float start[3]; // start axis
	float current[3]; // current axis
};
struct Apply {
	enum Work work;
	enum Tool tool;
};
enum Atwill atwill = 0;
enum Action action = 0;
enum Select select = 0;
enum Machine machine = 0;
enum Shader shader = 0;
struct Apply hand = {0};
struct Apply finger = {0};
int *points[3] = {0};
int *planes[3] = {0};
struct Kernel subject = {0};
struct Kernel object[NUMFILE] = {0};
struct Kernel facet = {0};

void planeInit()
{
}
float planeConfig(enum Configure cfg)
{
	return 0.0;
}