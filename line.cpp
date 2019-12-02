/*
*    line.cpp
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

extern "C" {
#include "type.h"
#include "base.h"
#include "face.h"
#include "portaudio.h"
}
#include <setjmp.h>
#include <unistd.h>
#include <time.h>
#include <sys/errno.h>
#include <map>

std::map < int, Event* > state;
std::map < double, std::pair < int, double > > change;
std::map < double, int > sample;
jmp_buf errbuf = {0};
double nowtime = 0.0;

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

int callback(const void *inputBuffer, void *outputBuffer,
	unsigned long framesPerBuffer,
	const PaStreamCallbackTimeInfo* timeInfo,
	PaStreamCallbackFlags statusFlags,
	void *userData)
{
	return 0;
}

double evaluate(Ratio *ratio)
{
	// TODO
	return 0.0;
}

void evalpush(Event *event)
{
	double upd = evaluate(&event->upd);
	double dly = evaluate(&event->dly);
	double sch = evaluate(&event->sch);
	change[nowtime+dly] = std::make_pair(event->idx,upd);
	sample[nowtime+sch] = event->idx;
}

int callwait()
{
	if (change.empty()) return waitAny();
	struct timespec ts = {0};
	if (clock_gettime(CLOCK_MONOTONIC,&ts) < 0) ERROR(exiterr,-1);
	nowtime = (double)ts.tv_sec+((double)ts.tv_nsec)*0.000000001;
	while ((*change.begin()).first <= nowtime) {
		std::pair < int, double > head = (*change.begin()).second;
		change.erase(change.begin());
		state[head.first]->val = head.second;
	}
	while ((*sample.begin()).first <= nowtime) {
		int head = (*sample.begin()).second;
		sample.erase(sample.begin());
		evalpush(state[head]);
	}
	return pauseAny((*change.begin()).first-nowtime);
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int hub = 0;
	int sub = 0;
	Event *event = (Event*)malloc(sizeof(Event));
	struct timespec ts = {0};
	if (clock_gettime(CLOCK_MONOTONIC,&ts) < 0) ERROR(exiterr,-1);
	nowtime = (double)ts.tv_sec+((double)ts.tv_nsec)*0.000000001;
	if (Pa_Initialize() != paNoError) ERROR(exiterr,-1);
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub);
	while (1) {if (setjmp(errbuf) == 0) {
	for (sub = waitAny(); sub >= 0; sub = waitAny()) {
	readEvent(event,sub);
	switch (event->cng) {
	case (Stock):
	state[event->idx] = event;
	event = (Event*)malloc(sizeof(Event));
	break;
	case (Flow):
	state[event->idx]->val = event->val;
	break;
	case (Start):
	sample[nowtime] = event->idx;
	break;
	default: ERROR(exiterr,-1);}
	event = (Event*)malloc(sizeof(Event));}}}
	if (Pa_Terminate() != paNoError) ERROR(exiterr,-1);
	return -1;
}
