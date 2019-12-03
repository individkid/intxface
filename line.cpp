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
#include <math.h>
#include <sys/errno.h>
#include <map>
#include <vector>

struct Channel {
	Channel(double l, int s) : len(l), siz(s), now(0), cnt(s), val(s) {}
	double len; // how long between callbacks
	int siz; // size of circular buffer
	int now; // index of last read by callback
	std::vector < int > cnt; // denomitor for average
	std::vector < double > val; // total for average
};
struct Update {
	Update() {}
	Update(int i, double v) : idx(i), val(v) {}
	int idx; // state to update
	double val; // value for update
};
std::map < int, Metric* > timer;
std::map < int, Channel* > audio;
std::map < int, Event* > state;
std::map < double, Update > change;
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

double condition(int *variable)
{
	if (state[variable[0]]->val > 0.0) return state[variable[1]]->val;
	return state[variable[2]]->val;
}

double polynomial(Nomial *nomial)
{
	double result = 0.0;
	for (int i = 0; i < nomial->num0; i++) result += nomial->trm0[i].cff;
	for (int i = 0; i < nomial->num1; i++) result += nomial->trm1[i].cff*state[nomial->trm1[i].var[0]]->val;
	for (int i = 0; i < nomial->num2; i++) result += nomial->trm2[i].cff*state[nomial->trm2[i].var[0]]->val*state[nomial->trm2[i].var[1]]->val;
	for (int i = 0; i < nomial->num3; i++) result += nomial->trm3[i].cff*condition(nomial->trm3[i].var);
	return result;
}

double evaluate(Ratio *ratio)
{
	double num = polynomial(&ratio->num);
	if (num == 0.0) return 0.0;
	if (!(num < 0.0) && !(num > 0.0)) return 0.0;
	double den = polynomial(&ratio->den);
	double sat = num/SATURATE;
	if (fabs(sat) > fabs(den)) {
		if (num < 0.0) return -SATURATE;
		else return SATURATE;
	}
	return num/den;
}

void schedule(Event *event)
{
	double upd = evaluate(&event->upd);
	double dly = evaluate(&event->dly);
	double sch = evaluate(&event->sch);
	change[nowtime+dly] = Update(event->idx,upd);
	sample[nowtime+sch] = event->idx;
	if (audio.find(event->chn) != audio.end()) {
		double aud = evaluate(&event->aud);
		Channel *channel = audio[event->chn];
		double div = nowtime/channel->len;
		long long rep = div;
		double rem = div-rep;
		int sub = rem*channel->siz;
		channel->cnt[sub]++;
		channel->val[sub] += aud;
	}
}

int callwait()
{
	struct timespec ts = {0};
	if (clock_gettime(CLOCK_MONOTONIC,&ts) < 0) ERROR(exiterr,-1);
	nowtime = (double)ts.tv_sec+((double)ts.tv_nsec)*0.000000001;
	while (!change.empty() && (*change.begin()).first <= nowtime) {
		Update head = (*change.begin()).second;
		change.erase(change.begin());
		Event *event = state[head.idx];
		event->val = head.val;
		if (timer.find(event->tmr) != timer.end()) {
			Metric *metric = timer[event->tmr];
			for (int i = 0; i < metric->num; i++) {

			}
		}
	}
	while (!sample.empty() && (*sample.begin()).first <= nowtime) {
		int head = (*sample.begin()).second;
		sample.erase(sample.begin());
		schedule(state[head]);
	}
	if (change.empty() && sample.empty()) return waitAny();
	if (change.empty()) return pauseAny((*sample.begin()).first-nowtime);
	if (sample.empty()) return pauseAny((*change.begin()).first-nowtime);
	if ((*sample.begin()).first < (*change.begin()).first)
	return pauseAny((*sample.begin()).first-nowtime);
	return pauseAny((*change.begin()).first-nowtime);
}

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int hub = 0;
	int sub = 0;
	Event *event = 0; allocEvent(&event,1);
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
	allocEvent(&event,1);
	break;
	case (Flow):
	state[event->idx]->val = event->val;
	break;
	case (Start):
	sample[nowtime] = event->idx;
	break;
	case (Lines):
	timer[event->idx] = event->met;
	event->met = 0;
	break;
	case (Linez):
	// TODO setup audio
	break;
	default: ERROR(exiterr,-1);}}}}
	if (Pa_Terminate() != paNoError) ERROR(exiterr,-1);
	return -1;
}
