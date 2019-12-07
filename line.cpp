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
	Channel(double l, int s) : nxt(0), str(0), len(l), gap(0.0), siz(s), sub(0), cnt(s,0), val(s,0.0) {}
	Channel *nxt;
	PaStream *str;
	double len; // how long between buffer wraps
	int gap; // optimum time from write to read
	int cdt; // extra time from write to read
	int siz; // size of circular buffer
	int sub; // index of last read by callback
	std::vector < int > cnt; // denomitor for average
	std::vector < float > val; // total for average
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

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

int location(double now, double len, int siz)
{
	double div = now/len;
	long long rep = div;
	double rem = div-rep;
	return rem*siz;
}

// 1 1 1 < | > 0 0 0 // > between | and 0 // < between | and 1
int between(int bef, int bet, int aft, int siz)
{
	while (bef > bet) bet += siz;
	while (bet > aft) aft += siz;
	return ((aft-bef)*2 < siz);
}

int modulus(int one, int oth, int siz)
{
	int res = one+oth;
	while (res < 0) res += siz;
	while (res >= siz) res -= siz;
	return res;
}

void repeat(Channel *ptr, int sub)
{
	if (ptr->cnt[sub] == 0) {
		ptr->val[sub] = ptr->val[modulus(sub,ptr->siz-1,ptr->siz)];
		ptr->cnt[sub] = 1;}
}

void copywave(float *dest, Channel *channel, int enb, int siz, double now)
{
	int dif[enb]; int sub[enb]; int pst[enb]; int pre[enb]; int i, dst; Channel *ptr;
	for (i = 0, ptr = channel; i < enb && ptr; i++, ptr = ptr->nxt) {
		int loc = location(now,ptr->len,ptr->siz);
		int gap = modulus(loc,ptr->gap,ptr->siz);
		int min = modulus(gap,-ptr->cdt,ptr->siz);
		int max = modulus(gap,ptr->cdt,ptr->siz);
		if (between(gap,max,ptr->sub,ptr->siz)) {
			// 1 1 w 0 0 < 0 0 | 0 0 > r 1 // temper between |++=r and r
			dif[i] = modulus(ptr->sub,-gap,ptr->siz);
			pre[i] = 0; sub[i] = gap; pst[i] = ptr->sub;
			repeat(ptr,pst[i]);
		} else if (between(ptr->sub,min,gap,ptr->siz)) {
			// 1 1 w 0 r < 1 1 | 1 1 > 1 1 // temper between r++ and |
			dif[i] = modulus(gap,-ptr->sub,ptr->siz);
			pre[i] = 0; sub[i] = ptr->sub; pst[i] = gap;
			repeat(ptr,pst[i]);
		} else {
			// 1 1 w 0 0 < 0 0 | r 1 > 1 1 // continue from r++
			// 1 1 w 0 0 < 0 r | 1 1 > 1 1 // continue from r++
			dif[i] = 0; sub[i] = ptr->sub;}
	}
	for (dst = 0; dst < siz;) {
	for (i = 0, ptr = channel; i < enb && ptr && dst < siz; i++, ptr = ptr->nxt) {
		if (sub[i] == pst[i]) dif[i] = 0;
		if (ptr->cnt[sub[i]] == 0 && dif[i]) {
			ptr->val[sub[i]] = ptr->val[pst[i]];
			ptr->cnt[sub[i]] = ptr->cnt[pst[i]];
		}
		repeat(ptr,sub[i]);
		ptr->val[sub[i]] = ptr->val[sub[i]]/ptr->cnt[sub[i]];
		ptr->cnt[sub[i]] = 0;
		if (dif[i]) {
			float rat = (float)pre[i]/(float)dif[i];
			ptr->val[sub[i]] = rat*ptr->val[sub[i]]+(1.0-rat)*ptr->val[pst[i]];
		}
		if (ptr->val[sub[i]] < -1.0) ptr->val[sub[i]] = -1.0;
		if (ptr->val[sub[i]] > 1.0) ptr->val[sub[i]] = 1.0;
		dest[dst++] = ptr->val[sub[i]];
		if (dif[i]) pre[i]++;
		sub[i] = modulus(sub[i],1,ptr->siz);}}
	for (i = 0, ptr = channel; i < enb && ptr; i++, ptr = ptr->nxt) {
		ptr->sub = sub[i];}
}

int callback(const void *inputBuffer, void *outputBuffer,
	unsigned long framesPerBuffer,
	const PaStreamCallbackTimeInfo* timeInfo,
	PaStreamCallbackFlags statusFlags,
	void *userData)
{
	Channel *channel = (Channel*)userData;
	int enb = 0; for (Channel *ptr = channel; ptr; ptr = ptr->nxt) enb++;
	copywave((float*)outputBuffer,channel,enb,framesPerBuffer*enb,Pa_GetStreamTime(channel->str)+channel->gap);
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

int main(int argc, char **argv)
{
	if (argc != 4) return -1;
	int hub = 0;
	int sub = 0;
	Channel *channel = 0;
	Event *event = 0; allocEvent(&event,1);
	struct timespec ts = {0};
	if (clock_gettime(CLOCK_MONOTONIC,&ts) < 0) ERROR(exiterr,-1);
	if (Pa_Initialize() != paNoError) ERROR(exiterr,-1);
	if ((hub = pipeInit(argv[1],argv[2])) < 0) ERROR(exiterr,-1);
	bothJump(huberr,hub);
	while (1) {if (setjmp(errbuf) == 0) {while (1) {
	struct timespec ts = {0};
	if (clock_gettime(CLOCK_MONOTONIC,&ts) < 0) ERROR(exiterr,-1);
	double nowtime = (double)ts.tv_sec+((double)ts.tv_nsec)*NANO2SEC;
	while (!change.empty() && (*change.begin()).first <= nowtime) {
		Update head = (*change.begin()).second;
		Event *event = state[head.idx];
		change.erase(change.begin());
		event->val = head.val;
		if (audio.find(event->chn) != audio.end()) {
			Channel *channel = audio[event->chn];
			double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
			int sub = location(strtime,channel->len,channel->siz);
			channel->val[sub] += head.val;
			channel->cnt[sub]++;
		}
		if (timer.find(event->tmr) != timer.end()) {
			Metric *metric = timer[event->tmr];
			double *ptr = metric->val;
			for (int i = 0; i < metric->num && ptr-metric->val < metric->tot; i++) {
				if (metric->siz[i] == 0) *(ptr++) = state[metric->idx[i]]->val; else {
					float val[metric->siz[i]];
					copywave(val,audio[metric->idx[i]],1,metric->siz[i],nowtime);
					for (int j = 0; j < metric->siz[i]; j++) *(ptr++) = val[j];
				}
			}
		}
	}
	while (!sample.empty() && (*sample.begin()).first <= nowtime) {
		int head = (*sample.begin()).second;
		Event *event = state[head];
		sample.erase(sample.begin());
		double upd = evaluate(&event->upd);
		double dly = evaluate(&event->dly);
		double sch = evaluate(&event->sch);
		change[nowtime+dly] = Update(event->idx,upd);
		sample[nowtime+sch] = event->idx;
	}
	int sub = -1;
	if (change.empty() && sample.empty()) sub = waitAny();
	else if (change.empty()) sub = pauseAny((*sample.begin()).first-nowtime);
	else if (sample.empty()) sub = pauseAny((*change.begin()).first-nowtime);
	else if ((*sample.begin()).first < (*change.begin()).first)
	sub = pauseAny((*sample.begin()).first-nowtime);
	else sub = pauseAny((*change.begin()).first-nowtime);
	if (sub < 0) continue;
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
	audio[event->idx] = channel = new Channel(event->len,event->siz);
	if (event->enb!=event->idx) {
		audio[event->enb] = channel->nxt = new Channel(event->len,event->siz);
		channel->nxt->gap = channel->gap = event->gap;
		if (Pa_OpenDefaultStream(&channel->str,0,2,paFloat32,CALLRATE,
		paFramesPerBufferUnspecified,callback,channel) != paNoError) ERROR(huberr,-1);
		if (Pa_StartStream(channel->str) != paNoError) ERROR(huberr,-1);
	}
	break;
	default: ERROR(exiterr,-1);}}}}
	if (Pa_Terminate() != paNoError) ERROR(exiterr,-1);
	return -1;
}
