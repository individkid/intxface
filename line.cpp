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
	Channel(double w, int l) : nxt(0), str(0), wrp(w), gap(0.0), len(l), sub(0), cnt(l,0), val(l,0.0) {}
	Channel *nxt;
	PaStream *str;
	double wrp; // how long between buffer wraps
	int gap; // optimum time from write to read
	int cdt; // extra time from write to read
	int len; // size of circular buffer
	int sub; // index of last read by callback
	std::vector < int > cnt; // denomitor for average
	std::vector < float > val; // total for average
};
struct Update {
	Update() {}
	static Update *alloc(double k);
	static void alloc(double k, int i, double v) {alloc(k)->init(i,v);}
	static void alloc(double k, Flow f, int i, int j, double v) {alloc(k)->init(f,i,j,v);}
	static Update *dealloc(double k);
	static Update *upd; // free pool
	void init(int i, double v) {flw=Back; idx=i; val=v;}
	void init(Flow f, int i, int j, double v) {flw=f; idx=i; oth=j; val=v;}
	Update *nxt; // linked list
	Flow flw; // kind of index
	int idx; // index of originator
	int oth; // index for transfer
	double val; // value for update
};

std::map < int, Event* > state;
std::map < double, Update* > change;
std::map < int, Metric* > timer;
std::map < int, Channel* > audio;
jmp_buf errbuf = {0};
int numbug = 0;

Update *Update::upd = 0;
Update *Update::alloc(double k)
{
	if (upd == 0) {
		upd = new Update [NUMPOOL];
		for (int i = 0; i < NUMPOOL-1; i++) upd[i].nxt = upd+i+1;
		upd[NUMPOOL-1].nxt = 0;}
	Update *ptr = upd; upd = upd->nxt;
	if (change.find(k) == change.end()) ptr->nxt = 0;
	else ptr->nxt = change[k];
	change[k] = ptr;
	return ptr;
}
Update *Update::dealloc(double k)
{
	if (change.begin() == change.end()) return 0;
	if ((*change.begin()).first > k) return 0;
	Update *ptr = (*change.begin()).second;
	if (ptr->nxt == 0) change.erase(change.begin());
	else (*change.begin()).second = ptr->nxt;
	ptr->nxt = upd; upd = ptr; return ptr;
}

void huberr(const char *str, int num, int arg)
{
	longjmp(errbuf,1);
}

void exiterr(const char *str, int num, int arg)
{
	exit(arg);
}

int location(double now, double wrp, int len)
{
	double div = now/wrp;
	long long rep = div;
	double rem = div-rep;
	return rem*len;
}

// 1 1 1 < | > 0 0 0 // > between | and 0 // < between | and 1
int between(int bef, int bet, int aft, int len)
{
	while (bef > bet) bet += len;
	while (bet > aft) aft += len;
	return ((aft-bef)*2 < len);
}

int modulus(int one, int oth, int len)
{
	int res = one+oth;
	while (res < 0) res += len;
	while (res >= len) res -= len;
	return res;
}

void normalize(Channel *ptr, int sub)
{
	if (ptr->cnt[sub] == 0) {
		int prd = modulus(sub,ptr->len-1,ptr->len);
		ptr->val[sub] = ptr->val[prd];
		ptr->cnt[sub] = ptr->cnt[prd];
		if (ptr->cnt[sub] == 0) ptr->cnt[sub] = 1;}
	ptr->val[sub] = ptr->val[sub]/ptr->cnt[sub];
	ptr->cnt[sub] = 1;
}

void copywave(float *dest, Channel *channel, int enb, int siz, double now, float sat)
{
	int dif[enb]; int num[enb]; int sub[enb]; int sup[enb]; int i, dst; Channel *ptr;
	for (i = 0, ptr = channel; i < enb && ptr; i++, ptr = ptr->nxt) {
		int loc = location(now,ptr->wrp,ptr->len);
		int gap = modulus(loc,ptr->gap,ptr->len);
		int min = modulus(gap,-ptr->cdt,ptr->len);
		int max = modulus(gap,ptr->cdt,ptr->len);
		if (between(gap,max,ptr->sub,ptr->len)) {
			// 1 1 w 0 0 < 0 0 | 0 0 > r 1 // temper between |++=r and r
			dif[i] = modulus(ptr->sub,-gap,ptr->len);
			num[i] = 0; sub[i] = gap; sup[i] = ptr->sub;
			normalize(ptr,sup[i]); numbug++;
		} else if (between(ptr->sub,min,gap,ptr->len)) {
			// 1 1 w 0 r < 1 1 | 1 1 > 1 1 // temper between r++ and |
			dif[i] = modulus(gap,-ptr->sub,ptr->len);
			num[i] = 0; sub[i] = ptr->sub; sup[i] = gap;
			normalize(ptr,sup[i]); numbug++;
		} else {
			// 1 1 w 0 0 < 0 0 | r 1 > 1 1 // continue from r++
			// 1 1 w 0 0 < 0 r | 1 1 > 1 1 // continue from r++
			dif[i] = 0; sub[i] = ptr->sub;}}
	for (dst = 0; dst < siz;) {
	for (i = 0, ptr = channel; i < enb && ptr && dst < siz; i++, ptr = ptr->nxt) {
		if (dif[i]) {
		if (sub[i] == sup[i]) dif[i] = 0;
		if (ptr->cnt[sub[i]] == 0) {
			ptr->val[sub[i]] = ptr->val[sup[i]];
			ptr->cnt[sub[i]] = ptr->cnt[sup[i]];}}
		normalize(ptr,sub[i]);
		if (dif[i]) {
			float rat = (float)num[i]/(float)dif[i]; num[i]++;
			ptr->val[sub[i]] = rat*ptr->val[sub[i]]+(1.0-rat)*ptr->val[sup[i]];}
		if (ptr->val[sub[i]] < -sat) dest[dst++] = -sat;
		else if (ptr->val[sub[i]] > sat) dest[dst++] = sat;
		else dest[dst++] = ptr->val[sub[i]];
		ptr->cnt[sub[i]] = 0;
		sub[i] = modulus(sub[i],1,ptr->len);}}
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
	copywave((float*)outputBuffer,channel,enb,framesPerBuffer*enb,Pa_GetStreamTime(channel->str)+channel->gap,1.0);
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

void alloc(double k) {Update::alloc(k);}
void alloc(double k, int i, double v) {Update::alloc(k,i,v);}
void alloc(double k, Flow f, int i, int j, double v) {Update::alloc(k,f,i,j,v);}
Update *dealloc(double k) {return Update::dealloc(k);}

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
	for (Update *head = dealloc(nowtime); head; head = dealloc(nowtime)) {
	Event *event = state[head->idx];
	double sch = nowtime+evaluate(&event->sch);
	switch (head->flw) {
	case (Sched): {
		double dly = nowtime+evaluate(&event->dly);
		double upd = evaluate(&event->upd);
		alloc(sch);
		alloc(dly,event->idx,upd);
		break;}
	case (Back): {
		event->val = head->val;
		break;}
	case (Peek): {
		Channel *channel = audio[head->oth];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		int loc = location(strtime,channel->wrp,channel->len);
		int gap = modulus(loc,channel->gap,channel->len);
		if (channel->cnt[gap] == 0) event->val = channel->val[gap];
		else event->val = channel->val[gap]/channel->cnt[gap];
		alloc(sch);
		break;}
	case (Poke): {
		double upd = evaluate(&event->upd);
		Channel *channel = audio[head->oth];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		int sub = location(strtime,channel->wrp,channel->len);
		channel->val[sub] += upd;
		channel->cnt[sub]++;
		alloc(sch);
		break;}
	case (Store): {
		Metric *metric = timer[head->oth];
		double *ptr = metric->val;
		for (int i = 0; i < metric->num && ptr-metric->val < metric->tot; i++) {
		if (metric->siz[i] == 0)
		*(ptr++) = state[metric->idx[i]]->val; else {
		float val[metric->siz[i]];
		copywave(val,audio[metric->idx[i]],1,metric->siz[i],nowtime,SATURATE);
		for (int j = 0; j < metric->siz[i]; j++)
		*(ptr++) = val[j];}}
		writeMetric(metric,hub);
		allocMetric(&metric,0);
		timer.erase(timer.find(head->oth));
		break;}
	case (Load): {
		Channel *channel = audio[head->oth];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		int sub = location(strtime,channel->wrp,channel->len);
		for (int i = 0; i < event->siz; i++) {
		channel->val[sub] += event->buf[i];
		channel->cnt[sub]++;
		sub = modulus(sub,1,channel->len);}
		break;}
	case (Flows): break;
	default: ERROR(huberr,-1);}}
	int sub = -1;
	if (change.empty()) sub = waitAny();
	else sub = pauseAny((*change.begin()).first-nowtime);
	if (sub < 0) continue;
	readEvent(event,sub);
	switch (event->cng) {
	case (State):
		state[event->idx] = event;
		allocEvent(&event,1);
		break;
	case (Change):
		alloc(event->key,event->flw,event->idx,event->oth,event->val);
		break;
	case (Timer):
		timer[event->idx] = event->met;
		event->met = 0;
		break;
	case (Audio):
		// TODO handle other configs, like numbug, callrate, multitrack, portaudio input
		audio[event->idx] = channel = new Channel(event->wrp,event->len);
		if (event->enb!=event->idx) {
		audio[event->enb] = channel->nxt = new Channel(event->wrp,event->len);
		channel->nxt->gap = channel->gap = event->gap;
		if (Pa_OpenDefaultStream(&channel->str,0,2,paFloat32,CALLRATE,
		paFramesPerBufferUnspecified,callback,channel) != paNoError) ERROR(huberr,-1);
		if (Pa_StartStream(channel->str) != paNoError) ERROR(huberr,-1);}
		break;
	default: ERROR(exiterr,-1);}}}}
	if (Pa_Terminate() != paNoError) ERROR(exiterr,-1);
	return -1;
}
