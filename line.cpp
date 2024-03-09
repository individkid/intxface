// phase delay lock

extern "C" {
#include "face.h"
#include "type.h"
#include "portaudio.h"
}
#include <unistd.h>
#include <time.h>
#include <math.h>
#include <map>
#include <vector>
#ifdef __linux__
#include <cstdio>
#endif

struct Channel {
	Channel(double w, int g, int c, int l) : nxt(0), str(0),
	wrp(w), gap(g), cdt(c), len(l), sub(0), cnt(l,0), val(l,0.0) {}
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
	Update() {buf = 0;}
	~Update() {allocNum(&buf,0);}
	static Update *alloc(double k);
	static void alloc(double k, int i) {alloc(k)->init(i);}
	static void alloc(double k, int i, double v) {alloc(k)->init(i,v);}
	static void alloc(double k, Flow f, int i, int j) {alloc(k)->init(f,i,j);}
	static void alloc(double k, int i, int j, int s, double **v) {alloc(k)->init(i,j,s,v);}
	static Update *deloc(double k);
	static Update *upd; // free pool
	void init(int i) {flw=Sched; idx=i;}
	void init(int i, double v) {flw=Back; idx=i; val=v;}
	void init(Flow f, int i, int j) {flw=f; idx=i; oth=j;}
	void init(int i, int j, int s, double **v)
	{
		allocNum(&buf,0);
		flw=Load; idx=i; oth=j;
		siz = s; buf = *v; *v = 0;
	}
	Update *nxt; // linked list
	Flow flw; // kind of index
	int idx; // index of originator
	int oth; // index for transfer
	double val; // value for update
	int siz; // how many values
	double *buf; // values for update
};

std::map < int, Event* > state;
std::map < double, Update* > change;
std::map < int, Event* > timer;
std::map < int, Channel* > audio;
int numbug = 0;
double nowtime = 0;
int hub = 0;
int sub = 0;
Event *event = 0;
Update *head = 0;
int goon = 0;

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
Update *Update::deloc(double k)
{
	if (change.empty()) return 0;
	if ((*change.begin()).first > k) return 0;
	Update *ptr = (*change.begin()).second;
	if (ptr->nxt == 0) change.erase(change.begin());
	else (*change.begin()).second = ptr->nxt;
	ptr->nxt = upd; upd = ptr; return ptr;
}

void exiterr(const char *str, int num, int arg)
{
	Pa_Terminate(); exit(arg);
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
		int prd = modulus(sub,-1,ptr->len);
		ptr->val[sub] = ptr->val[prd];
		ptr->cnt[sub] = ptr->cnt[prd];
		if (ptr->cnt[sub] == 0) ptr->cnt[sub] = 1;}
	ptr->val[sub] = ptr->val[sub]/ptr->cnt[sub];
	ptr->cnt[sub] = 1;
}

void prepwave(int *dif, int *num, int *sub, int *sup, Channel *ptr, int enb, double now)
{
	for (int i = 0; i < enb && ptr; i++, ptr = ptr->nxt) {
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
}

void procwave(float *dest, int *dif, int *num, int *sub, int *sup, Channel *channel, int enb, int siz, float sat)
{
	for (int dst = 0; dst < siz;) {
	Channel *ptr = channel;
	for (int i = 0; i < enb && ptr && dst < siz; i++, ptr = ptr->nxt) {
		if (dif[i] && sub[i] == sup[i]) dif[i] = 0;
		if (dif[i] && ptr->cnt[sub[i]] == 0) {
			ptr->val[sub[i]] = ptr->val[sup[i]];
			ptr->cnt[sub[i]] = ptr->cnt[sup[i]];}
		normalize(ptr,sub[i]);
		if (dif[i]) {
			float rat = (float)num[i]/(float)dif[i]; num[i]++;
			ptr->val[sub[i]] = rat*ptr->val[sub[i]]+(1.0-rat)*ptr->val[sup[i]];}
		if (ptr->val[sub[i]] < -sat) dest[dst++] = -sat;
		else if (ptr->val[sub[i]] > sat) dest[dst++] = sat;
		else dest[dst++] = ptr->val[sub[i]];
		ptr->cnt[sub[i]] = 0;
		sub[i] = modulus(sub[i],1,ptr->len);}}
}

void progwave(Channel *ptr, int *sub, int enb)
{
	for (int i = 0; i < enb && ptr; i++, ptr = ptr->nxt) {
		ptr->sub = sub[i];}
}

void copywave(float *dest, Channel *channel, int enb, int siz, double now, float sat)
{
	int dif[enb]; int num[enb]; int sub[enb]; int sup[enb];
	prepwave(dif,num,sub,sup,channel,enb,now);
	procwave(dest,dif,num,sub,sup,channel,enb,siz,sat);
	progwave(channel,sub,enb);
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

double condition(double val0, double val1, double val2)
{
	if (val0 > 0.0) return val1;
	return val2;
}

double polynomial(Nomial *nomial)
{
	double result = 0.0;
	for (int i = 0; i < nomial->num0; i++) {
		Term0 *trm = &nomial->trm0[i];
		result += trm->cff;}
	for (int i = 0; i < nomial->num1; i++) {
		Term1 *trm = &nomial->trm1[i];
		Event *var0 = state[trm->vry[0]];
		result += trm->cff*var0->val;}
	for (int i = 0; i < nomial->num2; i++) {
		Term2 *trm = &nomial->trm2[i];
		Event *var0 = state[trm->vry[0]];
		Event *var1 = state[trm->vry[1]];
		result += trm->cff*var0->val*var1->val;}
	for (int i = 0; i < nomial->num3; i++) {
		Term3 *trm = &nomial->trm3[i];
		Event *var0 = state[trm->vry[0]];
		Event *var1 = state[trm->vry[1]];
		Event *var2 = state[trm->vry[2]];
		result += trm->cff*condition(var0->val,var1->val,var2->val);}
	return result;
}

int iszero(double val)
{
	if (val == 0.0) return 1;
	if (!(val < 0.0) && !(val > 0.0)) return 1;
	return 0;
}

double evaluate(Ratio *ratio)
{
	double num = polynomial(&ratio->num);
	double den = polynomial(&ratio->den);
	double sat = num/SATURATE;
	if (fabs(sat) > fabs(den) || iszero(den)) {
	if (iszero(num)) return 0.0;
	if (num < 0.0) return -SATURATE;
	return SATURATE;}
	return num/den;
}

double gettime()
{
	struct timespec ts = {0};
	if (clock_gettime(CLOCK_MONOTONIC,&ts) < 0) ERROR();
	return (double)ts.tv_sec+((double)ts.tv_nsec)*NANO2SEC;
}

void alloc(double k) {Update::alloc(k);}
void alloc(double k, int i) {Update::alloc(k,i);}
void alloc(double k, int i, double v) {Update::alloc(k,i,v);}
void alloc(double k, Flow f, int i, int j) {Update::alloc(k,f,i,j);}
void alloc(double k, int i, int j, int s, double **v) {Update::alloc(k,i,j,s,v);}
Update *deloc(double k) {return Update::deloc(k);}
int adloc() {return !change.empty();}
double adloc(double k) {return (*change.begin()).first-k;}

void stock()
{
	switch (event->tag) {
	case (Stock):
		if (state.find(event->idx) != state.end()) {
		allocEvent(&state[event->idx],0);}
		state[event->idx] = event;
		allocEvent(&event,1);
		break;
	case (First): // Sched
		if (event->key > nowtime)
		alloc(event->key,event->idx);
		else numbug++;
		break;
	case (Assign): // Back
		if (event->key > nowtime)
		alloc(event->key,event->idx,event->val);
		else numbug++;
		break;
	case (Bind): // Peek Poke Store
		if (event->key > nowtime)
		alloc(event->key,event->flw,event->idx,event->oth);
		else numbug++;
		break;
	case (Wave): // Load
		if (event->key > nowtime)
		alloc(event->key,event->idx,event->oth,event->siz,&event->buf);
		else numbug++;
		break;
	case (Timer):
		if (timer.find(event->idx) != timer.end()) {
		allocEvent(&timer[event->idx],0);}
		timer[event->idx] = event;
		allocEvent(&event,1);
		break;
	case (Audio): {
		if (audio.find(event->idx) != audio.end()) {
		if (audio[event->idx]->str)
		if (Pa_CloseStream(audio[event->idx]->str) != paNoError) ERROR();
		delete audio[event->idx]; audio[event->idx] = 0;}
		Channel *channel = audio[event->idx] =
		new Channel(event->wrp,event->gap,event->cdt,event->len);
		if (event->oth!=event->idx && audio.find(event->oth) != audio.end())
		channel->nxt = audio[event->oth];
		if (event->enb) {
		int inputs = 0; int outputs = 0; int count;
		for (Channel *ptr = channel; ptr; ptr = ptr->nxt) count++;
		if (event->enb > 0) outputs = count; else inputs = count;
		if (Pa_OpenDefaultStream(&channel->str,inputs,outputs,paFloat32,CALLRATE,
		paFramesPerBufferUnspecified,callback,channel) != paNoError) ERROR();
		if (Pa_StartStream(channel->str) != paNoError) ERROR();}
		break;}
	case (Levels): {
		goon = 0;
		break;}
	default: ERROR();}
}

void flow()
{
	Event *event = state[head->idx];
	switch (head->flw) {
	case (Sched): {
		alloc(nowtime+evaluate(&event->sch));
		alloc(nowtime+evaluate(&event->dly),event->idx,evaluate(&event->upd));
		break;}
	case (Back): {
		event->val = head->val;
		break;}
	case (Peek): {
		Channel *channel = audio[head->oth];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		int dif[1]; int num[1]; int sub[1]; int sup[1];
		prepwave(dif,num,sub,sup,channel,1,strtime);
		float val[dif[0]+1];
		procwave(val,dif,num,sub,sup,channel,1,dif[0]+1,SATURATE);
		progwave(channel,sub,1);
		event->val = val[dif[0]];
		alloc(nowtime+evaluate(&event->sch));
		break;}
	case (Poke): {
		double upd = evaluate(&event->upd);
		Channel *channel = audio[head->oth];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		int sub = location(strtime,channel->wrp,channel->len);
		if (channel->cnt[sub] == 0) channel->val[sub] = 0;
		channel->val[sub] += upd;
		channel->cnt[sub]++;
		alloc(nowtime+evaluate(&event->sch));
		break;}
	case (Store): {
		Event *line = timer[head->oth];
		double *ptr = line->rsp;
		for (int i = 0; i < line->num && ptr-line->rsp < line->tot; i++) {
		if (line->req[i] == 0)
		*(ptr++) = state[line->ids[i]]->val; else {
		Channel *channel = audio[line->ids[i]];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		float val[line->req[i]];
		copywave(val,channel,1,line->req[i],nowtime,SATURATE);
		for (int j = 0; j < line->req[i] && ptr-line->rsp < line->tot; j++)
		*(ptr++) = val[j];}}
		writeEvent(line,hub);
		alloc(nowtime+evaluate(&event->sch));
		break;}
	case (Load): {
		Channel *channel = audio[head->oth];
		double strtime = (channel->str ? Pa_GetStreamTime(channel->str) : nowtime);
		int sub = location(strtime,channel->wrp,channel->len);
		for (int i = 0; i < head->siz; i++) {
		int sum = modulus(sub,i,channel->len);
		if (channel->cnt[sum] == 0) channel->val[sum] = 0;
		channel->val[sum] += head->buf[i];
		channel->cnt[sum]++;}
		alloc(nowtime+evaluate(&event->sch));
		break;}
	case (Flows): {
		printf("numbug %d\n",numbug); fflush(stdout);
		break;}
	default: ERROR();}
}

int main(int argc, char **argv)
{
	if (Pa_Initialize() != paNoError) ERROR();
	if ((hub = identWrap(Linez,argv[1])) < 0) ERROR();
	allocEvent(&event,1); goon = 1;
	while (goon) {
	for (head = deloc(nowtime = gettime()); head; head = deloc(nowtime)) flow();
	if (adloc()) sub = waitRead(adloc(nowtime),-1); else sub = waitRead(0.0,-1);
	if (sub < 0) continue; readEvent(event,sub); stock();}
	if (Pa_Terminate() != paNoError) ERROR();
	return 0;
}
