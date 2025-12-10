#include "stlx.h"
#include <iomanip>
#include <chrono>

SlogState slog; // TODO qualify with NDEBUG
int SmartState::seqnum = 0;
void SmartState::init(std::string str) {
    slog.safe.wait();
    num = slog.limnum++; vld = true; rdy = false; this->str.str("");
    slog.sstr[num] = new std::stringstream;
    slog.name[num] = str; slog.smart[num] = 1;
    slog.safe.post();
}
void SmartState::init(const SmartState &oth) {
    num = oth.num; vld = oth.vld; rdy = false; str.str("");
    if (vld) {slog.wait(num); slog.smart[num]++; slog.safe.post();}
}
void SmartState::done() {
    if (!vld) return;
    slog.wait(num);
    slog.smart[num]--; vld = false;
    bool zero = (slog.smart[num] == 0);
    slog.safe.post();
    if (zero) slog.clr(num);
}
#define NUM(N) std::setw(slog.num) << std::setfill('0') << N << ":"
#define STR(S) S << ":"
void SmartState::wait() {
    if (!vld) return;
    if (!rdy) {rdy = true;
    slog.wait(num);
    switch (slog.ord) {
    default: case (123): str << NUM(seqnum) << NUM(num) << STR(slog.name[num]);
    break; case (132): str << NUM(seqnum) << STR(slog.name[num]) << NUM(num);
    break; case (312): str << STR(slog.name[num]) << NUM(seqnum) << NUM(num);
    break; case (213): str << NUM(num) << NUM(seqnum) << STR(slog.name[num]);
    break; case (231): str << NUM(num) << STR(slog.name[num]) << NUM(seqnum);
    break; case (321): str << STR(slog.name[num]) << NUM(num) << NUM(seqnum);}
    seqnum++;}
}
void SmartState::post() {
    if (!vld) return;
    *slog.sstr[num] << str.str(); str.str(""); rdy = false;
    slog.safe.post();
}

void *allocDeque(int siz)
{
    return new std::deque<std::vector<uint8_t>>;
}
void pushDeque(int siz, void *val, void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    auto ui8 = (uint8_t*)val;
    std::vector<uint8_t> vec(siz);
    for (int i = 0; i < siz; i++) vec[i] = ui8[i];
    que->push_back(vec);
}
void popDeque(void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    que->pop_back();
}
void dropDeque(void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    que->pop_front();
}
void *frontDeque(void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    return (void*)que->front().data();
}
void *backDeque(void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    return (void*)que->back().data();
}
void *ptrDeque(int idx, void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    return (void*)(*que)[idx].data();
}
int sizeDeque(void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    return que->size();
}
void freeDeque(void *ptr)
{
    auto que = (std::deque<std::vector<uint8_t>>*)ptr;
    delete que;
}

void *allocSafe(int val)
{
    return new SafeState(val);
}
int waitSafe(void *ptr)
{
    auto saf = (SafeState*)ptr;
    return saf->wait();
}
int iWaitSafe(void *ptr, int idx)
{
    auto saf = (SafeState*)ptr;
    return saf->wait(idx);
}
int postSafe(void *ptr)
{
    auto saf = (SafeState*)ptr;
    return saf->post();
}
int iPostSafe(void *ptr, int idx)
{
    auto saf = (SafeState*)ptr;
    return saf->post(idx);
}
void doneSafe(void *ptr)
{
    auto saf = (SafeState*)ptr;
    saf->done();
}
void iDoneSafe(void *ptr, int idx)
{
    auto saf = (SafeState*)ptr;
    saf->done(idx);
}
void freeSafe(void *ptr)
{
    auto saf = (SafeState*)ptr;
    delete saf;
}
int timeSafe(void *ptr, double dif)
{
    auto saf = (SafeState*)ptr;
    return saf->wait(dif);
}
int iTimeSafe(void *ptr, double dif, int idx)
{
    auto saf = (SafeState*)ptr;
    return saf->wait(dif,idx);
}
int keepSafe(void *ptr)
{
    auto saf = (SafeState*)ptr;
    return saf->keep();
}
int iKeepSafe(void *ptr, int idx)
{
    auto saf = (SafeState*)ptr;
    return saf->keep(idx);
}
int iPeekSafe(void *ptr, int idx)
{
    auto saf = (SafeState*)ptr;
    return saf->peek(idx);
}
int peekSafe(void *ptr)
{
    auto saf = (SafeState*)ptr;
    return saf->peek();
}

float processTime()
{
    static auto startTime = std::chrono::high_resolution_clock::now();
    auto currentTime = std::chrono::high_resolution_clock::now();
    float time = std::chrono::duration<float, std::chrono::seconds::period>(currentTime - startTime).count();
    return time;
}
