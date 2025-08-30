#include "stlx.h"
#include <iomanip>
#include <chrono>

SlogState slog; // TODO qualify with NDEBUG
int SmartState::seqnum = 0;
SmartState::SmartState(const SmartState &oth) {
    num = oth.num; vld = oth.vld;
    if (vld) {slog.wait(num); slog.smart[num]++; slog.safe.post();}
}
SmartState &SmartState::operator=(const SmartState &oth) {
    clr(); num = oth.num; vld = oth.vld;
    if (vld) {slog.wait(num); slog.smart[num]++; slog.safe.post();}
    return *this;
}
SmartState::SmartState(std::string str) {
    slog.safe.wait();
    num = slog.limnum++; vld = true;
    slog.sstr[num] = new std::stringstream;
    slog.name[num] = str; slog.smart[num] = 1;
    slog.safe.post();
}
#define NUM(N) std::setw(slog.num) << std::setfill('0') << N << ":"
#define STR(S) S << ":"
std::ostream &SmartState::set() {
    if (!vld) return slog;
    slog.wait(num);
    std::stringstream &ret = *slog.sstr[num];
    switch (slog.ord) {
    default: case (123): ret << NUM(seqnum) << NUM(num) << STR(slog.name[num]);
    break; case (132): ret << NUM(seqnum) << STR(slog.name[num]) << NUM(num);
    break; case (312): ret << STR(slog.name[num]) << NUM(seqnum) << NUM(num);
    break; case (213): ret << NUM(num) << NUM(seqnum) << STR(slog.name[num]);
    break; case (231): ret << NUM(num) << STR(slog.name[num]) << NUM(seqnum);
    break; case (321): ret << STR(slog.name[num]) << NUM(num) << NUM(seqnum);}
    seqnum++;
    slog.safe.post();
    return ret;
}
void SmartState::clr() {
    if (!vld) return;
    slog.wait(num);
    slog.smart[num]--; vld = false;
    bool zero = (slog.smart[num] == 0);
    slog.safe.post();
    if (zero) slog.clr(num);
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

struct MaybeState {
    std::vector<uint8_t> val;
    SafeState sem;
    MaybeState() : sem(1) {}
};
void *allocMaybe(int siz)
{
    return new MaybeState();
}
void setMaybe(int siz, void *val, void *ptr)
{
    auto may = (MaybeState*)ptr;
    auto ui8 = (uint8_t*)val;
    may->sem.lock();
    may->val.resize(siz);
    may->sem.cast();
    for (int i = 0; i < siz; i++) may->val.at(i) = ui8[i];
    may->sem.ulck();
}
int getMaybe(int siz, void *val, void *ptr)
{
    auto may = (MaybeState*)ptr;
    auto ui8 = (uint8_t*)val;
    may->sem.lock();
    if (siz > may->val.size()) siz = may->val.size();
    for (int i = 0; i < siz; i++) ui8[i] = may->val.at(i);
    may->sem.ulck();
    return siz;
}
int clrMaybe(int siz, void *val, void *ptr)
{
    auto may = (MaybeState*)ptr;
    auto ui8 = (uint8_t*)val;
    may->sem.lock();
    if (siz > may->val.size()) siz = may->val.size();
    for (int i = 0; i < siz; i++) ui8[i] = may->val.at(i);
    may->val.resize(0);
    may->sem.cast();
    may->sem.ulck();
    return siz;
}
int notMaybe(int siz, void *val, void *ptr)
{
    auto may = (MaybeState*)ptr;
    auto ui8 = (uint8_t*)val;
    may->sem.lock();
    while (1) {
    int tmp = may->val.size();
    if (tmp >= siz) break;
    may->sem.cond();}
    for (int i = 0; i < siz; i++) ui8[i] = may->val.at(i);
    may->val.resize(0);
    may->sem.cast();
    may->sem.ulck();
    return siz;
}
void freeMaybe(void *ptr)
{
    auto may = (MaybeState*)ptr;
    delete may;
}

float processTime()
{
    static auto startTime = std::chrono::high_resolution_clock::now();
    auto currentTime = std::chrono::high_resolution_clock::now();
    float time = std::chrono::duration<float, std::chrono::seconds::period>(currentTime - startTime).count();
    return time;
}
