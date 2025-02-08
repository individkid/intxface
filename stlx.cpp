#include "stlx.h"

SlogState slog; // TODO qualify with NDEBUG
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
std::stringstream &SmartState::set() {
    if (!vld) {std::cerr << "smart set invalid!" << std::endl; exit(-1);}
    slog.wait(num);
    std::stringstream &ret = *slog.sstr[num];
    ret << slog.name[num] << "#" << num << ":";
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
