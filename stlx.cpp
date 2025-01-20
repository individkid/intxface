#include "stlx.h"

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
