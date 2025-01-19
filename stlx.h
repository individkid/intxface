#include <pthread.h>
#ifdef __APPLE__
#include <dispatch/dispatch.h>
#define sem_t dispatch_semaphore_t
#define sem_init(S,P,V) {*S = dispatch_semaphore_create(V);}
#define sem_post(S) {dispatch_semaphore_signal(*S);}
#define sem_wait(S) {dispatch_semaphore_wait(*S,DISPATCH_TIME_FOREVER);}
#else
#include <semaphore.h>
#endif
#ifdef __cplusplus
#include <deque>
#include <set>
#include <iostream>
struct SafeState {
    sem_t semaphore;
    SafeState(int val) {
        if (sem_init(&semaphore, 0, val) != 0) {std::cerr << "failed to create semaphore!" << std::endl; exit(-1);}
    }
    ~SafeState() {
        if (sem_destroy(&semaphore) != 0) {std::cerr << "cannot destroy semaphore!" << std::endl; exit(-1);}
    }
    void wait() {
        if (sem_wait(&semaphore) != 0) {std::cerr << "cannot wait for semaphore!" << std::endl; exit(-1);}
    }
    void post() {
        if (sem_post(&semaphore) != 0) {std::cerr << "cannot post to semaphore!" << std::endl; exit(-1);}
    }
    int get() {
        int sval;
        if (sem_getvalue(&semaphore,&sval) != 0) {std::cerr << "cannot get semaphore!" << std::endl; exit(-1);}
        return sval;
    }
    void wake() {
        if (get() == 0) post();
    }
};
struct CallState;
struct DoneState {
    CallState *ptr;
    pthread_t thread;
    char debug[64];
    virtual void call() = 0;
    virtual void done() = 0;
    virtual void func() = 0;
};
struct CallState {
    SafeState safe;
    // TODO use queues to start and stop in order
    std::set<DoneState*> todo;
    std::deque<DoneState*> doto;
    CallState() : safe(1) {std::cout << "CallState" << std::endl;}
    ~CallState() {std::cout << "~CallState before" << std::endl;
        while (1) {
        safe.wait(); if (todo.empty()) {safe.post(); break;}
        DoneState *ptr = *(todo.begin()); todo.erase(ptr);
        safe.post(); stop(ptr);} clear();
    }
    void clear() {
        while (1) {
        safe.wait(); if (doto.empty()) {safe.post(); break;}
        DoneState *ptr = doto.front(); doto.pop_front();
        safe.post(); if (pthread_join(ptr->thread,0) != 0)
        {std::cerr << "failed to join!" << std::endl; exit(-1);} ptr->func();}
    }
    void stop(DoneState *ptr) {
        safe.wait();
        ptr->done();
        todo.erase(ptr);
        doto.push_back(ptr);
        safe.post();
    }
    void push(DoneState *ptr) {
        safe.wait();
        ptr->ptr = this;
        todo.insert(ptr);
        if (pthread_create(&ptr->thread,0,call,ptr) != 0)
        {std::cerr << "failed to start thread!" << std::endl; exit(-1);}
        safe.post();
    }
    static void *call(void *ptr) { // running on separate thread
        DoneState *done = (DoneState*)ptr;
        CallState *call = done->ptr;
        done->call(); call->safe.wait();
        if (call->todo.find(done) != call->todo.end()) {
        call->doto.push_back(done); call->todo.erase(done);}
        call->safe.post(); return 0;
    }
};
#endif

#ifdef __cplusplus
extern "C" {
#endif

void *allocDeque(int siz);
void pushDeque(int siz, void *val, void *ptr);
void dropDeque(void *ptr);
void *frontDeque(void *ptr);
int sizeDeque(void *ptr);
void freeDeque(void *ptr);
#define DECLARE_DEQUE(TYPE,NAME) \
void *alloc ## NAME() {return allocDeque(sizeof(TYPE));} \
void push ## NAME(TYPE val, void *ptr) {pushDeque(sizeof(TYPE),&val,ptr);} \
void drop ## NAME(void *ptr) {dropDeque(ptr);} \
TYPE front ## NAME(void *ptr) {return *(TYPE*)frontDeque(ptr);} \
int size ## NAME(void *ptr) {return sizeDeque(ptr);} \
void free ## NAME(void *ptr) {freeDeque(ptr);} \
TYPE maybe ## NAME(TYPE val, void *ptr) {if (size ## NAME(ptr)) {val = front ## NAME(ptr); drop ## NAME(ptr);} return val;}

#ifdef __cplusplus
};
#endif
