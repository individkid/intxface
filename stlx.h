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
#include <set>
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
    virtual void call() = 0;
    virtual void done() = 0;
    virtual void func() = 0;
};
struct CallState {
    SafeState safe;
    std::set<DoneState*> done;
    std::set<pthread_t> todo;
    CallState() : safe(1) {std::cout << "CallState" << std::endl;}
    ~CallState() {std::cout << "~CallState" << std::endl; stop();}
    void stop() {
        safe.wait(); for (auto i = done.begin(); i != done.end(); i++) {
        todo.insert((*i)->thread); (*i)->done();}
        done.clear(); safe.post(); clear();
        safe.wait(); if (!done.empty() || !todo.empty())
        {std::cerr << "done not empty!" << std::endl; exit(-1);} safe.post();
    }
    void clear() { // joins any pushed to todo
        safe.wait(); std::set<pthread_t> doto = todo; todo.clear(); safe.post();
        for (auto i = doto.begin(); i != doto.end(); i++) pthread_join((*i),0);
    }
    void push(DoneState *ptr) {
        clear(); safe.wait();
        ptr->ptr = this;
        done.insert(ptr);
        if (pthread_create(&ptr->thread,0,call,ptr) != 0)
        {std::cerr << "failed to start thread!" << std::endl; exit(-1);}
        safe.post();
    }
    static void *call(void *ptr) { // running on separate thread
        DoneState *done = (DoneState*)ptr;
        CallState *call = done->ptr;
        done->call(); call->safe.wait();
        // if done() called by ~CallState, they're already inserted and erased
        call->todo.insert(done->thread); call->done.erase(done);
        call->safe.post(); done->func(); return 0;
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
