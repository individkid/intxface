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
#include <map>
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
template <class Conf, int Size> struct ChangeState {
    typedef void (*xftype)(Conf cfg, int sav, int val);
    typedef int (*yftype)(int *ref, int val);
    int config[Size];
    std::map<int,std::set<xftype>> back;
    SafeState safe; // prior protected
    int depth; pthread_t self; SafeState nest;
    ChangeState() : config{0}, safe(1), depth(0), nest(1) {std::cout << "ChangeState" << std::endl;}
    void call(Conf cfg, xftype ptr) {
        safe.wait();
        if (ptr) back[cfg].insert(ptr);
        else if (back.find(cfg) != back.end() && back[cfg].find(ptr) != back[cfg].end() && back[cfg].size() == 1) back.erase(cfg);
        else if (back.find(cfg) != back.end() && back[cfg].find(ptr) != back[cfg].end()) back[cfg].erase(ptr);
        safe.post();
    }
    int info(Conf cfg, int val, yftype fnc) {
        if (cfg < 0 || cfg >= Size) {std::cerr << "invalid info!" << std::endl; exit(-1);}
        safe.wait(); int ret = fnc(&config[cfg],val);
        safe.post(); return ret;
    }
    int jnfo(Conf cfg, int val, yftype fnc) {
        if (cfg < 0 || cfg >= Size) {std::cerr << "invalid jnfo!" << std::endl; exit(-1);}
        safe.wait(); int sav = config[cfg]; int ret = fnc(&config[cfg],val);
        std::set<xftype> todo; if (back.find(cfg) != back.end()) todo = back[cfg];
        nest.wait(); self = pthread_self(); depth++; nest.post();
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,config[cfg]);
        nest.wait(); depth--; nest.post();
        safe.post(); return ret;
    }
    int knfo(Conf cfg, int val, yftype fnc) {
        nest.wait(); if (!depth || !pthread_equal(self,pthread_self()))
        {std::cerr << "invalid knfo! " << depth << std::endl; exit(-1);} nest.post();
        int sav = config[cfg]; int ret = fnc(&config[cfg],val);
        std::set<xftype> todo; if (back.find(cfg) != back.end()) todo = back[cfg];
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,config[cfg]);
        return ret;
    }
    typedef int (*ChangeType)(int,int);
    int change(Conf cfg, int val, ChangeType opr, bool ret, bool typ) {
        if (cfg < 0 || cfg >= Size) {std::cerr << "invalid change!" << std::endl; exit(-1);}
        safe.wait(); int sav = config[cfg]; std::set<xftype> todo;
        if (typ && back.find(cfg) != back.end()) todo = back[cfg];
        config[cfg] = opr(config[cfg],val); val = config[cfg];
        nest.wait(); self = pthread_self(); depth++; nest.post();
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,config[cfg]);
        nest.wait(); depth--; nest.post();
        safe.post(); return (ret?sav:val);
    }
    static int readOp(int l, int r) {return l;}
    int read(Conf cfg) {return change(cfg,0,readOp,false,false);}
    static int writeOp(int l, int r) {return r;}
    void write(Conf cfg, int val) {change(cfg,val,writeOp,false,true);}
    static int wotsOp(int l, int r) {return l|r;}
    void wots(Conf cfg, int val) {change(cfg,val,wotsOp,false,true);}
    static int wotcOp(int l, int r) {return l&~r;}
    void wotc(Conf cfg, int val) {change(cfg,val,wotcOp,false,false);}
    static int rmwOp(int l, int r) {return l+r;}
    int rmw(Conf cfg, int val) {return change(cfg,val,rmwOp,true,true);}
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
    // TODO use queues to start and stop in order
    std::set<DoneState*> todo;
    std::deque<DoneState*> doto;
    bool lock;
    SafeState safe;
    CallState() : lock(false), safe(1) {std::cout << "CallState" << std::endl;}
    ~CallState() {std::cout << "~CallState" << std::endl;
        safe.wait(); lock = true; safe.post();
        while (1) {safe.wait();
        if (todo.empty()) {safe.post(); break;}
        DoneState *ptr = *(todo.begin()); todo.erase(ptr);
        safe.post(); stop(ptr);} clear();
    }
    void clear() {
        while (1) {safe.wait();
        if (doto.empty()) {safe.post(); break;}
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
        if (lock) {std::cerr << "push after destructor!" << std::endl; exit(-1);}
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
