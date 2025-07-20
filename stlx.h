#include <stdint.h>
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
#include <vector>
#include <deque>
#include <set>
#include <map>
#include <iostream>
#include <sstream>
#include <string>

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

struct SmartState {
    static int seqnum;
    int num;
    bool vld;
    SmartState() : num(0), vld(false) {}
    SmartState(const SmartState &oth);
    SmartState(const SmartState &&oth) = delete;
    SmartState(SmartState &&oth) = delete;
    SmartState(const volatile SmartState &&oth) = delete;
    SmartState(volatile SmartState &&oth) = delete;
    SmartState &operator=(const SmartState &oth);
    SmartState(std::string str);
    ~SmartState() {clr();}
    std::ostream &set();
    template<class Type> std::ostream &operator<<(Type val) {
    std::ostream &str = set(); str << val; return str;}
    void clr();
};

struct SlogState : public std::ostream {
    SafeState safe;
    std::map<int, std::stringstream*> sstr;
    std::map<int, std::string> name;
    std::map<int, int> smart;
    int minnum, limnum, min, lim, ord, num;
    SlogState() : safe(1), minnum(0), limnum(0), min(0), lim(0), ord(0), num(0) {}
    void onof(int m, int l, int o, int n) {
        min = m;
        lim = l;
        ord = o;
        num = n;
    }
    bool check(int num, int min, int lim) {
        if (min == lim) return false;
        if (min < lim && (num < min || num >= lim)) return false;
        if (min > lim && (num >= min || num < lim)) return false;
        return true;
    }
    void wait(int num) {
        safe.wait();
        if (!check(num,minnum,limnum))
        {std::cerr << "invalid num wait!" << std::endl; exit(-1);}
        if (sstr.find(num) == sstr.end())
        {std::cerr << "invalid find wait! " << num << std::endl; exit(-1);}
    }
    void clr(int num) {
        wait(num);
        if (smart[num] != 0) {std::cerr << "invalid clr smart!" << std::endl; exit(-1);}
        if (check(num,min,lim)) std::cout << sstr[num]->str();
        delete sstr[num]; sstr.erase(num); name.erase(num); smart.erase(num);
        while (sstr.begin() != sstr.end() &&
        sstr.find(minnum) == sstr.end()) minnum++;
        safe.post();
    }
    void clr() {
        safe.wait();
        for (auto i = sstr.begin(); i != sstr.end(); i++) {
        if (check((*i).first,min,lim)) {std::cout << (*i).second->str(); (*i).second->str("");}}
        safe.post();
    }
    template <class Type> std::ostream &operator<<(Type typ) {
        return *this;
    }
};
extern SlogState slog; // TODO qualify with NDEBUG

template <class Conf, int Size> struct ChangeState {
    typedef void (*xftype)(Conf cfg, int sav, int val);
    typedef int (*yftype)(int *ref, int val);
    int config[Size];
    std::map<Conf,std::set<xftype>> back;
    SafeState safe; // prior protected
    int depth; pthread_t self; SafeState nest;
    ChangeState() : config{0}, safe(1), depth(0), nest(1) {std::cout << "ChangeState" << std::endl;}
    ~ChangeState() {std::cout << "~ChangeState" << std::endl;}
    void call(Conf cfg, xftype ptr) {
        safe.wait();
        if (ptr) back[cfg].insert(ptr);
        else if (back.find(cfg) != back.end() && back[cfg].find(ptr) != back[cfg].end() && back[cfg].size() == 1) back.erase(cfg);
        else if (back.find(cfg) != back.end() && back[cfg].find(ptr) != back[cfg].end()) back[cfg].erase(ptr);
        safe.post();
    }
    int info(Conf cfg, int val, yftype fnc) { // no callback
        if (cfg < 0 || cfg >= Size) {std::cerr << "invalid info!" << std::endl; exit(-1);}
        safe.wait(); int ret = fnc(&config[cfg],val);
        safe.post(); return ret;
    }
    int jnfo(Conf cfg, int val, yftype fnc) { // call callbacks
        if (cfg < 0 || cfg >= Size) {std::cerr << "invalid jnfo!" << std::endl; exit(-1);}
        safe.wait(); int sav = config[cfg]; int ret = fnc(&config[cfg],val);
        std::set<xftype> todo; if (back.find(cfg) != back.end()) todo = back[cfg];
        nest.wait(); self = pthread_self(); depth++; nest.post();
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,config[cfg]);
        // would block forever if calls info or jnfo
        nest.wait(); depth--; nest.post();
        safe.post(); return ret;
    }
    int knfo(Conf cfg, int val, yftype fnc) { // called from callback
        nest.wait(); if (!depth || !pthread_equal(self,pthread_self()))
        {std::cerr << "invalid knfo! " << depth << std::endl; exit(-1);} nest.post();
        int sav = config[cfg]; int ret = fnc(&config[cfg],val);
        // would not block if called from jnfo, but thread safe since pthread_equal to calling jnfo
        std::set<xftype> todo; if (back.find(cfg) != back.end()) todo = back[cfg];
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,config[cfg]);
        return ret;
    }
    static int readFn(int *ref, int val) {return *ref;}
    int read(Conf cfg) {return info(cfg,0,readFn);}
    static int writeFn(int *ref, int val) {*ref = val; return 0;}
    void write(Conf cfg, int val) {jnfo(cfg,val,writeFn);}
    static int wotsFn(int *ref, int val) {*ref = *ref|val; return 0;}
    void wots(Conf cfg, int val) {jnfo(cfg,val,wotsFn);}
    static int wotcFn(int *ref, int val) {*ref = *ref&~val; return 0;}
    void wotc(Conf cfg, int val) {jnfo(cfg,val,wotcFn);}
    static int rmwFn(int *ref, int val) {int ret = *ref; *ref = *ref+val; return ret;}
    int rmw(Conf cfg, int val) {return jnfo(cfg,val,rmwFn);}
};

struct CallState;
struct DoneState {
    CallState *ptr;
    pthread_t thread;
    char debug[64];
    virtual void call() = 0;
    virtual void done() = 0;
    virtual void heap() = 0;
    virtual void noop() = 0;
};
struct CallState {
    // TODO use queues to start and stop in order
    std::set<DoneState*> todo;
    std::deque<DoneState*> doto;
    std::deque<bool> fall;
    std::deque<DoneState*> done;
    std::deque<int> mask;
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
        bool temp = fall.front(); fall.pop_front();
        safe.post(); if (!temp) ptr->done();
        if (pthread_join(ptr->thread,0) != 0)
        {std::cerr << "failed to join!" << std::endl; exit(-1);}
        ptr->heap();}
    }
    void stop(DoneState *ptr) {
        safe.wait();
        if (todo.find(ptr) != todo.end()) {
        doto.push_back(ptr);
        fall.push_back(false);
        todo.erase(ptr);}
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
        call->doto.push_back(done);
        call->fall.push_back(true);
        call->todo.erase(done);}
        call->safe.post(); return 0;
    }
    void back(DoneState *ptr, int thd) {
        done.push_back(ptr);
        mask.push_back(1<<thd);
    }
    void back(int sav, int val) {
        for (int i = 0; i < done.size(); i++) {
        if ((val & mask[i]) && !(sav & mask[i])) push(done[i]);
        if (!(val & mask[i]) && (sav & mask[i])) stop(done[i]);
        if ((val & mask[i]) && (sav & mask[i])) done[i]->noop();}
    }
};

template <class Type> struct HeapState {
    std::vector<Type> vec;
    int bas, siz;
    HeapState() : bas(0), siz(0) {
    }
    HeapState(int num) : vec(num), bas(0), siz(0) {
    }
    HeapState(int num, int siz) : vec(num), bas(0), siz(siz) {
        if (siz < 0 || siz > num)
        {std::cerr << "invalid heap size!" << std::endl; exit(-1);}
    }
    void push(int num) {
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; exit(-1);}
        int size = vec.size();
        int wrap = bas+siz-size;
        vec.resize(bas+siz+num);
        for (int i = 0; i < wrap; i++) vec[size++] = vec[i];
    }
    int size() {
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; exit(-1);}
        return siz;
    }
    void clear() {
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; exit(-1);}
        bas = 0; siz = 0;
    }
    void clear(int i) {
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; exit(-1);}
        if (siz < i) {std::cerr << "invalid clear siz!" << std::endl; exit(-1);}
        bas = (bas+i)%vec.size();
        siz -= i;
    }
    HeapState<Type> &operator<<(const Type &val) {
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; exit(-1);}
        if (siz == vec.size()) push(1);
        vec[(siz+bas)%vec.size()] = val;
        siz += 1;
        return *this;
    }
    Type &operator[](int i) {
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; exit(-1);}
        if (i < 0 || i >= siz) {std::cerr << "invalid heap sub!" << std::endl; *(int*)0 = 0; exit(-1);}
        return vec[(i+bas)%vec.size()];
    }
};
#endif

#ifdef __cplusplus
extern "C" {
#endif

void *allocDeque(int siz);
void pushDeque(int siz, void *val, void *ptr);
void popDeque(void *ptr);
void dropDeque(void *ptr);
void *frontDeque(void *ptr);
void *backDeque(void *ptr);
void *ptrDeque(int idx, void *ptr);
int sizeDeque(void *ptr);
void freeDeque(void *ptr);
#define DECLARE_DEQUE(TYPE,NAME) \
void *alloc ## NAME() {return allocDeque(sizeof(TYPE));} \
void push ## NAME(TYPE val, void *ptr) {pushDeque(sizeof(TYPE),&val,ptr);} \
void pop ## NAME(void *ptr) {popDeque(ptr);} \
void drop ## NAME(void *ptr) {dropDeque(ptr);} \
TYPE front ## NAME(void *ptr) {return *(TYPE*)frontDeque(ptr);} \
TYPE back ## NAME(void *ptr) {return *(TYPE*)backDeque(ptr);} \
TYPE *ptr ## NAME(int idx, void *ptr) {return (TYPE*)ptrDeque(idx,ptr);} \
int size ## NAME(void *ptr) {return sizeDeque(ptr);} \
void free ## NAME(void *ptr) {freeDeque(ptr);} \
TYPE maybe ## NAME(TYPE val, void *ptr) {if (size ## NAME(ptr)) {val = front ## NAME(ptr); drop ## NAME(ptr);} return val;}

float processTime();

#ifdef __cplusplus
};
#endif
