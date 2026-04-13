#include <stdint.h>
#include <pthread.h>
#include <string.h>
#include <math.h>
#include "proto.h"
#ifdef __cplusplus
#include <array>
#include <vector>
#include <deque>
#include <list>
#include <set>
#include <map>
#include <iostream>
#include <sstream>
#include <string>
#include <sstream>

struct SafeState {
    pthread_mutex_t mutex;
    pthread_cond_t condit;
    int count;
    SafeState(int val) {
        if (pthread_mutex_init(&mutex, 0) != 0) {std::cerr << "failed to create mutex!" << std::endl; exit(-1);}
        if (pthread_cond_init(&condit, 0) != 0) {std::cerr << "failed to create cond!" << std::endl; exit(-1);}
        count = val;
    }
    ~SafeState() {
        if (pthread_cond_destroy(&condit) != 0) {std::cerr << "cannot destroy cond!" << std::endl; exit(-1);}
        if (pthread_mutex_destroy(&mutex) != 0) {std::cerr << "cannot destroy mutex!" << std::endl; exit(-1);}
    }
    int wait() {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        while (count == 0) if (pthread_cond_wait(&condit,&mutex) != 0) {std::cerr << "cannot wait cond!" << std::endl; exit(-1);}
        if (count > 0) count -= 1;
        int ret = count;
        if (pthread_mutex_unlock(&mutex) != 0) {std::cerr << "cannot unlock mutex!" << std::endl; exit(-1);}
        return ret;
    }
    int wait(double dif) {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        struct timespec ts;
        if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {std::cerr << "cannot get time!" << std::endl; exit(-1);}
        dif += ts.tv_sec + (double)ts.tv_nsec/NANOSECONDS;
        double tsi, tsf; tsf = modf(dif, &tsi); ts.tv_sec = tsi; ts.tv_nsec = tsf*NANOSECONDS;
        int val = 0; while (count == 0 && val == 0) if ((val = pthread_cond_timedwait(&condit,&mutex,&ts)) != 0 && val != ETIMEDOUT) {std::cerr << "cannot timed wait!" << std::endl; exit(-1);}
        if (val != ETIMEDOUT && count > 0) count -= 1;
        int ret = count;
        if (pthread_mutex_unlock(&mutex) != 0) {std::cerr << "cannot unlock mutex!" << std::endl; exit(-1);}
        return ret;
    }
    int post() {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        if (count >= 0) count += 1;
        int ret = count;
        if (pthread_cond_broadcast(&condit) != 0) {std::cerr << "cannot broadcast cond!" << std::endl; exit(-1);}
        if (pthread_mutex_unlock(&mutex) != 0) {std::cerr << "cannot unlock mutex!" << std::endl; exit(-1);}
        return ret;
    }
    void done() {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        count = -1;
        if (pthread_cond_broadcast(&condit) != 0) {std::cerr << "cannot broadcast cond!" << std::endl; exit(-1);}
        if (pthread_mutex_unlock(&mutex) != 0) {std::cerr << "cannot unlock mutex!" << std::endl; exit(-1);}
    }
};

struct SmartState {
    static int seqnum;
    int num;
    bool vld, rdy;
    std::stringstream str;
    SmartState() : num(0), vld(false), rdy(false), str("") {}
    SmartState(const SmartState &oth) {init(oth);}
    SmartState(const SmartState &&oth) = delete;
    SmartState(SmartState &&oth) = delete;
    SmartState(const volatile SmartState &&oth) = delete;
    SmartState(volatile SmartState &&oth) = delete;
    SmartState &operator=(const SmartState &oth) {done(); init(oth); return *this;}
    SmartState(std::string str) {init(str);}
    ~SmartState() {done();}
    void init(std::string str);
    void init(const SmartState &oth);
    void done();
    void wait();
    void post();
    SmartState &operator<<(char val) {
        wait();
        if (val == '\n' && vld) {str << std::endl; post();}
        else if (vld) str << val;
        return *this;
    }
    template<class Type> SmartState &operator<<(Type val) {
        wait();
        if (vld) str << val;
        return *this;
    }
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
    typedef void (*xftype)(Conf cfg, int sav, int val, int act);
    typedef int (*yftype)(int *ref, int val);
    int config[Size];
    std::map<Conf,std::set<xftype>> back;
    SafeState safe; // prior protected
    int depth; pthread_t self; SafeState nest;
    ChangeState() : config{0}, safe(1), depth(0), nest(1) {
        // std::cout << "ChangeState" << std::endl;
    }
    ~ChangeState() {
        // std::cout << "~ChangeState" << std::endl;
    }
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
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,val,config[cfg]);
        // would block forever if calls info or jnfo
        nest.wait(); depth--; nest.post();
        safe.post(); return ret;
    }
    int knfo(Conf cfg, int val, yftype fnc) { // called from callback
        nest.wait(); if (!depth || !pthread_equal(self,pthread_self()))
        {std::cerr << "invalid knfo! " << depth << std::endl; *(int*)0=0; exit(-1);} nest.post();
        int sav = config[cfg]; int ret = fnc(&config[cfg],val);
        // would not block if called from jnfo, but thread safe since pthread_equal to calling jnfo
        std::set<xftype> todo; if (back.find(cfg) != back.end()) todo = back[cfg];
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,val,config[cfg]);
        return ret;
    }
    int hnfo() { // whether in callback
        nest.wait(); int ret = (depth && pthread_equal(self,pthread_self())); nest.post();
        return ret;
    }
    static int readFn(int *ref, int val) {return *ref;}
    int read(Conf cfg) {return info(cfg,0,readFn);}
    static int writeFn(int *ref, int val) {*ref = val; return 0;}
    void write(Conf cfg, int val) {jnfo(cfg,val,writeFn);}
    void poke(Conf cfg, int val) {info(cfg,val,writeFn);}
    static int wotsFn(int *ref, int val) {*ref = *ref|val; return 0;}
    void wots(Conf cfg, int val) {jnfo(cfg,val,wotsFn);}
    static int wotcFn(int *ref, int val) {*ref = *ref&~val; return 0;}
    void wotc(Conf cfg, int val) {jnfo(cfg,val,wotcFn);}
    static int rmwFn(int *ref, int val) {int ret = *ref; *ref = *ref+val; return ret;}
    int rmw(Conf cfg, int val) {return jnfo(cfg,val,rmwFn);}
    static int razFn(int *ref, int val) {*ref = 0; return 0;}
    void raz(Conf cfg, int val) {jnfo(cfg,val,razFn);}
};

struct CallState;
struct DoneState {
    CallState *back;
    pthread_t thread;
    char debug[64];
    virtual void call() = 0;
    virtual void done() = 0;
    virtual void heap() = 0;
    virtual void noop() = 0;
};
struct CallState {
    std::set<DoneState*> todo;
    std::set<DoneState*> doto;
    std::vector<DoneState*> mask;
    bool lock;
    SafeState safe;
    pthread_t thrd;
    CallState() : lock(false), safe(1), thrd(pthread_self()) {
        // std::cout << "CallState" << std::endl;
    }
    ~CallState() {
        // std::cout << "~CallState" << std::endl;
        safe.wait(); lock = true; safe.post();
        while (1) {
        safe.wait();
        if (todo.empty()) {safe.post(); break;}
        DoneState *ptr = *todo.begin();
        safe.post();
        ptr->done();}
        clear();
    }
    bool self() {
        return (pthread_self() == thrd);
    }
    void clear() {
        safe.wait();
        std::set<DoneState*> both;
        for (auto i = todo.begin(); i != todo.end(); i++) both.insert(*i);
        for (auto i = doto.begin(); i != doto.end(); i++) both.insert(*i);
        safe.post();
        for (auto i = both.begin(); i != both.end(); i++) {
        if (pthread_join((*i)->thread,0) != 0)
        {std::cerr << "failed to join!" << std::endl; exit(-1);}
        (*i)->heap();}
    }
    void push(DoneState *ptr) {
        safe.wait();
        if (lock) {std::cerr << "push after destructor!" << std::endl; exit(-1);}
        ptr->back = this;
        todo.insert(ptr);
        if (pthread_create(&ptr->thread,0,call,ptr) != 0)
        {std::cerr << "failed to start thread!" << std::endl; exit(-1);}
        safe.post();
    }
    static void *call(void *ptr) { // running on separate thread
        DoneState *done = (DoneState*)ptr;
        CallState *call = done->back;
        done->call();
        call->safe.wait();
        if ((call->doto.find(done) == call->doto.end()) == (call->todo.find(done) == call->todo.end()))
        {std::cerr << "fall not found! " << done->debug << std::endl; exit(-1);}
        call->todo.erase(done); call->doto.insert(done);
        call->safe.post();
        return 0;
    }
    void back(DoneState *ptr, int thd) {
        safe.wait();
        if (mask.size() <= thd) mask.resize(thd+1,0);
        mask[thd] = ptr;
        safe.post();
    }
    DoneState *get(int i) {
        safe.wait();
        if (i < 0 || i >= mask.size()) {safe.post(); return 0;}
        DoneState *ptr = mask[i];
        safe.post();
        return ptr;
    }
    void open(int sav, int val, int act) {
        int open = act & ~sav;
        int done = ~act & sav;
        for (int i = ffs(open)-1; open; i = ffs(open&=~(1<<i))-1) if (get(i)) push(get(i));
        for (int i = ffs(done)-1; done; i = ffs(done&=~(1<<i))-1) if (get(i)) get(i)->done();
    }
    void wake(int sav, int val, int act) {
        int wake = val & ~sav;
        for (int i = ffs(wake)-1; wake; i = ffs(wake&=~(1<<i))-1) if (get(i)) get(i)->noop();
    }
};

template <class Type, int Size = 0> struct HeapState {
    int bas, siz, rub, spr;
    std::vector<Type> vec;
    std::array<Type,Size> ary;
    HeapState() : bas(0), siz(0), rub(0), spr(0) {}
    int chk() const {
        return (Size?Size:vec.size());
    }
    void push(int num) {
        if (Size) {std::cerr << "invalid bas use!" << std::endl; *(int*)0=0; exit(-1);}
        if (siz < 0 || siz > vec.size() || bas < 0 || (vec.size() > 0 && bas >= vec.size()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        // TODO allow for negative num
        int size = vec.size();
        int wrap = bas+siz-size;
        vec.resize(bas+siz+num);
        for (int i = 0; i < wrap; i++) vec[(size++)%vec.size()] = vec[i];
    }
    int size() {
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        return siz;
    }
    void clear() {
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        bas = 0; siz = 0; rub = 0; spr = 0;
    }
    void clear(int i) {
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        if (siz < i) {std::cerr << "invalid clear siz!" << std::endl; *(int*)0=0; exit(-1);}
        bas = (bas+i)%chk();
        siz -= i;
    }
    void fill(Type val, int num) {
        while (siz < num) *this << val;
    }
    void fill(Type val) {
        fill(val,chk());
    }
    HeapState<Type,Size> &operator<<(const Type &val) {
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        if (siz == chk()) push(1);
        if (Size) ary[(siz+bas)%Size] = val;
        else vec[(siz+bas)%vec.size()] = val;
        siz += 1;
        return *this;
    }
    Type &add(int j) {
        if (rub == 0) {spr = 0; siz = 0;}
        int i = rub;
        rub += j;
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        return operator[](i);
    }
    Type &get(int j) {
        int i = spr;
        if (siz == 0) {siz = rub; rub = 0;}
        spr += j;
        if (spr >= siz) spr = 0;
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        return operator[](i);
    }
    int get() {
        return (siz?siz:rub);
    }
    void chk(int i) const {
        if (siz < 0 || siz > chk() || bas < 0 || bas >= chk())
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        if (i < 0) {std::cerr << "invalid vec index!" << std::endl; exit(-1);}
    }
    Type &operator[](int i) {
        chk(i);
        if (Size) return ary[(i+bas)%Size];
        return vec[(i+bas)%vec.size()];
    }
    const Type &operator[](int i) const {
        chk(i);
        if (Size) return ary[(i+bas)%Size];
        return vec[(i+bas)%vec.size()];
    }
    Type *data() {
        if (Size) return ary.data();
        return vec.data();
    }
};

template <int Size, int Dim, int Min> struct SimpleState {
    struct Pair {
        int resrc;
        bool reuse;
    };
    enum Prior {
        PoolPri,
        QualPri,
        GlobPri,
        Priors,
    };
    typedef int Indx; // interface identifier
    struct IndxLess {
        bool operator()(const Indx &lhs, const Indx &rhs) const {
            return lhs < rhs;
        }
    };
    typedef std::set<Indx,IndxLess> Pool;
    typedef std::list<Indx> List;
    typedef List::iterator Seqn; // sequence number
    typedef std::array<int,Dim> Only; // key value only
    struct OnlyLess {
        bool operator()(const Only &lhs, const Only &rhs) const {
            for (int i = 0; i < Dim; i++) if (lhs[i] < rhs[i]) return true;
            return false;
        }
    };
    typedef int Hndl; // class base handle
    struct HndlLess {
        bool operator()(const Hndl &lhs, const Hndl &rhs) const {
            return lhs < rhs;
        }
    };
    std::map<Only,List,OnlyLess> ording; // to find next or last of list
    List seqing; // to find next or last of all
    std::map<Indx,Only,IndxLess> keyval; // which list index is in
    std::map<Indx,Seqn,IndxLess> ordnum; // sparse ordering in ording
    std::map<Indx,Seqn,IndxLess> allnum; // sparse ordering in seqing
    Pool pool, pend;
    std::map<Hndl,Only,HndlLess> idxkey; // handle for qualifier
    std::map<Hndl,Pool,HndlLess> idxign; // handle for unrepeats
    SimpleState() {
        for (int i = 0; i < Size; i++) pool.insert(i);
    }
    Only get(int idx, int tag, int val) {
        if (idxkey.find(idx) == idxkey.end()) {
        Only tmp = {0}; idxkey[idx] = tmp;}
        Only tmp = idxkey[idx];
        if (tag >= 0 && tag < Dim) tmp[tag] = val;
        return tmp;
    }
    Indx first(List &map, Indx def, Pool &ign) {
        for (auto i = map.begin(); i != map.end(); i++)
        if (ign.find(*i) == ign.end()) return *i;
        return def;
    }
    Indx last(List &map, Indx def, Pool &ign) {
        for (auto i = map.rbegin(); i != map.rend(); i++)
        if (ign.find(*i) == ign.end()) return *i;
        return def;
    }
    Indx find(Pool &set, Indx def, Pool &ign) { // TODO pool and pend should be disjoint
        for (auto i = set.begin(); i != set.end(); i++)
        if (ign.find(*i) == ign.end()) return *i;
        return def;
    }
    void show(int idx, int tag, int val, char **str) {
        Only tmp = get(idx,tag,val);
        std::stringstream sst; sst << tmp[0];
        for (int i = 1; i < Dim; i++) sst << "/" << tmp[i];
        *str = (char*)realloc((void*)*str,sst.str().size()+1); strcpy(*str,sst.str().c_str());
    }
    // TODO add resize function
    void remove(int idx) {
        if (pool.find(idx) != pool.end()) return;
        if (pend.find(idx) != pend.end()) return;
        if (keyval.find(idx) == keyval.end()) *(int*)0=0;
        Only tmp = keyval.at(idx);
        if (ordnum.find(idx) == ordnum.end()) *(int*)0=0;
        if (allnum.find(idx) == allnum.end()) *(int*)0=0;
        ording.at(tmp).erase(ordnum.at(idx));
        seqing.erase(allnum.at(idx));
        keyval.erase(idx);
        ordnum.erase(idx);
        allnum.erase(idx);
        pool.insert(idx);
    }
    void insert(Only &tmp, int idx) { // after insert given idx is newest
        if (pool.find(idx) != pool.end()) pool.erase(idx);
        else if (pend.find(idx) != pend.end()) pend.erase(idx);
        else *(int*)0=0;
        if (keyval.find(idx) != keyval.end()) *(int*)0=0; keyval[idx] = tmp;
        ordnum[idx] = ording[tmp].insert(ording[tmp].end(),idx);
        allnum[idx] = seqing.insert(seqing.end(),idx);
    }
    void reserve(int idx) {
        remove(idx);
        if (pool.find(idx) == pool.end()) *(int*)0=0; pool.erase(idx);
        if (pend.find(idx) != pend.end()) *(int*)0=0; pend.insert(idx);
    }
    void release(int idx) {
        if (pend.find(idx) == pend.end()) *(int*)0=0; pend.erase(idx);
        if (pool.find(idx) != pool.end()) *(int*)0=0; pool.insert(idx);
    }
    void advance(Only &tmp, int idx) {
        remove(idx);
        insert(tmp,idx);
    }
    Pair reuse(Pool &ign, SmartState log) {
        Pair ret;
        ret.reuse = 1; ret.resrc = find(pool,-1,ign);
        if (ret.resrc < 0) ret.resrc = first(seqing,-1,ign);
        if (ret.resrc < 0) {ret.resrc = 0; ret.reuse = -1;}
        if (ret.resrc < 0) *(int*)0=0;
        return ret;
    }
    Pair oldbuf(Only &tmp, Pool &ign, SmartState log) { // used for insert in advance, so no preemptive insert
        Pair ret;
        // log << "oldbuf " << tmp[0]; for (int i = 1; i < Dim; i++) log << "/" << tmp[i]; log << '\n';
        ret.reuse = 0; ret.resrc = first(ording[tmp],-1,ign);
        if (ret.resrc < 0) return reuse(ign,log);
        if (ret.resrc < 0) *(int*)0=0;
        return ret;
    }
    Pair getbuf(Only &tmp, Pool &ign, SmartState log) { // used for reserve, so returns new oldest if possible
        Pair ret;
        // log << "getbuf " << tmp[0]; for (int i = 1; i < Dim; i++) log << "/" << tmp[i]; log << '\n';
        ret.reuse = 0; ret.resrc = first(ording[tmp],-1,ign);
        if (ret.resrc < 0 || ording[tmp].size() < Min) return reuse(ign,log);
        if (ret.resrc < 0) *(int*)0=0;
        return ret;
    }
    Pair newbuf(Only &tmp, Pool &ign, SmartState log) { // used for depend, so returns result of last advance
        Pair ret;
        // log << "newbuf " << tmp[0]; for (int i = 1; i < Dim; i++) log << "/" << tmp[i]; log << '\n';
        ret.reuse = 0; ret.resrc = last(ording[tmp],-1,ign);
        if (ret.resrc < 0) return reuse(ign,log);
        if (ret.resrc < 0) *(int*)0=0;
        return ret;
    }
    void qualify(int idx, int tag, int val) {
        if (idxkey.find(idx) == idxkey.end()) {
        Only tmp = {0}; idxkey[idx] = tmp;}
        if (tag >= 0 && tag < Dim) idxkey[idx][tag] = val;
    }
    void ignore(int idx, int res) {
        idxign[idx].insert(res);
    }
    void notice(int idx, int res) {
        idxign[idx].erase(res);
    }
    Pair oldbuf(int idx, int tag, int val, SmartState log) {
        Only tmp = get(idx,tag,val);
        return oldbuf(tmp,idxign[idx],log);
    }
    Pair getbuf(int idx, int tag, int val, SmartState log) {
        Only tmp = get(idx,tag,val);
        return getbuf(tmp,idxign[idx],log);
    }
    Pair newbuf(int idx, int tag, int val, SmartState log) {
        Only tmp = get(idx,tag,val);
        return newbuf(tmp,idxign[idx],log);
    }
    int get(int idx, int tag) {
        if (keyval.find(idx) == keyval.end()) {*(int*)0=0;exit(-1);}
        if (tag < 0 || tag >= Dim) {*(int*)0=0;exit(-1);}
        return keyval[idx][tag];
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

void *allocMap();
void insertMap(int ksz, void *key, int siz, void *val, void *ptr);
void eraseMap(int ksz, void *key, void *ptr);
int existMap(int ksz, void *key, void *ptr);
void *findMap(int ksz, void *key, void *ptr);
void freeMap(void *ptr);
#define DECLARE_MAP(KEY,TYPE,NAME) \
void *alloc ## NAME() {return allocMap();} \
void insert ## NAME(KEY key, TYPE val, void *ptr) {insertMap(sizeof(KEY),&key,sizeof(TYPE),&val,ptr);} \
void erase ## NAME(KEY key, void *ptr) {eraseMap(sizeof(KEY),&key,ptr);} \
int exist ## NAME(KEY key, void *ptr) {return existMap(sizeof(KEY),&key,ptr);} \
TYPE find ## NAME(KEY key, void *ptr) {return *(TYPE*)findMap(sizeof(KEY),&key,ptr);} \
TYPE *ptr ## NAME(KEY key, void *ptr) {return findMap(sizeof(KEY),&key,ptr);} \
void free ## NAME(void *ptr) {freeMap(ptr);}

void *allocSafe(int val);
int waitSafe(void *ptr);
int postSafe(void *ptr);
void doneSafe(void *ptr);
void freeSafe(void *ptr);
int timeSafe(void *ptr, double dif);

float processTime();

#ifdef __cplusplus
};
#endif
