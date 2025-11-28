#include <stdint.h>
#include <pthread.h>
#ifdef __cplusplus
#include <vector>
#include <deque>
#include <set>
#include <map>
#include <iostream>
#include <sstream>
#include <string>
#include <strings.h>

struct SafeState {
    pthread_mutex_t mutex;
    pthread_cond_t condit;
    int semaphore;
    SafeState(int val) {
        semaphore = val;
        if (pthread_mutex_init(&mutex, 0) != 0) {std::cerr << "failed to create mutex!" << std::endl; exit(-1);}
        if (pthread_cond_init(&condit, 0) != 0) {std::cerr << "failed to create cond!" << std::endl; exit(-1);}
    }
    ~SafeState() {
        if (pthread_cond_destroy(&condit) != 0) {std::cerr << "cannot destroy cond!" << std::endl; exit(-1);}
        if (pthread_mutex_destroy(&mutex) != 0) {std::cerr << "cannot destroy mutex!" << std::endl; exit(-1);}
    }
    int wait() {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        while (semaphore == 0) if (pthread_cond_wait(&condit,&mutex) != 0) {std::cerr << "cannot wait cond!" << std::endl; exit(-1);}
        if (semaphore > 0) semaphore -= 1;
        int ret = semaphore;
        if (pthread_mutex_unlock(&mutex) != 0) {std::cerr << "cannot unlock mutex!" << std::endl; exit(-1);}
        return ret;
    }
    int post() {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        if (semaphore >= 0) semaphore += 1;
        int ret = semaphore;
        if (pthread_cond_broadcast(&condit) != 0) {std::cerr << "cannot broadcast cond!" << std::endl; exit(-1);}
        if (pthread_mutex_unlock(&mutex) != 0) {std::cerr << "cannot unlock mutex!" << std::endl; exit(-1);}
        return ret;
    }
    void hack() {
        if (pthread_mutex_lock(&mutex) != 0) {std::cerr << "cannot lock mutex!" << std::endl; exit(-1);}
        semaphore = -1;
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
    SmartState(const SmartState &oth);
    SmartState(const SmartState &&oth) = delete;
    SmartState(SmartState &&oth) = delete;
    SmartState(const volatile SmartState &&oth) = delete;
    SmartState(volatile SmartState &&oth) = delete;
    SmartState &operator=(const SmartState &oth) = delete;
    SmartState(std::string str);
    ~SmartState();
    void wait();
    void post();
    void done();
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
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,val,config[cfg]);
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
        for (auto i = todo.begin(); i != todo.end(); i++) (*i)(cfg,sav,val,config[cfg]);
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
        std::cout << "CallState" << std::endl;
    }
    ~CallState() {std::cout << "~CallState" << std::endl;
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
    int bas, siz;
    std::vector<Type> vec;
    std::array<Type,Size> ary;
    HeapState() : bas(0), siz(0) {}
    int chk() {
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
        bas = 0; siz = 0;
    }
    void clear(int i) {
        if (siz < 0 || siz > chk() || bas < 0 || (chk() > 0 && bas >= chk()))
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        if (siz < i) {std::cerr << "invalid clear siz!" << std::endl; *(int*)0=0; exit(-1);}
        bas = (bas+i)%chk();
        siz -= i;
    }
    void fill(const Type val, int num) {
        for (int i = 0; i < num; i++) *this << val;
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
    Type &operator[](int i) {
        if (siz < 0 || siz > chk() || bas < 0 || bas >= chk())
        {std::cerr << "invalid bas size!" << std::endl; *(int*)0=0; exit(-1);}
        if (i < 0) {std::cerr << "invalid vec index!" << std::endl; exit(-1);}
        if (Size) return ary[(i+bas)%Size];
        return vec[(i+bas)%vec.size()];
    }
    Type *data() {
        if (Size) return ary.data();
        return vec.data();
    }
};

template <int Size, int Dim, int Min> struct SimpleState {
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
    typedef int Seqn; // sequence number
    static const Seqn wrap = 10000;
    struct SeqnLess {
        bool operator()(const Seqn &lhs, const Seqn &rhs) const {
            if (lhs < rhs && rhs-lhs < wrap) return true;
            if (lhs > rhs && lhs-rhs >= wrap) return true;
            return false;
        }
    };
    typedef std::array<int,Dim> Only; // key value only
    struct OnlyLess {
        bool operator()(const Only &lhs, const Only &rhs) const {
            for (int i = 0; i < Dim; i++) if (lhs[i] < rhs[i]) return true;
            return false;
        }
    };
    typedef std::array<int,Dim+1> Wseq; // with sequence number
    struct WseqLess {
        bool operator()(const Wseq &lhs, const Wseq &rhs) const {
            for (int i = 0; i < Dim; i++) if (lhs[i] < rhs[i]) return true;
            for (int i = 0; i < Dim; i++) if (lhs[i] > rhs[i]) return false;
            return SeqnLess()(lhs[Dim],rhs[Dim]);
        }
    };
    typedef std::array<int,Dim+1> Wsiz; // with list size
    struct WsizLess {
        bool operator()(const Wsiz &lhs, const Wsiz &rhs) const {
            if (lhs[Dim] < rhs[Dim]) return true;
            if (lhs[Dim] > rhs[Dim]) return false;
            return (lhs < rhs);
        }
    };
    std::map<Only,Indx,OnlyLess> oldest, newest; // first and last in list
    std::map<Wseq,Indx,WseqLess> ording; // to find next in sparse ording
    std::map<Indx,Only,IndxLess> keyval; // which list index is in
    std::map<Indx,Seqn,IndxLess> seqnum; // sparse ordering in list
    std::map<Only,int,OnlyLess> keysiz; // number in list
    std::set<Wsiz,WsizLess> keyord; // ordered by number
    std::deque<Indx> pool; // push_front, so insert after remove uses removed idx
    Seqn seqn;
    std::map<int,Only> idxkey; // handle for qualifier
    std::map<int,int> orders; // multiple insance helper
    int ordkey; // unique per thread
    SimpleState() : seqn(0), ordkey(0) {
        for (int i = 0; i < Size; i++) pool.push_back(i);
    }
    Only get(int idx, int tag, int val) {
        if (idxkey.find(idx) == idxkey.end()) {Only tmp = {0}; idxkey[idx] = tmp;}
        Only tmp = idxkey[idx]; if (tag >= 0 && tag < Dim) tmp[tag] = val; return tmp;
    }
    Wseq get(const Only &key, int num) {
        Wseq tmp; for (int i = 0; i < Dim; i++) tmp[i] = key[i]; tmp[Dim] = num; return tmp;
    }
    Only get(const Wsiz &ord) {
        Only tmp; for (int i = 0; i < Dim; i++) tmp[i] = ord[i]; return tmp;
    }
    void set(int siz) {
        // remove any Indx not less than siz
        int size = pool.size() + keyval.size();
        for (int i = size-1; i >= siz; i--) remove(i);
        std::deque<Indx> temp;
        for (auto i = pool.begin(); i != pool.end(); i++)
        if (*i < siz) temp.push_back(*i);
        pool = temp;
        // add to pool to increase to siz
        Indx max = 0;
        for (auto i = pool.begin(); i != pool.end(); i++)
        if (*i >= max) max = *i+1;
        for (auto i = keyval.begin(); i != keyval.end(); i++)
        if ((*i).first >= max) max = (*i).first+1;
        for (int i = max; i < siz; i++) pool.push_back(i);
    }
    void remove(int idx) {
        auto itr = keyval.find(idx);
        if (itr == keyval.end()) return; // TODO move idx to front of pool
        Only tmp = (*itr).second;
        Seqn num = seqnum[idx];
        Wseq seq = get(tmp,num);
        if (oldest[tmp] == idx && newest[tmp] == idx) {
        oldest.erase(tmp);
        newest.erase(tmp);}
        else if (oldest[tmp] == idx) {
        Wseq seq = get(tmp,(num+1)%wrap);
        oldest[tmp] = (*ording.lower_bound(seq)).second;}
        else if (newest[tmp] == idx) {
        Wseq seq = get(tmp,(num+wrap-1)%wrap);
        newest[tmp] = (*ording.upper_bound(seq)).second;}
        ording.erase(seq);
        keyval.erase(idx);
        seqnum.erase(idx);
        if (keysiz[tmp] == 1) {keyord.erase(get(tmp,1)); keysiz.erase(tmp);}
        else {keyord.erase(get(tmp,keysiz[tmp])); keysiz[tmp] -= 1; keyord.insert(get(tmp,keysiz[tmp]));}
        pool.push_front(idx);
    }
    void insert(Only &tmp, int idx) {
        if (oldest.find(tmp) == oldest.end()) oldest[tmp] = idx;
        newest[tmp] = idx;
        ording[get(tmp,seqn)] = idx;
        keyval[idx] = tmp;
        seqnum[idx] = seqn;
        if (keysiz.find(tmp) == keysiz.end()) {keysiz[tmp] = 1; keyord.insert(get(tmp,1));}
        else {keyord.erase(get(tmp,keysiz[tmp])); keysiz[tmp] += 1; keyord.insert(get(tmp,keysiz[tmp]));}
        seqn = (seqn+1)%wrap;
    }
    int insert(Only &tmp) {
        if (pool.empty()) {
        auto itr = keyord.rbegin();
        if (itr != keyord.rend() && (*itr)[Dim] > keysiz[tmp]+1)
        remove(oldest[get(*itr)]); else remove(oldest[tmp]);}
        Indx idx = pool.front(); pool.pop_front();
        insert(tmp,idx);
        return idx;
    }
    int oldbuf(Only &tmp) { // used for insert in advance, so no preemptive insert
        if (oldest.find(tmp) == oldest.end()) insert(tmp);
        return oldest[tmp];
    }
    int getbuf(Only &tmp) { // used for reserve, so returns new oldest if possible
        if (keysiz.find(tmp) == keysiz.end() || keysiz[tmp] < Min || !pool.empty()) {
        int idx = insert(tmp);
        while (oldest[tmp] != idx) {
        remove(oldest[tmp]); insert(tmp);}}
        return oldest[tmp];
    }
    int newbuf(Only &tmp) { // used for depend, so returns result of last advance
        if (newest.find(tmp) == newest.end()) insert(tmp);
        return newest[tmp];
    }
    void qualify(int idx, int tag, int val) {
        if (idxkey.find(idx) == idxkey.end()) {Only tmp = {0}; idxkey[idx] = tmp;}
        if (tag >= 0 && tag < Dim) idxkey[idx][tag] = val;
    }
    int insert(int idx, int tag, int val) {
        Only tmp = get(idx,tag,val);
        return insert(tmp);
    }
    int oldbuf(int idx, int tag, int val) {
        Only tmp = get(idx,tag,val);
        return oldbuf(tmp);
    }
    int getbuf(int idx, int tag, int val) {
        Only tmp = get(idx,tag,val);
        return getbuf(tmp);
    }
    int newbuf(int idx, int tag, int val) {
        Only tmp = get(idx,tag,val);
        return newbuf(tmp);
    }
    int setord() {
        int tmp = ordkey; ordkey += 1; orders[tmp] = 0;
        return tmp;
    }
    int getord(int idx) {
        if (orders.find(idx) == orders.end()) {*(int*)0=0;exit(-1);}
        int tmp = orders[idx]; orders[idx] += 1;
        return tmp;
    }
    int limord(int idx) {
        if (orders.find(idx) == orders.end()) {*(int*)0=0;exit(-1);}
        return 0; // TODO maintain current and limit
    }
    void setord(int idx) {
        if (orders.find(idx) == orders.end()) {*(int*)0=0;exit(-1);}
        orders[idx] = 0;
    }
    void clrord(int idx) {
        if (orders.find(idx) == orders.end()) {*(int*)0=0;exit(-1);}
        orders.erase(idx);
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

void *allocSafe(int val);
int waitSafe(void *ptr);
int postSafe(void *ptr);
void hackSafe(void *ptr);
void freeSafe(void *ptr);

float processTime();

#ifdef __cplusplus
};
#endif
