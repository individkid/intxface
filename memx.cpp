extern "C" {
#include "luax.h"
#include "face.h"
}
#include "memx.h"
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <vector>
#include <string>
#include <map>
#include <sstream>
#include <ctype.h>

struct Memx;
int memx = 0;
std::map<int,Memx*> memy;
std::map<Memx*,int> memz;
std::map<int,Memx*> memt;

struct Memx;
int scan(const char *giv, int &val);
int scan(const char *giv, std::vector<Memx*> &lst);
int scan(const char *giv, std::string &exp);
int scan(const char *giv, std::vector<char> &raw);

struct Memx {
	int val;
	std::string str;
	std::vector<Memx*> lst;
	std::string exp; int lua;
	std::vector<char> raw;
	MemxTag tag;
	struct Function fnc; void **fem;
	struct Function gnc; void **gem;
	std::vector<Memx*> mak;
	int idx;
	Memx() {init();}
	Memx(const char *giv) {init();
		if (scan(giv,exp)) {tag = MemxLua; return;}
		if (scan(giv,lst)) {tag = MemxLst; return;}
		if (scan(giv,val)) {tag = MemxInt; return;}
		tag = MemxStr; str = giv;}
	Memx(enum MemxTag t, const char *giv) {init(); tag = t;
		switch (tag) {
		case (MemxNul): break;
		case (MemxInt): scan(giv,val); break;
		case (MemxStr): str = giv; break;
		case (MemxLst): scan(giv,lst); break;
		case (MemxLua): scan(giv,exp); break;
		case (MemxRaw): scan(giv,raw); break;
		default: ERROR();}}
	Memx(const Memx *giv) {init();
		init(giv);}
	~Memx() {
		std::vector<Memx*>::iterator i;
		done();
		if (idx) closeIdent(idx);
		if (memz.find(this) == memz.end()) ERROR();
		memy.erase(memz[this]); memz.erase(this);
		for (i = mak.begin(); i != mak.end(); i++) delete *i;
		mak.clear();}
	void done() {
		switch (tag) {
		case (MemxNul): break;
		case (MemxInt): val = 0; break;
		case (MemxStr): str.clear(); break;
		case (MemxLst): lst.clear(); break;
		case (MemxLua): exp.clear(); lua = 0; break;
		case (MemxRaw): raw.clear(); break;
		default: ERROR();}
		tag = MemxNul;}
	void init(const Memx *giv) {
		done();
		switch (giv->tag) {
		case (MemxNul): break;
		case (MemxInt): val = giv->val; break;
		case (MemxStr): str = giv->str; break;
		case (MemxLst): lst = giv->lst; break;
		case (MemxLua): exp = giv->exp; lua = giv->lua; break;
		case (MemxRaw): raw = giv->raw; break;
		default: ERROR();}
		tag = giv->tag;}
	void init() {
		tag = MemxNul; val = 0; idx = 0;
		fnc.vp = 0; fem = 0; gnc.vp = 0; gem = 0;
		memy[memx] = this; memz[this] = memx; memx++;}
	void list() {
		if (tag != MemxLst) {
		if (tag != MemxNul) lst.push_back(new Memx(this));
		done(); tag = MemxLst;}}
	void list(Memx *giv) {
		list(); lst.push_back(giv);}
	void sort(Memx *giv) {
		Memx *cpy = new Memx(MemxLst,0);
		Memx *one = new Memx(MemxLst,0);
		Memx *oth = new Memx(MemxLst,0);
		Memx *lft = new Memx(MemxLst,0);
		Memx *rgt = new Memx(MemxLst,0);
		std::vector<Memx*>::iterator i, j;
		bool odd = false;
		if (giv->tag == MemxLst && giv->lst.size() == 0) return;
		cpy->init(giv);
		if (tag != MemxLst) list();
		if (cpy->tag != MemxLst) cpy->list();
		for (i = lst.begin(), odd = false; i != lst.end(); i++, odd = !odd)
		if (odd) one->lst.push_back(*i); else lft->lst.push_back(*i);
		for (i = cpy->lst.begin(), odd = false; i != cpy->lst.end(); i++, odd = !odd)
		if (odd) oth->lst.push_back(*i); else rgt->lst.push_back(*i);
		lft->sort(one); rgt->sort(oth);
		lst.clear(); i = lft->lst.begin(); j = rgt->lst.begin();
		while (i != lft->lst.end() || j != rgt->lst.end()) {
		if (i == lft->lst.end() || *j < *i) {lst.push_back(*j); j++;}
		else {lst.push_back(*i); i++;}}
		delete cpy; delete one; delete oth; delete lft; delete rgt;}
	void sort() {
		Memx *giv = new Memx(MemxLst,0);
		sort(giv);
		delete giv;}
	void uniq(Memx *giv) {
		std::vector<Memx*>::iterator i,j;
		sort(giv);
		for (i = lst.begin(); i != lst.end(); i++)
		if ((j = i, j++) != lst.end() && *i == *j) i = lst.erase(i);}
	void uniq() {
		Memx *giv = new Memx(MemxLst,0);
		uniq(giv);
		delete giv;}
	Memx *skip(int key) {
		if (tag == MemxLst && key < lst.size()) return lst[key];
		return 0;}
	void add(Memx *giv, int key) {
		if (tag == MemxLst) {
		while (lst.size() < key-1) lst.push_back(0);
		if (key == lst.size()) lst.push_back(giv);
		else lst.insert(lst.begin()+key,giv);}}
	void del(int key) {
		if (tag == MemxLst) lst.erase(lst.begin()+key);}
	int mcpy(const char *buf, int len) {
		done();
		tag = MemxRaw;
		for (int i = 0; i < len; i++) raw.push_back(buf[i]);
		return len;}
	void back() {
		if (fnc.vp) memxCall(fem,this,fnc);}
	void dflt() {
		if (gnc.vp) {void *tmp = this; memxCall(&tmp,*gem,gnc);}}
};
int scan(const char *giv) {
	for (int i = 0; giv[i]; i++) if (!isspace(giv[i])) return 0;
	return 1;}
int scan(const char *giv, int &val) {
	std::stringstream sstr(giv); std::string pst;
	if (!(sstr >> val)) return 0; sstr >> pst;
	return scan(pst.c_str());}
int scan(const char *giv, std::string &pre, std::string &mid, std::string &pst) {
	int lvl = 0; int cmt = 0; int esc = 0;
	for (int i = 0; giv[i]; i++) {
	if (!esc && !cmt && giv[i] == '(') {lvl++;
	if (lvl == 1) pre = std::string(giv,0,i);}
	if (!esc && !cmt && giv[i] == ')') {lvl--;
	if (lvl == 0) {pst = std::string(giv,i+1,strlen(giv)); mid = std::string(giv,pre.size()+1,i); return 1;}}
	if (!esc && giv[i] == '"') cmt = !cmt;
	esc = (!esc && giv[i] == '\\');}
	return 0;}
int scan(const char *giv, std::vector<Memx*> &lst) {
	std::string pre,mid,pst;
	for (std::string tmp = giv;
	scan(tmp.c_str(),pre,mid,pst) && scan(pre.c_str());
	tmp = pst) lst.push_back(new Memx(mid.c_str()));
	if (!scan(pst.c_str())) {
	for (std::vector<Memx*>::iterator i = lst.begin();
	i != lst.end(); i++) delete(*i);
	lst.clear();}
	return (lst.size() > 0);}
int scan(const char *giv, std::string &exp) {
	std::string pre,mid,pst;
	if (!scan(giv,pre,mid,pst)) return 0;
	if (pre.size() > 0 && pre[pre.size()-1] == '%') {
	exp = giv; return 1;}
	if (scan(mid.c_str(),exp)) return 1;
	if (scan(pst.c_str(),exp)) return 1;
	return 0;}
int scan(const char *giv, std::vector<char> &raw) {
        // TODO convert from hex digits
        return 0;}
Memx *cast(void *ptr) {
	return static_cast<Memx*>(ptr);}
Memx *cast(void **mem) {
	if (!*mem) *mem = new Memx();
	return cast(*mem);}
extern "C" void memxScan() {
	int siz = 0;
	for (std::map<Memx*,int>::iterator i = memz.begin(); i != memz.end(); i++)
	if ((*i).first->tag == MemxLua) (*i).first->lua = siz++;
	nestInit(siz);
	for (std::map<Memx*,int>::iterator i = memz.begin(); i != memz.end(); i++)
	if ((*i).first->tag == MemxLua) nestElem((*i).first->lua,(*i).first->exp.c_str());
	nestScan();}
extern "C" int memxRd(int fildes, void *buf, int nbyte) {
	Memx *ptr = memy[fildes]; const char *tmp = 0; int len = 0;
	tmp = memxRaw(ptr,&len);
	if (len > nbyte) len = nbyte;
	memcpy(buf,tmp,len);
	return len;}
extern "C" int memxWr(int fildes, const void *buf, int nbyte) {
	Memx *ptr = memy[fildes];
	return ptr->mcpy(static_cast<const char *>(buf),nbyte);}
extern "C" void memxLuax()
{
	luaxAdd("memxSize",protoTypeOf(memxSize));
	luaxAdd("memxInt",protoTypeOf(memxInt));
	luaxAdd("memxMask",protoTypeOf(memxMask));
	luaxAdd("memxStr",protoTypeAf(memxStr));
	luaxAdd("memxInit",protoTypeNf(memxInit));
	luaxAdd("memxTemp",protoTypeTf(memxTemp));
	luaxAdd("memxOpen",protoTypeLf(memxOpen));
	luaxAdd("memxCopy",protoTypeMf(memxCopy));
	luaxAdd("memxList",protoTypeMf(memxList));
	luaxAdd("memxKeep",protoTypeMf(memxKeep));
	luaxAdd("memxMake",protoTypeMf(memxMake));
	luaxAdd("memxDone",protoTypeDf(memxDone));
	luaxAdd("memxSkip",protoTypeKf(memxSkip));
	luaxAdd("memxAdd",protoTypeIg(memxAdd));
	luaxAdd("memxDel",protoTypeIf(memxDel));
}
extern "C" int memxSize(void *mem) { // get size
	Memx *ptr = cast(mem);
	if (ptr->tag == MemxLst) return ptr->lst.size();
	if (ptr->tag == MemxRaw) return ptr->raw.size();
	return 0;}
extern "C" int memxInt(void *mem) { // get int
	Memx *ptr = cast(mem); int val = 0;
	if (ptr->tag == MemxInt) val = ptr->val;
	if (ptr->tag == MemxStr) scan(ptr->str.c_str(),val);
	if (ptr->tag == MemxLua) scan(nestRepl(ptr->lua),val);
	return 0;}
extern "C" int memxMask(void *mem) { // mask from collection
	int val = 0; int len = memxSize(mem);
	for (int i = 0; i < len; i++) val |= (1 << memxInt(memxSkip(mem,i)));
	return val;}
extern "C" const char *memxStr(void *mem) { // get string
	Memx *ptr = cast(mem);
	if (ptr->tag == MemxInt) {std::stringstream sstr; sstr << ptr->val; ptr->str = sstr.str(); return ptr->str.c_str();}
	if (ptr->tag == MemxStr) return ptr->str.c_str();
	if (ptr->tag == MemxLua) return nestRepl(ptr->lua);
	return 0;}
extern "C" const char *memxRaw(void *mem, int *len) { // get bytes
	Memx *ptr = cast(mem);
	if (ptr->tag == MemxRaw) {*len = ptr->raw.size(); return ptr->raw.data();}
	return 0;}
extern "C" const void *memxDat(void *mem) { // get dat
	int len = 0;
	const char *dat = memxRaw(mem,&len);
	if (*(int*)dat != len) ERROR();
	return dat;}
extern "C" void memxConst(void **mem, enum MemxTag tag, const char *str) { // init as string
	Memx *tmp = new Memx(tag,str);
	if (*mem) {cast(*mem)->init(tmp); delete tmp;}
	else *mem = tmp;}
extern "C" void memxInit(void **mem, const char *str) { // interpret string
	Memx *tmp = new Memx(str);
	if (*mem) {cast(*mem)->init(tmp); delete tmp;}
	else *mem = tmp;}
extern "C" void memxForm(void **mem, const char *fmt, ...) { // use vasprintf
	va_list args; char *str;
	va_start(args,fmt); vasprintf(&str,fmt,args); va_end(args);
	memxInit(mem,str); free(str);}
extern "C" void memxAloc(void **mem, const void *dat) { // use raw data
	cast(mem)->mcpy((const char *)(((const int*)dat)+1),*(const int*)dat);}
extern "C" void *memxTemp(int idx) { // realloc indexed memory
	if (memt.find(idx) != memt.end()) delete memt[idx];
	memt[idx] = new Memx(); return memt[idx];}
extern "C" int memxOpen(void **mem) { // get pipe punted to given
	Memx *ptr = cast(mem);
	if (!ptr->idx) ptr->idx = puntInit(memz[ptr],memz[ptr],memxRd,memxWr);
	return ptr->idx;}
extern "C" void memxCopy(void **mem, void *giv) { // replaces target with given
	if (*mem == giv) cast(mem)->dflt();
	cast(mem)->init(cast(giv));
	if (*mem != giv) cast(mem)->back();}
extern "C" void memxList(void **mem, void *giv) { // adds given to target in order
	if (*mem == giv) cast(mem)->dflt();
	cast(mem)->list(cast(giv));
	if (*mem != giv) cast(mem)->back();}
extern "C" void memxKeep(void **mem, void *giv) { // sorts given into target
	if (*mem == giv) cast(mem)->dflt();
	cast(mem)->uniq(cast(giv));
	if (*mem != giv) cast(mem)->back();}
extern "C" void memxMake(void **mem, void *giv) { // delete of mem also deletes giv
	cast(mem)->mak.push_back(cast(giv));}
extern "C" void memxDone(void **mem) { // delete now
	delete cast(*mem); *mem = 0;}
extern "C" void memxCall(void **mem, void *giv, struct Function fnc) { // call function on mem with giv
	switch (fnc.ft) {
	case (Function::Fftype): {
		void *tmp = 0;
		memxForm(&tmp,"%d",fnc.ff(memxStr(giv)));
		memxCopy(mem,tmp); memxDone(&tmp); break;}
	case (Function::Gftype): {
		void *tmp = 0;
		memxForm(&tmp,"%d",fnc.gf(memxStr(memxSkip(giv,0)),memxStr(memxSkip(giv,1))));
		memxCopy(mem,tmp); memxDone(&tmp); break;}
	case (Function::Oftype): {
		void *tmp = 0;
		memxForm(&tmp,"%d",fnc.of(giv));
		memxCopy(mem,tmp); memxDone(&tmp); break;}
	case (Function::Nftype): fnc.nf(mem,memxStr(giv)); break;
	case (Function::Mftype): fnc.mf(mem,giv); break;
	default: ERROR();}}
extern "C" void memxBack(void **mem, void **giv, struct Function fnc) { // causes given call on given with target as given
	cast(mem)->fnc = fnc; cast(mem)->fem = giv;}
extern "C" void memxDflt(void **mem, void **giv, struct Function fnc) { // trivial change causes call on mem with giv
	cast(mem)->gnc = fnc; cast(mem)->gem = giv;}
extern "C" void *memxSkip(void *mem, int key) { // skip to given iterator
	return cast(mem)->skip(key);}
extern "C" void memxAdd(void **mem, void *giv, int key) { // insert at given location
	cast(mem)->add(cast(giv),key);}
extern "C" void memxDel(void **mem, int key) { // delete at given location
	cast(mem)->del(key);}
