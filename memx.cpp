extern "C" {
#include "memx.h"
#include "luax.h"
#include "face.h"
}
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <vector>
#include <string>
#include <map>

class Memx;
int memx = 0;
std::map<int,Memx*> memy;
std::map<Memx*,int> memz;
std::map<int,Memx*> memt;

int numstr(const char *str, int *val) {
	char *ptr = const_cast<char*>(str);
	int tmp = (errno = 0, strtoimax(str,&ptr,0));
	if (errno != 0 || ptr == str) return 0; *val = tmp;
	return ptr-str;}
int dgtstr(const char *str, int *val) {
	char ptr[3] = {0}; ptr[0] = str[0]; ptr[1] = str[1]; ptr[2] = 0;
	return numstr(ptr,val);}
int rawstr(const char *str, std::vector<char> *raw) {
	char *ptr = const_cast<char*>(str);
	char *tmp = 0; int val = 0; int odd = 0; int len = 0;
	if (ptr[0] != '0' || (ptr[1] != 'x' && ptr[1] == 'X')) return 0;
	for (len = 0; (odd = dgtstr(ptr+len+2,&val)); len += odd);
	if (len & 1) asprintf(&tmp,"0%s",ptr+2);
	else asprintf(&tmp,"%s",ptr+2);
	for (len = 0; dgtstr(tmp+len,&val) == 2; len += 2) raw->push_back(val);
	free(tmp);
	return len+2;}
char *repstr(char *str, int sub, int len) {
	char *tmp = strndup(str+sub,len); free(str); return tmp;}
void repstr(char *&d, int &l) {
	while (l > 0 && isspace(*d)) d = repstr(d,1,--l);
	while (l > 0 && isspace(d[l-1])) d = repstr(d,0,--l);}
char *repstr(char *one, char *oth) {
	int len = strlen(one)+strlen(oth)+1;
	char *ret = (char *)malloc(len);
	strlcpy(ret,one,len); free(one);
	strlcat(ret,oth,len); free(oth);
	return ret;}
char *repstr(char *exp) {
	char *str = (luaxCall(exp,protoCloseBg()),strdup(protoResultBg()));
	int len = strlen(str);
	char *tmp = strdup(str);
	int val = 0;
	std::vector<char> vec;
	repstr(tmp,len);
	if (numstr(tmp,&val) != len && rawstr(tmp,&vec) != len) {
		char *pre = strdup("\"");
		char *pst = strdup("\"");
		str = repstr(pre,repstr(str,pst));}
	free(tmp); free(exp); return str;}
int nest(char chr, char *last, int *level) {
	if (chr == '"' && *last == 0) {*last = chr; return 1;}
	if (chr == '"' && *last == chr) {*last = 0; return 1;}
	if (chr == '(' && *last == 0) {*level += 1; return 1;}
	if (chr == ')' && *last == 0) {*level -= 1; return 1;}
	if (chr == '`' && *last == 0) {*level += 1; return 1;}
	if (chr == '\'' && *last == 0) {*level -= 1; return 1;}
	if (chr == '\\' && *last == '"') {*last = '\\'; return 0;}
	if (*last == '\\') *last = '"';
	return 0;}
int open(const char *str) {
	if (str) {char last = 0; int level = 0; for (int i = 0; str[i]; i++) if (nest(str[i],&last,&level) && str[i] == '`' && last == 0 && level == 1) return i;} return -1;}
int close(const char *str) {
	if (str) {char last = 0; int level = 0; for (int i = 0; str[i]; i++) if (nest(str[i],&last,&level) && str[i] == '\'' && last == 0 && level == 0) return i;} return -1;}
int quote(const char *str) {
	if (str) {char last = 0; int level = 0; for (int i = 0; str[i]; i++) if (nest(str[i],&last,&level) && str[i] == '"' && last == 0) return i;} return -1;}
int para(const char *str) {
	if (str) {char last = 0; int level = 0; for (int i = 0; str[i]; i++) if (nest(str[i],&last,&level) && str[i] == ')' && last == 0 && level == 0) return i;} return -1;}
struct Memx {
	enum {
		MemxNul,
		MemxInt,
		MemxStr,
		MemxLst,
		MemxLua,
		MemxRaw,
	} tag;
	int val;
	std::string str;
	std::vector<Memx*> lst;
	std::string lua;
	std::vector<char> raw;
	struct Function fnc; void **fem;
	struct Function gnc; void **gem;
	int idx;
	std::vector<Memx*> mak;
	Memx(const char *s): tag(MemxNul), idx(0) {
		fnc.vp = 0; gnc.vp = 0;
		memy[memx] = this; memz[this] = memx; memx++;
		if (s == 0) {
			tag = MemxNul; return;}
		char *d = strdup(s);
		int l = strlen(d);
		int o = open(d);
		int c = close(d);
		while (l > 0 && o >= 0 && c >= 0) {
			char *e = repstr(strdup(d),o+1,c-o-1);
			char *f = repstr(strdup(d),c+1,l-c-1);
			char *g = repstr(d,o,l-o);
			char *v = repstr(e);
			d = repstr(repstr(g,v),f);
			l = strlen(d); o = open(d); c = close(d);}
		repstr(d,l);
		if (l == 0) {
			tag = MemxLst; free(d); return;}
		if (numstr(d,&val) == l) {
			tag = MemxInt; free(d); return;}
		if (rawstr(d,&raw) == l) {
			tag = MemxRaw; free(d); return;}
		if (d[0] == '"' && quote(d) == l-1) {
			tag = MemxStr; d = repstr(d,1,l-2); str = d; free(d); return;}
		if (d[0] == '(') {
			while (d[0] == '(') {
				int c = para(d);
				lst.push_back(new Memx(repstr(strdup(d),1,c-2)));
				d = repstr(d,c+1,l-=c+1); repstr(d,l);}
			tag = MemxLst; free(d); return;}
		tag = MemxLua; lua = d; free(d);}
	~Memx() {
		std::vector<Memx*>::iterator i;
		if (memz.find(this) == memz.end()) ERROR(exitErr,0);
		memy.erase(memz[this]); memz.erase(this);
		for (i = mak.begin(); i != mak.end(); i++) delete *i;}
	void done() {
		switch (tag) {
		case (MemxNul): break;
		case (MemxInt): val = 0; break;
		case (MemxStr): str.clear(); break;
		case (MemxLst): lst.clear(); break;
		case (MemxLua): lua.clear(); break;
		case (MemxRaw): raw.clear(); break;
		default: break;}
		tag = MemxNul;}
	void init(Memx *giv) {
		done();
		switch (giv->tag) {
		case (MemxNul): break;
		case (MemxInt): val = giv->val; break;
		case (MemxStr): str = giv->str; break;
		case (MemxLst): lst = giv->lst; break;
		case (MemxLua): lua = giv->lua; break;
		case (MemxRaw): raw = giv->raw; break;
		default: break;}
		tag = giv->tag;}
	void list() {
		if (tag != MemxLst) {lst.push_back(this); tag = MemxLst;}}
	void list(Memx *giv) {
		list(); lst.push_back(giv);}
	void sort(Memx *giv) {
		Memx *cpy = new Memx("");
		Memx *one = new Memx("");
		Memx *oth = new Memx("");
		Memx *lft = new Memx("");
		Memx *rgt = new Memx("");
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
		Memx *giv = new Memx("");
		sort(giv);
		delete giv;}
	void uniq(Memx *giv) {
		std::vector<Memx*>::iterator i,j;
		sort(giv);
		for (i = lst.begin(); i != lst.end(); i++)
		if ((j = i, j++) != lst.end() && *i == *j) i = lst.erase(i);}
	void uniq() {
		Memx *giv = new Memx("");
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
Memx *cast(void *ptr) {
	return static_cast<Memx*>(ptr);}
Memx *cast(void **mem) {
	if (!*mem) *mem = new Memx(0);
	return cast(*mem);}
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
	if (ptr->tag == Memx::MemxLst) return ptr->lst.size();
	if (ptr->tag == Memx::MemxRaw) return ptr->raw.size();
	return 0;}
extern "C" int memxInt(void *mem) { // get int
	Memx *ptr = cast(mem);
	if (ptr->tag == Memx::MemxInt) return ptr->val;
	if (ptr->tag == Memx::MemxLua) return (luaxCall(ptr->lua.c_str(),protoCloseRg()),protoResultRg());
	return 0;}
extern "C" int memxMask(void *mem) { // mask from collection
	int val = 0; int len = memxSize(mem);
	for (int i = 0; i < len; i++) val |= (1 << memxInt(memxSkip(mem,i)));
	return val;}
extern "C" const char *memxStr(void *mem) { // get string
	Memx *ptr = cast(mem);
	if (ptr->tag == Memx::MemxStr) return ptr->str.c_str();
	if (ptr->tag == Memx::MemxLua) return (luaxCall(ptr->lua.c_str(),protoCloseBg()),protoResultBg());
	return 0;}
extern "C" const char *memxRaw(void *mem, int *len) { // get bytes
	Memx *ptr = cast(mem);
	if (ptr->tag == Memx::MemxRaw) {*len = ptr->raw.size(); return ptr->raw.data();}
	if (ptr->tag == Memx::MemxInt) {*len = sizeof(ptr->val); return (char*)&ptr->val;}
	if (ptr->tag == Memx::MemxLua) return (luaxCall(ptr->lua.c_str(),protoCloseBh()),protoResultBh(len));
	return 0;}
extern "C" void memxInit(void **mem, const char *str) { // interpret string
	Memx *tmp = new Memx(str);
	if (*mem) {cast(*mem)->init(tmp); delete tmp;}
	else *mem = tmp;}
extern "C" void memxForm(void **mem, const char *fmt, ...) { // use vasprintf
	va_list args; char *str;
	va_start(args,fmt); vasprintf(&str,fmt,args); va_end(args);
	memxInit(mem,str); free(str);}
extern "C" void *memxTemp(int idx) { // realloc indexed memory
	if (memt.find(idx) != memt.end()) delete memt[idx];
	memt[idx] = new Memx(0); return memt[idx];}
extern "C" int memxOpen(void **mem) { // get pipe punted to given
	Memx *ptr = cast(mem);
	if (!ptr->idx) ptr->idx = puntInit(memz[ptr],memz[ptr],memxRd,memxWr);
	return ptr->idx;}
extern "C" void memxCopy(void **mem, void *giv) { // replaces target with given
	if (*mem) {if (*mem == giv) cast(*mem)->dflt();
	else {cast(*mem)->init(cast(giv)); cast(*mem)->back();}}
	else {*mem = new Memx(0); cast(*mem)->init(cast(giv));}}
extern "C" void memxList(void **mem, void *giv) { // adds given to target in order
	if (*mem) {if (*mem == giv) cast(*mem)->dflt();
	else {cast(*mem)->list(cast(giv)); cast(*mem)->back();}}
	else {*mem = new Memx(0); cast(*mem)->list(cast(giv));}}
extern "C" void memxKeep(void **mem, void *giv) { // sorts given into target
	if (*mem) {if (*mem == giv) cast(*mem)->dflt();
	else {cast(*mem)->uniq(cast(giv)); cast(*mem)->back();}}
	else {*mem = new Memx(0); cast(*mem)->uniq(cast(giv));}}
extern "C" void memxMake(void **mem, void *giv) { // delete of mem also deletes giv
	Memx *ptr = cast(mem);
	ptr->mak.push_back(cast(giv));}
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
	default: break;}}
extern "C" void memxBack(void **mem, void **giv, struct Function fnc) { // causes given call on given with target as given
	if (!*mem) *mem = new Memx(0); cast(*mem)->fnc = fnc; cast(*mem)->fem = giv;}
extern "C" void memxDflt(void **mem, void **giv, struct Function fnc) { // trivial change causes call on mem with giv
	if (!*mem) *mem = new Memx(0); cast(*mem)->gnc = fnc; cast(*mem)->gem = giv;}
extern "C" void *memxSkip(void *mem, int key) { // skip to given iterator
	return cast(mem)->skip(key);}
extern "C" void memxAdd(void **mem, void *giv, int key) { // insert at given location
	cast(mem)->add(cast(giv),key);}
extern "C" void memxDel(void **mem, int key) { // delete at given location
	cast(mem)->del(key);}
