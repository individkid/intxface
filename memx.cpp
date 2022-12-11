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

char *repstr(char *str, int sub, int len) {
	char *tmp = strndup(str+sub,len); free(str); return tmp;}
int numstr(const char *str, int *val) {
	char *ptr = const_cast<char*>(str); *val = strtoimax(str,&ptr,0); return ptr-str;}
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
	repstr(tmp,len);
	if (numstr(tmp,&val) != len) {
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
	} opt;
	int val;
	std::string str;
	std::vector<Memx*> lst;
	std::string lua;
	std::vector<char> raw;
	struct Function fnc; void **fem;
	struct Function gnc; void **gem;
	Memx(const char *s) {
		memy[memx] = this; memz[this] = memx; memx++;
		if (s == 0) {
			opt = MemxNul; return;}
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
			opt = MemxLst; free(d); return;}
		if (numstr(d,&val) == l) {
			opt = MemxInt; free(d); return;}
		if (d[0] == '"' && quote(d) == l-1) {
			opt = MemxStr; d = repstr(d,1,l-2); str = d; free(d); return;}
		if (d[0] == '(') {
			while (d[0] == '(') {
				int c = para(d);
				lst.push_back(new Memx(repstr(strdup(d),1,c-2)));
				d = repstr(d,c+1,l-=c+1); repstr(d,l);}
			opt = MemxLst; free(d); return;}
		opt = MemxLua; lua = d; free(d);}
	~Memx() {
		if (memz.find(this) == memz.end()) ERROR(exitErr,0);
		memy.erase(memz[this]); memz.erase(this);
		for (std::vector<Memx*>::iterator i = lst.begin(); i != lst.end(); i++) delete *i;}
	void init(Memx *giv) {
		switch (giv->opt) {
		case (MemxNul): break;
		case (MemxInt): val = giv->val; break;
		case (MemxStr): str = giv->str; break;
		case (MemxLst): lst = giv->lst; break;
		case (MemxLua): lua = giv->lua; break;
		default: break;}
		opt = giv->opt;}
	void back() {
		if (fnc.vp) memxCall(fem,this,fnc);}
	void dflt() {
		if (gnc.vp) {void *tmp = this; memxCall(&tmp,*gem,gnc);}}
};
Memx *cast(void *ptr) {
		return static_cast<Memx*>(ptr);}
extern "C" int memxRd(int fildes, void *buf, int nbyte) {
	Memx *ptr = memy[fildes];
	return 0;}
extern "C" int memxWr(int fildes, const void *buf, int nbyte) {
	Memx *ptr = memy[fildes];
	return 0;}
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
	luaxAdd("memxDone",protoTypeDf(memxDone));
	luaxAdd("memxFirst",protoTypeMf(memxFirst));
	luaxAdd("memxTest",protoTypeOf(memxTest));
	luaxAdd("memxNext",protoTypeMf(memxNext));
	luaxAdd("memxFind",protoTypeJf(memxFind));
	luaxAdd("memxSkip",protoTypeKf(memxSkip));
	luaxAdd("memxAdd",protoTypeKf(memxAdd));
	luaxAdd("memxDel",protoTypeIf(memxDel));
}
extern "C" int memxSize(void *mem) { // get size
	Memx *ptr = cast (mem);
	if (ptr->opt == Memx::MemxLst) return ptr->lst.size();
	if (ptr->opt == Memx::MemxRaw) return ptr->raw.size();
	return 0;}
extern "C" int memxInt(void *mem) { // get int
	Memx *ptr = cast (mem);
	if (ptr->opt == Memx::MemxInt) return ptr->val;
	return 0;}
extern "C" int memxMask(void *mem) { // mask from collection
	int val = 0; int len = memxSize(mem);
	for (int i = 0; i < len; i++) {void *tmp = 0; memxSkip(&tmp,mem,i); val |= (1 << memxInt(tmp));}
	return val;}
extern "C" const char *memxStr(void *mem) { // get string
	if (cast(mem)->opt == Memx::MemxStr) return cast(mem)->str.c_str(); return 0;}
extern "C" void memxInit(void **mem, const char *str) { // interpret string
	Memx *tmp = new Memx(str); if (*mem) {cast(*mem)->init(tmp); delete tmp;} else *mem = tmp;}
extern "C" void memxForm(void **mem, const char *fmt, ...) { // use vasprintf
	va_list args; char *str; va_start(args,fmt); vasprintf(&str,fmt,args); va_end(args); memxInit(mem,str); free(str);}
extern "C" void *memxTemp(int idx) { // realloc indexed memory
	if (memt.find(idx) != memt.end()) delete memt[idx];
	memt[idx] = new Memx(0); return memt[idx];}
extern "C" int memxOpen(void **mem) { // get pipe punted to given
	Memx *ptr = cast(*mem);
	return puntInit(memz[ptr],memz[ptr],memxRd,memxWr);}
extern "C" void memxCopy(void **mem, void *giv) { // replaces target with given
	if (*mem) {if (*mem == giv) cast(*mem)->dflt(); else {cast(*mem)->init(cast(giv)); cast(*mem)->back();}} else *mem = giv;}
extern "C" void memxList(void **mem, void *giv) {} // adds given to target in order
extern "C" void memxKeep(void **mem, void *giv) {} // adds given to target unordered
extern "C" void memxDone(void **mem) { // deletes target
	delete cast(*mem); *mem = 0;}
extern "C" void memxCall(void **mem, void *giv, struct Function fnc) { // call function on mem with giv
	switch (fnc.ft) {
	case (Function::Fftype): {
		void *tmp = 0;
		memxForm(&tmp,"%d",fnc.ff(memxStr(giv)));
		memxCopy(mem,tmp); memxDone(&tmp); break;}
	case (Function::Gftype): {
		void *tmp = 0; void *tmp0 = 0; void *tmp1 = 0;
		memxSkip(&tmp0,giv,0); memxSkip(&tmp1,giv,1);
		memxForm(&tmp,"%d",fnc.gf(memxStr(tmp0),memxStr(tmp1)));
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
extern "C" void memxFirst(void **mem, void *giv) {} // get iterator from given
extern "C" int memxTest(void *giv) {return 0;} // check iterator of given
extern "C" void memxNext(void **mem, void *giv) {} // get iterator from given iterator
extern "C" void memxFind(void **mem, void *giv, void *key) {} // find iterator with given as first
extern "C" void memxSkip(void **mem, void *giv, int key) {} // skip to given iterator
extern "C" void memxAdd(void **mem, void *giv, int key) {} // insert at given location
extern "C" void memxDel(void **mem, int key) {} // delete at given location
