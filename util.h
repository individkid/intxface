union UtilHash {
	int i;
	long long l;
	float f;
	double d;
};
typedef int (*UtilCompAS)(int lst, const char *one, const char *oth);
typedef int (*UtilCompBS)(const char *one, const char *oth);
typedef int (*UtilCompAT)(int lst, int one, int oth);
typedef int (*UtilCompBT)(int one, int oth);
typedef int (*UtilIdentAS)(int lst, const char *arg);
typedef int (*UtilIdentBS)(const char *arg);
typedef int (*UtilIdentAT)(int lst, int arg);
typedef int (*UtilIdentBT)(int arg);
typedef union UtilHash (*UtilHashAS)(int lst, const char *arg);
typedef union UtilHash (*UtilHashBS)(const char *arg);
typedef union UtilHash (*UtilHashAT)(int lst, int arg);
typedef union UtilHash (*UtilHashBT)(int arg);
typedef int (*UtilDfltAS)(int lst);
typedef int (*UtilDfltBS)();
typedef int (*UtilDfltAT)(int lst);
typedef int (*UtilDfltBT)();
enum UtilFuncTag {
	UtilCompTag,
	UtilIdentTag,
	UtilBothTag,
};
enum UtilOverTag {
	UtilASTag,
	UtilBSTag,
	UtilATTag,
	UtilBTTag,
};
enum UtilHowTag {
	UtilArgTag,
	UtilPreTag,
};
enum UtilWhenTag {
	UtilNeverTag,
	UtilFirstTag,
	UtilValidTag,
	UtilEveryTag,
};
enum UtilDfltTag {
	UtilCustTag,
	UtilHoleTag,
};
struct UtilFunc {
	enum UtilFuncTag tag;
	enum UtilOverTag ovl;
	enum UtilHowTag arg;
	enum UtilWhenTag act;
	enum UtilDfltTag def;
	union {
		UtilCompAS as;
		UtilCompBS bs;
		UtilCompAT at;
		UtilCompBT bt;
	} comp;
	union {
		UtilIdentAS as;
		UtilIdentBS bs;
		UtilIdentAT at;
		UtilIdentBT bt;
	} ident;
	union {
		UtilHashAS as;
		UtilHashBS bs;
		UtilHashAT at;
		UtilHashBT bt;
	} hash;
	union {
		UtilDfltAS as;
		UtilDfltBS bs;
		UtilDfltAT at;
		UtilDfltBT bt;
	} dflt;
};
void utilAlloc(int argc, int optc, int lstc);
void utilArg(int arg, const char *str);
void utilList(int lst, const char *str);
void utilMerge(int size, int *index, UtilCompBT func);
void utilLink(int lst, struct UtilFunc func);
void utilFlag(int lst, const char *opt);
void utilSetup(int lst, const char *opt, UtilHashAS fnc);
int utilListMin(int lst); // -> arg
int utilListMax(int lst); // -> arg
int utilLast(int lst, int arg); // -> arg
int utilNext(int lst, int arg); // -> arg
int utilLastMin(int lst, int arg); // -> arg
int utilNextMax(int lst, int arg); // -> arg
int utilLastMax(int lst, int arg); // -> arg
int utilNextMin(int lst, int arg); // -> arg
int utilPart(int lst, int arg); // -> opt
int utilMinPart(int lst); // -> opt
int utilMaxPart(int lst); // -> opt
int utilPartMin(int lst, int opt); // -> arg
int utilPartMax(int lst, int opt); // -> arg
int utilIsList(int arg); // -> bool
int utilIsPart(int lst, int opt); // -> bool
int utilEquiv(int lst, int one, int oth); // -> bool
union UtilHash utilHash(int lst, int opt);
