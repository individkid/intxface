union UtilHash {
	int c;
	int i;
	int s;
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
typedef int (*UtilTestAS)(int lst, const char *arg);
typedef int (*UtilTestBS)(const char *arg);
typedef int (*UtilTestAT)(int lst, int arg);
typedef int (*UtilTestBT)(int arg);
typedef union UtilHash (*UtilHashAS)(int lst, const char *arg);
typedef union UtilHash (*UtilHashBS)(const char *arg);
typedef union UtilHash (*UtilHashAT)(int lst, int arg);
typedef union UtilHash (*UtilHashBT)(int arg);
enum UtilFuncTag {
	UtilCompTag,
	UtilIdentTag,
	UtilStepTag,
	UtilSetupTag,
};
enum UtilOverTag {
	UtilASTag,
	UtilBSTag,
	UtilATTag,
	UtilBTTag,
};
struct UtilFunc {
	enum UtilFuncTag tag;
	enum UtilOverTag ovl;
	int act;
	union {
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
			UtilTestAS as;
			UtilTestBS bs;
			UtilTestAT at;
			UtilTestBT bt;
		} test;
	};
	union {
		UtilHashAS as;
		UtilHashBS bs;
		UtilHashAT at;
		UtilHashBT bt;
	} hash;
};
void utilAlloc(int argc, int prtc, int lstc);
void utilOpt(const char *str, int lst);
void utilArg(const char *str, int arg);
void utilMerge(UtilCompBT func, int size, int *index);
void utilLink(struct UtilFunc func, int lst);
struct UtilFunc utilFlagFact();
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
