union UtilHash {
	int c;
	int i;
	int s;
	long long l;
	float f;
	double d;
};
typedef int (*UtilCompAS)(int opt, const char *one, const char *oth);
typedef int (*UtilCompBS)(const char *one, const char *oth);
typedef int (*UtilCompAT)(int opt, int one, int oth);
typedef int (*UtilCompBT)(int one, int oth);
typedef int (*UtilIdentAS)(int opt, const char *arg);
typedef int (*UtilIdentBS)(const char *arg);
typedef int (*UtilIdentAT)(int opt, int arg);
typedef int (*UtilIdentBT)(int arg);
typedef int (*UtilTestAS)(int opt, const char *arg);
typedef int (*UtilTestBS)(const char *arg);
typedef int (*UtilTestAT)(int opt, int arg);
typedef int (*UtilTestBT)(int arg);
typedef union UtilHash (*UtilHashAS)(int opt, const char *arg);
typedef union UtilHash (*UtilHashBS)(const char *arg);
typedef union UtilHash (*UtilHashAT)(int opt, int arg);
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
void utilAlloc(int argc, int prtc, int optc);
void utilOpt(const char *str, int opt);
void utilArg(const char *str, int arg);
void utilMerge(UtilCompBT func, int size, int *index);
void utilLink(struct UtilFunc func, int opt);
struct UtilFunc utilFlagFact();
int utilListMin(int opt); // -> arg
int utilListMax(int opt); // -> arg
int utilLast(int opt, int arg); // -> arg
int utilNext(int opt, int arg); // -> arg
int utilLastMin(int opt, int arg); // -> arg
int utilNextMax(int opt, int arg); // -> arg
int utilLastMax(int opt, int arg); // -> arg
int utilNextMin(int opt, int arg); // -> arg
int utilPart(int opt, int arg); // -> typ
int utilMinPart(int opt); // -> typ
int utilMaxPart(int opt); // -> typ
int utilPartMin(int opt, int typ); // -> arg
int utilPartMax(int opt, int typ); // -> arg
int utilIsList(int arg); // -> bool
int utilIsPart(int opt, int typ); // -> bool
int utilEquiv(int opt, int one, int oth); // -> bool
union UtilHash utilHash(int opt, int typ);
