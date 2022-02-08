union UtilUnion {
	int i;
	long long l;
	float f;
	double d;
	const char *s;
};
struct UtilStruct {
	int i;
	union UtilUnion u;
};
typedef struct UtilStruct (*UtilFunc)(int lst, int arg);
typedef int (*UtilComp)(int left, int right);
void utilAlloc(int lstc, int argc, int glbc);
void utilArgc(int arg, int siz);
void utilArgv(int arg, int idx, union UtilUnion val);
void utilLstc(int lst, int siz);
void utilLstv(int lst, int idx, union UtilUnion val);
void utilOptc(int lst, int siz);
void utilFunc(int lst, UtilFunc fnc);
void utilGlbv(int glb, union UtilUnion val);
void utilMerge(int size, int *index, UtilComp func);
int utilListMin(int lst); // -> arg
int utilListMax(int lst); // -> arg
int utilLast(int lst, int arg); // -> arg
int utilNext(int lst, int arg); // -> arg
int utilLastMin(int lst, int arg); // -> arg
int utilNextMax(int lst, int arg); // -> arg
int utilLastMax(int lst, int arg); // -> arg
int utilNextMin(int lst, int arg); // -> arg
int utilMinPart(int lst); // -> opt
int utilMaxPart(int lst); // -> opt
int utilPartMin(int lst, int opt); // -> arg
int utilPartMax(int lst, int opt); // -> arg
int utilIsList(int arg); // -> bool
int utilIsPart(int lst, int opt); // -> bool
int utilEquiv(int lst, int one, int oth); // -> bool
int utilPart(int lst, int arg); // -> opt
union UtilUnion utilHash(int lst, int arg);
union UtilUnion utilUnionI(int i);
union UtilUnion utilUnionL(long long l);
union UtilUnion utilUnionS(const char *s);
struct UtilStruct utilStructI(int i, int u);
struct UtilStruct utilStructL(int i, long long u);
struct UtilStruct utilStructS(int i, const char *u);
void utilFlag(int lst, const char *str);
void utilEnv(int lst, const char *str);
void utilPipe(int lst, const char *str);
