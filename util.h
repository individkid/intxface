#define MIN 0 // arg
#define WLD -1 // opt for any
#define WRD 0 // idx for over
#define PAT 0 // idx for bind
#define RAW 0 // glb for lst
#define ENV 1 // glb for lst
#define NUM 0 // opt for raw
#define STR 1 // opt for raw
#define OPT 2 // opt for raw
#define LST -1 // cnt for last
#define INP 1 // cnt for raw num
#define OUT 2 // cnt for raw num
#define EQU 0 // cmp for opt
#define ICH "i" // pat for opt
#define OCH "o" // pat for opt
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
union UtilUnion utilUnionI(int i);
union UtilUnion utilUnionL(long long l);
union UtilUnion utilUnionS(const char *s);
struct UtilStruct utilStructI(int i, int u);
struct UtilStruct utilStructL(int i, long long u);
struct UtilStruct utilStructS(int i, const char *u);
union UtilUnion utilOver(int lst, int arg, int opt, int cnt, int cmp, int idx);
union UtilUnion utilHash(int lst, int arg, int opt, int cnt, int cmp);
union UtilUnion utilBind(int lst, int idx);
union UtilUnion utilSelf(int arg, int idx);
union UtilUnion utilGlob(int glb);
void utilFlag(int lst, const char *str);
void utilRaw(int lst, const char *str);
void utilEnv(int lst, const char *str);
void utilPipe(int lst, const char *str);
