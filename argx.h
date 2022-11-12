#define NUMARGX 256
struct ArgxNest;
typedef void (*nftype)(int idx, struct ArgxNest *nst);
typedef void *(*mftype)(int idx, struct ArgxNest *nst);
typedef int (*oftype)(int idx, struct ArgxNest *nst);
enum ArgxTag {
	FlowTag, // data flow stream
	JumpTag, // loop break nest
	NestTag, // nesting control
	NoopTag, // data container
	ArgxTags
};
enum ArgxStep {
	FwdSkpStep,
	RevSkpStep,
	FwdEntStep,
	RevEntStep,
	FwdExtStep,
	RevExtStep,
	ArgxSteps
};
struct ArgxNest {
	enum ArgxTag opt; // type of data flow control step
	int idx; // dash char from before str
	const char *str; // string after dash option
	void *arg; // configuration data
	union {
		nftype fnc;
		mftype gnc;
		oftype hnc;
	};
};
int nestJump(int idx, struct ArgxNest *nst, void *jmp);
int addFlow(const char *opt, nftype fnc);
int addJump(const char *opt, mftype gnc);
int addNest(const char *opt, oftype hnc);
int useAcum(int idx);
int useHist(int idx);
int useNoop();
int useArgument(const char *str);
void runProgram();
