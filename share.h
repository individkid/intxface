enum Share {
	WrapShare,
	FlowShare,
	GateShare,
	TypeShare,
	ShareShare
};
struct ArgxNest shareFactory(const char *arg, int *mod, int lim);
void shareInit();
