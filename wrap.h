struct Para {
	enum {
		Ttype,
		Itype,
		Jtype,
		Ktype,
		Mtype,
		Ntype,
		Utype,
		Vtype,
	} t;
	union {
		int i;
		int32_t j;
		long long k;
		float m;
		double n;
		const char *u;
		struct {void *v; int w;};
	};
};
struct Close {
	int n,m;
	struct Para *a,*b;
	char *str;
};
void wrapCallback(const struct Close *arg);
void wrapFaceLuax();
