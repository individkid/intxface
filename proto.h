#ifndef PROTO_H
#define PROTO_H
#include <stdint.h>
#define ERROR() exitErr(__FILE__,__LINE__)
#define SATURATE 1000000000000.0
#define SEC2NANO 1000000000.0
#define NANO2SEC 0.000000001
#define BACKOFF 0.001
#define CALLRATE 44100
#define FILESIZE 4096
#define NUMPOOL 1024
#define NUMOPEN 1024
#define NUMARGX 256
#define NUMFILE 64
#define BUFSIZE 64
#define NUMINET 16
#define NUMPEND 10
// SATURATE largest stock value
// SEC2NANO for pselect delay
// NANO2SEC for timewheel stamp
// BACKOFF file process livelock
// CALLRATE portaudio sample rate
// FILESIZE rough limit to helper file
// NUMPOOL number of pending stock changes
// NUMOPEN total number of file descriptors
// NUMARGX number of argument types
// NUMFILE limit on number of file threads
// BUFSIZE amount read from file at a time
// NUMINET open address port pair limit
// NUMPEND connection request queue length

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
		Wtype,
		Ptype,
		Qtype,
	} t;
	union {
		int i;
		int32_t j;
		long long k;
		double m;
		float n;
		const char *u;
		char *v;
		char w;
		const void *p;
		void *q;
	};
};
struct Close {
	int n,m;
	struct Para *a,*b;
	int *c;
};

void stackErr();
void exitErr(const char *file, int line);
void protoSet(const char *str);
const char *protoGet(int i);
void protoErr(const char *fmt, ...);
const char *protoMsg();
int protoPath(const char *exp);
#endif
