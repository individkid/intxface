#include "call.h"
int lft[NUMCALL] = {0}; // flow control opens
int rgt[NUMCALL] = {0}; // flow control closes
void *bnd[NUMCALL] = {0}; // per-idx argument
tftype clk[NUMCALL] = {0}; // time wheel function
fftype flw[NUMCALL] = {0}; // data flow function
nftype nst[NUMCALL] = {0}; // flow control function
int lim = 0;
void callNest(void *ovr)
{
	int idx = 0;
	while (idx < lim) {
		int ret = nst[idx](bnd[idx],ovr);
		if (ret == 0) idx++;
		while (ret > 0 && idx < lim) {
			if (rgt[idx] < ret) ret -= rgt[idx];
			else ret = 0;
			idx++;}
		while (ret < 0 && idx > 0) {
			if (lft[idx] < -ret) ret += lft[idx];
			else ret = 0;
			if (ret < 0 && idx > 0) idx--;}}
}
