#include "wrap.h"

char *WrapClose::str = 0;

void wrapCallback(const struct Close *arg)
{
	const WrapClose *ptr = static_cast<const WrapClose *>(arg);
	ptr->fnc(ptr);
}
