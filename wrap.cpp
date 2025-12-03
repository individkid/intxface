#include "wrap.h"

char **WrapCloseStr()
{
	static char *str = 0;
	return &str;
}

void wrapCallback(const struct Close *arg)
{
	const WrapClose *ptr = static_cast<const WrapClose *>(arg);
	ptr->fnc(ptr);
}
