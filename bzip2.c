#include <bzlib.h>

int
bz2readoffset(BZFILE *file, void *buf, unsigned offset, unsigned len)
{
	return BZ2_bzread(file, buf + offset, len);
}

int
bz2writeoffset(BZFILE file, void *buf, unsigned offset, unsigned len)
{
	return BZ2_bzwrite(file, buf + offset, len);
}
