#ifndef __MAPGEN_H__
#define __MAPGEN_H__

struct mapgen
{
	void (*generate_chunk)(struct mapgen *mg, int x, int z);
	/* mapgen data common to all generators */
	char *seed;
	char *path;
	/* data private to this specific mapgen */
	void *data;
};

#endif
