#ifndef __MAPGEN_H__
#define __MAPGEN_H__

#include <pthread.h>
#include <sys/queue.h>

pthread_cond_t mapgen_cv;
pthread_mutex_t mapgen_cvmutex;
STAILQ_HEAD(MG_stailqhead, MG_entry) mapgen_head;

struct MG_entry
{
	STAILQ_ENTRY(MG_entry) entries;
	char *dim;
	int x;
	int z;
};

void *mg_run_worker(void *arg);

#endif
