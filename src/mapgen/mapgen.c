#include <sys/queue.h>
#include <pthread.h>
#include <dlfcn.h>
#include <ltdl.h>

#include "mapgen_plugin.h"
#include "mapgen.h"
#include "../util.h"

/* FIXME: make it an array or hash
 * to handle multiple dimensions */
struct mapgen *(*init_mapgen)(char *seed, char *path);

/* FIXME: ugly define of map generator library */
#define MAPGEN "./src/libmg_classic.la"

/**
 * The worker thread for map generation
 *
 * @param arg (unused)
 * @return (unused)
 */
void *mg_run_worker(void *arg)
{
	struct MG_entry *mapgen_request;
	struct mapgen *plugin;
	lt_dlhandle dlop;

	lt_dlinit();
	/* FIXME: use config defined generators */
	dlop = lt_dlopen(MAPGEN);
	if (!dlop) {
		LOG(LOG_ERR, "Could not open map generator \"%s\" : %s", MAPGEN, lt_dlerror());
		return NULL;
	}
	init_mapgen = lt_dlsym(dlop, "init_mapgen");
	if (!init_mapgen) {
		LOG(LOG_ERR, "Could not find symbol \"init_mapgen\" in module %s", MAPGEN);
		return NULL;
	}
	plugin = init_mapgen("", "./world/");

	LOG(LOG_INFO, "Loaded mapgen plugin \"%s\"", MAPGEN);

	STAILQ_INIT(&mapgen_head);

	LOG(LOG_INFO, "Mapgen thread started!");
	while(1) {
		pthread_mutex_lock(&mapgen_cvmutex);
		pthread_cond_wait(&mapgen_cv, &mapgen_cvmutex);
		while ( (mapgen_request = STAILQ_FIRST(&mapgen_head)) != NULL) {
			STAILQ_REMOVE_HEAD(&mapgen_head, entries);
			plugin->generate_chunk(plugin, mapgen_request->x, mapgen_request->z);

			free(mapgen_request);
		}
		pthread_mutex_unlock(&mapgen_cvmutex);
	}
	dlclose(dlop);
	return NULL;
}
