;

    CDHash* players;
    CDMap*  clients;
    CDMap*  entities;

    CD_DEFINE_PRIVATE;
    CD_DEFINE_CACHE;
    CD_DEFINE_ERROR;
} CDWorld;

CDWorld* CD_CreateWorld (CDServer* server);

void CD_DestroyWorld (CDWorld* self);

MCEntityId CD_WorldGenerateEntityId (CDWorld* self);

void CD_WorldAddPlayer (CDWorld* self, CDPlayer* player);

void CD_WorldBroadcast (CDWorld* self, CDString* message);

uint16_t CD_WorldGetTime (CDWorld* self);

uint16_t CD_WorldSetTime (CDWorld* self, uint16_t time);

#endif
