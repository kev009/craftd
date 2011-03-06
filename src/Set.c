/*
 * Adapted Set implementation for craftd.
 *
 * Contains significant code and influence from the book:
 * "C Interfaces and Implementations" by David R. Hanson (ISBN 0-201-49841-3)
 *
 * See https://github.com/kev009/cii/blob/master/LICENSE for the original MIT
 * license.
 */

#include <craftd/Set.h>

static
bool
cmpAtom (const CDPointer a, const CDPointer b)
{
    return a == b;
}

static
unsigned int
hashAtom (const CDPointer pointer)
{
    return (unsigned long) pointer >> 2;
}

CDSet*
CD_CreateSet (int hint, CDSetCompare cmp, CDSetHash hash)
{
    CDSet*     self;
    static int primes[] = { 509, 509, 1021, 2053, 4093, 8191, 16381, 32771, 65521, INT_MAX };
    size_t     i;

    assert(hint >= 0);

    for (i = 1; primes[i] < hint; i++) {
        continue;
    }

    self = CD_malloc(sizeof(CDSet) + primes[i - 1] * sizeof(self->buckets[0]));

    assert(self);

    self->size    = primes[i - 1];
    self->cmp     = cmp  ? cmp  : cmpAtom;
    self->hash    = hash ? hash : hashAtom;
    self->buckets = (CDSetMember**) (self + 1);

    for (size_t i = 0; i < self->size; i++) {
        self->buckets[i] = NULL;
    }

    self->length    = 0;
    self->timestamp = 0;

    return self;
}

CDSet*
CD_CloneSet (CDSet* self, int hint)
{
    CDSet* cloned = CD_CreateSet(hint, self->cmp, self->hash);

    assert(self);
    assert(cloned);

    CD_DO {
        CDSetMember* oldMember;

        for (size_t i = 0; i < self->size; i++) {
            for (oldMember = self->buckets[i]; oldMember != NULL; oldMember = oldMember->next) {
                CDSetMember*    newMember = CD_malloc(sizeof(CDSetMember));
                const CDPointer value     = oldMember->value;
                int             index     = cloned->hash(value) % cloned->size;

                assert(newMember);

                newMember->value = value;
                newMember->next  = cloned->buckets[index];

                cloned->buckets[i] = newMember;
                cloned->length++;
            }
        }
    }

    return cloned;
}

void
CD_DestroySet (CDSet* self)
{
    assert(self);

    if (self->length > 0) {
        CDSetMember* currentMember;
        CDSetMember* nextMember;

        for (size_t i = 0; i < self->size; i++) {
            for (currentMember = self->buckets[i]; currentMember != NULL; currentMember = nextMember) {
                nextMember = currentMember->next;

                CD_free(currentMember);
            }
        }
    }

    CD_free(self);
}

bool
CD_SetHas (CDSet* self, const CDPointer value)
{
    int          index;
    CDSetMember* member;

    assert(self);
    assert(value);

    index = self->hash(value) % self->size;

    for (member = self->buckets[index]; member != NULL; member = member->next) {
        if (self->cmp(value, member->value)) {
            return true;
        }
    }

    return false;
}

void
CD_SetPut (CDSet* self, const CDPointer value)
{
    int          index;
    CDSetMember* member;

    assert(self);
    assert(value);

    index = self->hash(value) % self->size;

    for (member = self->buckets[index]; member != NULL; member = member->next) {
        if (self->cmp(value, member->value)) {
            break;
        }
    }

    if (member == NULL) {
        member = CD_malloc(sizeof(CDSetMember));

        assert(member);

        member->value        = value;
        member->next         = self->buckets[index];
        self->buckets[index] = member;
        self->length++;
    }
    else {
        member->value = value;
    }

    self->timestamp++;
}

CDPointer
CD_SetDelete (CDSet* self, CDPointer value)
{
    int           index;
    CDSetMember** members;

    assert(self);
    assert(value);

    self->timestamp++;

    index = self->hash(value) % self->size;

    for (members = &self->buckets[index]; *members != NULL; members = &(*members)->next) {
        if (self->cmp(value, (*members)->value)) {
            CDSetMember* member = *members;
            *members            = member->next;
            value               = member->value;

            CD_free(member);

            self->length--;

            return (CDPointer) value;
        }
    }

    return CDNull;
}

int
CD_SetLength (CDSet* self)
{
    assert(self);

    return self->length;
}

void
CD_SetMap (CDSet* self, CDSetApply apply, CDPointer context)
{
    unsigned int stamp;
    CDSetMember* member;

    assert(self);
    assert(apply);

    stamp = self->timestamp;

    for (size_t i = 0; i < self->size; i++) {
        for (member = self->buckets[i]; member != NULL; member = member->next) {
            apply(member->value, context);

            assert(self->timestamp == stamp);
        }
    }
}

CDPointer*
CD_SetToArray (CDSet* self, CDPointer end)
{
    int          j = 0;
    CDPointer*   array;
    CDSetMember* member;

    assert(self);

    array = CD_malloc((self->length + 1) * sizeof(CDPointer));

    assert(array);

    for (size_t i = 0; i < self->size; i++) {
        for (member = self->buckets[i]; member != NULL; member = member->next) {
            array[j++] = member->value;
        }
    }

    array[j] = end;

    return array;
}

CDSet*
CD_SetUnion (CDSet* a, CDSet* b)
{
    if (a == NULL) {
        assert(b);

        return CD_CloneSet(b, b->size);
    }

    if (b == NULL) {
        return CD_CloneSet(a, a->size);
    }

    CDSet* result = CD_CloneSet(a, CD_Max(a->size, b->size));

    assert(a->cmp == b->cmp && a->hash == b->hash);

    CD_DO {
        CDSetMember* member;

        for (size_t i = 0; i < b->size; i++) {
            for (member = b->buckets[i]; member != NULL; member = member->next) {
                CD_SetPut(result, member->value);
            }
        }
    }

    return result;
}

CDSet*
CD_SetIntersect (CDSet* a, CDSet* b)
{
    if (a == NULL) {
        assert(b);

        return CD_CreateSet(b->size, b->cmp, b->hash);
    }

    if (b == NULL) {
        return CD_CreateSet(a->size, a->cmp, a->hash);
    }

    if (a->length < b->length) {
        return CD_SetIntersect(b, a);
    }

    CDSet* result = CD_CreateSet(CD_Min(a->size, b->size), a->cmp, a->hash);

    assert(a->cmp == b->cmp && a->hash == b->hash);

    CD_DO {
        CDSetMember* member;

        for (size_t i = 0; i < b->size; i++) {
            for (member = b->buckets[i]; member != NULL; member = member->next) {
                if (CD_SetHas(a, member->value)) {
                    CDSetMember*    current = CD_malloc(sizeof(CDSetMember));
                    const CDPointer value   = member->value;
                    int             index   = result->hash(value) % result->size;

                    assert(current);

                    current->value     = value;
                    current->next      = result->buckets[index];
                    result->buckets[i] = current;
                    result->length++;
                }
            }
        }
    }

    return result;
}

CDSet*
CD_SetMinus (CDSet* a, CDSet* b)
{
    if (a == NULL) {
        assert(b);

        return CD_CreateSet(b->size, b->cmp, b->hash);
    }

    if (b == NULL) {
        return CD_CloneSet(a, a->size);
    }

    CDSet* result = CD_CreateSet(CD_Min(a->size, b->size), a->cmp, a->hash);

    assert(a->cmp == b->cmp && a->hash == b->hash);

    CD_DO {
        CDSetMember* member;

        for (size_t i = 0; i < a->size; i++) {
            for (member = a->buckets[i]; member != NULL; member = member->next) {
                if (!CD_SetHas(b, member->value)) {
                    CDSetMember*    current = CD_malloc(sizeof(CDSetMember));
                    const CDPointer value   = member->value;
                    int             index   = result->hash(value) % result->size;

                    assert(current);

                    current->value     = value;
                    current->next      = result->buckets[index];
                    result->buckets[i] = current;
                    result->length++;
                }
            }
        }
    }

    return result;
}

CDSet*
CD_SetDifference (CDSet* a, CDSet* b)
{
    if (a == NULL) {
        assert(b);

        return CD_CloneSet(b, b->size);
    }

    if (b == NULL) {
        return CD_CloneSet(a, a->size);
    }

    CDSet* result = CD_CreateSet(CD_Min(a->size, b->size), a->cmp, a->hash);

    assert(a->cmp == b->cmp && a->hash == b->hash);

    CDSet* sets[] = { a, b, b, a };

    for (size_t i = 0; i < 4; i += 2) {
        a = sets[i];
        b = sets[i + 1];

        CD_DO {
            CDSetMember* member;

            for (size_t i = 0; i < b->size; i++) {
                for (member = b->buckets[i]; member != NULL; member = member->next) {
                    if (!CD_SetHas(a, member->value)) {
                        CDSetMember*    current = CD_malloc(sizeof(CDSetMember));
                        const CDPointer value   = member->value;
                        int             index   = result->hash(value) % result->size;

                        assert(current);

                        current->value     = value;
                        current->next      = result->buckets[index];
                        result->buckets[i] = current;
                        result->length++;
                    }
                }
            }
        }
    }

    return result;
}
