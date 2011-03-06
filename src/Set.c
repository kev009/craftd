/*
 * Adapted Set implementation for craftd.
 *
 * Contains significant code and influence from the book:
 * "C Interfaces and Implementations" by David R. Hanson (ISBN 0-201-49841-3)
 *
 * See https://github.com/kev009/cii/blob/master/LICENSE for the original MIT
 * license.
 */

#include <assert.h>

#include <craftd/Arithmetic.h>
#include <craftd/Set.h>

/* Map to craftd internals */
#define  ALLOC(size) (CD_malloc(size))
#define  FREE(ptr) (CD_free(ptr))
#define  NEW(p) ((p) = ALLOC((long)sizeof *(p)))

/* Opaque CDSet type */
struct CDSet {
	int length;
	unsigned timestamp;
	int (*cmp)(const CDPointer x, const CDPointer y);
	unsigned (*hash)(const CDPointer x);
	int size;
	struct member {
		struct member* link;
		CDPointer member;
	} **buckets;
};

static 
int 
cmpatom(const CDPointer x, const CDPointer y) {
	return x != y;
}

static 
unsigned 
hashatom(const CDPointer x) {
	return (unsigned long)x>>2;
}

static
CDSet copy(CDSet t, int hint) {
	CDSet set;
	assert(t);
	set = CD_CreateSet(hint, t->cmp, t->hash);
	{ int i;
	  struct member* q;
	  for (i = 0; i < t->size; i++)
	  	for (q = t->buckets[i]; q; q = q->link)
		{
			struct member* p;
			const CDPointer member = q->member;
			int i = (*set->hash)(member)%set->size;
			NEW(p);
			p->member = member;
			p->link = set->buckets[i];
			set->buckets[i] = p;
			set->length++;
		}
	}
	return set;
}

CDSet 
CD_CreateSet(int hint,
	int cmp(const CDPointer x, const CDPointer y),
	unsigned hash(const CDPointer x)) {
	CDSet set;
	int i;
	static int primes[] = { 509, 509, 1021, 2053, 4093,
		8191, 16381, 32771, 65521, INT_MAX };
	assert(hint >= 0);
	for (i = 1; primes[i] < hint; i++)
		;
	set = ALLOC(sizeof (*set) +
		primes[i-1]*sizeof (set->buckets[0]));
	set->size = primes[i-1];
	set->cmp  = cmp  ?  cmp : cmpatom;
	set->hash = hash ? hash : hashatom;
	set->buckets = (struct member **)(set + 1);
	for (i = 0; i < set->size; i++)
		set->buckets[i] = NULL;
	set->length = 0;
	set->timestamp = 0;
	return set;
}

int 
CD_SetMember(CDSet set, const CDPointer member) {
	int i;
	struct member* p;
	assert(set);
	assert(member);
	i = (*set->hash)(member)%set->size;
	for (p = set->buckets[i]; p; p = p->link)
		if ((*set->cmp)(member, p->member) == 0)
			break;
	return p != NULL;
}

void 
CD_SetPut(CDSet set, const CDPointer member) {
	int i;
	struct member* p;
	assert(set);
	assert(member);
	i = (*set->hash)(member)%set->size;
	for (p = set->buckets[i]; p; p = p->link)
		if ((*set->cmp)(member, p->member) == 0)
			break;
	if (p == NULL) {
		NEW(p);
		p->member = member;
		p->link = set->buckets[i];
		set->buckets[i] = p;
		set->length++;
	} else
		p->member = member;
	set->timestamp++;
}

CDPointer
CD_SetDelete(CDSet set, CDPointer member) {
	int i;
	struct member **pp;
	assert(set);
	assert(member);
	set->timestamp++;
	i = (*set->hash)(member)%set->size;
	for (pp = &set->buckets[i]; *pp; pp = &(*pp)->link)
		if ((*set->cmp)(member, (*pp)->member) == 0) {
			struct member *p = *pp;
			*pp = p->link;
			member = p->member;
			FREE(p);
			set->length--;
			return (CDPointer)member;
		}
	return CDNull;
}

int 
CD_SetLength(CDSet set) {
	assert(set);
	return set->length;
}

void 
CD_DestroySet(CDSet* set) {
	assert(set && *set);
	if ((*set)->length > 0) {
		int i;
		struct member *p, *q;
		for (i = 0; i < (*set)->size; i++)
			for (p = (*set)->buckets[i]; p; p = q) {
				q = p->link;
				FREE(p);
			}
	}
	FREE(*set);
}

void 
CD_SetMap(CDSet set,
	void apply(const CDPointer member, CDPointer cl), CDPointer cl) {
	int i;
	unsigned stamp;
	struct member* p;
	assert(set);
	assert(apply);
	stamp = set->timestamp;
	for (i = 0; i < set->size; i++)
		for (p = set->buckets[i]; p; p = p->link) {
			apply(p->member, cl);
			assert(set->timestamp == stamp);
		}
}

CDPointer*
Set_toArray(CDSet set, CDPointer end) {
	int i, j = 0;
	CDPointer* array;
	struct member* p;
	assert(set);
	array = ALLOC((set->length + 1)*sizeof (*array));
	for (i = 0; i < set->size; i++)
		for (p = set->buckets[i]; p; p = p->link)
			array[j++] = (CDPointer)p->member;
	array[j] = end;
	return array;
}

CDSet 
CD_SetUnion(CDSet s, CDSet t) {
	if (s == NULL) {
		assert(t);
		return copy(t, t->size);
	} else if (t == NULL)
		return copy(s, s->size);
	else {
		CDSet set = copy(s, CD_Max(s->size, t->size));
		assert(s->cmp == t->cmp && s->hash == t->hash);
		{ int i;
		  struct member* q;
		  for (i = 0; i < t->size; i++)
		  	for (q = t->buckets[i]; q; q = q->link)
			CD_SetPut(set, q->member);
		}
		return set;
	}
}

CDSet CD_SetIntersect(CDSet s, CDSet t) {
	if (s == NULL) {
		assert(t);
		return CD_CreateSet(t->size, t->cmp, t->hash);
	} else if (t == NULL)
		return CD_CreateSet(s->size, s->cmp, s->hash);
	else if (s->length < t->length)
		return CD_SetIntersect(t, s);
	else {
		CDSet set = CD_CreateSet(CD_Min(s->size, t->size),
			s->cmp, s->hash);
		assert(s->cmp == t->cmp && s->hash == t->hash);
		{ int i;
		  struct member* q;
		  for (i = 0; i < t->size; i++)
		  	for (q = t->buckets[i]; q; q = q->link)
			if (CD_SetMember(s, q->member))
				{
					struct member* p;
					const CDPointer member = q->member;
					int i = (*set->hash)(member)%set->size;
					NEW(p);
					p->member = member;
					p->link = set->buckets[i];
					set->buckets[i] = p;
					set->length++;
				}
		}
		return set;
	}
}

CDSet CD_SetMinus(CDSet t, CDSet s) {
	if (t == NULL){
		assert(s);
		return CD_CreateSet(s->size, s->cmp, s->hash);
	} else if (s == NULL)
		return copy(t, t->size);
	else {
		CDSet set = CD_CreateSet(CD_Min(s->size, t->size),
			s->cmp, s->hash);
		assert(s->cmp == t->cmp && s->hash == t->hash);
		{ int i;
		  struct member* q;
		  for (i = 0; i < t->size; i++)
		  	for (q = t->buckets[i]; q; q = q->link)
			if (!CD_SetMember(s, q->member))
				{
					struct member* p;
					const CDPointer member = q->member;
					int i = (*set->hash)(member)%set->size;
					NEW(p);
					p->member = member;
					p->link = set->buckets[i];
					set->buckets[i] = p;
					set->length++;
				}
		}
		return set;
	}
}

CDSet CD_SetDifference(CDSet s, CDSet t) {
	if (s == NULL) {
		assert(t);
		return copy(t, t->size);
	} else if (t == NULL)
		return copy(s, s->size);
	else {
		CDSet set = CD_CreateSet(CD_Min(s->size, t->size),
			s->cmp, s->hash);
		assert(s->cmp == t->cmp && s->hash == t->hash);
		{ int i;
		  struct member* q;
		  for (i = 0; i < t->size; i++)
		  	for (q = t->buckets[i]; q; q = q->link)
			if (!CD_SetMember(s, q->member))
				{
					struct member* p;
					const CDPointer member = q->member;
					int i = (*set->hash)(member)%set->size;
					NEW(p);
					p->member = member;
					p->link = set->buckets[i];
					set->buckets[i] = p;
					set->length++;
				}
		}
		{ CDSet u = t; t = s; s = u; }
		{ int i;
		  struct member* q;
		  for (i = 0; i < t->size; i++)
		  	for (q = t->buckets[i]; q; q = q->link)
			if (!CD_SetMember(s, q->member))
				{
					struct member* p;
					const CDPointer member = q->member;
					int i = (*set->hash)(member)%set->size;
					NEW(p);
					p->member = member;
					p->link = set->buckets[i];
					set->buckets[i] = p;
					set->length++;
				}
		}
		return set;
	}
}

