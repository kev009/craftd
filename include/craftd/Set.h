/*
 * Adapted Set implementation for craftd.
 *
 * Contains significant code and influence from the book:
 * "C Interfaces and Implementations" by David R. Hanson (ISBN 0-201-49841-3)
 *
 * See https://github.com/kev009/cii/blob/master/LICENSE for the original MIT
 * license.
 */

#ifndef CRAFTD_SET_H
#define CRAFTD_SET_H

#include <craftd/common.h>

typedef struct _CDSetMember {
    struct _CDSetMember* next;
    CDPointer            value;
} CDSetMember;

typedef bool         (*CDSetCompare) (const CDPointer a, const CDPointer b);
typedef unsigned int (*CDSetHash)    (const CDPointer pointer);
typedef void         (*CDSetApply)   (const CDPointer value, CDPointer context);

typedef struct _CDSet {
    size_t       length;
    unsigned int timestamp;

    CDSetCompare cmp;
    CDSetHash    hash;

    size_t size;

    CDSetMember** buckets;
} CDSet;

/**
 * Allocate and create a new Set
 *
 * @param hint a hint at the number of values the set may hold
 * @param cmp a comparison function for two members
 * @param hash a hash function for the member
 *
 * @return The instantiated Set object
 */
CDSet* CD_CreateSet (int hint, CDSetCompare cmp, CDSetHash hash);

CDSet* CD_CloneSet (CDSet* self, int hint);

/**
 * Free a Set
 */
void CD_DestroySet (CDSet* self);

/**
 * Get the number of lengths in the Set
 */
int CD_SetLength (CDSet* self);

/**
 * Test for membership of an element in the Set
 *
 * @param member pointer to the member to search for
 *
 * @return true if member exists, false otherwise
 */
bool CD_SetHas (CDSet* self, const CDPointer member);

/**
 * Add a member to the Set
 *
 * @param member pointer to a member
 */
void CD_SetPut (CDSet* self, const CDPointer member);

/**
 * Remove a member from the set
 *
 * @param member pointer to a member
 *
 * @return pointer to the removed member
 */
CDPointer CD_SetDelete (CDSet* self, const CDPointer member);

/**
 * Apply a function to all members of the Set
 *
 * @param apply function to apply to the set
 */
void CD_SetMap (CDSet* set, void (apply) (const CDPointer member, CDPointer cl), CDPointer cl);

/**
 * Create a C array of the Set members
 *
 * @param set a pointer to a CDSet
 * @param end a pointer to a value to set the end of the array to (terminator)
 *
 * @return a C array of with the members of the set
 */
CDPointer* CD_SetToArray (CDSet* set, CDPointer end);

/**
 * Perfrom a union of s+t of to sets, returning a new set with all elements of both
 *
 * @param s pointer to a CDSet
 * @param t pointer to another CDSet
 *
 * @return a new CDSet with the union, s+t,  of s and t
 */
CDSet* CD_SetUnion (CDSet* s, CDSet* t);

/**
 * Perform an intersection s*t of two sets, returing a new set with the common
 * elements of both
 *
 * @param s pointer to a CDSet
 * @param t pointer to another CDSet
 *
 * @return a new CDSet with the intersection, s*t, of s and t
 */
CDSet* CD_SetIntersect (CDSet* s, CDSet* t);

/**
 * Perform a Set minus, s-t, returning  members from s that do not appear in t
 *
 * @param s pointer to a CDSet
 * @param t pointer to another CDSet
 *
 * @return a new CDSet, s-t, of s and t
 */
CDSet* CD_SetMinus (CDSet* s, CDSet* t);

/**
 * Perform the symmetric difference, s/t, returning the members that appear in
 * s or t but not both.  If s or t is the empty set, s/t is t or s.
 *
 * @param s a pointer to a CDSet
 * @param t a pointer to another CDSet
 *
 * @return the symmetric difference, s/t, of s and t
 */
CDSet* CD_SetDifference (CDSet* s, CDSet* t);

#endif
