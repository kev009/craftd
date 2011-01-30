#ifndef CRAFTD_SET_H
#define CRAFTD_SET_H

/*
 * Contains significant code and influence from the book:
 * "C Interfaces and Implementations" by David R. Hanson (ISBN 0-201-49841-3)
 * 
 * See algos/LICENSE-MIT for the original MIT license
 */

typedef struct Set_T *Set_T;
extern Set_T    Set_new (int hint,
        int cmp(const void *x, const void *y),
        unsigned hash(const void *x));
extern void Set_free(Set_T *set);
extern int   Set_length(Set_T set);
extern int   Set_member(Set_T set, const void *member);
extern void  Set_put   (Set_T set, const void *member);
extern void *Set_remove(Set_T set, const void *member);
extern void   Set_map    (Set_T set,
        void apply(const void *member, void *cl), void *cl);
extern void **Set_toArray(Set_T set, void *end);
extern Set_T Set_union(Set_T s, Set_T t);
extern Set_T Set_inter(Set_T s, Set_T t);
extern Set_T Set_minus(Set_T s, Set_T t);
extern Set_T Set_diff (Set_T s, Set_T t);

#endif
