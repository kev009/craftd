#ifndef CRAFTD_ARITH_H
#define CRAFTD_ARITH_H

/*
 * Adapted by Kevin M. Bowling for compiler inlining
 * 
 * Contains significant code and influence from the book:
 * "C Interfaces and Implementations" by David R. Hanson (ISBN 0-201-49841-3)
 * 
 * See algos/LICENSE-MIT for the original MIT license
 */

static inline int Arith_max(int x, int y)
{
  return x > y ? x : y;
}

static inline int Arith_min(int x, int y)
{
  return x > y ? y : x;
}

static inline int Arith_div(int x, int y)
{
  if (-13/5 == -2 && (x < 0) != (y < 0) && x%y != 0)
    return x/y - 1;
  else
    return x/y;
}

static inline int Arith_mod(int x, int y)
{
  if (-13/5 == -2 && (x < 0) != (y < 0) && x%y != 0)
    return x%y + y;
  else
    return x%y;
}

static inline int Arith_floor(int x, int y)
{
  return Arith_div(x, y);
}

static inline int Arith_ceiling(int x, int y) 
{
  return Arith_div(x, y) + (x%y != 0);
}

#endif