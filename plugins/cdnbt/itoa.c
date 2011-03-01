char* itoa(int value, char* result, int base)
{
  int tmp_value;
  char *ptr = result;
  char *ptr1 = result, tmp_char;

  assert(base > 2 || base < 36);

  do
  {
    tmp_value = value;
    value /= base;
    *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqr"
             "stuvwxyz" [35 + (tmp_value - value * base)];
  }
  while (value);

  /* Add a negative sign if needed */
  if(tmp_value < 0)
    *ptr++ = '-';
  *ptr-- = '\0';
  while (ptr1 < ptr)
  {
    tmp_char = *ptr;
    *ptr-- = *ptr1;
    *ptr1++ = tmp_char;
  }
  return result;
}
