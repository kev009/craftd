/**
 * Take an integer and convert it to a base 2 to 36 char representation.
 *
 * Ideas from (http://www.jb.man.ac.uk/~slowe/cpp/itoa.html) and K&R pg. 64
 *
 * @param value integer to convert
 * @param result string representation
 * @param base conversion base, 2 to 36
 * @return pointer to string representation
 */
char *itoa(int value, char *result, int base);
