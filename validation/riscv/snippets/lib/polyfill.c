// https://blog.stephencleary.com/2010/10/implementing-gccs-builtin-functions.html

// Returns the number of leading 0-bits in x, starting at the most significant bit position.
// If x is zero, the result is undefined.
int __clzsi2(unsigned x);
int __clzsi2(unsigned x)
{
  // This uses a binary search (counting down) algorithm from Hacker's Delight.
   unsigned y;
   int n = 32;
   y = x >>16;  if (y != 0) {n = n -16;  x = y;}
   y = x >> 8;  if (y != 0) {n = n - 8;  x = y;}
   y = x >> 4;  if (y != 0) {n = n - 4;  x = y;}
   y = x >> 2;  if (y != 0) {n = n - 2;  x = y;}
   y = x >> 1;  if (y != 0) return n - 2;
   return n - x;
}

int __clzdi2(unsigned long x) {
    return __clzsi2(x);
}

// Returns the number of trailing 0-bits in x, starting at the least significant bit position.
// If x is zero, the result is undefined.
int __ctzsi2(unsigned x);
int __ctzsi2(unsigned x)
{
  // This uses a binary search algorithm from Hacker's Delight.
  int n = 1;
  if ((x & 0x0000FFFF) == 0) {n = n +16; x = x >>16;}
  if ((x & 0x000000FF) == 0) {n = n + 8; x = x >> 8;}
  if ((x & 0x0000000F) == 0) {n = n + 4; x = x >> 4;}
  if ((x & 0x00000003) == 0) {n = n + 2; x = x >> 2;}
  return n - (x & 1);
}

int __ctzdi2(unsigned long x) {
    return __ctzsi2(x);
}
