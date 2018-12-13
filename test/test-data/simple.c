#include <stdint.h>
#include <string.h>

int foo(int32_t p1, int8_t p2, int64_t p3, float p4)
{
  return 0;
}

int _start()
{
  // This is necessary to set the actual bits for a float instead of hex being
  // interpreted as a number, and then being converted to IEEE754
  int32_t i = 0x44;
  float f;
  memcpy(&f, &i, sizeof(f));

  int x = fun(0x11, 0x22, 0x33, f);
  return x;
}
