#include <stdint.h>

int foo(int64_t p1, int64_t p2, int64_t p3, int64_t p4, int64_t p5,
        int64_t p6, int64_t p7, int64_t p8, int64_t p9, int64_t p10)
{
  return 0;
}

int _start()
{
  int x = foo(0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xAA);
  return 0;
}
