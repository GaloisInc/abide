#include <stdint.h>
#include <string.h>

int foo(float p1, float p2, float p3, float p4, float p5, float p6,
        float p7, float p8, float p9, float p10, float p11, float p12)
{
  return 0;
}

int main()
{
  // This is necessary to set the actual bits for a float instead of hex being
  // interpreted as a number, and then being converted to IEEE754
  int32_t i = 0x11;
  float f1;
  memcpy(&f1, &i, sizeof(f1));

  i = 0x22;
  float f2;
  memcpy(&f2, &i, sizeof(f2));

  i = 0x33;
  float f3;
  memcpy(&f3, &i, sizeof(f3));

  i = 0x44;
  float f4;
  memcpy(&f4, &i, sizeof(f4));

  i = 0x55;
  float f5;
  memcpy(&f5, &i, sizeof(f5));

  i = 0x66;
  float f6;
  memcpy(&f6, &i, sizeof(f6));

  i = 0x77;
  float f7;
  memcpy(&f7, &i, sizeof(f7));

  i = 0x88;
  float f8;
  memcpy(&f8, &i, sizeof(f8));

  i = 0x99;
  float f9;
  memcpy(&f9, &i, sizeof(f9));

  i = 0xAA;
  float f10;
  memcpy(&f10, &i, sizeof(f10));

  i = 0xBB;
  float f11;
  memcpy(&f11, &i, sizeof(f11));

  i = 0xCC;
  float f12;
  memcpy(&f12, &i, sizeof(f12));

  int x = foo(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12);
  return 0;
}
