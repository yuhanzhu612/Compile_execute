#include "trap.h"

#define N 10

static uint32_t seed = 1;

uint32_t bench_rand() {
  seed = (seed * (uint32_t)214013L + (uint32_t)2531011L);
  return (seed >> 16) & 0x7fff;
}

int main() {
    int i;
    for (i=0;i<N;i++) {
        int a = bench_rand();
        int b = bench_rand();
        int sum = a * b;
        printf("a = %d, b = %d, sum = %d\n", a, b, sum);
    }

    return 0;
}