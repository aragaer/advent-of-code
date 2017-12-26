long base = 2147483647;
unsigned long step(unsigned long start, unsigned int fac, unsigned int mul) {
  do {
    start = start * fac % base;
  } while (start & mul);
  return start;
}

int main() {
  long a = 699;
  long b = 124;
  unsigned int factorA = 16807;
  unsigned int factorB = 48271;
  long i;
  int result = 0;
  for (i = 0; i < 5000000; i++) {
    a = step(a, factorA, 3U);
    b = step(b, factorB, 7U);
    result += !((a ^ b) & 65535);
  }
  printf("%d\n", result);
}
