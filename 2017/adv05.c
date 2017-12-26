#define LEN 1066
int instrs[LEN];

main() {
  int pos;
  for (pos = 0; pos < LEN; pos++)
    scanf("%d", instrs+pos);

  int count = 0, npos;
  for (pos = 0; pos < LEN && pos >= 0; pos = npos) {
    int *ins = instrs + pos;
    npos = pos + *ins;
    if (*ins >= 3)
      (*ins)--;
    else
      (*ins)++;
    count++;
  }
  printf("%d\n", count);
}
