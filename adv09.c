#include <stdio.h>
int main() {
  int t;
  int next_group_cost = 1;
  int total_cost = 0;
  int group_count = 0;
  int cancel = 0, is_garbage = 0;
  int garbage_count = 0;
  while ((t = getchar()) != EOF) {
    char this_char = (char) t;
    if (cancel == 1) {
      cancel = 0;
      continue;
    }
    garbage_count += is_garbage && this_char != '>' && this_char != '!';
    switch (this_char) {
    case '!':
      cancel = 1;
      break;
    case '<':
      is_garbage = 1;
      break;
    case '>':
      is_garbage = 0;
      break;
    default:
      break;
    }
    if (is_garbage)
      continue;
    switch (this_char) {
    case '{':
      total_cost += next_group_cost;
      next_group_cost++;
      break;
    case '}':
      next_group_cost--;
      group_count++;
      break;
    default:
      break;
    }
  }
  printf("%d %d %d\n", group_count, total_cost, garbage_count);
}
