#include <stdio.h>
#include <string.h>
#define SIZE 1024

char map[SIZE][SIZE];
char letters[SIZE];
int letter_count = 0;

int height, width;

typedef enum {
  up, down, left, right,
} direction;

int x, y;
direction dir;

void move() {
  switch (dir) {
  case up:
    y--;
    break;
  case down:
    y++;
    break;
  case left:
    x--;
    break;
  case right:
    x++;
    break;
  }
}

void search_left_right() {
  if (x == 0 || map[y][x-1] == ' ')
    dir = right;
  else
    dir = left;
}

void search_up_down() {
  if (y == 0 || strlen(map[y-1]) < x || map[y-1][x] == ' ')
    dir = down;
  else
    dir = up;
}

void turn() {
  switch (dir) {
  case up:
  case down:
    search_left_right();
    break;
  case left:
  case right:
    search_up_down();
  }
}

void dump_map() {
  int l;
  char L[SIZE];
  for (l = 0; l < y; l++)
    printf("%s\n", map[l]);
  strcpy(L, map[y]);
  L[x] = '*';
  printf("%s\n", L);
  for (l = y+1; l < height; l++)
    printf("%s\n", map[l]);
}

int main() {
  while (fgets(map[height], SIZE, stdin)) {
    map[height][strlen(map[height])] = 0;
    if (strlen(map[height]) > width)
      width = strlen(map[height]);
    height++;
  }

  printf("%d x %d\n", width, height);

  while (map[0][x] == ' ')
    x++;
  dir = down;

  int step_count = 0;
  do {
    move();
    if (x < 0 || x >= width || y < 0 || y >= height)
      break;
    step_count++;
    if (map[y][x] >= 'A' && map[y][x] <= 'Z')
      letters[letter_count++] = map[y][x];
    if (map[y][x] == '+')
      turn();
    if (map[y][x] == ' ')
      break;
    // dump_map();
  } while (x >= 0 && x < width && y >= 0 && y < height);
  printf("exited at %d %d\n", x, y);
  printf("%s %d\n", letters, step_count);
}
