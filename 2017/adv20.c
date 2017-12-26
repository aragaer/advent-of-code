#include <stdlib.h>
#include <stdio.h>

typedef struct {
  int x, y, z;
  int vx, vy, vz;
  int ax, ay, az;
  int dead, removed;
} particle;

particle particles[1000];

int main() {
  int n = 0;
  char buf[1024];

  int i = 0;
  int count = 0;
  particle *p = particles;
  while (fgets(buf, sizeof(buf), stdin)) {
    sscanf(buf, "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>",
	   &p->x, &p->y, &p->z, &p->vx, &p->vy, &p->vz, &p->ax, &p->ay, &p->az);
    p++;
    count++;
  }
  printf("%d\n", count);

  int dead = 0;
  while (1) {
    for (i = 0; i < count; i++) {
      p = particles + i;
      if (p->removed)
	continue;
      p->vx += p->ax;
      p->vy += p->ay;
      p->vz += p->az;
      p->x += p->vx;
      p->y += p->vy;
      p->z += p->vz;
    }
    for (i = 0; i < count; i++) {
      p = particles + i;
      if (p->dead)
	continue;
      int j;
      for (j = i + 1; j < count; j++) {
	if (particles[j].removed)
	  continue;
	if (p->x == particles[j].x && p->y == particles[j].y && p->z == particles[j].z) {
	  p->dead = 1;
	  particles[j].dead = 1;
	  printf("Collizion: %d and %d at %d-%d-%d\n", i, j, p->x, p->y, p->z);
	}
      }
    }
    dead = 0;
    for (i = 0; i < count; i++)
      if (particles[i].dead) {
	particles[i].removed = 1;
	dead++;
      }
    printf("%d\n", count-dead);
  }
}
