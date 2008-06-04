#include <stdio.h>
#include <string.h>
#include "sha1.h"

int main ( int argc, char **argv )
{

  char *s = "But the scientific cast of mind examines the world critically as if many alternative worlds might exist, as if other things might be here which are not. Then we are forced to ask why what we see is present and not something else. Why are the Sun and the Moon and the planets spheres? Why not pyramids, or cubes, or dodecahedra? Why not irregular, jumbly shapes? Why so symmetrical worlds? If you spend any time spinning hypotheses, checking to see whether they make sense, whether they conform to what else we know, thinking of tests you can pose to substantiate or deflate your hypotheses, you will find yourself doing science.";
  unsigned char result[20];
  int i;
  int j;

  for( j = 0; j < 100; j ++ )
  {
    sha1( (unsigned char *) s, strlen(s), result );
  }

  for( i = 0; i < 20; i++ )
  {
    printf("%02x", result[i]);
  }
  printf("\n", result[i]);

  return 0;
}
