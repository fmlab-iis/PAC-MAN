#include <stdio.h>
#include <assert.h>
#include "amore/language.h"
#include "amore/fileIO.h"

int main (int argc, char **argv)
{
  language lanin;
  lanin = rdinlan (argv[2], argv[1]);
  assert (lanin != NULL);
  writelan(lanin, NULL);
  return 0;
}
