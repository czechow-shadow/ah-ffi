#include <stdio.h>
#include <unistd.h>

#include <pcre.h>

double mysin(double x) {
  return 2.234; 
}

void okfree(void* mem) {
  pcre_free(mem);
}

void wrong_free(void* mem) {
  // This is deliberately broken...
}
