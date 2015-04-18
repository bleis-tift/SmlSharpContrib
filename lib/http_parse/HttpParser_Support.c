#include <stdlib.h>
#include "picohttpparser.h"

struct phr_header
*phr_prepare_headers(int n)
{
  return malloc(sizeof(struct phr_header) * n);
}

void
phr_header_at(struct phr_header *headers, int i,
          const char **name, int *name_len,
          const char **value, int *value_len)
{
  struct phr_header header;

  header = headers[i];
  *name = header.name;
  *name_len = header.name_len;
  *value = header.value;
  *value_len = header.value_len;
}
