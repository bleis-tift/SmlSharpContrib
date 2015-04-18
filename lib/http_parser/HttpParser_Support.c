#include <stdlib.h>
#include <string.h>
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
  if(header.name)
    *name = header.name;
  *name_len = header.name_len;
  *value = header.value;
  *value_len = header.value_len;
}

struct phr_chunked_decoder
*phr_prepare_decoder()
{
  struct phr_chunked_decoder *decoder;

  decoder =  malloc(sizeof(*decoder));
  if(decoder)
    memset(decoder, 0, sizeof(*decoder));
  return decoder;
}

ssize_t
phr_decode_chunked_aux(struct phr_chunked_decoder *decoder, char *buf, int start, size_t *size)
{
  return phr_decode_chunked(decoder, buf + start, size);
}
