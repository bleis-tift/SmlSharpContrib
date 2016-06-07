#include <stdlib.h>
#include <string.h>
#include "picohttpparser.h"

int
http_parser_parse_request(const char *buf, size_t len,
                          size_t* method_start, size_t* method_len,
                          size_t* path_start, size_t* path_len,
                          int* minor_version,
                          void (callback)(int, size_t, int, size_t))
{
  struct phr_header header, headers[100];
  const char *method, *path;
  int ret;
  size_t nheaders, i;


  nheaders = sizeof(headers)/sizeof(headers[0]);

  ret = phr_parse_request(buf, len,
                          &method, method_len,
                          &path, path_len,
                          minor_version,
                          headers, &nheaders, 0);
  if (ret < 0) {
    return ret;
  }

  *method_start = (size_t)(method - buf);
  *path_start   = (size_t)(path   - buf);

  for (i = 0; i < nheaders; i += 1) {
    header = headers[i];
    callback(header.name ?(int)(header.name - buf) : -1, header.name_len,
             (int)(header.value - buf), header.value_len);
  }
  return ret;
}

int
http_parser_parse_response(const char *buf, size_t len,
                           int* minor_version,
                           int* status,
                           size_t *msg_start, size_t *msg_len,
                           void (callback)(size_t, size_t, size_t, size_t))
{
  struct phr_header header, headers[100];
  const char *msg;
  int ret;
  size_t nheaders, i;

  nheaders = sizeof(headers)/sizeof(headers[0]);

  ret = phr_parse_response(buf, len,
                           minor_version,
                           status,
                           &msg, msg_len,
                           headers, &nheaders, 0);
  if (ret < 0) {
    return ret;
  }

  *msg_start = (size_t)(msg - buf);

  for (i = 0; i < nheaders; i += 1) {
    header = headers[i];
    callback(header.name?(int)(header.name - buf):-1, header.name_len,
             (int)(header.value - buf), header.value_len);
  }
  return ret;
}

int
http_parser_parse_headers(const char *buf, size_t len,
                           void (callback)(int, size_t, int, size_t))
{
  struct phr_header header, headers[100];
  int ret;
  size_t nheaders, i;

  nheaders = sizeof(headers)/sizeof(headers[0]);

  ret = phr_parse_headers(buf, len,
                           headers, &nheaders, 0);
  if (ret < 0) {
    return ret;
  }

  for (i = 0; i < nheaders; i += 1) {
    header = headers[i];
    callback(header.name?(int)(header.name - buf):-1, header.name_len,
             (int)(header.value - buf), header.value_len);
  }
  return ret;
}



struct phr_chunked_decoder
*http_parser_decoder()
{
  struct phr_chunked_decoder *decoder;

  decoder =  malloc(sizeof(*decoder));
  if(decoder)
    memset(decoder, 0, sizeof(*decoder));
  return decoder;
}

size_t
http_parser_decode_chunked(struct phr_chunked_decoder *decoder, char *buf, int start, size_t *size)
{
  return phr_decode_chunked(decoder, buf + start, size);
}
