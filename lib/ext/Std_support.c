#include <stdlib.h>
#include <stdio.h>
#include <smlsharp.h>
#include <intinf.h>
#include <object.h>

static void obj_dump__(int indent, void *obj)
{
  unsigned int i;
  unsigned int *bitmap;
  void **field = obj;
  char *buf;

  if (obj == NULL) {
    printf("%*sNULL\n", indent, "");
    return;
  }

  switch (OBJ_TYPE(obj)) {
    case OBJTYPE_UNBOXED_ARRAY:
    case OBJTYPE_UNBOXED_VECTOR:
      if (indent == 0) {
        for (i = 0; i < OBJ_SIZE(obj) / sizeof(unsigned int); i++)
          printf("%*s0x%08x\n",
              0, "", ((unsigned int *)field)[i]);
        for (i = i * sizeof(unsigned int); i < OBJ_SIZE(obj); i++)
          printf("%*s0x%02x\n",
              0, "", ((unsigned char*)field)[i]);
      } else {
        printf("%*s%p:%u:%s\n",
            indent, "", obj, OBJ_SIZE(obj),
            (OBJ_TYPE(obj) == OBJTYPE_UNBOXED_ARRAY)
            ? "UNBOXED_ARRAY" : "UNBOXED_VECTOR");
        for (i = 0; i < OBJ_SIZE(obj) / sizeof(unsigned int); i++)
          printf("%*s0x%08x\n",
              indent + 2, "", ((unsigned int *)field)[i]);
        for (i = i * sizeof(unsigned int); i < OBJ_SIZE(obj); i++)
          printf("%*s0x%02x\n",
              indent + 2, "", ((unsigned char*)field)[i]);
      }
      break;

    case OBJTYPE_BOXED_ARRAY:
    case OBJTYPE_BOXED_VECTOR:
      if (indent == 0) {
        for (i = 0; i < OBJ_SIZE(obj) / sizeof(void*); i++)
          obj_dump__(0, field[i]);
        for (i = i * sizeof(void*); i < OBJ_SIZE(obj); i++)
          printf("%*s0x%02x\n",
              0, "", ((char*)field)[i]);
      } else {
        printf("%*s%p:%u:%s\n",
            indent, "", obj, OBJ_SIZE(obj),
            (OBJ_TYPE(obj) == OBJTYPE_BOXED_ARRAY)
            ? "BOXED_ARRAY" : "BOXED_VECTOR");
        for (i = 0; i < OBJ_SIZE(obj) / sizeof(void*); i++)
          obj_dump__(indent + 2, field[i]);
        for (i = i * sizeof(void*); i < OBJ_SIZE(obj); i++)
          printf("%*s0x%02x\n",
              indent + 2, "", ((char*)field)[i]);
      }
      break;

    case OBJTYPE_RECORD:
      printf("%*s%p:%u:RECORD\n",
          indent, "", obj, OBJ_SIZE(obj));
      bitmap = OBJ_BITMAP(obj);
      for (i = 0; i < OBJ_SIZE(obj) / sizeof(void*); i++) {
        if (BITMAP_BIT(bitmap, i) != TAG_UNBOXED)
          obj_dump__(indent + 2, field[i]);
        else
          printf("%*s%p\n", indent + 2, "", field[i]);
      }
      break;

    case OBJTYPE_INTINF:
      buf = sml_intinf_fmt((sml_intinf_t*)obj, 10);
      printf("%*s%p:%u:INTINF: %s\n",
          indent, "", obj, OBJ_SIZE(obj), buf);
      free(buf);
      break;

    default:
      printf("%*s%p:%u:unknown type %u",
          indent, "", obj, OBJ_SIZE(obj), OBJ_TYPE(obj));
      break;
  }
}

void c_dump(void* p, size_t size) {
  obj_dump__(0, p);
}
