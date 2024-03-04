#include <stdint.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

intmax_t tiger_main(intmax_t unused_static_link);

struct ChString
{
  char str[2];
};

enum Type {
  kTypeArray = 1,
  kTypeRecord = 2,
};

#pragma pack(1)
struct ArrayInfo {
  intmax_t num_of_elements;
};

struct RecordInfo {
  intmax_t num_of_fields;
  uint8_t *is_pointer_table;
};

struct Descriptor {
  intmax_t type;
  union {
    struct ArrayInfo array_info;
    struct RecordInfo record_info;
  } u;
};
#pragma options align=reset

static struct ChString ch_strings[CHAR_MAX - CHAR_MIN];

static void CheckMemAlloc(void *ptr, const char *label) {
  if (!ptr) {
    printf("%s: allocate memeory failed\n", label);
    exit(1);
  }
}

void* tiger_AllocRecord(const char *descriptor_str) {
  // The descriptor string is something like "npp",
  // the length of the string tell us the number of fields,
  // the 0th character 'n' tell us the 0th field is not a pointer,
  // the 1th character 'p' tell us the 1th field is a pointer,
  // etc.
  size_t num_of_fields = strlen(descriptor_str);
  size_t size = num_of_fields * sizeof(intmax_t);
  void *ptr = malloc(size + sizeof(struct Descriptor *));
  CheckMemAlloc(ptr, "AllocRecord");
  struct Descriptor *descriptor = (struct Descriptor *)malloc(sizeof(struct Descriptor));
  CheckMemAlloc(ptr, "AllocRecord(descriptor)");
  descriptor->type = kTypeRecord;
  descriptor->u.record_info.num_of_fields = num_of_fields;
  descriptor->u.record_info.is_pointer_table = (uint8_t *)malloc(num_of_fields * sizeof(uint8_t));
  CheckMemAlloc(descriptor->u.record_info.is_pointer_table, "AllocRecord(is_pointer_table)");
  for (size_t i = 0; i < num_of_fields; i++)
  {
    descriptor->u.record_info.is_pointer_table[i] = descriptor_str[i] == 'p';
  }
  *(struct Descriptor **)ptr = descriptor;
  return ptr;
}

void* tiger_AllocArray(intmax_t num_of_elements, intmax_t init) {
  assert(num_of_elements >= 0);
  void *ptr = (intmax_t *)malloc(((size_t)num_of_elements) * sizeof(intmax_t) + sizeof(struct Descriptor*));
  CheckMemAlloc(ptr, "AllocArray");
  struct Descriptor *descriptor = (struct Descriptor *)malloc(sizeof(struct Descriptor));
  descriptor->type = kTypeArray;
  descriptor->u.array_info.num_of_elements = num_of_elements;
  *(struct Descriptor **)ptr = descriptor;
  intmax_t *elements = (intmax_t *)((uint8_t *)ptr + sizeof(struct Descriptor*));
  for (intmax_t i = 0; i < num_of_elements; i++) {
    elements[i] = init;
  }
  return ptr;
}

intmax_t tiger_StringCompare(const char *s1, const char *s2) {
  return (intmax_t)strcmp(s1, s2);
}

intmax_t tiger_CheckNilRecord(void *record) {
  if (!record) {
    printf("Reference a field of a nil record\n");
    exit(1);
  }
  return 0;
}

intmax_t tiger_CheckArraySubscript(void *array_head, intmax_t index) {
  struct Descriptor *descriptor = *(struct Descriptor **)array_head;
  assert(descriptor->type == kTypeArray);
  if (index < 0 || index >= descriptor->u.array_info.num_of_elements) {
    printf("Index %jd out of range [0, %jd)\n", index, descriptor->u.array_info.num_of_elements);
    exit(1);
  }
  return 0;
}

intmax_t tiger_print(const char *s) {
  printf("%s", s);
  return 0;
}

intmax_t tiger_printn(intmax_t n) {
  printf("%jd", n);
  return 0;
}

intmax_t tiger_flush(void) {
  fflush(stdin);
  return 0;
}

const char* tiger_getchar(void) {
  char ch = getc(stdin);
  if (ch == EOF) {
    return "";
  } else {
    return ch_strings[ch - CHAR_MIN].str;
  }
}

intmax_t tiger_ord(const char *s) {
  if (*s == '\0') {
    return -1;
  } else {
    return (intmax_t)s[0];
  }
}

const char* tiger_chr(intmax_t ch_value) {
  if (ch_value < CHAR_MIN || ch_value >= CHAR_MAX) {
    printf("chr: value %jd is out of range\n", ch_value);
    exit(1);
  }
  return ch_strings[ch_value - CHAR_MIN].str;
}

static intmax_t CheckStringLen(const char *s, const char *label) {
  size_t len = strlen(s);
  if (len > INTMAX_MAX) {
    printf("%s: string %s is too long (%zu characters), we can't return a reasonable value\n",
           label, s, len);
    exit(1);
  }
  return (intmax_t)len;
}

intmax_t tiger_size(const char *s) {
  return CheckStringLen(s, "size");
}

const char* tiger_substring(const char *s, intmax_t first, intmax_t n) {
  intmax_t len = CheckStringLen(s, "substring");
  if (first < 0 || first + n > len) {
    printf("substring: our of range, s - %s, first - %jd, n - %jd\n",
           s, first, n);
    exit(1);
  }
  if (n == 1) {
    return ch_strings[s[first] - CHAR_MIN].str;
  }
  char *ss = (char *)malloc(sizeof(char) * (n + 1));
  CheckMemAlloc(ss, "substring");
  for (intmax_t i = 0; i < n; i++) {
    ss[i] = s[first + i];
  }
  ss[n] = '\0';
  return ss;
}

const char* tiger_concat(const char *s1, const char *s2) {
  intmax_t len1 = CheckStringLen(s1, "concat arg1");
  intmax_t len2 = CheckStringLen(s2, "concat arg2");
  char *cs = (char *)malloc(sizeof(char) * (len1 + len2 + 1));
  CheckMemAlloc(cs, "concat");
  intmax_t next = 0;
  for (intmax_t i = 0; i < len1; i++) {
    cs[next++] = s1[i];
  }
  for (intmax_t i = 0; i < len2; i++) {
    cs[next++] = s2[i];
  }
  cs[next] = '\0';
  return cs;
}

intmax_t tiger_not(intmax_t i) {
  return !i;
}

static void CheckExitCode(intmax_t code, const char *label) {
  if (code < INT_MIN || code > INT_MAX) {
    printf("%s: exit code %jd is too large\n", label, code);
    exit(1);
  }
}

intmax_t tiger_exit(intmax_t code) {
  CheckExitCode(code, "exit");
  exit((int)code);
  return 0;
}

static void InitChStrings(void) {
  for (char ch = CHAR_MIN; ch < CHAR_MAX; ch++) {
    struct ChString ch_string;
    ch_string.str[0] = ch;
    ch_string.str[1] = '\0';
    ch_strings[ch - CHAR_MIN] = ch_string;
  }
}

int main(void)
{
  InitChStrings();
  /* intmax_t code =  */tiger_main(0);
  /* CheckExitCode(code, "main"); */
  /* return (int)code; */
  // Many test cases of the book don't return a meaningful value as
  // exit code from the main expression, so we simply ignore the value
  // of the main expression, if the tiger program want different exit
  // code, it can use the exit function.
  return 0;
}
