#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define TIGER_CALL __stdcall

intmax_t TIGER_CALL tiger_main(intmax_t unused_static_link);

struct ChString
{
  char str[2];
};

static struct ChString ch_strings[sizeof(char)];

static void CheckMemAlloc(void *ptr, const char *label) {
  if (!ptr) {
    printf("%s: allocate memeory failed\n", label);
    exit(1);
  }
}

void* TIGER_CALL tiger_AllocRecord(intmax_t size) {
  assert(size >= 0);
  void *ptr = malloc((size_t)size);
  CheckMemAlloc(ptr, "AllocRecord");
  return ptr;
}

intmax_t* TIGER_CALL tiger_AllocArray(intmax_t num_of_elements, intmax_t init) {
  assert(num_of_elements >= 0);
  intmax_t *ptr = (intmax_t *)malloc((size_t)num_of_elements * sizeof(intmax_t));
  CheckMemAlloc(ptr, "AllocArray");
  for (intmax_t i = 0; i < num_of_elements; i++) {
    ptr[i] = init;
  }
  return ptr;
}

intmax_t TIGER_CALL tiger_StringCompare(const char *s1, const char *s2) {
  return (intmax_t)strcmp(s1, s2);
}

void TIGER_CALL tiger_print(const char *s) {
  printf("%s", s);
}

void TIGER_CALL tiger_flush(void) {
  fflush(stdin);
}

const char* TIGER_CALL tiger_getchar(void) {
  char ch = getc(stdin);
  if (ch == EOF) {
    return "";
  } else {
    return ch_strings[ch].str;
  }
}

intmax_t TIGER_CALL tiger_ord(const char *s) {
  if (*s == '\0') {
    return -1;
  } else {
    return (intmax_t)s[0];
  }
}

const char* TIGER_CALL tiger_chr(intmax_t ch_value) {
  if (ch_value < 0 || ch_value >= sizeof(char)) {
    printf("chr: value %jd is out of range\n", ch_value);
    exit(1);
  }
  return ch_strings[ch_value].str;
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

intmax_t TIGER_CALL tiger_size(const char *s) {
  return CheckStringLen(s, "size");
}

const char* TIGER_CALL tiger_substring(const char *s, intmax_t first, intmax_t n) {
  intmax_t len = CheckStringLen(s, "substring");
  if (first < 0 || first + n > len) {
    printf("substring: our of range, s - %s, first - %jd, n - %jd\n",
           s, first, n);
    exit(1);
  }
  if (n == 1) {
    return ch_strings[s[first]].str;
  }
  char *ss = (char *)malloc(sizeof(char) * (n + 1));
  CheckMemAlloc(ss, "substring");
  for (intmax_t i = 0; i < n; i++) {
    ss[i] = s[first + i];
  }
  ss[n] = '\0';
  return ss;
}

const char* TIGER_CALL tiger_concat(const char *s1, const char *s2) {
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

intmax_t TIGER_CALL tiger_not(intmax_t i) {
  return !i;
}

static void CheckExitCode(intmax_t code, const char *label) {
  if (code < INT_MIN || code > INT_MAX) {
    printf("%s: exit code %jd is too large\n", label, code);
    exit(1);
  }
}

void TIGER_CALL tiger_exit(intmax_t code) {
  CheckExitCode(code, "exit");
  exit((int)code);
}

static void InitChStrings(void) {
  for (size_t i = 0; i < sizeof(char); i++) {
    struct ChString ch_string;
    ch_string.str[0] = (char)i;
    ch_string.str[1] = '\0';
    ch_strings[i] = ch_string;
  }
}

int main(void)
{
  InitChStrings();
  intmax_t code = tiger_main(0);
  CheckExitCode(code, "main");
  return code;
}
