#ifdef _MSC_VER
#define inline __inline
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <getopt.h>
static int detail_memory = 0;
static int debugging = 0;
static int binary_output = 0;
/* Crash and burn. */

#include <stdarg.h>

static const char *fut_progname;

void panic(int eval, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
        fprintf(stderr, "%s: ", fut_progname);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
        exit(eval);
}

//// Text I/O

struct array_reader {
  char* elems;
  int64_t n_elems_space;
  int64_t elem_size;
  int64_t n_elems_used;
  int64_t *shape;
  int (*elem_reader)(void*);
};

static int peekc() {
  int c = getchar();
  if (c != EOF) {
    ungetc(c,stdin);
  }
  return c;
}

static int next_is_not_constituent() {
  int c = peekc();
  return c == EOF || !isalnum(c);
}

static void skipspaces() {
  int c = getchar();
  if (isspace(c)) {
    skipspaces();
  } else if (c == '-' && peekc() == '-') {
    // Skip to end of line.
    for (; c != '\n' && c != EOF; c = getchar());
    // Next line may have more spaces.
    skipspaces();
  } else if (c != EOF) {
    ungetc(c, stdin);
  }
}

static int read_str_elem(struct array_reader *reader) {
  int ret;
  if (reader->n_elems_used == reader->n_elems_space) {
    reader->n_elems_space *= 2;
    reader->elems = (char*) realloc(reader->elems,
                                    reader->n_elems_space * reader->elem_size);
  }

  ret = reader->elem_reader(reader->elems + reader->n_elems_used * reader->elem_size);

  if (ret == 0) {
    reader->n_elems_used++;
  }

  return ret;
}

static int read_str_array_elems(struct array_reader *reader, int dims) {
  int c;
  int ret;
  int first = 1;
  char *knows_dimsize = (char*) calloc(dims,sizeof(char));
  int cur_dim = dims-1;
  int64_t *elems_read_in_dim = (int64_t*) calloc(dims,sizeof(int64_t));
  while (1) {
    skipspaces();

    c = getchar();
    if (c == ']') {
      if (knows_dimsize[cur_dim]) {
        if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
          ret = 1;
          break;
        }
      } else {
        knows_dimsize[cur_dim] = 1;
        reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
      }
      if (cur_dim == 0) {
        ret = 0;
        break;
      } else {
        cur_dim--;
        elems_read_in_dim[cur_dim]++;
      }
    } else if (c == ',') {
      skipspaces();
      c = getchar();
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        first = 1;
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else if (cur_dim == dims - 1) {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
      } else {
        ret = 1;
        break;
      }
    } else if (c == EOF) {
      ret = 1;
      break;
    } else if (first) {
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
        first = 0;
      }
    } else {
      ret = 1;
      break;
    }
  }

  free(knows_dimsize);
  free(elems_read_in_dim);
  return ret;
}

static int read_str_empty_array(const char *type_name, int64_t *shape, int64_t dims) {
  char c;
  if (scanf("empty") == EOF) {
    return 1;
  }

  c = getchar();
  if (c != '(') {
    return 1;
  }

  for (int i = 0; i < dims-1; i++) {
    c = getchar();
    if (c != '[') {
      return 1;
    }
    c = getchar();
    if (c != ']') {
      return 1;
    }
  }

  int n = strlen(type_name);
  for (int i = 0; i < n; i++) {
    c = getchar();
    if (c != type_name[i]) {
      return 1;
    }
  }

  if (getchar() != ')') {
    return 1;
  }

  for (int i = 0; i < dims; i++) {
    shape[i] = 0;
  }

  return 0;
}

static int read_str_array(int64_t elem_size, int (*elem_reader)(void*),
                          const char *type_name,
                          void **data, int64_t *shape, int64_t dims) {
  int ret;
  struct array_reader reader;
  int64_t read_dims = 0;

  while (1) {
    int c;
    skipspaces();
    c = getchar();
    if (c=='[') {
      read_dims++;
    } else {
      if (c != EOF) {
        ungetc(c, stdin);
      }
      break;
    }
  }

  if (read_dims == 0) {
    return read_str_empty_array(type_name, shape, dims);
  }

  if (read_dims != dims) {
    return 1;
  }

  reader.shape = shape;
  reader.n_elems_used = 0;
  reader.elem_size = elem_size;
  reader.n_elems_space = 16;
  reader.elems = (char*) realloc(*data, elem_size*reader.n_elems_space);
  reader.elem_reader = elem_reader;

  ret = read_str_array_elems(&reader, dims);

  *data = reader.elems;

  return ret;
}

/* Makes a copy of numeric literal removing any underscores, and
   length of the literal. */
static int remove_underscores(char* buf) {
  int buf_index = 0;
  char c = getchar();
  while (isxdigit(c) || c == '.' || c == '+' || c == '-' ||
         c == 'x' || c == 'X' ||
         c == 'p' || c == 'P' || /* exponent for hex. floats */
         c == 'e' || c == 'E' || c == '_') {
    if (c == '_') {
      c = getchar();
      continue;
    }
    else {
      buf[buf_index++] = c;
      c = getchar();
    }
  }
  buf[buf_index] = 0;
  ungetc(c, stdin);             /* unget 'i' */
  return buf_index;
}

static int read_str_i8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNi8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(int8_t*)dest = x;
    scanf("i8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNu8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(uint8_t*)dest = x;
    scanf("u8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("i16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    printf("fail\n");
    return 1;
  }
}

static int read_str_u16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("u16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("i32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("u32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi64, (int64_t*)dest) == 1) {
    scanf("i64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  // FIXME: This is not correct, as SCNu64 only permits decimal
  // literals.  However, SCNi64 does not handle very large numbers
  // correctly (it's really for signed numbers, so that's fair).
  if (sscanf(buf, "%"SCNu64, (int64_t*)dest) == 1) {
    scanf("u64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%f", (float*)dest) == 1) {
    scanf("f32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%lf", (double*)dest) == 1) {
    scanf("f64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_bool(void* dest) {
  /* This is a monstrous hack.  Maybe we should get a proper lexer in here. */
  char b[4];
  skipspaces();
  if (scanf("%4c", b) == 1) {
    if (strncmp(b, "true", 4) == 0) {
      *(char*)dest = 1;
      return 0;
    } else if (strncmp(b, "fals", 4) == 0 && getchar() == 'e') {
      *(char*)dest = 0;
      return 0;
    } else {
      return 1;
    }
  } else {
    return 1;
  }
}

static int write_str_i8(FILE *out, int8_t *src) {
  return fprintf(out, "%hhdi8", *src);
}

static int write_str_u8(FILE *out, uint8_t *src) {
  return fprintf(out, "%hhuu8", *src);
}

static int write_str_i16(FILE *out, int16_t *src) {
  return fprintf(out, "%hdi16", *src);
}

static int write_str_u16(FILE *out, uint16_t *src) {
  return fprintf(out, "%huu16", *src);
}

static int write_str_i32(FILE *out, int32_t *src) {
  return fprintf(out, "%di32", *src);
}

static int write_str_u32(FILE *out, uint32_t *src) {
  return fprintf(out, "%uu32", *src);
}

static int write_str_i64(FILE *out, int64_t *src) {
  return fprintf(out, "%"PRIi64"i64", *src);
}

static int write_str_u64(FILE *out, uint64_t *src) {
  return fprintf(out, "%"PRIu64"u64", *src);
}

static int write_str_f32(FILE *out, float *src) {
  return fprintf(out, "%.6ff32", *src);
}

static int write_str_f64(FILE *out, double *src) {
  return fprintf(out, "%.6ff64", *src);
}

static int write_str_bool(FILE *out, void *src) {
  return fprintf(out, *(char*)src ? "true" : "false");
}

//// Binary I/O

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

// Reading little-endian byte sequences.  On big-endian hosts, we flip
// the resulting bytes.

static int read_byte(void* dest) {
  int num_elems_read = fread(dest, 1, 1, stdin);
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_2byte(void* dest) {
  uint16_t x;
  int num_elems_read = fread(&x, 2, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  *(uint16_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_4byte(void* dest) {
  uint32_t x;
  int num_elems_read = fread(&x, 4, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  *(uint32_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_8byte(void* dest) {
  uint64_t x;
  int num_elems_read = fread(&x, 8, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  *(uint64_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int write_byte(void* dest) {
  int num_elems_written = fwrite(dest, 1, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_2byte(void* dest) {
  uint16_t x = *(uint16_t*)dest;
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  int num_elems_written = fwrite(&x, 2, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_4byte(void* dest) {
  uint32_t x = *(uint32_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  int num_elems_written = fwrite(&x, 4, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_8byte(void* dest) {
  uint64_t x = *(uint64_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  int num_elems_written = fwrite(&x, 8, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

//// Types

typedef int (*writer)(FILE*, void*);
typedef int (*reader)(void*);

struct primtype_info_t {
  const char binname[4]; // Used for parsing binary data.
  const char* type_name; // Same name as in Futhark.
  const int size; // in bytes
  const writer write_str; // Write in text format.
  const reader read_str; // Read in text format.
  const writer write_bin; // Write in binary format.
  const reader read_bin; // Read in binary format.
};

const static struct primtype_info_t i8 =
  {.binname = "  i8", .type_name = "i8",   .size = 1,
   .write_str = (writer)write_str_i8, .read_str = (reader)read_str_i8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
const static struct primtype_info_t i16 =
  {.binname = " i16", .type_name = "i16",  .size = 2,
   .write_str = (writer)write_str_i16, .read_str = (reader)read_str_i16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
const static struct primtype_info_t i32 =
  {.binname = " i32", .type_name = "i32",  .size = 4,
   .write_str = (writer)write_str_i32, .read_str = (reader)read_str_i32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
const static struct primtype_info_t i64 =
  {.binname = " i64", .type_name = "i64",  .size = 8,
   .write_str = (writer)write_str_i64, .read_str = (reader)read_str_i64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
const static struct primtype_info_t u8 =
  {.binname = "  u8", .type_name = "u8",   .size = 1,
   .write_str = (writer)write_str_u8, .read_str = (reader)read_str_u8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
const static struct primtype_info_t u16 =
  {.binname = " u16", .type_name = "u16",  .size = 2,
   .write_str = (writer)write_str_u16, .read_str = (reader)read_str_u16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
const static struct primtype_info_t u32 =
  {.binname = " u32", .type_name = "u32",  .size = 4,
   .write_str = (writer)write_str_u32, .read_str = (reader)read_str_u32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
const static struct primtype_info_t u64 =
  {.binname = " u64", .type_name = "u64",  .size = 8,
   .write_str = (writer)write_str_u64, .read_str = (reader)read_str_u64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
const static struct primtype_info_t f32 =
  {.binname = " f32", .type_name = "f32",  .size = 4,
   .write_str = (writer)write_str_f32, .read_str = (reader)read_str_f32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
const static struct primtype_info_t f64 =
  {.binname = " f64", .type_name = "f64",  .size = 8,
   .write_str = (writer)write_str_f64, .read_str = (reader)read_str_f64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
const static struct primtype_info_t bool =
  {.binname = "bool", .type_name = "bool", .size = 1,
   .write_str = (writer)write_str_bool, .read_str = (reader)read_str_bool,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};

static const struct primtype_info_t* primtypes[] = {
  &i8, &i16, &i32, &i64,
  &u8, &u16, &u32, &u64,
  &f32, &f64,
  &bool,
  NULL // NULL-terminated
};

// General value interface.  All endian business taken care of at
// lower layers.

static int read_is_binary() {
  skipspaces();
  int c = getchar();
  if (c == 'b') {
    int8_t bin_version;
    int ret = read_byte(&bin_version);

    if (ret != 0) { panic(1, "binary-input: could not read version.\n"); }

    if (bin_version != BINARY_FORMAT_VERSION) {
      panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
            bin_version, BINARY_FORMAT_VERSION);
    }

    return 1;
  }
  ungetc(c, stdin);
  return 0;
}

static const struct primtype_info_t* read_bin_read_type_enum() {
  char read_binname[4];

  int num_matched = scanf("%4c", read_binname);
  if (num_matched != 1) { panic(1, "binary-input: Couldn't read element type.\n"); }

  const struct primtype_info_t **type = primtypes;

  for (; *type != NULL; type++) {
    // I compare the 4 characters manually instead of using strncmp because
    // this allows any value to be used, also NULL bytes
    if (memcmp(read_binname, (*type)->binname, 4) == 0) {
      return *type;
    }
  }
  panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
  return NULL;
}

static void read_bin_ensure_scalar(const struct primtype_info_t *expected_type) {
  int8_t bin_dims;
  int ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != 0) {
    panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
          bin_dims);
  }

  const struct primtype_info_t *bin_type = read_bin_read_type_enum();
  if (bin_type != expected_type) {
    panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
          expected_type->type_name,
          bin_type->type_name);
  }
}

//// High-level interface

static int read_bin_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  int ret;

  int8_t bin_dims;
  ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != dims) {
    panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
          dims, bin_dims);
  }

  const struct primtype_info_t *bin_primtype = read_bin_read_type_enum();
  if (expected_type != bin_primtype) {
    panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
          dims, expected_type->type_name, dims, bin_primtype->type_name);
  }

  uint64_t elem_count = 1;
  for (int i=0; i<dims; i++) {
    uint64_t bin_shape;
    ret = read_le_8byte(&bin_shape);
    if (ret != 0) { panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i); }
    elem_count *= bin_shape;
    shape[i] = (int64_t) bin_shape;
  }

  size_t elem_size = expected_type->size;
  void* tmp = realloc(*data, elem_count * elem_size);
  if (tmp == NULL) {
    panic(1, "binary-input: Failed to allocate array of size %i.\n",
          elem_count * elem_size);
  }
  *data = tmp;

  size_t num_elems_read = fread(*data, elem_size, elem_count, stdin);
  if (num_elems_read != elem_count) {
    panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
          elem_count, num_elems_read);
  }

  // If we're on big endian platform we must change all multibyte elements
  // from using little endian to big endian
  if (IS_BIG_ENDIAN && elem_size != 1) {
    char* elems = (char*) *data;
    for (uint64_t i=0; i<elem_count; i++) {
      char* elem = elems+(i*elem_size);
      for (int j=0; j<elem_size/2; j++) {
        char head = elem[j];
        int tail_index = elem_size-1-j;
        elem[j] = elem[tail_index];
        elem[tail_index] = head;
      }
    }
  }

  return 0;
}

static int read_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  if (!read_is_binary()) {
    return read_str_array(expected_type->size, (reader)expected_type->read_str, expected_type->type_name, data, shape, dims);
  } else {
    return read_bin_array(expected_type, data, shape, dims);
  }
}

static int write_str_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  if (rank==0) {
    elem_type->write_str(out, (void*)data);
  } else {
    int64_t len = shape[0];
    int64_t slice_size = 1;

    int64_t elem_size = elem_type->size;
    for (int64_t i = 1; i < rank; i++) {
      slice_size *= shape[i];
    }

    if (len*slice_size == 0) {
      printf("empty(");
      for (int64_t i = 1; i < rank; i++) {
        printf("[]");
      }
      printf("%s", elem_type->type_name);
      printf(")");
    } else if (rank==1) {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        elem_type->write_str(out, (void*) (data + i * elem_size));
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    } else {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        write_str_array(out, elem_type, data + i * slice_size * elem_size, shape+1, rank-1);
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    }
  }
  return 0;
}

static int write_bin_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fputs(elem_type->binname, out);
  fwrite(shape, sizeof(int64_t), rank, out);

  if (IS_BIG_ENDIAN) {
    for (size_t i = 0; i < num_elems; i++) {
      unsigned char *elem = data+i*elem_type->size;
      for (size_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size-j], 1, 1, out);
      }
    }
  } else {
    fwrite(data, elem_type->size, num_elems, out);
  }

  return 0;
}

static int write_array(FILE *out, int write_binary,
                       const struct primtype_info_t *elem_type, void *data, int64_t *shape, int8_t rank) {
  if (write_binary) {
    return write_bin_array(out, elem_type, data, shape, rank);
  } else {
    return write_str_array(out, elem_type, data, shape, rank);
  }
}

static int read_scalar(const struct primtype_info_t *expected_type, void *dest) {
  if (!read_is_binary()) {
    return expected_type->read_str(dest);
  } else {
    read_bin_ensure_scalar(expected_type);
    return expected_type->read_bin(dest);
  }
}

static int write_scalar(FILE *out, int write_binary, const struct primtype_info_t *type, void *src) {
  if (write_binary) {
    return write_bin_array(out, type, src, NULL, 0);
  } else {
    return type->write_str(out, src);
  }
}

/* Some simple utilities for wall-clock timing.

   The function get_wall_time() returns the wall time in microseconds
   (with an unspecified offset).
*/

#ifdef _WIN32

#include <windows.h>

int64_t get_wall_time() {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
/* Assuming POSIX */

#include <time.h>
#include <sys/time.h>

int64_t get_wall_time() {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

#endif

#define FUT_BLOCK_DIM 16
/* The simple OpenCL runtime framework used by Futhark. */

#ifdef __APPLE__
  #include <OpenCL/cl.h>
#else
  #include <CL/cl.h>
#endif

#define OPENCL_SUCCEED(e) opencl_succeed(e, #e, __FILE__, __LINE__)

static cl_context fut_cl_context;
static cl_command_queue fut_cl_queue;
static const char *cl_preferred_platform = "";
static const char *cl_preferred_device = "";
static int cl_preferred_device_num = 0;
static int cl_debug = 0;

static size_t cl_group_size = 256;
static size_t cl_num_groups = 128;
static size_t cl_tile_size = 32;
static size_t cl_lockstep_width = 1;
static const char* cl_dump_program_to = NULL;
static const char* cl_load_program_from = NULL;

struct opencl_device_option {
  cl_platform_id platform;
  cl_device_id device;
  cl_device_type device_type;
  char *platform_name;
  char *device_name;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */
static void post_opencl_setup(struct opencl_device_option*);

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

static const char* opencl_error_string(unsigned int err)
{
    switch (err) {
        case CL_SUCCESS:                            return "Success!";
        case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
        case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES:                   return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
        case CL_MAP_FAILURE:                        return "Map failure";
        case CL_INVALID_VALUE:                      return "Invalid value";
        case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
        case CL_INVALID_PLATFORM:                   return "Invalid platform";
        case CL_INVALID_DEVICE:                     return "Invalid device";
        case CL_INVALID_CONTEXT:                    return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
        case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
        case CL_INVALID_SAMPLER:                    return "Invalid sampler";
        case CL_INVALID_BINARY:                     return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
        case CL_INVALID_PROGRAM:                    return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
        case CL_INVALID_KERNEL:                     return "Invalid kernel";
        case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
        case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
        case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
        case CL_INVALID_EVENT:                      return "Invalid event";
        case CL_INVALID_OPERATION:                  return "Invalid operation";
        case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

static void opencl_succeed(unsigned int ret,
                    const char *call,
                    const char *file,
                    int line) {
  if (ret != CL_SUCCESS) {
    panic(-1, "%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opencl_error_string(ret));
  }
}

void set_preferred_platform(const char *s) {
  cl_preferred_platform = s;
}

void set_preferred_device(const char *s) {
  int x = 0;
  if (*s == '#') {
    s++;
    while (isdigit(*s)) {
      x = x * 10 + (*s++)-'0';
    }
    // Skip trailing spaces.
    while (isspace(*s)) {
      s++;
    }
  }
  cl_preferred_device = s;
  cl_preferred_device_num = x;
}

static char* opencl_platform_info(cl_platform_id platform,
                                  cl_platform_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetPlatformInfo(platform, param, req_bytes, info, NULL));

  return info;
}

static char* opencl_device_info(cl_device_id device,
                                cl_device_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, 0, NULL, &req_bytes));

  info = malloc(req_bytes);

  OPENCL_SUCCEED(clGetDeviceInfo(device, param, req_bytes, info, NULL));

  return info;
}

static void opencl_all_device_options(struct opencl_device_option **devices_out,
                                      size_t *num_devices_out) {
  size_t num_devices = 0, num_devices_added = 0;

  cl_platform_id *all_platforms;
  cl_uint *platform_num_devices;

  cl_uint num_platforms;

  // Find the number of platforms.
  OPENCL_SUCCEED(clGetPlatformIDs(0, NULL, &num_platforms));

  // Make room for them.
  all_platforms = calloc(num_platforms, sizeof(cl_platform_id));
  platform_num_devices = calloc(num_platforms, sizeof(cl_uint));

  // Fetch all the platforms.
  OPENCL_SUCCEED(clGetPlatformIDs(num_platforms, all_platforms, NULL));

  // Count the number of devices for each platform, as well as the
  // total number of devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    if (clGetDeviceIDs(all_platforms[i], CL_DEVICE_TYPE_ALL,
                       0, NULL, &platform_num_devices[i]) == CL_SUCCESS) {
      num_devices += platform_num_devices[i];
    } else {
      platform_num_devices[i] = 0;
    }
  }

  // Make room for all the device options.
  struct opencl_device_option *devices =
    calloc(num_devices, sizeof(struct opencl_device_option));

  // Loop through the platforms, getting information about their devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    cl_platform_id platform = all_platforms[i];
    cl_uint num_platform_devices = platform_num_devices[i];

    if (num_platform_devices == 0) {
      continue;
    }

    char *platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
    cl_device_id *platform_devices =
      calloc(num_platform_devices, sizeof(cl_device_id));

    // Fetch all the devices.
    OPENCL_SUCCEED(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL,
                                  num_platform_devices, platform_devices, NULL));

    // Loop through the devices, adding them to the devices array.
    for (cl_uint i = 0; i < num_platform_devices; i++) {
      char *device_name = opencl_device_info(platform_devices[i], CL_DEVICE_NAME);
      devices[num_devices_added].platform = platform;
      devices[num_devices_added].device = platform_devices[i];
      OPENCL_SUCCEED(clGetDeviceInfo(platform_devices[i], CL_DEVICE_TYPE,
                                     sizeof(cl_device_type),
                                     &devices[num_devices_added].device_type,
                                     NULL));
      // We don't want the structs to share memory, so copy the platform name.
      // Each device name is already unique.
      devices[num_devices_added].platform_name = strclone(platform_name);
      devices[num_devices_added].device_name = device_name;
      num_devices_added++;
    }
    free(platform_devices);
    free(platform_name);
  }
  free(all_platforms);
  free(platform_num_devices);

  *devices_out = devices;
  *num_devices_out = num_devices;
}

static struct opencl_device_option get_preferred_device() {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  int num_platform_matches = 0;
  int num_device_matches = 0;

  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strstr(device.platform_name, cl_preferred_platform) != NULL &&
        strstr(device.device_name, cl_preferred_device) != NULL &&
        num_device_matches++ == cl_preferred_device_num) {
      // Free all the platform and device names, except the ones we have chosen.
      for (size_t j = 0; j < num_devices; j++) {
        if (j != i) {
          free(devices[j].platform_name);
          free(devices[j].device_name);
        }
      }
      free(devices);
      return device;
    }
  }

  panic(1, "Could not find acceptable OpenCL device.\n");
}

static void describe_device_option(struct opencl_device_option device) {
  fprintf(stderr, "Using platform: %s\n", device.platform_name);
  fprintf(stderr, "Using device: %s\n", device.device_name);
}

static cl_build_status build_opencl_program(cl_program program, cl_device_id device, const char* options) {
  cl_int ret_val = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (ret_val != CL_SUCCESS && ret_val != CL_BUILD_PROGRAM_FAILURE) {
    assert(ret_val == 0);
  }

  cl_build_status build_status;
  ret_val = clGetProgramBuildInfo(program,
                                  device,
                                  CL_PROGRAM_BUILD_STATUS,
                                  sizeof(cl_build_status),
                                  &build_status,
                                  NULL);
  assert(ret_val == 0);

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size);
    assert(ret_val == 0);

    build_log = malloc(ret_val_size+1);
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL);
    assert(ret_val == 0);

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s\n", build_log);

    free(build_log);
  }

  return build_status;
}

// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
static cl_program setup_opencl(const char *srcs[]) {

  cl_int error;
  cl_platform_id platform;
  cl_device_id device;
  cl_uint platforms, devices;
  size_t max_group_size;

  struct opencl_device_option device_option = get_preferred_device();

  if (cl_debug) {
    describe_device_option(device_option);
  }

  device = device_option.device;
  platform = device_option.platform;

  OPENCL_SUCCEED(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                 sizeof(size_t), &max_group_size, NULL));

  size_t max_tile_size = sqrt(max_group_size);

  if (max_group_size < cl_group_size) {
    fprintf(stderr, "Warning: Device limits group size to %zu (setting was %zu)\n",
            max_group_size, cl_group_size);
    cl_group_size = max_group_size;
  }

  if (max_tile_size < cl_tile_size) {
    fprintf(stderr, "Warning: Device limits tile size to %zu (setting was %zu)\n",
            max_tile_size, cl_tile_size);
    cl_tile_size = max_tile_size;
  }

  cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (cl_context_properties)platform,
    0
  };
  // Note that nVidia's OpenCL requires the platform property
  fut_cl_context = clCreateContext(properties, 1, &device, NULL, NULL, &error);
  assert(error == 0);

  fut_cl_queue = clCreateCommandQueue(fut_cl_context, device, 0, &error);
  assert(error == 0);

  // Make sure this function is defined.
  post_opencl_setup(&device_option);

  char *fut_opencl_src = NULL;
  size_t src_size = 0;

  // Maybe we have to read OpenCL source from somewhere else (used for debugging).
  if (cl_load_program_from) {
    FILE *f = fopen(cl_load_program_from, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    src_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    fut_opencl_src = malloc(src_size);
    fread(fut_opencl_src, 1, src_size, f);
    fclose(f);
  } else {
    // Build the OpenCL program.  First we have to concatenate all the fragments.
    for (const char **src = srcs; *src; src++) {
      src_size += strlen(*src);
    }

    fut_opencl_src = malloc(src_size + 1);

    size_t n, i;
    for (i = 0, n = 0; srcs[i]; i++) {
      strncpy(fut_opencl_src+n, srcs[i], src_size-n);
      n += strlen(srcs[i]);
    }
    fut_opencl_src[src_size] = 0;

  }

  cl_program prog;
  error = 0;
  const char* src_ptr[] = {fut_opencl_src};

  if (cl_dump_program_to) {
    FILE *f = fopen(cl_dump_program_to, "w");
    assert(f != NULL);
    fputs(fut_opencl_src, f);
    fclose(f);
  }

  prog = clCreateProgramWithSource(fut_cl_context, 1, src_ptr, &src_size, &error);
  assert(error == 0);
  char compile_opts[1024];
  snprintf(compile_opts, sizeof(compile_opts), "-DFUT_BLOCK_DIM=%d -DLOCKSTEP_WIDTH=%d -DDEFAULT_GROUP_SIZE=%d -DDEFAULT_NUM_GROUPS=%d  -DDEFAULT_TILE_SIZE=%d", FUT_BLOCK_DIM, cl_lockstep_width, cl_group_size, cl_num_groups, cl_tile_size);
  OPENCL_SUCCEED(build_opencl_program(prog, device, compile_opts));
  free(fut_opencl_src);

  return prog;
}

static const char *fut_opencl_program[] =
                  {"__kernel void dummy_kernel(__global unsigned char *dummy, int n)\n{\n    const int thread_gid = get_global_id(0);\n    \n    if (thread_gid >= n)\n        return;\n}\ntypedef char int8_t;\ntypedef short int16_t;\ntypedef int int32_t;\ntypedef long int64_t;\ntypedef uchar uint8_t;\ntypedef ushort uint16_t;\ntypedef uint uint32_t;\ntypedef ulong uint64_t;\n#define ALIGNED_LOCAL_MEMORY(m,size) __local unsigned char m[size] __attribute__ ((align))\nstatic inline int8_t add8(int8_t x, int8_t y)\n{\n    return x + y;\n}\nstatic inline int16_t add16(int16_t x, int16_t y)\n{\n    return x + y;\n}\nstatic inline int32_t add32(int32_t x, int32_t y)\n{\n    return x + y;\n}\nstatic inline int64_t add64(int64_t x, int64_t y)\n{\n    return x + y;\n}\nstatic inline int8_t sub8(int8_t x, int8_t y)\n{\n    return x - y;\n}\nstatic inline int16_t sub16(int16_t x, int16_t y)\n{\n    return x - y;\n}\nstatic inline int32_t sub32(int32_t x, int32_t y)\n{\n    return x - y;\n}\nstatic inline int64_t sub64(int64_t x, int64_t y)\n{\n    return x - y;\n}\nstatic inline int8_t mul8(int8_t x, int8_t y)\n{\n    return x * y;\n}\nstatic inline int16_t mul16(int16_t x, int16_t y)\n{\n    return x * y;\n}\nstatic inline int32_t mul32(int32_t x, int32_t y)\n{\n    return x * y;\n}\nstatic inline int64_t mul64(int64_t x, int64_t y)\n{\n    return x * y;\n}\nstatic inline uint8_t udiv8(uint8_t x, uint8_t y)\n{\n    return x / y;\n}\nstatic inline uint16_t udiv16(uint16_t x, uint16_t y)\n{\n    return x / y;\n}\nstatic inline uint32_t udiv32(uint32_t x, uint32_t y)\n{\n    return x / y;\n}\nstatic inline uint64_t udiv64(uint64_t x, uint64_t y)\n{\n    return x / y;\n}\nstatic inline uint8_t umod8(uint8_t x, uint8_t y)\n{\n    return x % y;\n}\nstatic inline uint16_t umod16(uint16_t x, uint16_t y)\n{\n    return x % y;\n}\nstatic inline uint32_t umod32(uint32_t x, uint32_t y)\n{\n    return x % y;\n}\nstatic inline uint64_t umod64(uint64_t x, uint64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t sdiv8(int8_t x, int8_t y)\n{\n    int8_t q = x / y;\n    int8_t r = x % y;\n    \n    return q - ((",
                   "r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int16_t sdiv16(int16_t x, int16_t y)\n{\n    int16_t q = x / y;\n    int16_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int32_t sdiv32(int32_t x, int32_t y)\n{\n    int32_t q = x / y;\n    int32_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int64_t sdiv64(int64_t x, int64_t y)\n{\n    int64_t q = x / y;\n    int64_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int8_t smod8(int8_t x, int8_t y)\n{\n    int8_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int16_t smod16(int16_t x, int16_t y)\n{\n    int16_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int32_t smod32(int32_t x, int32_t y)\n{\n    int32_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int64_t smod64(int64_t x, int64_t y)\n{\n    int64_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int8_t squot8(int8_t x, int8_t y)\n{\n    return x / y;\n}\nstatic inline int16_t squot16(int16_t x, int16_t y)\n{\n    return x / y;\n}\nstatic inline int32_t squot32(int32_t x, int32_t y)\n{\n    return x / y;\n}\nstatic inline int64_t squot64(int64_t x, int64_t y)\n{\n    return x / y;\n}\nstatic inline int8_t srem8(int8_t x, int8_t y)\n{\n    return x % y;\n}\nstatic inline int16_t srem16(int16_t x, int16_t y)\n{\n    return x % y;\n}\nstatic inline int32_t srem32(int32_t x, int32_t y)\n{\n    return x % y;\n}\nstatic inline int64_t srem64(int64_t x, int64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t smin8(int8_t x, int8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int16_t smin16(int16_t x, int16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int32_t smin32(int32_t x, int32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int64_t smin64(int64_t x, int64_t",
                   " y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint8_t umin8(uint8_t x, uint8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint16_t umin16(uint16_t x, uint16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint32_t umin32(uint32_t x, uint32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint64_t umin64(uint64_t x, uint64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int8_t smax8(int8_t x, int8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int16_t smax16(int16_t x, int16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int32_t smax32(int32_t x, int32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int64_t smax64(int64_t x, int64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t umax8(uint8_t x, uint8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint16_t umax16(uint16_t x, uint16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint32_t umax32(uint32_t x, uint32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint64_t umax64(uint64_t x, uint64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t shl8(uint8_t x, uint8_t y)\n{\n    return x << y;\n}\nstatic inline uint16_t shl16(uint16_t x, uint16_t y)\n{\n    return x << y;\n}\nstatic inline uint32_t shl32(uint32_t x, uint32_t y)\n{\n    return x << y;\n}\nstatic inline uint64_t shl64(uint64_t x, uint64_t y)\n{\n    return x << y;\n}\nstatic inline uint8_t lshr8(uint8_t x, uint8_t y)\n{\n    return x >> y;\n}\nstatic inline uint16_t lshr16(uint16_t x, uint16_t y)\n{\n    return x >> y;\n}\nstatic inline uint32_t lshr32(uint32_t x, uint32_t y)\n{\n    return x >> y;\n}\nstatic inline uint64_t lshr64(uint64_t x, uint64_t y)\n{\n    return x >> y;\n}\nstatic inline int8_t ashr8(int8_t x, int8_t y)\n{\n    return x >> y;\n}\nstatic inline int16_t ashr16(int16_t x, int16_t y)\n{\n    return x >> y;\n}\nstatic inline int32_t ashr32(int32_t x, int32_t y)\n{\n    return x >> y;\n}\nstatic inline int64_t ashr64(int64_t x, int64_t y)\n{\n    return x >> y;\n}\nstatic inline uint8_t and8(uint8_t x, uint8_t y)\n{\n    return x & y;\n}\nstatic inline u",
                   "int16_t and16(uint16_t x, uint16_t y)\n{\n    return x & y;\n}\nstatic inline uint32_t and32(uint32_t x, uint32_t y)\n{\n    return x & y;\n}\nstatic inline uint64_t and64(uint64_t x, uint64_t y)\n{\n    return x & y;\n}\nstatic inline uint8_t or8(uint8_t x, uint8_t y)\n{\n    return x | y;\n}\nstatic inline uint16_t or16(uint16_t x, uint16_t y)\n{\n    return x | y;\n}\nstatic inline uint32_t or32(uint32_t x, uint32_t y)\n{\n    return x | y;\n}\nstatic inline uint64_t or64(uint64_t x, uint64_t y)\n{\n    return x | y;\n}\nstatic inline uint8_t xor8(uint8_t x, uint8_t y)\n{\n    return x ^ y;\n}\nstatic inline uint16_t xor16(uint16_t x, uint16_t y)\n{\n    return x ^ y;\n}\nstatic inline uint32_t xor32(uint32_t x, uint32_t y)\n{\n    return x ^ y;\n}\nstatic inline uint64_t xor64(uint64_t x, uint64_t y)\n{\n    return x ^ y;\n}\nstatic inline char ult8(uint8_t x, uint8_t y)\n{\n    return x < y;\n}\nstatic inline char ult16(uint16_t x, uint16_t y)\n{\n    return x < y;\n}\nstatic inline char ult32(uint32_t x, uint32_t y)\n{\n    return x < y;\n}\nstatic inline char ult64(uint64_t x, uint64_t y)\n{\n    return x < y;\n}\nstatic inline char ule8(uint8_t x, uint8_t y)\n{\n    return x <= y;\n}\nstatic inline char ule16(uint16_t x, uint16_t y)\n{\n    return x <= y;\n}\nstatic inline char ule32(uint32_t x, uint32_t y)\n{\n    return x <= y;\n}\nstatic inline char ule64(uint64_t x, uint64_t y)\n{\n    return x <= y;\n}\nstatic inline char slt8(int8_t x, int8_t y)\n{\n    return x < y;\n}\nstatic inline char slt16(int16_t x, int16_t y)\n{\n    return x < y;\n}\nstatic inline char slt32(int32_t x, int32_t y)\n{\n    return x < y;\n}\nstatic inline char slt64(int64_t x, int64_t y)\n{\n    return x < y;\n}\nstatic inline char sle8(int8_t x, int8_t y)\n{\n    return x <= y;\n}\nstatic inline char sle16(int16_t x, int16_t y)\n{\n    return x <= y;\n}\nstatic inline char sle32(int32_t x, int32_t y)\n{\n    return x <= y;\n}\nstatic inline char sle64(int64_t x, int64_t y)\n{\n    return x <= y;\n}\nstatic inline int8_t pow8(int8_t x, int8_t y)\n{\n    int8_t res = 1, rem = y;\n    \n    ",
                   "while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int16_t pow16(int16_t x, int16_t y)\n{\n    int16_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int32_t pow32(int32_t x, int32_t y)\n{\n    int32_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int64_t pow64(int64_t x, int64_t y)\n{\n    int64_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int8_t sext_i8_i8(int8_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i8_i16(int8_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i8_i32(int8_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i8_i64(int8_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i16_i8(int16_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i16_i16(int16_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i16_i32(int16_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i16_i64(int16_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i32_i8(int32_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i32_i16(int32_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i32_i32(int32_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i32_i64(int32_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i64_i8(int64_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i64_i16(int64_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i64_i32(int64_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i64_i64(int64_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i8_i8(uint8_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i8_i16(uint8_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i8_i32(uint8_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i8_i64(uint8_t x)",
                   "\n{\n    return x;\n}\nstatic inline uint8_t zext_i16_i8(uint16_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i16_i16(uint16_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i16_i32(uint16_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i16_i64(uint16_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i32_i8(uint32_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i32_i16(uint32_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i32_i32(uint32_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i32_i64(uint32_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i64_i8(uint64_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i64_i16(uint64_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i64_i32(uint64_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i64_i64(uint64_t x)\n{\n    return x;\n}\nstatic inline float fdiv32(float x, float y)\n{\n    return x / y;\n}\nstatic inline float fadd32(float x, float y)\n{\n    return x + y;\n}\nstatic inline float fsub32(float x, float y)\n{\n    return x - y;\n}\nstatic inline float fmul32(float x, float y)\n{\n    return x * y;\n}\nstatic inline float fmin32(float x, float y)\n{\n    return x < y ? x : y;\n}\nstatic inline float fmax32(float x, float y)\n{\n    return x < y ? y : x;\n}\nstatic inline float fpow32(float x, float y)\n{\n    return pow(x, y);\n}\nstatic inline char cmplt32(float x, float y)\n{\n    return x < y;\n}\nstatic inline char cmple32(float x, float y)\n{\n    return x <= y;\n}\nstatic inline float sitofp_i8_f32(int8_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i16_f32(int16_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i32_f32(int32_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i64_f32(int64_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i8_f32(uint8_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i16_f32(uint16_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i32_f32(uint32_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i64_f32(uint64_t x)\n{\n    return x;\n}\nstatic inline int8_t fptosi_f32_i8(float x)\n{\n    return x;",
                   "\n}\nstatic inline int16_t fptosi_f32_i16(float x)\n{\n    return x;\n}\nstatic inline int32_t fptosi_f32_i32(float x)\n{\n    return x;\n}\nstatic inline int64_t fptosi_f32_i64(float x)\n{\n    return x;\n}\nstatic inline uint8_t fptoui_f32_i8(float x)\n{\n    return x;\n}\nstatic inline uint16_t fptoui_f32_i16(float x)\n{\n    return x;\n}\nstatic inline uint32_t fptoui_f32_i32(float x)\n{\n    return x;\n}\nstatic inline uint64_t fptoui_f32_i64(float x)\n{\n    return x;\n}\n#define group_sizze_2351 (DEFAULT_GROUP_SIZE)\n#define y_2353 (DEFAULT_GROUP_SIZE - 1)\n__kernel void map_kernel_2479(int32_t conc_tmp_2310, int32_t y_2401, __global\n                              unsigned char *mem_2496, __global\n                              unsigned char *mem_2499, __global\n                              unsigned char *mem_2520, __global\n                              unsigned char *mem_2523, __global\n                              unsigned char *mem_2526, __global\n                              unsigned char *mem_2529)\n{\n    int32_t wave_sizze_2570;\n    int32_t group_sizze_2571;\n    char thread_active_2572;\n    int32_t j_2462;\n    int32_t global_tid_2479;\n    int32_t local_tid_2480;\n    int32_t group_id_2481;\n    \n    global_tid_2479 = get_global_id(0);\n    local_tid_2480 = get_local_id(0);\n    group_sizze_2571 = get_local_size(0);\n    wave_sizze_2570 = LOCKSTEP_WIDTH;\n    group_id_2481 = get_group_id(0);\n    j_2462 = global_tid_2479;\n    thread_active_2572 = slt32(j_2462, conc_tmp_2310);\n    \n    int32_t y_flag_2456;\n    int32_t y_2457;\n    int32_t group_id_2467;\n    char cond_2468;\n    int32_t final_result_2471;\n    int32_t final_result_2472;\n    \n    if (thread_active_2572) {\n        y_flag_2456 = *(__global int32_t *) &mem_2496[j_2462 * 4];\n        y_2457 = *(__global int32_t *) &mem_2499[j_2462 * 4];\n        group_id_2467 = squot32(j_2462, y_2401);\n        cond_2468 = 0 == group_id_2467;\n        if (cond_2468) {\n            final_result_2471 = y_flag_2456;\n            final_result_2472 = y_2457;\n       ",
                   " } else {\n            int32_t carry_in_index_2469 = group_id_2467 - 1;\n            int32_t x_flag_2454 = *(__global\n                                    int32_t *) &mem_2520[carry_in_index_2469 *\n                                                         4];\n            int32_t x_2455 = *(__global\n                               int32_t *) &mem_2523[carry_in_index_2469 * 4];\n            char cond_2458 = slt32(0, y_flag_2456);\n            int32_t res_2459;\n            int32_t res_2460;\n            \n            if (cond_2458) {\n                res_2459 = y_flag_2456;\n                res_2460 = 0;\n            } else {\n                int32_t res_2461 = x_2455 + y_2457;\n                \n                res_2459 = x_flag_2454;\n                res_2460 = res_2461;\n            }\n            final_result_2471 = res_2459;\n            final_result_2472 = res_2460;\n        }\n    }\n    if (thread_active_2572) {\n        *(__global int32_t *) &mem_2526[j_2462 * 4] = final_result_2471;\n    }\n    if (thread_active_2572) {\n        *(__global int32_t *) &mem_2529[j_2462 * 4] = final_result_2472;\n    }\n}\n__kernel void scan1_kernel_2392(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                int32_t conc_tmp_2310,\n                                int32_t num_iterations_2397, int32_t y_2401,\n                                __global unsigned char *flags_mem_2491, __global\n                                unsigned char *vals_mem_2493, __global\n                                unsigned char *mem_2496, __global\n                                unsigned char *mem_2499, __global\n                                unsigned char *mem_2508, __global\n                                unsigned char *mem_2511)\n{\n    __local volatile char *restrict mem_2502 = mem_aligned_0;\n    __local volatile char *restrict mem_2505 = mem_aligned_1;\n    int32_t wave_sizze_2536;\n    int32_t group_sizze_2537;\n    char thread_active_2538;\n  ",
                   "  int32_t global_tid_2392;\n    int32_t local_tid_2393;\n    int32_t group_id_2394;\n    \n    global_tid_2392 = get_global_id(0);\n    local_tid_2393 = get_local_id(0);\n    group_sizze_2537 = get_local_size(0);\n    wave_sizze_2536 = LOCKSTEP_WIDTH;\n    group_id_2394 = get_group_id(0);\n    thread_active_2538 = 1;\n    \n    int32_t x_2402;\n    char is_first_thread_2422;\n    int32_t result_2429;\n    int32_t result_2430;\n    \n    if (thread_active_2538) {\n        x_2402 = group_id_2394 * y_2401;\n        is_first_thread_2422 = local_tid_2393 == 0;\n        \n        int32_t x_flag_merge_2398;\n        int32_t x_merge_2399;\n        \n        x_flag_merge_2398 = 0;\n        x_merge_2399 = 0;\n        for (int32_t i_2400 = 0; i_2400 < num_iterations_2397; i_2400++) {\n            int32_t y_2403 = i_2400 * group_sizze_2351;\n            int32_t offset_2404 = x_2402 + y_2403;\n            int32_t j_2405 = offset_2404 + local_tid_2393;\n            char cond_2406 = slt32(j_2405, conc_tmp_2310);\n            int32_t foldres_2409;\n            int32_t foldres_2410;\n            \n            if (cond_2406) {\n                int32_t flags_elem_2407 = *(__global\n                                            int32_t *) &flags_mem_2491[j_2405 *\n                                                                       4];\n                char index_concat_cmp_2483 = sle32(1, j_2405);\n                int32_t index_concat_branch_2487;\n                \n                if (index_concat_cmp_2483) {\n                    int32_t index_concat_i_2484 = j_2405 - 1;\n                    int32_t index_concat_2485 = *(__global\n                                                  int32_t *) &vals_mem_2493[index_concat_i_2484 *\n                                                                            4];\n                    \n                    index_concat_branch_2487 = index_concat_2485;\n                } else {\n                    index_concat_branch_2487 = 0;\n                }\n                \n                char cond_2",
                   "369 = slt32(0, flags_elem_2407);\n                int32_t res_2370;\n                int32_t res_2371;\n                \n                if (cond_2369) {\n                    res_2370 = flags_elem_2407;\n                    res_2371 = 0;\n                } else {\n                    int32_t res_2372 = x_merge_2399 + index_concat_branch_2487;\n                    \n                    res_2370 = x_flag_merge_2398;\n                    res_2371 = res_2372;\n                }\n                foldres_2409 = res_2370;\n                foldres_2410 = res_2371;\n            } else {\n                foldres_2409 = x_flag_merge_2398;\n                foldres_2410 = x_merge_2399;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            if (slt32(local_tid_2393, group_sizze_2351) && 1) {\n                *(__local int32_t *) &mem_2502[local_tid_2393 * 4] =\n                    foldres_2409;\n                *(__local int32_t *) &mem_2505[local_tid_2393 * 4] =\n                    foldres_2410;\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            \n            int32_t my_index_2373;\n            int32_t other_index_2374;\n            int32_t x_flag_2375;\n            int32_t x_2376;\n            int32_t y_flag_2377;\n            int32_t y_2378;\n            int32_t my_index_2543;\n            int32_t other_index_2544;\n            int32_t x_flag_2545;\n            int32_t x_2546;\n            int32_t y_flag_2547;\n            int32_t y_2548;\n            \n            my_index_2373 = local_tid_2393;\n            if (slt32(local_tid_2393, group_sizze_2351)) {\n                y_flag_2377 = *(volatile __local\n                                int32_t *) &mem_2502[local_tid_2393 *\n                                                     sizeof(int32_t)];\n                y_2378 = *(volatile __local\n                           int32_t *) &mem_2505[local_tid_2393 *\n                                                sizeof(int32_t)];\n            }\n            // in-block scan (hopefully no barriers ne",
                   "eded)\n            {\n                int32_t skip_threads_2553 = 1;\n                \n                while (slt32(skip_threads_2553, 32)) {\n                    if (slt32(local_tid_2393, group_sizze_2351) &&\n                        sle32(skip_threads_2553, local_tid_2393 -\n                              squot32(local_tid_2393, 32) * 32)) {\n                        // read operands\n                        {\n                            x_flag_2375 = *(volatile __local\n                                            int32_t *) &mem_2502[(local_tid_2393 -\n                                                                  skip_threads_2553) *\n                                                                 sizeof(int32_t)];\n                            x_2376 = *(volatile __local\n                                       int32_t *) &mem_2505[(local_tid_2393 -\n                                                             skip_threads_2553) *\n                                                            sizeof(int32_t)];\n                        }\n                        // perform operation\n                        {\n                            char cond_2379 = slt32(0, y_flag_2377);\n                            int32_t res_2380;\n                            int32_t res_2381;\n                            \n                            if (cond_2379) {\n                                res_2380 = y_flag_2377;\n                                res_2381 = 0;\n                            } else {\n                                int32_t res_2382 = x_2376 + y_2378;\n                                \n                                res_2380 = x_flag_2375;\n                                res_2381 = res_2382;\n                            }\n                            y_flag_2377 = res_2380;\n                            y_2378 = res_2381;\n                        }\n                    }\n                    if (sle32(wave_sizze_2536, skip_threads_2553)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n      ",
                   "              }\n                    if (slt32(local_tid_2393, group_sizze_2351) &&\n                        sle32(skip_threads_2553, local_tid_2393 -\n                              squot32(local_tid_2393, 32) * 32)) {\n                        // write result\n                        {\n                            *(volatile __local\n                              int32_t *) &mem_2502[local_tid_2393 *\n                                                   sizeof(int32_t)] =\n                                y_flag_2377;\n                            *(volatile __local\n                              int32_t *) &mem_2505[local_tid_2393 *\n                                                   sizeof(int32_t)] = y_2378;\n                        }\n                    }\n                    if (sle32(wave_sizze_2536, skip_threads_2553)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_2553 *= 2;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // last thread of block 'i' writes its result to offset 'i'\n            {\n                if ((local_tid_2393 - squot32(local_tid_2393, 32) * 32) == 31 &&\n                    slt32(local_tid_2393, group_sizze_2351)) {\n                    *(volatile __local\n                      int32_t *) &mem_2502[squot32(local_tid_2393, 32) *\n                                           sizeof(int32_t)] = y_flag_2377;\n                    *(volatile __local\n                      int32_t *) &mem_2505[squot32(local_tid_2393, 32) *\n                                           sizeof(int32_t)] = y_2378;\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n            {\n                if (squot32(local_tid_2393, 32) == 0 && slt32(local_tid_2393,\n                                                              group_sizze_2351)) {\n                    y_flag_2547 = *(volatil",
                   "e __local\n                                    int32_t *) &mem_2502[local_tid_2393 *\n                                                         sizeof(int32_t)];\n                    y_2548 = *(volatile __local\n                               int32_t *) &mem_2505[local_tid_2393 *\n                                                    sizeof(int32_t)];\n                }\n                // in-block scan (hopefully no barriers needed)\n                {\n                    int32_t skip_threads_2554 = 1;\n                    \n                    while (slt32(skip_threads_2554, 32)) {\n                        if ((squot32(local_tid_2393, 32) == 0 &&\n                             slt32(local_tid_2393, group_sizze_2351)) &&\n                            sle32(skip_threads_2554, local_tid_2393 -\n                                  squot32(local_tid_2393, 32) * 32)) {\n                            // read operands\n                            {\n                                x_flag_2545 = *(volatile __local\n                                                int32_t *) &mem_2502[(local_tid_2393 -\n                                                                      skip_threads_2554) *\n                                                                     sizeof(int32_t)];\n                                x_2546 = *(volatile __local\n                                           int32_t *) &mem_2505[(local_tid_2393 -\n                                                                 skip_threads_2554) *\n                                                                sizeof(int32_t)];\n                            }\n                            // perform operation\n                            {\n                                char cond_2549 = slt32(0, y_flag_2547);\n                                int32_t res_2550;\n                                int32_t res_2551;\n                                \n                                if (cond_2549) {\n                                    res_2550 = y_flag_2547;\n      ",
                   "                              res_2551 = 0;\n                                } else {\n                                    int32_t res_2552 = x_2546 + y_2548;\n                                    \n                                    res_2550 = x_flag_2545;\n                                    res_2551 = res_2552;\n                                }\n                                y_flag_2547 = res_2550;\n                                y_2548 = res_2551;\n                            }\n                        }\n                        if (sle32(wave_sizze_2536, skip_threads_2554)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        if ((squot32(local_tid_2393, 32) == 0 &&\n                             slt32(local_tid_2393, group_sizze_2351)) &&\n                            sle32(skip_threads_2554, local_tid_2393 -\n                                  squot32(local_tid_2393, 32) * 32)) {\n                            // write result\n                            {\n                                *(volatile __local\n                                  int32_t *) &mem_2502[local_tid_2393 *\n                                                       sizeof(int32_t)] =\n                                    y_flag_2547;\n                                *(volatile __local\n                                  int32_t *) &mem_2505[local_tid_2393 *\n                                                       sizeof(int32_t)] =\n                                    y_2548;\n                            }\n                        }\n                        if (sle32(wave_sizze_2536, skip_threads_2554)) {\n                            barrier(CLK_LOCAL_MEM_FENCE);\n                        }\n                        skip_threads_2554 *= 2;\n                    }\n                }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // carry-in for every block except the first\n            {\n                if (!(squot32(local_tid_2393, 32) == 0 || !slt32(lo",
                   "cal_tid_2393,\n                                                                 group_sizze_2351))) {\n                    // read operands\n                    {\n                        x_flag_2375 = *(volatile __local\n                                        int32_t *) &mem_2502[(squot32(local_tid_2393,\n                                                                      32) - 1) *\n                                                             sizeof(int32_t)];\n                        x_2376 = *(volatile __local\n                                   int32_t *) &mem_2505[(squot32(local_tid_2393,\n                                                                 32) - 1) *\n                                                        sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n                        char cond_2379 = slt32(0, y_flag_2377);\n                        int32_t res_2380;\n                        int32_t res_2381;\n                        \n                        if (cond_2379) {\n                            res_2380 = y_flag_2377;\n                            res_2381 = 0;\n                        } else {\n                            int32_t res_2382 = x_2376 + y_2378;\n                            \n                            res_2380 = x_flag_2375;\n                            res_2381 = res_2382;\n                        }\n                        y_flag_2377 = res_2380;\n                        y_2378 = res_2381;\n                    }\n                    // write final result\n                    {\n                        *(volatile __local int32_t *) &mem_2502[local_tid_2393 *\n                                                                sizeof(int32_t)] =\n                            y_flag_2377;\n                        *(volatile __local int32_t *) &mem_2505[local_tid_2393 *\n                                                                sizeof(int32_t)] =\n                            y_2378;\n                    }\n    ",
                   "            }\n            }\n            barrier(CLK_LOCAL_MEM_FENCE);\n            // restore correct values for first block\n            {\n                if (squot32(local_tid_2393, 32) == 0) {\n                    *(volatile __local int32_t *) &mem_2502[local_tid_2393 *\n                                                            sizeof(int32_t)] =\n                        y_flag_2377;\n                    *(volatile __local int32_t *) &mem_2505[local_tid_2393 *\n                                                            sizeof(int32_t)] =\n                        y_2378;\n                }\n            }\n            if (cond_2406) {\n                int32_t scanned_elem_2416 = *(__local\n                                              int32_t *) &mem_2502[local_tid_2393 *\n                                                                   4];\n                int32_t scanned_elem_2417 = *(__local\n                                              int32_t *) &mem_2505[local_tid_2393 *\n                                                                   4];\n                \n                *(__global int32_t *) &mem_2496[j_2405 * 4] = scanned_elem_2416;\n                *(__global int32_t *) &mem_2499[j_2405 * 4] = scanned_elem_2417;\n            }\n            \n            int32_t new_carry_2425;\n            int32_t new_carry_2426;\n            \n            if (is_first_thread_2422) {\n                int32_t carry_2423 = *(__local int32_t *) &mem_2502[y_2353 * 4];\n                int32_t carry_2424 = *(__local int32_t *) &mem_2505[y_2353 * 4];\n                \n                new_carry_2425 = carry_2423;\n                new_carry_2426 = carry_2424;\n            } else {\n                new_carry_2425 = 0;\n                new_carry_2426 = 0;\n            }\n            \n            int32_t x_flag_merge_tmp_2541 = new_carry_2425;\n            int32_t x_merge_tmp_2542;\n            \n            x_merge_tmp_2542 = new_carry_2426;\n            x_flag_merge_2398 = x_flag_merge_tmp_2541;\n            x",
                   "_merge_2399 = x_merge_tmp_2542;\n        }\n        result_2429 = x_flag_merge_2398;\n        result_2430 = x_merge_2399;\n    }\n    if (local_tid_2393 == 0) {\n        *(__global int32_t *) &mem_2508[group_id_2394 * 4] = result_2429;\n    }\n    if (local_tid_2393 == 0) {\n        *(__global int32_t *) &mem_2511[group_id_2394 * 4] = result_2430;\n    }\n}\n__kernel void scan2_kernel_2441(__local volatile int64_t *mem_aligned_0,\n                                __local volatile int64_t *mem_aligned_1,\n                                int32_t num_groups_2357, __global\n                                unsigned char *mem_2508, __global\n                                unsigned char *mem_2511, __global\n                                unsigned char *mem_2520, __global\n                                unsigned char *mem_2523)\n{\n    __local volatile char *restrict mem_2514 = mem_aligned_0;\n    __local volatile char *restrict mem_2517 = mem_aligned_1;\n    int32_t wave_sizze_2555;\n    int32_t group_sizze_2556;\n    char thread_active_2557;\n    int32_t global_tid_2441;\n    int32_t local_tid_2442;\n    int32_t group_id_2443;\n    \n    global_tid_2441 = get_global_id(0);\n    local_tid_2442 = get_local_id(0);\n    group_sizze_2556 = get_local_size(0);\n    wave_sizze_2555 = LOCKSTEP_WIDTH;\n    group_id_2443 = get_group_id(0);\n    thread_active_2557 = 1;\n    barrier(CLK_LOCAL_MEM_FENCE);\n    if (slt32(local_tid_2442, num_groups_2357) && 1) {\n        int32_t res_group_sums_elem_2444 = *(__global\n                                             int32_t *) &mem_2508[local_tid_2442 *\n                                                                  4];\n        int32_t res_group_sums_elem_2445 = *(__global\n                                             int32_t *) &mem_2511[local_tid_2442 *\n                                                                  4];\n        \n        *(__local int32_t *) &mem_2514[local_tid_2442 * 4] =\n            res_group_sums_elem_2444;\n        *(__local int32_t *) &mem_2517[local_ti",
                   "d_2442 * 4] =\n            res_group_sums_elem_2445;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t my_index_2431;\n    int32_t other_index_2432;\n    int32_t x_flag_2433;\n    int32_t x_2434;\n    int32_t y_flag_2435;\n    int32_t y_2436;\n    int32_t my_index_2558;\n    int32_t other_index_2559;\n    int32_t x_flag_2560;\n    int32_t x_2561;\n    int32_t y_flag_2562;\n    int32_t y_2563;\n    \n    my_index_2431 = local_tid_2442;\n    if (slt32(local_tid_2442, num_groups_2357)) {\n        y_flag_2435 = *(volatile __local int32_t *) &mem_2514[local_tid_2442 *\n                                                              sizeof(int32_t)];\n        y_2436 = *(volatile __local int32_t *) &mem_2517[local_tid_2442 *\n                                                         sizeof(int32_t)];\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        int32_t skip_threads_2568 = 1;\n        \n        while (slt32(skip_threads_2568, 32)) {\n            if (slt32(local_tid_2442, num_groups_2357) &&\n                sle32(skip_threads_2568, local_tid_2442 -\n                      squot32(local_tid_2442, 32) * 32)) {\n                // read operands\n                {\n                    x_flag_2433 = *(volatile __local\n                                    int32_t *) &mem_2514[(local_tid_2442 -\n                                                          skip_threads_2568) *\n                                                         sizeof(int32_t)];\n                    x_2434 = *(volatile __local\n                               int32_t *) &mem_2517[(local_tid_2442 -\n                                                     skip_threads_2568) *\n                                                    sizeof(int32_t)];\n                }\n                // perform operation\n                {\n                    char cond_2437;\n                    int32_t res_2438;\n                    int32_t res_2439;\n                    \n                    if (thread_active_2557) {\n                        cond_",
                   "2437 = slt32(0, y_flag_2435);\n                        if (cond_2437) {\n                            res_2438 = y_flag_2435;\n                            res_2439 = 0;\n                        } else {\n                            int32_t res_2440 = x_2434 + y_2436;\n                            \n                            res_2438 = x_flag_2433;\n                            res_2439 = res_2440;\n                        }\n                    }\n                    y_flag_2435 = res_2438;\n                    y_2436 = res_2439;\n                }\n            }\n            if (sle32(wave_sizze_2555, skip_threads_2568)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (slt32(local_tid_2442, num_groups_2357) &&\n                sle32(skip_threads_2568, local_tid_2442 -\n                      squot32(local_tid_2442, 32) * 32)) {\n                // write result\n                {\n                    *(volatile __local int32_t *) &mem_2514[local_tid_2442 *\n                                                            sizeof(int32_t)] =\n                        y_flag_2435;\n                    *(volatile __local int32_t *) &mem_2517[local_tid_2442 *\n                                                            sizeof(int32_t)] =\n                        y_2436;\n                }\n            }\n            if (sle32(wave_sizze_2555, skip_threads_2568)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_2568 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if ((local_tid_2442 - squot32(local_tid_2442, 32) * 32) == 31 &&\n            slt32(local_tid_2442, num_groups_2357)) {\n            *(volatile __local int32_t *) &mem_2514[squot32(local_tid_2442,\n                                                            32) *\n                                                    sizeof(int32_t)] =\n                y_flag_2435;\n            *(volatile __local int32_t *",
                   ") &mem_2517[squot32(local_tid_2442,\n                                                            32) *\n                                                    sizeof(int32_t)] = y_2436;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for warp 'i+1'\n    {\n        if (squot32(local_tid_2442, 32) == 0 && slt32(local_tid_2442,\n                                                      num_groups_2357)) {\n            y_flag_2562 = *(volatile __local\n                            int32_t *) &mem_2514[local_tid_2442 *\n                                                 sizeof(int32_t)];\n            y_2563 = *(volatile __local int32_t *) &mem_2517[local_tid_2442 *\n                                                             sizeof(int32_t)];\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            int32_t skip_threads_2569 = 1;\n            \n            while (slt32(skip_threads_2569, 32)) {\n                if ((squot32(local_tid_2442, 32) == 0 && slt32(local_tid_2442,\n                                                               num_groups_2357)) &&\n                    sle32(skip_threads_2569, local_tid_2442 -\n                          squot32(local_tid_2442, 32) * 32)) {\n                    // read operands\n                    {\n                        x_flag_2560 = *(volatile __local\n                                        int32_t *) &mem_2514[(local_tid_2442 -\n                                                              skip_threads_2569) *\n                                                             sizeof(int32_t)];\n                        x_2561 = *(volatile __local\n                                   int32_t *) &mem_2517[(local_tid_2442 -\n                                                         skip_threads_2569) *\n                                                        sizeof(int32_t)];\n                    }\n                    // perform operation\n                    {\n        ",
                   "                char cond_2564;\n                        int32_t res_2565;\n                        int32_t res_2566;\n                        \n                        if (thread_active_2557) {\n                            cond_2564 = slt32(0, y_flag_2562);\n                            if (cond_2564) {\n                                res_2565 = y_flag_2562;\n                                res_2566 = 0;\n                            } else {\n                                int32_t res_2567 = x_2561 + y_2563;\n                                \n                                res_2565 = x_flag_2560;\n                                res_2566 = res_2567;\n                            }\n                        }\n                        y_flag_2562 = res_2565;\n                        y_2563 = res_2566;\n                    }\n                }\n                if (sle32(wave_sizze_2555, skip_threads_2569)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if ((squot32(local_tid_2442, 32) == 0 && slt32(local_tid_2442,\n                                                               num_groups_2357)) &&\n                    sle32(skip_threads_2569, local_tid_2442 -\n                          squot32(local_tid_2442, 32) * 32)) {\n                    // write result\n                    {\n                        *(volatile __local int32_t *) &mem_2514[local_tid_2442 *\n                                                                sizeof(int32_t)] =\n                            y_flag_2562;\n                        *(volatile __local int32_t *) &mem_2517[local_tid_2442 *\n                                                                sizeof(int32_t)] =\n                            y_2563;\n                    }\n                }\n                if (sle32(wave_sizze_2555, skip_threads_2569)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_2569 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    ",
                   "// carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_2442, 32) == 0 || !slt32(local_tid_2442,\n                                                         num_groups_2357))) {\n            // read operands\n            {\n                x_flag_2433 = *(volatile __local\n                                int32_t *) &mem_2514[(squot32(local_tid_2442,\n                                                              32) - 1) *\n                                                     sizeof(int32_t)];\n                x_2434 = *(volatile __local\n                           int32_t *) &mem_2517[(squot32(local_tid_2442, 32) -\n                                                 1) * sizeof(int32_t)];\n            }\n            // perform operation\n            {\n                char cond_2437;\n                int32_t res_2438;\n                int32_t res_2439;\n                \n                if (thread_active_2557) {\n                    cond_2437 = slt32(0, y_flag_2435);\n                    if (cond_2437) {\n                        res_2438 = y_flag_2435;\n                        res_2439 = 0;\n                    } else {\n                        int32_t res_2440 = x_2434 + y_2436;\n                        \n                        res_2438 = x_flag_2433;\n                        res_2439 = res_2440;\n                    }\n                }\n                y_flag_2435 = res_2438;\n                y_2436 = res_2439;\n            }\n            // write final result\n            {\n                *(volatile __local int32_t *) &mem_2514[local_tid_2442 *\n                                                        sizeof(int32_t)] =\n                    y_flag_2435;\n                *(volatile __local int32_t *) &mem_2517[local_tid_2442 *\n                                                        sizeof(int32_t)] =\n                    y_2436;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_2442",
                   ", 32) == 0) {\n            *(volatile __local int32_t *) &mem_2514[local_tid_2442 *\n                                                    sizeof(int32_t)] =\n                y_flag_2435;\n            *(volatile __local int32_t *) &mem_2517[local_tid_2442 *\n                                                    sizeof(int32_t)] = y_2436;\n        }\n    }\n    \n    int32_t scanned_elem_2450;\n    int32_t scanned_elem_2451;\n    \n    if (thread_active_2557) {\n        scanned_elem_2450 = *(__local int32_t *) &mem_2514[local_tid_2442 * 4];\n        scanned_elem_2451 = *(__local int32_t *) &mem_2517[local_tid_2442 * 4];\n    }\n    *(__global int32_t *) &mem_2520[global_tid_2441 * 4] = scanned_elem_2450;\n    *(__global int32_t *) &mem_2523[global_tid_2441 * 4] = scanned_elem_2451;\n}\n",
                   NULL};
static cl_kernel map_kernel_2479;
static int map_kernel_2479total_runtime = 0;
static int map_kernel_2479runs = 0;
static cl_kernel scan1_kernel_2392;
static int scan1_kernel_2392total_runtime = 0;
static int scan1_kernel_2392runs = 0;
static cl_kernel scan2_kernel_2441;
static int scan2_kernel_2441total_runtime = 0;
static int scan2_kernel_2441runs = 0;
void setup_opencl_and_load_kernels()
{
    cl_int error;
    cl_program prog = setup_opencl(fut_opencl_program);
    
    {
        map_kernel_2479 = clCreateKernel(prog, "map_kernel_2479", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "map_kernel_2479");
    }
    {
        scan1_kernel_2392 = clCreateKernel(prog, "scan1_kernel_2392", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan1_kernel_2392");
    }
    {
        scan2_kernel_2441 = clCreateKernel(prog, "scan2_kernel_2441", &error);
        assert(error == 0);
        if (debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan2_kernel_2441");
    }
}
void post_opencl_setup(struct opencl_device_option *option)
{
    if (strcmp(option->platform_name, "NVIDIA CUDA") == 0 &&
        option->device_type == CL_DEVICE_TYPE_GPU) {
        cl_lockstep_width = 32;
        if (debugging)
            fprintf(stderr, "Setting lockstep width to: %d\n",
                    cl_lockstep_width);
    }
    if (strcmp(option->platform_name, "AMD Accelerated Parallel Processing") ==
        0 && option->device_type == CL_DEVICE_TYPE_GPU) {
        cl_lockstep_width = 64;
        if (debugging)
            fprintf(stderr, "Setting lockstep width to: %d\n",
                    cl_lockstep_width);
    }
}
int64_t peak_mem_usage_device = 0;
int64_t cur_mem_usage_device = 0;
struct memblock_device {
    int *references;
    cl_mem mem;
    int64_t size;
} ;
static void memblock_unref_device(struct memblock_device *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (detail_memory)
            fprintf(stderr,
                    "Unreferencing block in space 'device': %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            cur_mem_usage_device -= block->size;
            OPENCL_SUCCEED(clReleaseMemObject(block->mem));
            free(block->references);
            block->references = NULL;
            if (detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, cur_mem_usage_device);
        }
    }
}
static void memblock_alloc_device(struct memblock_device *block, int32_t size)
{
    memblock_unref_device(block);
    
    cl_int clCreateBuffer_succeeded_2592;
    
    block->mem = clCreateBuffer(fut_cl_context, CL_MEM_READ_WRITE, size >
                                0 ? size : 1, NULL,
                                &clCreateBuffer_succeeded_2592);
    OPENCL_SUCCEED(clCreateBuffer_succeeded_2592);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    cur_mem_usage_device += size;
    if (detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in space 'device' (now allocated: %ld bytes)",
                size, cur_mem_usage_device);
    if (cur_mem_usage_device > peak_mem_usage_device) {
        peak_mem_usage_device = cur_mem_usage_device;
        if (detail_memory)
            fprintf(stderr, " (new peak).\n", peak_mem_usage_device);
    } else if (detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set_device(struct memblock_device *lhs,
                                struct memblock_device *rhs)
{
    memblock_unref_device(lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
int64_t peak_mem_usage_local = 0;
int64_t cur_mem_usage_local = 0;
struct memblock_local {
    int *references;
    unsigned char mem;
    int64_t size;
} ;
static void memblock_unref_local(struct memblock_local *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (detail_memory)
            fprintf(stderr,
                    "Unreferencing block in space 'local': %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            cur_mem_usage_local -= block->size;
            free(block->references);
            block->references = NULL;
            if (detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, cur_mem_usage_local);
        }
    }
}
static void memblock_alloc_local(struct memblock_local *block, int32_t size)
{
    memblock_unref_local(block);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    cur_mem_usage_local += size;
    if (detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in space 'local' (now allocated: %ld bytes)",
                size, cur_mem_usage_local);
    if (cur_mem_usage_local > peak_mem_usage_local) {
        peak_mem_usage_local = cur_mem_usage_local;
        if (detail_memory)
            fprintf(stderr, " (new peak).\n", peak_mem_usage_local);
    } else if (detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set_local(struct memblock_local *lhs,
                               struct memblock_local *rhs)
{
    memblock_unref_local(lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
int64_t peak_mem_usage_default = 0;
int64_t cur_mem_usage_default = 0;
struct memblock {
    int *references;
    char *mem;
    int64_t size;
} ;
static void memblock_unref(struct memblock *block)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (detail_memory)
            fprintf(stderr,
                    "Unreferencing block in default space: %d references remaining.\n",
                    *block->references);
        if (*block->references == 0) {
            cur_mem_usage_default -= block->size;
            free(block->mem);
            free(block->references);
            block->references = NULL;
            if (detail_memory)
                fprintf(stderr, "%ld bytes freed (now allocated: %ld bytes)\n",
                        block->size, cur_mem_usage_default);
        }
    }
}
static void memblock_alloc(struct memblock *block, int32_t size)
{
    memblock_unref(block);
    block->mem = (char *) malloc(size);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    cur_mem_usage_default += size;
    if (detail_memory)
        fprintf(stderr,
                "Allocated %d bytes in default space (now allocated: %ld bytes)",
                size, cur_mem_usage_default);
    if (cur_mem_usage_default > peak_mem_usage_default) {
        peak_mem_usage_default = cur_mem_usage_default;
        if (detail_memory)
            fprintf(stderr, " (new peak).\n", peak_mem_usage_default);
    } else if (detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set(struct memblock *lhs, struct memblock *rhs)
{
    memblock_unref(lhs);
    (*rhs->references)++;
    *lhs = *rhs;
}
struct tuple_int32_t_device_mem_int32_t {
    int32_t elem_0;
    struct memblock_device elem_1;
    int32_t elem_2;
} ;
static struct tuple_int32_t_device_mem_int32_t
futhark_main(int64_t flags_mem_sizze_2490, int64_t vals_mem_sizze_2492,
             struct memblock_device flags_mem_2491,
             struct memblock_device vals_mem_2493, int32_t sizze_2287,
             int32_t sizze_2288);
static inline float futhark_log32(float x)
{
    return log(x);
}
static inline float futhark_sqrt32(float x)
{
    return sqrt(x);
}
static inline float futhark_exp32(float x)
{
    return exp(x);
}
static inline float futhark_cos32(float x)
{
    return cos(x);
}
static inline float futhark_sin32(float x)
{
    return sin(x);
}
static inline float futhark_acos32(float x)
{
    return acos(x);
}
static inline float futhark_asin32(float x)
{
    return asin(x);
}
static inline double futhark_atan32(float x)
{
    return atan(x);
}
static inline float futhark_atan2_32(float x, float y)
{
    return atan2(x, y);
}
static inline char futhark_isnan32(float x)
{
    return isnan(x);
}
static inline char futhark_isinf32(float x)
{
    return isinf(x);
}
static inline double futhark_log64(double x)
{
    return log(x);
}
static inline double futhark_sqrt64(double x)
{
    return sqrt(x);
}
static inline double futhark_exp64(double x)
{
    return exp(x);
}
static inline double futhark_cos64(double x)
{
    return cos(x);
}
static inline double futhark_sin64(double x)
{
    return sin(x);
}
static inline double futhark_acos64(double x)
{
    return acos(x);
}
static inline double futhark_asin64(double x)
{
    return asin(x);
}
static inline double futhark_atan64(double x)
{
    return atan(x);
}
static inline double futhark_atan2_64(double x, double y)
{
    return atan2(x, y);
}
static inline char futhark_isnan64(double x)
{
    return isnan(x);
}
static inline char futhark_isinf64(double x)
{
    return isinf(x);
}
static inline int8_t add8(int8_t x, int8_t y)
{
    return x + y;
}
static inline int16_t add16(int16_t x, int16_t y)
{
    return x + y;
}
static inline int32_t add32(int32_t x, int32_t y)
{
    return x + y;
}
static inline int64_t add64(int64_t x, int64_t y)
{
    return x + y;
}
static inline int8_t sub8(int8_t x, int8_t y)
{
    return x - y;
}
static inline int16_t sub16(int16_t x, int16_t y)
{
    return x - y;
}
static inline int32_t sub32(int32_t x, int32_t y)
{
    return x - y;
}
static inline int64_t sub64(int64_t x, int64_t y)
{
    return x - y;
}
static inline int8_t mul8(int8_t x, int8_t y)
{
    return x * y;
}
static inline int16_t mul16(int16_t x, int16_t y)
{
    return x * y;
}
static inline int32_t mul32(int32_t x, int32_t y)
{
    return x * y;
}
static inline int64_t mul64(int64_t x, int64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline int8_t smin8(int8_t x, int8_t y)
{
    return x < y ? x : y;
}
static inline int16_t smin16(int16_t x, int16_t y)
{
    return x < y ? x : y;
}
static inline int32_t smin32(int32_t x, int32_t y)
{
    return x < y ? x : y;
}
static inline int64_t smin64(int64_t x, int64_t y)
{
    return x < y ? x : y;
}
static inline uint8_t umin8(uint8_t x, uint8_t y)
{
    return x < y ? x : y;
}
static inline uint16_t umin16(uint16_t x, uint16_t y)
{
    return x < y ? x : y;
}
static inline uint32_t umin32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}
static inline uint64_t umin64(uint64_t x, uint64_t y)
{
    return x < y ? x : y;
}
static inline int8_t smax8(int8_t x, int8_t y)
{
    return x < y ? y : x;
}
static inline int16_t smax16(int16_t x, int16_t y)
{
    return x < y ? y : x;
}
static inline int32_t smax32(int32_t x, int32_t y)
{
    return x < y ? y : x;
}
static inline int64_t smax64(int64_t x, int64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t umax8(uint8_t x, uint8_t y)
{
    return x < y ? y : x;
}
static inline uint16_t umax16(uint16_t x, uint16_t y)
{
    return x < y ? y : x;
}
static inline uint32_t umax32(uint32_t x, uint32_t y)
{
    return x < y ? y : x;
}
static inline uint64_t umax64(uint64_t x, uint64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline char ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline char ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline char ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline char ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline char ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline char ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline char ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline char ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline char slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline char slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline char slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline char slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline char sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline char sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline char sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline char sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int8_t sext_i8_i8(int8_t x)
{
    return x;
}
static inline int16_t sext_i8_i16(int8_t x)
{
    return x;
}
static inline int32_t sext_i8_i32(int8_t x)
{
    return x;
}
static inline int64_t sext_i8_i64(int8_t x)
{
    return x;
}
static inline int8_t sext_i16_i8(int16_t x)
{
    return x;
}
static inline int16_t sext_i16_i16(int16_t x)
{
    return x;
}
static inline int32_t sext_i16_i32(int16_t x)
{
    return x;
}
static inline int64_t sext_i16_i64(int16_t x)
{
    return x;
}
static inline int8_t sext_i32_i8(int32_t x)
{
    return x;
}
static inline int16_t sext_i32_i16(int32_t x)
{
    return x;
}
static inline int32_t sext_i32_i32(int32_t x)
{
    return x;
}
static inline int64_t sext_i32_i64(int32_t x)
{
    return x;
}
static inline int8_t sext_i64_i8(int64_t x)
{
    return x;
}
static inline int16_t sext_i64_i16(int64_t x)
{
    return x;
}
static inline int32_t sext_i64_i32(int64_t x)
{
    return x;
}
static inline int64_t sext_i64_i64(int64_t x)
{
    return x;
}
static inline uint8_t zext_i8_i8(uint8_t x)
{
    return x;
}
static inline uint16_t zext_i8_i16(uint8_t x)
{
    return x;
}
static inline uint32_t zext_i8_i32(uint8_t x)
{
    return x;
}
static inline uint64_t zext_i8_i64(uint8_t x)
{
    return x;
}
static inline uint8_t zext_i16_i8(uint16_t x)
{
    return x;
}
static inline uint16_t zext_i16_i16(uint16_t x)
{
    return x;
}
static inline uint32_t zext_i16_i32(uint16_t x)
{
    return x;
}
static inline uint64_t zext_i16_i64(uint16_t x)
{
    return x;
}
static inline uint8_t zext_i32_i8(uint32_t x)
{
    return x;
}
static inline uint16_t zext_i32_i16(uint32_t x)
{
    return x;
}
static inline uint32_t zext_i32_i32(uint32_t x)
{
    return x;
}
static inline uint64_t zext_i32_i64(uint32_t x)
{
    return x;
}
static inline uint8_t zext_i64_i8(uint64_t x)
{
    return x;
}
static inline uint16_t zext_i64_i16(uint64_t x)
{
    return x;
}
static inline uint32_t zext_i64_i32(uint64_t x)
{
    return x;
}
static inline uint64_t zext_i64_i64(uint64_t x)
{
    return x;
}
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fmin32(float x, float y)
{
    return x < y ? x : y;
}
static inline float fmax32(float x, float y)
{
    return x < y ? y : x;
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline char cmplt32(float x, float y)
{
    return x < y;
}
static inline char cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return x;
}
static inline double fdiv64(double x, double y)
{
    return x / y;
}
static inline double fadd64(double x, double y)
{
    return x + y;
}
static inline double fsub64(double x, double y)
{
    return x - y;
}
static inline double fmul64(double x, double y)
{
    return x * y;
}
static inline double fmin64(double x, double y)
{
    return x < y ? x : y;
}
static inline double fmax64(double x, double y)
{
    return x < y ? y : x;
}
static inline double fpow64(double x, double y)
{
    return pow(x, y);
}
static inline char cmplt64(double x, double y)
{
    return x < y;
}
static inline char cmple64(double x, double y)
{
    return x <= y;
}
static inline double sitofp_i8_f64(int8_t x)
{
    return x;
}
static inline double sitofp_i16_f64(int16_t x)
{
    return x;
}
static inline double sitofp_i32_f64(int32_t x)
{
    return x;
}
static inline double sitofp_i64_f64(int64_t x)
{
    return x;
}
static inline double uitofp_i8_f64(uint8_t x)
{
    return x;
}
static inline double uitofp_i16_f64(uint16_t x)
{
    return x;
}
static inline double uitofp_i32_f64(uint32_t x)
{
    return x;
}
static inline double uitofp_i64_f64(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f64_i8(double x)
{
    return x;
}
static inline int16_t fptosi_f64_i16(double x)
{
    return x;
}
static inline int32_t fptosi_f64_i32(double x)
{
    return x;
}
static inline int64_t fptosi_f64_i64(double x)
{
    return x;
}
static inline uint8_t fptoui_f64_i8(double x)
{
    return x;
}
static inline uint16_t fptoui_f64_i16(double x)
{
    return x;
}
static inline uint32_t fptoui_f64_i32(double x)
{
    return x;
}
static inline uint64_t fptoui_f64_i64(double x)
{
    return x;
}
static inline float fpconv_f32_f32(float x)
{
    return x;
}
static inline double fpconv_f32_f64(float x)
{
    return x;
}
static inline float fpconv_f64_f32(double x)
{
    return x;
}
static inline double fpconv_f64_f64(double x)
{
    return x;
}
static int detail_timing = 0;
static
struct tuple_int32_t_device_mem_int32_t futhark_main(int64_t flags_mem_sizze_2490,
                                                     int64_t vals_mem_sizze_2492,
                                                     struct memblock_device flags_mem_2491,
                                                     struct memblock_device vals_mem_2493,
                                                     int32_t sizze_2287,
                                                     int32_t sizze_2288)
{
    int32_t out_memsizze_2534;
    struct memblock_device out_mem_2533;
    
    out_mem_2533.references = NULL;
    
    int32_t out_arrsizze_2535;
    char assert_arg_2291 = sizze_2288 == sizze_2287;
    char shape_cert_2292;
    
    if (!assert_arg_2291) {
        fprintf(stderr, "Assertion failed at %s: %s\n",
                "sgm-scan.fut:34:5-34:5", "function arguments of wrong shape");
        exit(1);
    }
    
    int32_t j_2298 = sizze_2288 - 1;
    int32_t x_2299 = abs(j_2298);
    char empty_slice_2300 = x_2299 == 0;
    int32_t m_2301 = x_2299 - 1;
    char zzero_leq_i_p_m_t_s_2302 = sle32(0, m_2301);
    char i_p_m_t_s_leq_w_2303 = slt32(m_2301, sizze_2288);
    char i_lte_j_2304 = sle32(0, j_2298);
    char y_2305 = zzero_leq_i_p_m_t_s_2302 && i_p_m_t_s_leq_w_2303;
    char y_2306 = i_lte_j_2304 && y_2305;
    char ok_or_empty_2307 = empty_slice_2300 || y_2306;
    char slice_cert_2308;
    
    if (!ok_or_empty_2307) {
        fprintf(stderr, "Assertion failed at %s: %s\n",
                "sgm-scan.fut:35:39-35:39", "slice out of bounds");
        exit(1);
    }
    
    int32_t conc_tmp_2310 = 1 + x_2299;
    char assert_arg_2312 = conc_tmp_2310 == sizze_2288;
    char shape_cert_2313;
    
    if (!assert_arg_2312) {
        fprintf(stderr, "Assertion failed at %s: %s\n",
                "sgm-scan.fut:35:3-35:3", "function arguments of wrong shape");
        exit(1);
    }
    
    int32_t group_sizze_2351;
    
    group_sizze_2351 = cl_group_size;
    
    int32_t max_num_groups_2352;
    
    max_num_groups_2352 = cl_num_groups;
    
    int32_t y_2353 = group_sizze_2351 - 1;
    int32_t x_2354 = conc_tmp_2310 + y_2353;
    int32_t w_div_group_sizze_2355 = squot32(x_2354, group_sizze_2351);
    int32_t num_groups_maybe_zzero_2356 = smin32(w_div_group_sizze_2355,
                                                 max_num_groups_2352);
    int32_t num_groups_2357 = smax32(1, num_groups_maybe_zzero_2356);
    int32_t num_threads_2358 = num_groups_2357 * group_sizze_2351;
    int64_t binop_x_2495 = sext_i32_i64(conc_tmp_2310);
    int64_t bytes_2494 = binop_x_2495 * 4;
    struct memblock_device mem_2496;
    
    mem_2496.references = NULL;
    memblock_alloc_device(&mem_2496, bytes_2494);
    
    struct memblock_device mem_2499;
    
    mem_2499.references = NULL;
    memblock_alloc_device(&mem_2499, bytes_2494);
    
    int32_t y_2395 = num_threads_2358 - 1;
    int32_t x_2396 = conc_tmp_2310 + y_2395;
    int32_t num_iterations_2397 = squot32(x_2396, num_threads_2358);
    int32_t y_2401 = num_iterations_2397 * group_sizze_2351;
    int64_t binop_x_2507 = sext_i32_i64(num_groups_2357);
    int64_t bytes_2506 = binop_x_2507 * 4;
    struct memblock_device mem_2508;
    
    mem_2508.references = NULL;
    memblock_alloc_device(&mem_2508, bytes_2506);
    
    struct memblock_device mem_2511;
    
    mem_2511.references = NULL;
    memblock_alloc_device(&mem_2511, bytes_2506);
    
    int64_t binop_y_2501 = sext_i32_i64(group_sizze_2351);
    int64_t bytes_2500 = 4 * binop_y_2501;
    struct memblock_local mem_2502;
    
    mem_2502.references = NULL;
    
    struct memblock_local mem_2505;
    
    mem_2505.references = NULL;
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 0, bytes_2500, NULL));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 1, bytes_2500, NULL));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 2, sizeof(conc_tmp_2310),
                                  &conc_tmp_2310));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 3,
                                  sizeof(num_iterations_2397),
                                  &num_iterations_2397));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 4, sizeof(y_2401),
                                  &y_2401));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 5,
                                  sizeof(flags_mem_2491.mem),
                                  &flags_mem_2491.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 6,
                                  sizeof(vals_mem_2493.mem),
                                  &vals_mem_2493.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 7, sizeof(mem_2496.mem),
                                  &mem_2496.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 8, sizeof(mem_2499.mem),
                                  &mem_2499.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 9, sizeof(mem_2508.mem),
                                  &mem_2508.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan1_kernel_2392, 10, sizeof(mem_2511.mem),
                                  &mem_2511.mem));
    if (1 * (num_groups_2357 * group_sizze_2351) != 0) {
        const size_t global_work_sizze_2574[1] = {num_groups_2357 *
                     group_sizze_2351};
        const size_t local_work_sizze_2578[1] = {group_sizze_2351};
        int64_t time_start_2575, time_end_2576;
        
        if (debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan1_kernel_2392");
            fprintf(stderr, "%zu", global_work_sizze_2574[0]);
            fprintf(stderr, "].\n");
            time_start_2575 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, scan1_kernel_2392,
                                              1, NULL, global_work_sizze_2574,
                                              local_work_sizze_2578, 0, NULL,
                                              NULL));
        if (debugging) {
            OPENCL_SUCCEED(clFinish(fut_cl_queue));
            time_end_2576 = get_wall_time();
            
            long time_diff_2577 = time_end_2576 - time_start_2575;
            
            if (detail_timing) {
                scan1_kernel_2392total_runtime += time_diff_2577;
                scan1_kernel_2392runs++;
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "scan1_kernel_2392", (int) time_diff_2577);
            }
        }
    }
    
    struct memblock_device mem_2520;
    
    mem_2520.references = NULL;
    memblock_alloc_device(&mem_2520, bytes_2506);
    
    struct memblock_device mem_2523;
    
    mem_2523.references = NULL;
    memblock_alloc_device(&mem_2523, bytes_2506);
    
    int64_t bytes_2512 = 4 * binop_x_2507;
    struct memblock_local mem_2514;
    
    mem_2514.references = NULL;
    
    struct memblock_local mem_2517;
    
    mem_2517.references = NULL;
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 0, bytes_2512, NULL));
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 1, bytes_2512, NULL));
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 2, sizeof(num_groups_2357),
                                  &num_groups_2357));
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 3, sizeof(mem_2508.mem),
                                  &mem_2508.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 4, sizeof(mem_2511.mem),
                                  &mem_2511.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 5, sizeof(mem_2520.mem),
                                  &mem_2520.mem));
    OPENCL_SUCCEED(clSetKernelArg(scan2_kernel_2441, 6, sizeof(mem_2523.mem),
                                  &mem_2523.mem));
    if (1 * num_groups_2357 != 0) {
        const size_t global_work_sizze_2579[1] = {num_groups_2357};
        const size_t local_work_sizze_2583[1] = {num_groups_2357};
        int64_t time_start_2580, time_end_2581;
        
        if (debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan2_kernel_2441");
            fprintf(stderr, "%zu", global_work_sizze_2579[0]);
            fprintf(stderr, "].\n");
            time_start_2580 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, scan2_kernel_2441,
                                              1, NULL, global_work_sizze_2579,
                                              local_work_sizze_2583, 0, NULL,
                                              NULL));
        if (debugging) {
            OPENCL_SUCCEED(clFinish(fut_cl_queue));
            time_end_2581 = get_wall_time();
            
            long time_diff_2582 = time_end_2581 - time_start_2580;
            
            if (detail_timing) {
                scan2_kernel_2441total_runtime += time_diff_2582;
                scan2_kernel_2441runs++;
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "scan2_kernel_2441", (int) time_diff_2582);
            }
        }
    }
    
    int32_t num_threads_2478 = w_div_group_sizze_2355 * group_sizze_2351;
    struct memblock_device mem_2526;
    
    mem_2526.references = NULL;
    memblock_alloc_device(&mem_2526, bytes_2494);
    
    struct memblock_device mem_2529;
    
    mem_2529.references = NULL;
    memblock_alloc_device(&mem_2529, bytes_2494);
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 0, sizeof(conc_tmp_2310),
                                  &conc_tmp_2310));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 1, sizeof(y_2401), &y_2401));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 2, sizeof(mem_2496.mem),
                                  &mem_2496.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 3, sizeof(mem_2499.mem),
                                  &mem_2499.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 4, sizeof(mem_2520.mem),
                                  &mem_2520.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 5, sizeof(mem_2523.mem),
                                  &mem_2523.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 6, sizeof(mem_2526.mem),
                                  &mem_2526.mem));
    OPENCL_SUCCEED(clSetKernelArg(map_kernel_2479, 7, sizeof(mem_2529.mem),
                                  &mem_2529.mem));
    if (1 * (w_div_group_sizze_2355 * group_sizze_2351) != 0) {
        const size_t global_work_sizze_2584[1] = {w_div_group_sizze_2355 *
                     group_sizze_2351};
        const size_t local_work_sizze_2588[1] = {group_sizze_2351};
        int64_t time_start_2585, time_end_2586;
        
        if (debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "map_kernel_2479");
            fprintf(stderr, "%zu", global_work_sizze_2584[0]);
            fprintf(stderr, "].\n");
            time_start_2585 = get_wall_time();
        }
        OPENCL_SUCCEED(clEnqueueNDRangeKernel(fut_cl_queue, map_kernel_2479, 1,
                                              NULL, global_work_sizze_2584,
                                              local_work_sizze_2588, 0, NULL,
                                              NULL));
        if (debugging) {
            OPENCL_SUCCEED(clFinish(fut_cl_queue));
            time_end_2586 = get_wall_time();
            
            long time_diff_2587 = time_end_2586 - time_start_2585;
            
            if (detail_timing) {
                map_kernel_2479total_runtime += time_diff_2587;
                map_kernel_2479runs++;
                fprintf(stderr, "kernel %s runtime: %ldus\n", "map_kernel_2479",
                        (int) time_diff_2587);
            }
        }
    }
    
    char assert_arg_2337 = sizze_2288 == conc_tmp_2310;
    char shape_cert_2338;
    
    if (!assert_arg_2337) {
        fprintf(stderr, "Assertion failed at %s: %s\n",
                "sgm-scan.fut:34:5-34:5",
                "shape of function result does not match shapes in return type");
        exit(1);
    }
    memblock_set_device(&out_mem_2533, &mem_2529);
    out_arrsizze_2535 = sizze_2288;
    out_memsizze_2534 = bytes_2494;
    
    struct tuple_int32_t_device_mem_int32_t retval_2573;
    
    retval_2573.elem_0 = out_memsizze_2534;
    retval_2573.elem_1.references = NULL;
    memblock_set_device(&retval_2573.elem_1, &out_mem_2533);
    retval_2573.elem_2 = out_arrsizze_2535;
    memblock_unref_device(&out_mem_2533);
    memblock_unref_device(&mem_2496);
    memblock_unref_device(&mem_2499);
    memblock_unref_device(&mem_2508);
    memblock_unref_device(&mem_2511);
    memblock_unref_local(&mem_2502);
    memblock_unref_local(&mem_2505);
    memblock_unref_device(&mem_2520);
    memblock_unref_device(&mem_2523);
    memblock_unref_local(&mem_2514);
    memblock_unref_local(&mem_2517);
    memblock_unref_device(&mem_2526);
    memblock_unref_device(&mem_2529);
    return retval_2573;
}
static FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
static const char *entry_point = "main";
int parse_options(int argc, char *const argv[])
{
    int ch;
    static struct option long_options[] = {{"write-runtime-to",
                                            required_argument, NULL, 1},
                                           {"runs", required_argument, NULL, 2},
                                           {"memory-reporting", no_argument,
                                            NULL, 3}, {"entry-point",
                                                       required_argument, NULL,
                                                       4}, {"binary-output",
                                                            no_argument, NULL,
                                                            5}, {"platform",
                                                                 required_argument,
                                                                 NULL, 6},
                                           {"device", required_argument, NULL,
                                            7}, {"synchronous", no_argument,
                                                 NULL, 8}, {"group-size",
                                                            required_argument,
                                                            NULL, 9},
                                           {"num-groups", required_argument,
                                            NULL, 10}, {"dump-opencl",
                                                        required_argument, NULL,
                                                        11}, {"load-opencl",
                                                              required_argument,
                                                              NULL, 12}, {0, 0,
                                                                          0,
                                                                          0}};
    
    while ((ch = getopt_long(argc, argv, ":t:r:me:bp:d:s", long_options,
                             NULL)) != -1) {
        if (ch == 1 || ch == 't') {
            runtime_file = fopen(optarg, "w");
            if (runtime_file == NULL)
                panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
        }
        if (ch == 2 || ch == 'r') {
            num_runs = atoi(optarg);
            perform_warmup = 1;
            if (num_runs <= 0)
                panic(1, "Need a positive number of runs, not %s\n", optarg);
        }
        if (ch == 3 || ch == 'm')
            detail_memory = 1;
        if (ch == 4 || ch == 'e')
            entry_point = optarg;
        if (ch == 5 || ch == 'b')
            binary_output = 1;
        if (ch == 6 || ch == 'p')
            set_preferred_platform(optarg);
        if (ch == 7 || ch == 'd')
            set_preferred_device(optarg);
        if (ch == 8 || ch == 's')
            cl_debug = debugging = 1;
        if (ch == 9)
            cl_group_size = atoi(optarg);
        if (ch == 10)
            cl_num_groups = atoi(optarg);
        if (ch == 11)
            cl_dump_program_to = optarg;
        if (ch == 12)
            cl_load_program_from = optarg;
        if (ch == ':')
            panic(-1, "Missing argument for option %s\n", argv[optind - 1]);
        if (ch == '?')
            panic(-1, "Unknown option %s\n", argv[optind - 1]);
    }
    return optind;
}
void entry_main()
{
    int64_t t_start, t_end;
    int time_runs;
    int64_t flags_mem_sizze_2490;
    int64_t vals_mem_sizze_2492;
    struct memblock flags_mem_2491;
    
    flags_mem_2491.references = NULL;
    memblock_alloc(&flags_mem_2491, 0);
    
    struct memblock vals_mem_2493;
    
    vals_mem_2493.references = NULL;
    memblock_alloc(&vals_mem_2493, 0);
    
    int32_t sizze_2287;
    int32_t sizze_2288;
    struct tuple_int32_t_device_mem_int32_t main_ret_2589;
    
    {
        int64_t shape[1];
        
        errno = 0;
        if (read_array(&i32, (void **) &flags_mem_2491.mem, shape, 1) != 0)
            panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
                  i32.type_name, strerror(errno));
        sizze_2287 = shape[0];
        flags_mem_sizze_2490 = sizeof(int32_t) * shape[0];
        flags_mem_2491.size = flags_mem_sizze_2490;
    }
    {
        int64_t shape[1];
        
        errno = 0;
        if (read_array(&i32, (void **) &vals_mem_2493.mem, shape, 1) != 0)
            panic(1, "Failed reading input of type %s%s (errno: %s).\n", "[]",
                  i32.type_name, strerror(errno));
        sizze_2288 = shape[0];
        vals_mem_sizze_2492 = sizeof(int32_t) * shape[0];
        vals_mem_2493.size = vals_mem_sizze_2492;
    }
    
    int32_t out_memsizze_2534;
    struct memblock out_mem_2533;
    
    out_mem_2533.references = NULL;
    
    int32_t out_arrsizze_2535;
    
    if (perform_warmup) {
        time_runs = 0;
        
        struct memblock_device flags_mem_copy_2590;
        
        flags_mem_copy_2590.references = NULL;
        memblock_alloc_device(&flags_mem_copy_2590, flags_mem_2491.size);
        if (flags_mem_2491.size > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                flags_mem_copy_2590.mem,
                                                CL_TRUE, 0, flags_mem_2491.size,
                                                flags_mem_2491.mem + 0, 0, NULL,
                                                NULL));
        
        struct memblock_device vals_mem_copy_2591;
        
        vals_mem_copy_2591.references = NULL;
        memblock_alloc_device(&vals_mem_copy_2591, vals_mem_2493.size);
        if (vals_mem_2493.size > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                vals_mem_copy_2591.mem, CL_TRUE,
                                                0, vals_mem_2493.size,
                                                vals_mem_2493.mem + 0, 0, NULL,
                                                NULL));
        t_start = get_wall_time();
        main_ret_2589 = futhark_main(flags_mem_sizze_2490, vals_mem_sizze_2492,
                                     flags_mem_copy_2590, vals_mem_copy_2591,
                                     sizze_2287, sizze_2288);
        OPENCL_SUCCEED(clFinish(fut_cl_queue));
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        memblock_unref_device(&main_ret_2589.elem_1);
        memblock_unref_device(&flags_mem_copy_2590);
        memblock_unref_device(&vals_mem_copy_2591);
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        if (run == num_runs - 1)
            detail_timing = 1;
        
        struct memblock_device flags_mem_copy_2590;
        
        flags_mem_copy_2590.references = NULL;
        memblock_alloc_device(&flags_mem_copy_2590, flags_mem_2491.size);
        if (flags_mem_2491.size > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                flags_mem_copy_2590.mem,
                                                CL_TRUE, 0, flags_mem_2491.size,
                                                flags_mem_2491.mem + 0, 0, NULL,
                                                NULL));
        
        struct memblock_device vals_mem_copy_2591;
        
        vals_mem_copy_2591.references = NULL;
        memblock_alloc_device(&vals_mem_copy_2591, vals_mem_2493.size);
        if (vals_mem_2493.size > 0)
            OPENCL_SUCCEED(clEnqueueWriteBuffer(fut_cl_queue,
                                                vals_mem_copy_2591.mem, CL_TRUE,
                                                0, vals_mem_2493.size,
                                                vals_mem_2493.mem + 0, 0, NULL,
                                                NULL));
        t_start = get_wall_time();
        main_ret_2589 = futhark_main(flags_mem_sizze_2490, vals_mem_sizze_2492,
                                     flags_mem_copy_2590, vals_mem_copy_2591,
                                     sizze_2287, sizze_2288);
        OPENCL_SUCCEED(clFinish(fut_cl_queue));
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%ld\n", elapsed_usec);
        if (run < num_runs - 1) {
            memblock_unref_device(&main_ret_2589.elem_1);
            memblock_unref_device(&flags_mem_copy_2590);
            memblock_unref_device(&vals_mem_copy_2591);
        }
    }
    memblock_unref(&flags_mem_2491);
    memblock_unref(&vals_mem_2493);
    out_memsizze_2534 = main_ret_2589.elem_0;
    memblock_alloc(&out_mem_2533, main_ret_2589.elem_1.size);
    if (main_ret_2589.elem_1.size > 0)
        OPENCL_SUCCEED(clEnqueueReadBuffer(fut_cl_queue,
                                           main_ret_2589.elem_1.mem, CL_TRUE, 0,
                                           main_ret_2589.elem_1.size,
                                           out_mem_2533.mem + 0, 0, NULL,
                                           NULL));
    out_arrsizze_2535 = main_ret_2589.elem_2;
    {
        int64_t shape[] = {out_arrsizze_2535};
        
        write_array(stdout, binary_output, &i32, out_mem_2533.mem, shape, 1);
    }
    printf("\n");
    memblock_unref_device(&main_ret_2589.elem_1);
}
typedef void entry_point_fun();
struct entry_point_entry {
    const char *name;
    entry_point_fun *fun;
} ;
int main(int argc, char **argv)
{
    fut_progname = argv[0];
    
    struct entry_point_entry entry_points[] = {{.name ="main", .fun =
                                                entry_main}};
    int parsed_options = parse_options(argc, argv);
    
    argc -= parsed_options;
    argv += parsed_options;
    setup_opencl_and_load_kernels();
    
    int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
    entry_point_fun *entry_point_fun = NULL;
    
    for (int i = 0; i < num_entry_points; i++) {
        if (strcmp(entry_points[i].name, entry_point) == 0) {
            entry_point_fun = entry_points[i].fun;
            break;
        }
    }
    if (entry_point_fun == NULL) {
        fprintf(stderr,
                "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                entry_point);
        for (int i = 0; i < num_entry_points; i++)
            fprintf(stderr, "%s\n", entry_points[i].name);
        return 1;
    }
    entry_point_fun();
    if (runtime_file != NULL)
        fclose(runtime_file);
    
    int total_runtime = 0;
    int total_runs = 0;
    
    if (debugging) {
        fprintf(stderr,
                "Kernel map_kernel_2479   executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                map_kernel_2479runs, (long) map_kernel_2479total_runtime /
                (map_kernel_2479runs != 0 ? map_kernel_2479runs : 1),
                (long) map_kernel_2479total_runtime);
        total_runtime += map_kernel_2479total_runtime;
        total_runs += map_kernel_2479runs;
        fprintf(stderr,
                "Kernel scan1_kernel_2392 executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan1_kernel_2392runs, (long) scan1_kernel_2392total_runtime /
                (scan1_kernel_2392runs != 0 ? scan1_kernel_2392runs : 1),
                (long) scan1_kernel_2392total_runtime);
        total_runtime += scan1_kernel_2392total_runtime;
        total_runs += scan1_kernel_2392runs;
        fprintf(stderr,
                "Kernel scan2_kernel_2441 executed %6d times, with average runtime: %6ldus\tand total runtime: %6ldus\n",
                scan2_kernel_2441runs, (long) scan2_kernel_2441total_runtime /
                (scan2_kernel_2441runs != 0 ? scan2_kernel_2441runs : 1),
                (long) scan2_kernel_2441total_runtime);
        total_runtime += scan2_kernel_2441total_runtime;
        total_runs += scan2_kernel_2441runs;
    }
    if (debugging)
        fprintf(stderr, "Ran %d kernels with cumulative runtime: %6ldus\n",
                total_runs, total_runtime);
    if (detail_memory) {
        fprintf(stderr, "Peak memory usage for space 'device': %ld bytes.\n",
                peak_mem_usage_device);
        fprintf(stderr, "Peak memory usage for space 'local': %ld bytes.\n",
                peak_mem_usage_local);
        fprintf(stderr, "Peak memory usage for default space: %ld bytes.\n",
                peak_mem_usage_default);
    }
    return 0;
}
