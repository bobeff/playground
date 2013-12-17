/*
  Exercise 29.1:
    Modify the implementation of setarray so that it accepts only boolean
values.

  Exercise 29.2:
    We can see a boolean array as a set of integers (the indices with true
values in the array). Add to the implementation of boolean arrays functions
to compute the union and intersection of two arrays. These functions should
receive two arrays and return a new one, without modifying its parameters.

  Exercise 29.3:
    Modify the implementation of the __tostring metamethod so that it shows
the full contents of the array in an appropriate way. Use the buffer facility
to create the resulting string.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>

typedef struct BitArray
{
  int size;
  unsigned values[1];
}
BitArray;

#define BITS_PER_WORD (CHAR_BIT * sizeof(unsigned))
#define I_WORD(i)     ((unsigned)(i) / BITS_PER_WORD)
#define I_BIT(i)      (1 << ((unsigned)(i) % BITS_PER_WORD))

#define LIB_META_INDEX "bitarray"
#define checkarray(L, index) \
  (BitArray*)luaL_checkudata(L, index, LIB_META_INDEX)
#define arraysize(n) \
  sizeof(BitArray) + I_WORD(n - 1) * sizeof(unsigned)

static int newarray(lua_State *L)
{
  size_t i;
  BitArray *array;
  int n = luaL_checkint(L, 1);

  luaL_argcheck(L, n >= 1, 1, "invalid size");
  array = (BitArray*)lua_newuserdata(L, arraysize(n));

  array->size = n;
  for (i = 0; i <= I_WORD(n - 1); ++i)
    array->values[i] = 0;

  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  return 1;
}

static int array_from_string(lua_State *L)
{
  int i, len;
  const char *s = luaL_checklstring(L, 1, &len);
  BitArray *array = (BitArray*)lua_newuserdata(L, arraysize(len));
  
  luaL_argcheck(L, len >= 1, 1, "invalid size");
  array->size = len;

  for (i = 0; i < len; ++i)
  {
    unsigned index = I_WORD(i);
    unsigned mask = I_BIT(i);

    if (s[i] == '1')
      array->values[index] |= mask;
    else if (s[i] == '0')
      array->values[index] &= ~mask;
    else
    {
      return luaL_error(L,
        "invalud character '%c' at position %d, '0' or '1' expected",
        s[i], i + 1);
    }
  }

  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  return 1;
}

static unsigned *getindex(lua_State *L, unsigned *mask)
{
  BitArray *array = checkarray(L, 1);
  int index = luaL_checkint(L, 2) - 1;

  luaL_argcheck(L, 0 <= index && index < array->size, 2,
    "index out of range");

  *mask = I_BIT(index);
  return &array->values[I_WORD(index)];
}

static int setarray(lua_State *L)
{
  unsigned mask;
  unsigned *entry = getindex(L, &mask);
  luaL_argcheck(L, lua_isboolean(L, 3), 3, "boolean value expected");
  if (lua_toboolean(L, 3))
    *entry |= mask;
  else
    *entry &= ~mask;

  return 0;
}

static int getarray(lua_State *L)
{
  unsigned mask;
  unsigned *entry = getindex(L, &mask);
  lua_pushboolean(L, *entry & mask);
  return 1;
}

static int getsize(lua_State *L)
{
  BitArray *array = checkarray(L, 1);
  lua_pushinteger(L, array->size);
  return 1;
}

static int array2string(lua_State *L)
{
  int i;
  luaL_Buffer buffer;
  BitArray *array = checkarray(L, 1);

  luaL_buffinit(L, &buffer);

  for (i = 0; i < array->size; ++i)
  {
    unsigned mask = I_BIT(i);
    unsigned entry = array->values[I_WORD(i)];

    if (entry & mask)
      lua_pushlstring(L, "1", 1);
    else
      lua_pushlstring(L, "0", 1);

    luaL_addvalue(&buffer);
  }

  luaL_pushresult(&buffer);

  return 1;
}

static int arrays_union(lua_State *L)
{
  int i;
  BitArray *a;
  BitArray *a1 = checkarray(L, 1);
  BitArray *a2 = checkarray(L, 2);

  if (a1->size < a2->size)
  {
    a = a1;
    a1 = a2;
    a2 = a;
  }
 
  a = (BitArray*)lua_newuserdata(L, arraysize(a1->size));
  a->size = a1->size;

  for (i = 0; i < a1->size; ++i)
  {
    unsigned index = I_WORD(i);
    unsigned mask = I_BIT(i);

    if ((a1->values[index] & mask) ||
        (i < a2->size && (a2->values[index] & mask)))
      a->values[index] |= mask;
    else
      a->values[index] &= ~mask;
  }

  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  return 1;
}

static int arrays_intersect(lua_State *L)
{
  int i;
  BitArray *a;
  BitArray *a1 = checkarray(L, 1);
  BitArray *a2 = checkarray(L, 2);

  if (a1->size > a2->size)
  {
    a = a1;
    a1 = a2;
    a2 = a;
  }

  a = (BitArray*)lua_newuserdata(L, arraysize(a1->size));
  a->size = a1->size;

  for (i = 0; i < a1->size; ++i)
  {
    unsigned index = I_WORD(i);
    unsigned mask = I_BIT(i);

    if ((a1->values[index] & mask) && (a2->values[index] & mask))
      a->values[index] |= mask;
    else
      a->values[index] &= ~mask;
  }

  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  return 1;
}

static const struct luaL_Reg bitarraylib_f[] =
{
  { "new", newarray },
  { "from_string", array_from_string },
  { "union", arrays_union },
  { "intersect", arrays_intersect },
  { NULL, NULL }
};

static const struct luaL_Reg bitarraylib_m[] =
{
  { "__newindex", setarray },
  { "__index", getarray },
  { "__len", getsize },
  { "__tostring", array2string },
  { "__add", arrays_union },
  { "__mul", arrays_intersect },
  { NULL, NULL }
};

int LUA_EXPORT luaopen_bitarray(lua_State *L)
{
  luaL_newmetatable(L, LIB_META_INDEX);
  luaL_setfuncs(L, bitarraylib_m, 0);
  luaL_newlib(L, bitarraylib_f);
  return 1;
}
