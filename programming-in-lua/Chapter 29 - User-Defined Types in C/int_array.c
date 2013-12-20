/*
  Exercise 29.4:
    Based on the example for boolean arrays, implement a small C library for
integer arrays.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>

typedef struct IntArray
{
  int size;
  int values[1];
}
IntArray;

#define LIB_META_INDEX "intarray"
#define checkarray(L) (IntArray*)luaL_checkudata(L, 1, LIB_META_INDEX)
#define arraysize(n) \
  sizeof(IntArray) + (n - 1) * sizeof(int)

int newarray(lua_State *L)
{
  IntArray *array;
  int i, n = luaL_checkint(L, 1);

  luaL_argcheck(L, n >= 1, 1, "invalid size");
  array = (IntArray*)lua_newuserdata(L, arraysize(n));

  array->size = n;
  for (i = 0; i < n; ++i)
    array->values[i] = 0;

  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  return 1;
}

int array_from_sequence(lua_State *L)
{
  int i, n;
  IntArray *array;

  luaL_checktype(L, 1, LUA_TTABLE);
  n = luaL_len(L, 1);
  luaL_argcheck(L, n >= 1, 1, "invalid size");
  array = (IntArray*)lua_newuserdata(L, arraysize(n));

  array->size = n;
  for (i = 1; i <= n; ++i)
  {
    int isnum, num;
    lua_rawgeti(L, 1, i);
    num = lua_tointegerx(L, -1, &isnum);
    if (!isnum)
      luaL_error(L, "The value at position %d is not valid number.", i);
    array->values[i - 1] = num;
    lua_pop(L, 1);
  }

  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  return 1;
}

int setarray(lua_State *L)
{
  IntArray *array = checkarray(L);
  int value, index = luaL_checkint(L, 2) - 1;
  luaL_argcheck(L, 0 <= index && index < array->size, 2,
    "index out of range");
  value = luaL_checkint(L, 3);
  array->values[index] = value;
  return 0;
}

int getarray(lua_State *L)
{
  IntArray *array = checkarray(L);
  int index = luaL_checkint(L, 2) - 1;
  luaL_argcheck(L, 0 <= index && index < array->size, 2,
    "index out of range");
  lua_pushinteger(L, array->values[index]);
  return 1;
}

int getsize(lua_State *L)
{
  IntArray *array = checkarray(L);
  lua_pushinteger(L, array->size);
  return 1;
}

int array2string(lua_State *L)
{
  int i;
  luaL_Buffer buffer;
  IntArray *array = checkarray(L);

  luaL_buffinit(L, &buffer);

  for (i = 0; i < array->size - 1; ++i)
  {
    lua_pushinteger(L, array->values[i]);
    lua_pushstring(L, " ");
    lua_concat(L, 2);
    luaL_addvalue(&buffer);
  }

  lua_pushinteger(L, array->values[array->size - 1]);
  luaL_addvalue(&buffer);
  luaL_pushresult(&buffer);

  return 1;
}

static const struct luaL_Reg intarraylib_f[] =
{
  { "new", newarray },
  { "from_sequence", array_from_sequence },
  { NULL, NULL }
};

static const struct luaL_Reg intarraylib_m[] =
{
  { "__newindex", setarray },
  { "__index", getarray },
  { "__len", getsize },
  { "__tostring", array2string },
  { NULL, NULL }
};

int LUA_EXPORT luaopen_intarray(lua_State *L)
{
  luaL_newmetatable(L, LIB_META_INDEX);
  luaL_setfuncs(L, intarraylib_m, 0);
  luaL_newlib(L, intarraylib_f);
  return 1;
}
