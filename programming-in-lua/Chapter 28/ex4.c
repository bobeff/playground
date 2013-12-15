/*
  Exercise 28.4:
    
    Implement a library with a modification of transliterate so that the
transliteration table is not given as an argument, but instead is kept by
the library. Your library should offer the following functions:
      
      lib.settrans(table)  -- set the transliteration table
      lib.gettrans()       -- get the transliteration table
      lib.transliterate(s) -- transliterate 's' according to the current table

    Use the registry to keep the transliteration table.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>

#define TRANSLITERATE_TABLE_INDEX "trans_table"

static int l_settrans(lua_State *L)
{
  luaL_checktype(L, 1, LUA_TTABLE);
  lua_setfield(L, LUA_REGISTRYINDEX, TRANSLITERATE_TABLE_INDEX);
  return 0;
}

static int l_gettrans(lua_State *L)
{
  lua_getfield(L, LUA_REGISTRYINDEX, TRANSLITERATE_TABLE_INDEX);
  return 1;
}

static int l_transliterate(lua_State *L)
{
  int i, ssize, rsize = 0;
  luaL_Buffer buffer;
  char *buf;
  const char *s = luaL_checklstring(L, 1, &ssize);

  lua_getfield(L, LUA_REGISTRYINDEX, TRANSLITERATE_TABLE_INDEX);
  luaL_checktype(L, 2, LUA_TTABLE);
  buf = luaL_buffinitsize(L, &buffer, ssize);

  for (i = 0; i < ssize; ++i)
  {
    char chr = s[i];
    lua_pushnil(L);
    while (lua_next(L, 2) != 0)
    {
      luaL_checktype(L, -2, LUA_TSTRING);
      const char *key = lua_tostring(L, -2);
      if (*key == chr)
      {
        if (lua_isstring(L, -1))
          chr = *lua_tostring(L, -1);
        else if (!lua_toboolean(L, -1))
          chr = 0;
        lua_pop(L, 1);
        break;
      }
      lua_pop(L, 1);
    }
    if (chr != 0) buf[rsize++] = chr;
    if (lua_gettop(L) > 2) lua_pop(L, 1);
  }

  luaL_pushresultsize(&buffer, rsize);

  return 1;
}
static const struct luaL_Reg ex4mod[] =
{
  { "settrans", l_settrans },
  { "gettrans", l_gettrans },
  { "transliterate", l_transliterate },
  { NULL, NULL },
};

int LUA_EXPORT luaopen_ex4mod(lua_State *L)
{
  luaL_newlib(L, ex4mod);
  return 1;
}
