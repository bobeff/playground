/*
  Exercise 28.1:
    Implement a filter function in C. It should receive a list and a predicate
and return a new list with all elements from the given list that satisfy the
preficate:
    t = filter({1, 3, 20, -4, 5}, function (x) return x < 5 end)
    -- t = {1, 3, -4}

  Exercise 28.2:
    Modify the function l_split so that it can work with strings containing
zeroes.

  Exercise 28.3:
    Reimplement the transliterate function form exercise 21.3 in C.
    
  Exercise 21.3:
    Write a transliterate function. This function receives a string and
replaces each character in that string by another character, according to a
table given as a second argument. If the table maps 'a' to 'b', the function
should replace any occurence of 'a' by 'b'. If the table maps 'a' to false,
the function should remove occurences of 'a' from the resulting string.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <string.h>
#include <lauxlib.h>

static int l_filter(lua_State *L)
{
  int i, j, n;
  luaL_checktype(L, 1, LUA_TTABLE);
  luaL_checktype(L, 2, LUA_TFUNCTION);
  lua_newtable(L);
  n = luaL_len(L, 1);
  for (i = 1, j = 0; i <= n; ++i)
  {
    lua_pushvalue(L, 2);
    lua_rawgeti(L, 1, i);
    lua_call(L, 1, 1);
    if (lua_toboolean(L, -1))
    {
      lua_rawgeti(L, 1, i);
      lua_rawseti(L, 3, ++j);
    }
    lua_pop(L, 1);
  }
  return 1;
}

static int l_split(lua_State *L)
{
  int ssize, i = 1;
  const char *s = luaL_checklstring(L, 1, &ssize);
  const char *sep = luaL_checkstring(L, 2);
  const char *e;

  lua_newtable(L);

  while ((e = memchr(s, *sep, ssize)) != NULL)
  {
    lua_pushlstring(L, s, e - s);
    lua_rawseti(L, -2, i++);
    ssize -= e - s + 1;
    s = e + 1;
  }

  lua_pushlstring(L, s, ssize);
  lua_rawseti(L, -2, i);

  return 1;
}

static int l_transliterate(lua_State *L)
{
  int i, ssize, rsize = 0;
  luaL_Buffer buffer;
  char *buf;
  const char *s = luaL_checklstring(L, 1, &ssize);

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

static const struct luaL_Reg ex28mod[] =
{
  { "filter", l_filter },
  { "split", l_split },
  { "transliterate", l_transliterate },
  { NULL, NULL },
};

int LUA_EXPORT luaopen_ex28mod(lua_State *L)
{
  luaL_newlib(L, ex28mod);
  return 1;
}
