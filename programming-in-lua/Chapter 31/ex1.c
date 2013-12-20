/*
  Exercise 31.1:
    As we saw, if a function calls lua_yield (the version with nocontinuation),
control returns to the function that called it when the thread resumes again.
What values does the calling function receives as results from that call ?
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  LUA_EXPORT
#endif

#include <lauxlib.h>

static int l_fooc(lua_State *L)
{
  lua_pushstring(L, "foo1");
  lua_pushinteger(L, 13);
  return lua_yield(L, 2);
}

static const struct luaL_Reg ex1mod[] =
{
  { "fooc", l_fooc },
  { NULL, NULL }
};

int LUA_EXPORT luaopen_ex1mod(lua_State *L)
{
  luaL_newlib(L, ex1mod);
  return 1;
}
