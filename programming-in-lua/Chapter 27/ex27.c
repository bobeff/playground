/*
Exercise 27.1:
  Write a sumation function, in C, that computes the sum of its variable
number of arguments:
    print(sumation()) --> 0
    print(sumation(2.3, 5.4)) --> 7.7
    print(sumation(2.3, 5.4, -34)) --> -26.3
    print(sumation(2.3, 5.4, {}))
      --> stdin:1: bad argument to 'sumation' (number expected, got table)

Exercise 27.2:
  Implement a function equivalent to table.pack, from the standard library.

Exercise 27.3:
  Write a function that receives any number of parameters and returns tham in
reverse order.

Exercise 27.4:
  Write a funciton foreach that receives a table and a function and calls that
function for each pair key-value in the table.
    foreach({x = 10, y = 20}, print)
      --> x  10
      --> y  20

Exercise 27.5:
  Rewrite function foreach, from the previous exercise, so that the function
being called can yield.

Exercise 27.6:
  Create a C module with all funcitons from the previous exercises.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>

static int l_sumation(lua_State *L)
{
  int i, n = lua_gettop(L);
  lua_Number sum = 0;
  for (i = 1; i <= n; ++i)
    sum += luaL_checknumber(L, i);
  lua_pushnumber(L, sum);
  return 1;
}

static int l_pack(lua_State *L)
{
  int i, n = lua_gettop(L);
  lua_createtable(L, n, 1);
  for (i = 1; i <= n; ++i)
  {
    lua_pushinteger(L, i);
    lua_pushvalue(L, i);
    lua_rawset(L, -3);
  }
  lua_pushlstring(L, "n", 1);
  lua_pushinteger(L, n);
  lua_rawset(L, -3);
  return 1;
}

static int l_reverse(lua_State *L)
{
  int i, n = lua_gettop(L);
  for (i = 1; i <= n - 1; ++i)
    lua_insert(L, i);
  return n;
}

int foreach_cont(lua_State *L);

static int foreach_main(lua_State *L)
{
  while (lua_next(L, 1) != 0)
  {
    lua_pushvalue(L, 2);
    lua_pushvalue(L, 3);
    lua_pushvalue(L, 4);
    lua_callk(L, 2, 0, 0, foreach_cont);
    lua_pop(L, 1);
  }
  return 0;
}

static int foreach_cont(lua_State *L)
{
  lua_pop(L, 1);
  return foreach_main(L);
}

static int l_foreach(lua_State *L)
{
  luaL_checktype(L, 1, LUA_TTABLE);
  luaL_checktype(L, 2, LUA_TFUNCTION);
  lua_pushnil(L);
  return foreach_main(L);
}

static const struct luaL_Reg ex27mod[] =
{
  { "sumation", l_sumation },
  { "pack", l_pack },
  { "reverse", l_reverse },
  { "foreach", l_foreach },
  { NULL, NULL },
};

int LUA_EXPORT luaopen_ex27mod(lua_State *L)
{
  luaL_newlib(L, ex27mod);
  return 1;
}
