/*
Exercise 25.2:
    Assume the stack is empty.What will be its contents after the following
sequence of calls ?
  lua_pushnumber(L, 3.5)
  lua_pushstring(L, "hello")
  lua_pushnil(L)
  lua_pushvalue(L, -2)
  lua_remove(L, 1)
  lua_insert(L, -2)

Exercise 25.3:
    Use the simple stand - alone interpreter and the stackDump function to check
your answer to the previous exercise.
*/

#include <stdio.h>
#include "lua.h"
#include "lauxlib.h"

void stackDump(lua_State *L)
{
  int i, top = lua_gettop(L);
  for (i = 1; i <= top; ++i)
  {
    int t = lua_type(L, i);
    switch (t)
    {
    case LUA_TSTRING:
      printf("'%s'", lua_tostring(L, i));
      break;
    case LUA_TBOOLEAN:
      printf(lua_toboolean(L, i) ? "true" : "false");
      break;
    case LUA_TNUMBER:
      printf("%g", lua_tonumber(L, i));
      break;
    default:
      printf("%s", lua_typename(L, t));
    }
    printf(" ");
  }
  printf("\n");
}

int main(void)
{
  lua_State* L = luaL_newstate();
  
  lua_pushnumber(L, 3.5);
  stackDump(L); /* 3.5 */

  lua_pushstring(L, "hello");
  stackDump(L); /* 3.5 'hello' */

  lua_pushnil(L);
  stackDump(L); /* 3.5 'hello' nil */

  lua_pushvalue(L, -2);
  stackDump(L); /* 3.5 'hello' nil 'hello' */

  lua_remove(L, 1);
  stackDump(L); /* 'hello' nil 'hello' */

  lua_insert(L, -2);
  stackDump(L); /* 'hello' 'hello' nil */
 
  return 0;
}
