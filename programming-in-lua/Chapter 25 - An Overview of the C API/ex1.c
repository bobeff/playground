/*
Exercise 25.1:
    Compile and run the simple stand-alone interpreter.
*/

#include <stdio.h>
#include <string.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

int main(void)
{
  char buffer[256];
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);

  while (fgets(buffer, sizeof(buffer), stdin) != NULL)
  {
    int error = luaL_loadstring(L, buffer) || lua_pcall(L, 0, 0, 0);
    if (error)
    {
      fprintf(stderr, "%s\n", lua_tostring(L, -1));
      lua_pop(L, -1);
    }
  }

  lua_close(L);

  return 0;
}
