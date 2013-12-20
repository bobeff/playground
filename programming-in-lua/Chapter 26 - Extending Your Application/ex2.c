/*
  Modify the function call_va to handle boolean values.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

void error(lua_State *L, const char *format, ...)
{
  va_list argp;
  va_start(argp, format);
  vfprintf(stderr, format, argp);
  va_end(argp);
  lua_close(L);
  exit(EXIT_FAILURE);
}

void loadFile(lua_State *L, const char *fileName)
{
  if (luaL_loadfile(L, fileName) || lua_pcall(L, 0, 0, 0))
    error(L, "%s\n", lua_tostring(L, -1));
}

void call_va(lua_State *L, const char *function, const char *signature, ...)
{
  va_list vl;
  int argumentsCount, resultsCount;

  va_start(vl, signature);
  lua_getglobal(L, function);
  
  for (argumentsCount = 0; *signature; ++argumentsCount)
  {
    luaL_checkstack(L, 1, "too many arguments");
    switch (*signature++)
    {
      case 'b': lua_pushboolean(L, va_arg(vl, int));       break;
      case 'd': lua_pushnumber(L, va_arg(vl, double));     break;
      case 'i': lua_pushinteger(L, va_arg(vl, int));       break;
      case 'u': lua_pushunsigned(L, va_arg(vl, unsigned)); break;
      case 's': lua_pushstring(L, va_arg(vl, char*));      break;
      case '>': goto endargs;
      default: error(L, "invalid option (%c)\n", *(signature - 1));
    }
  }

endargs:
  resultsCount = strlen(signature);
  if (lua_pcall(L, argumentsCount, resultsCount, 0) != LUA_OK)
    error(L, "error calling '%s': %s\n", function, lua_tostring(L, -1));

  resultsCount = -resultsCount;

  while (*signature)
  {
    switch (*signature++)
    {
    case 'b':
      {
        int b = lua_toboolean(L, resultsCount);    
        *va_arg(vl, int*) = b;
      }
      break;
    case 'd':
      {
        int isNumber;
        double d = lua_tonumberx(L, resultsCount, &isNumber);
        if (!isNumber)
          error(L, "wrong result type\n");
        *va_arg(vl, double*) = d;
      }
      break;
    case 'i':
      {
        int isNumber;
        int i = lua_tointegerx(L, resultsCount, &isNumber);
        if (!isNumber)
          error(L, "wrong result type\n");
        *va_arg(vl, int*) = i;
      }
      break;
    case 'u':
      {
        int isNumber;
        unsigned u = lua_tounsignedx(L, resultsCount, &isNumber);
        if (!isNumber)
          error(L, "wrong result type\n");
        *va_arg(vl, unsigned*) = u;
      }
      break;
    case 's':
      {
        const char *s = lua_tostring(L, resultsCount);
        if (s == NULL)
          error(L, "wrong result type\n");
        *va_arg(vl, const char**) = s;
      }
      break;
    default:
      error(L, "invalid option (%c)\n", *(signature - 1));
    }

    ++resultsCount;
  }

  va_end(vl);
}

int main()
{
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  loadFile(L, "ex2.lua");

  int areEqual;
  call_va(L, "foo", "dd>b", 3.4, 3.5, &areEqual);
  printf("%s\n", areEqual ? "true" : "false");
  lua_pop(L, 1);

  int sum, difference;
  call_va(L, "bar", "ii>ii", 10, 20, &sum, &difference);
  printf("sum = %d\tdiffernece = %d\n", sum, difference);
  lua_pop(L, 2);

  lua_close(L);
  return EXIT_SUCCESS;
}
