/*
  Write a C program that reads a Lua file defining a function f from numbers
to numbers and plots that function.
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <math.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#define PI 3.14159

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

lua_Number call(lua_State *L, const char *functionName, lua_Number x)
{
  int isNumber;
  lua_Number y;

  lua_getglobal(L, functionName);
  lua_pushnumber(L, x);
  if (lua_pcall(L, 1, 1, 0) != LUA_OK)
  {
    error(L, "Error running function '%s': %s\n", functionName,
      lua_tostring(L, -1));
  }

  y = lua_tonumberx(L, -1, &isNumber);
  if (!isNumber)
    error(L, "Function '%s' must return a number.\n", functionName);
  lua_pop(L, 1);

  return y;
}


void plot(lua_State *L, const char *functionName,
  lua_Number lower, lua_Number upper, lua_Number step)
{
  unsigned i, minElemIndex, count =
    (unsigned)(fabs(lower - upper) / step + 1);
  lua_Number x, y, *buffer = malloc(sizeof(lua_Number) * count);

  for (i = 0, x = lower; i < count; x += step, ++i)
    buffer[i] = call(L, functionName, x);

  minElemIndex = 0;
  for (i = 1; i < count; ++i)
    if (buffer[minElemIndex] > buffer[i])
      minElemIndex = i;

  for (i = 0, x = lower; i < count; x += step, ++i)
  {
    printf("%.2f\t", x);
    for (y = buffer[minElemIndex]; y <= buffer[i]; y += step)
      printf("*");
    printf("\n");
  }

  free(buffer);
}

int main(void)
{
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  loadFile(L, "ex1.lua");

  printf("y = foo(x)\n");
  plot(L, "foo", -2 * PI, 2 * PI, 0.1);

  printf("\ny = bar(x)\n");
  plot(L, "bar", -5, 5, 1);

  lua_close(L);
  return EXIT_SUCCESS;
}
