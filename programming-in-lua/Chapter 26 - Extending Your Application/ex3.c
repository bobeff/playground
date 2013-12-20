/*
  Let us suppose a program that needs to monitor several weather stations.
Internally, it uses a four-byte string to represent each station, and there is
configuration file to map each string to the actual URL, of the corresponding
station. A Lua configuration file could do this mapping in several ways:
    * A bunch of global variables, one for each station.
    * One table mapping string codes to URLs.
    * One function mapping string codes to URLs.

  Implement the different kinds of mappings.
*/

#include <stdio.h>
#include <stdlib.h>
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

char *getStringValue(lua_State *L)
{
  char *ret;
  const char *res;
  unsigned len;

  if (!lua_isstring(L, -1))
    error(L, "station URL should be a string\n");

  res = lua_tostring(L, -1);
  len = strlen(res);
  ret = malloc(len + 1);
  strcpy(ret, res);
  lua_pop(L, -1);

  return ret;
}

char *getFromGlobal(lua_State *L, const char *stationName)
{
  lua_getglobal(L, stationName);
  return getStringValue(L);
}

char *getFromTable(lua_State *L, const char *stationName)
{
  char *ret;

  lua_getglobal(L, "stations");
  if (!lua_istable(L, -1))
    error(L, "'stations' must be a table\n");

  lua_getfield(L, -1, stationName);
  ret = getStringValue(L);
  lua_pop(L, -1);

  return ret;
}

char *getFromFunction(lua_State *L, const char *stationName)
{
  lua_getglobal(L, "getstation");
  if (!lua_isfunction(L, -1))
    error(L, "'getstation' must be a function\n");

  lua_pushstring(L, stationName);
  if (lua_pcall(L, 1, 1, 0) != LUA_OK)
  {
    error(L, "error running function 'getstation': %s\n",
      lua_tostring(L, -1));
  }

  return getStringValue(L);
}

void runTest(lua_State *L, char*(*fn)(lua_State*, const char*))
{
  char *station1 = fn(L, "sta1");
  char *station2 = fn(L, "sta2");
  printf("%s\n", station1);
  printf("%s\n", station2);
  free(station1);
  free(station2);
}

int main()
{
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  loadFile(L, "ex3.lua");

  runTest(L, getFromGlobal);
  runTest(L, getFromTable);
  runTest(L, getFromFunction);

  lua_close(L);
  return EXIT_SUCCESS;
}
