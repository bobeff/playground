/*
  Exercise 32.1:
    Write a library that allows ascript to limit the total amount of memory
used by its Lua state. It may offer a single function, setlimit, to set that
limit.
    The library should set its own allocation funciton. This function, before
calling the original allocator, checks the total memory in use and returns
NULL if the requested memory exeeds the limit.
    (Hint: the library can use lua_gc to initialize its byte count when it
starts. It also can use the user data of the allocation function to keep its
state: the byte count, the current memory limit, etc.; remember to use the
original user data when calling the original allocation function.)
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>

typedef struct MemLimitUData
{
  size_t mem_limit;
  size_t currently_used;
  lua_Alloc original_alloc;
  void *original_ud;
}
MemLimitUData;

static int l_setlimit(lua_State *L)
{
  MemLimitUData *ud;
  int mem_limit = luaL_checkint(L, 1);

  luaL_argcheck(L, mem_limit >= 0, 1, "Invalid memory limit.");
  lua_getallocf(L, &ud);
  ud->mem_limit = mem_limit;

  return 0;
}

static int l_getlimit(lua_State *L)
{
  MemLimitUData *ud;

  lua_getallocf(L, &ud);
  lua_pushnumber(L, ud->mem_limit);

  return 1;
}

static void *l_alloc(void *ud, void *ptr, size_t osize, size_t nsize)
{
  MemLimitUData *udata = (MemLimitUData*)ud;
  
  if (udata->mem_limit != 0 &&
      udata->mem_limit < udata->currently_used - osize + nsize)
  {
    return NULL;
  }

  udata->currently_used += nsize - osize;
  return udata->original_alloc(udata->original_ud, ptr, osize, nsize);
}

static const luaL_Reg memlimit[] =
{
  { "setlimit", l_setlimit },
  { "getlimit", l_getlimit },
  { NULL, NULL }
};

int LUA_EXPORT luaopen_memlimit(lua_State *L)
{
  MemLimitUData *ud =
    (MemLimitUData*)lua_newuserdata(L, sizeof(MemLimitUData));
  
  ud->mem_limit = 0;
  ud->currently_used =
    lua_gc(L, LUA_GCCOUNT, 0) * 1024 + lua_gc(L, LUA_GCCOUNTB, 0);
  ud->original_alloc = lua_getallocf(L, &ud->original_ud);

  lua_pushvalue(L, -1);
  lua_setfield(L, LUA_REGISTRYINDEX, "memlimit.userdata");

  lua_setallocf(L, l_alloc, ud);
  luaL_newlib(L, memlimit);

  return 1;
}
