/*
  Exercise 30.1:
    Modify the dir_iter function in the directory example so that it closes
the DIR structure when it reaches the end of the traversal. With this change,
the program does not need to wait for the garbage collection to release a
resource that it knows it will not nees anymore.
    (When you close the directory, you should set the address stored in the
userdatum to NULL, to signal to the finilizer that the directory is already
closed. Also, function dir_iter will have to check whether the directory is
not closed before it.)
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>
#include <dirent.h>

#define LIB_META_INDEX "dir"

static int dir_iter(lua_State *L);

static int l_dir(lua_State *L)
{
  const char *path = luaL_checkstring(L, 1);

  /* create userdatum to store DIR address */
  DIR **d = (DIR**)lua_newuserdata(L, sizeof(DIR*));

  /* set its metatable */
  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  /* try to open the given directory */
  *d = opendir(path);
  if (*d == NULL) // error opening the directory ? */
    luaL_error(L, "cannot open %s: %s", path, strerror(errno));

  /* creates and returns the iterator function;
     its sole upvalue, the directory userdatum,
     is already on the top of the stack */
  lua_pushcclosure(L, dir_iter, 1);
  return 1;
}

static int dir_iter(lua_State *L)
{
  DIR **d = (DIR**)lua_touserdata(L, lua_upvalueindex(1));
  if (*d == NULL) return 0;
  struct dirent *entry;
  if ((entry = readdir(*d)) != NULL) {
    lua_pushstring(L, entry->d_name);
    return 1;
  }
  closedir(*d);
  *d = NULL;
  return 0;
}

static int dir_gc(lua_State *L)
{
  DIR *d = *(DIR**)lua_touserdata(L, 1);
  if (d) closedir(d);
  return 0;
}

static const struct luaL_Reg dirlib[] =
{
  {"open", l_dir},
  {NULL, NULL}
};

int LUA_EXPORT luaopen_dir(lua_State *L)
{
  luaL_newmetatable(L, LIB_META_INDEX);
  lua_pushcfunction(L, dir_gc);
  lua_setfield(L, -2, "_gc");
  luaL_newlib(L, dirlib);
  return 1;
}
