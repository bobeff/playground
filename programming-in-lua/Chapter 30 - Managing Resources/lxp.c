/*
  Exercise 30.2:
    In the lxp example, the handler for start elements receives a table with
the element attribures. In that table, the original order in which the
attributes appear inside the element is lost. How can you pass this information
to the callback ?

  Exercise 30.3:
    In the lxp example, we used user values to associate the callback table
with the userdatum that represents a parser. This choice created a small
problem, because what the C callbacks receive is the lxp_userdata structure,
and that structure does not offer direct access to the table. We solved this
problem by storing the callback table at a fixed stack index during the parse
of each fragment.
    An alternative design would be to associate the callback table with the
userdatum through references (Section 28.3): we create a reference to the
callback table and store the reference (an integer) in the lxp_userdata
structure. Implement this alternative. Do not forget to release the reference
when closing the parser.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#define XML_STATIC

#include <lauxlib.h>
#include <expat.h>

#define LIB_META_INDEX "Expat"

typedef struct lxp_userdata
{
  XML_Parser parser;
  lua_State *L;
  int callbackTableRef;
}
lxp_userdata;

/* forward declarations for callback functions */
static void f_StartElement(void *ud, const char *name, const char **attr);
static void f_CharData(void *ud, const char *s, int len);
static void f_EndElement(void *ud, const char *name);

static int lxp_make_parser(lua_State *L)
{
  XML_Parser p;

  /* create a parser object */
  lxp_userdata *xpu = (lxp_userdata*)lua_newuserdata(L, sizeof(lxp_userdata));

  /* pre-initialize it, in case of error */
  xpu->parser = NULL;

  /* set its metatable */
  luaL_getmetatable(L, LIB_META_INDEX);
  lua_setmetatable(L, -2);

  /* create the Expat parser */
  p = xpu->parser = XML_ParserCreate(NULL);
  if (!p)
    luaL_error(L, "XML_ParserCreate fialed");

  /* check and store the callback table */
  luaL_checktype(L, 1, LUA_TTABLE);
  lua_pushvalue(L, 1);
  xpu->callbackTableRef = luaL_ref(L, LUA_REGISTRYINDEX);

  xpu->L = L;

  /* configure Expat parser */
  XML_SetUserData(p, xpu);
  XML_SetElementHandler(p, f_StartElement, f_EndElement);
  XML_SetCharacterDataHandler(p, f_CharData);
  return 1;
}

static int lxp_parse(lua_State *L)
{
  int status;
  size_t len;
  const char *s;
  lxp_userdata *xpu;

  /* get and check first element (should be a parser) */
  xpu = (lxp_userdata*)luaL_checkudata(L, 1, LIB_META_INDEX);

  /* check whether it is not closed */
  luaL_argcheck(L, xpu->parser != NULL, 1, "parser is closed");

  /* get second argument (a string) */
  s = luaL_optlstring(L, 2, NULL, &len);

  /* call Expat to parse string */
  status = XML_Parse(xpu->parser, s, (int)len, s == NULL);

  /* return error code */
  lua_pushboolean(L, status);
  return 1;
}

static void f_CharData(void *ud, const char *s, int len)
{
  lxp_userdata *xpu = (lxp_userdata*)ud;
  lua_State *L = xpu->L;

  /* get handler */
  lua_rawgeti(L, LUA_REGISTRYINDEX, xpu->callbackTableRef);
  lua_getfield(L, -1, "CharacterData");
  if (lua_isnil(L, -1)) /* no handler ? */
  {
    lua_pop(L, 1);
    return;
  }

  lua_pushvalue(L, 1); /* push the parser (self) */
  lua_pushlstring(L, s, len); /* push char data */
  lua_call(L, 2, 0); /* call the handler */
}

static void f_EndElement(void *ud, const char *name)
{
  lxp_userdata *xpu = (lxp_userdata*)ud;
  lua_State *L = xpu->L;

  /* get handler */
  lua_rawgeti(L, LUA_REGISTRYINDEX, xpu->callbackTableRef);
  lua_getfield(L, -1, "EndElement");
  if (lua_isnil(L, -1)) /* no handler ? */
  {
    lua_pop(L, 1);
    return;
  }

  lua_pushvalue(L, 1); /* push the parser (self) */
  lua_pushstring(L, name); /* push tag name */
  lua_call(L, 2, 0); /* call the handler */
}

static void f_StartElement(void *ud, const char *name, const char **attr)
{
  int i;
  lxp_userdata *xpu = (lxp_userdata*)ud;
  lua_State *L = xpu->L;

  /* get handler */
  lua_rawgeti(L, LUA_REGISTRYINDEX, xpu->callbackTableRef);
  lua_getfield(L, -1, "StartElement");
  if (lua_isnil(L, -1)) /* no handler ? */
  {
    lua_pop(L, 1);
    return;
  }

  lua_pushvalue(L, 1); /* push the parser (self) */
  lua_pushstring(L, name); /* push tag name */

  /* create and fill the attribute table */
  lua_newtable(L);
  for (i = 1; *attr; attr += 2, ++i)
  {
    lua_pushstring(L, *(attr + 1));
    lua_setfield(L, -2, *attr); /* table[*attr] = *(attr + 1) */
    
    /* set sequence indices to point to table keys in the original order */
    lua_pushstring(L, *attr);
    lua_rawseti(L, -2, i);
  }

  lua_call(L, 3, 0); /* call the handler */
}

static int lxp_close(lua_State *L)
{
  lxp_userdata *xpu = (lxp_userdata*)luaL_checkudata(L, 1, LIB_META_INDEX);

  /* free Expat parser (if there is one) */
  if (xpu->parser)
  {
    XML_ParserFree(xpu->parser);
    luaL_unref(L, LUA_REGISTRYINDEX, xpu->callbackTableRef);
  }
  xpu->parser = NULL;
  return 0;
}

static const struct luaL_Reg lxp_meths[] =
{
  { "parse", lxp_parse },
  { "close", lxp_close },
  { "__gc", lxp_close },
  { NULL, NULL }
};

static const struct luaL_Reg lxp_funcs[] =
{
  { "new", lxp_make_parser },
  { NULL, NULL }
};

int LUA_EXPORT luaopen_lxp(lua_State *L)
{
  /* create metatable */
  luaL_newmetatable(L, LIB_META_INDEX);

  /* matatable.__index = metatable */
  lua_pushvalue(L, -1);
  lua_setfield(L, -2, "__index");

  /* register methods */
  luaL_setfuncs(L, lxp_meths, 0);

  /* register functions (only lxp.new) */
  luaL_newlib(L, lxp_funcs);
  return 1;
}
