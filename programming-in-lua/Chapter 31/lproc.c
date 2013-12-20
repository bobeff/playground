/*
  Exercise 31.2:
    Modify the lproc library so that it can send and receive other primitive
types such as booleans and numbers. (Hint: you only have to modify the
movevalues function.)

  Exercise 31.3:
    Implement in the lproc library a non-blocking send operation.
*/

#ifdef WIN32
  #define LUA_EXPORT __declspec(dllexport)
#else
  #define LUA_EXPORT
#endif

#include <lauxlib.h>
#include <lualib.h>
#include <pthread.h>

typedef struct Proc
{
  lua_State *L;
  pthread_t thread;
  pthread_cond_t cond;
  const char *channel;
  struct Proc *previous, *next;
} 
Proc;

static Proc *waitsend = NULL;
static Proc *waitreceive = NULL;

static pthread_mutex_t kernel_access = PTHREAD_MUTEX_INITIALIZER;

static Proc *getself(lua_State *L)
{
  Proc *p;
  lua_getfield(L, LUA_REGISTRYINDEX, "_SELF");
  p = (Proc*)lua_touserdata(L, -1);
  lua_pop(L, 1);
  return p;
}

static void movevalues(lua_State *send, lua_State *rec)
{
  int i, n = lua_gettop(send);
  
  /* move values to receiver */
  for (i = 2; i <= n; ++i)
  {
    int type = lua_type(send, i);
    switch (type)
    {
    case LUA_TBOOLEAN:
      lua_pushboolean(rec, lua_toboolean(send, i));
      break;
    case LUA_TNUMBER:
      lua_pushnumber(rec, lua_tonumber(send, i));
      break;
    case LUA_TSTRING:
      lua_pushstring(rec, lua_tostring(send, i));
      break;
    default:
      fprintf(stderr, "type '%s' can not be send to other thread\n",
        lua_typename(send, type));
    }
  }
}

static Proc *searchmatch(const char *channel, Proc **list)
{
  Proc *node = *list;
  if (node == NULL) return NULL;

  do
  {
    if (strcmp(channel, node->channel) == 0) /* match ? */
    {
      /* remove node from the list */
      if (*list == node) /* is this node the first element ? */
        *list = (node->next == node) ? NULL : node->next;
      node->previous->next = node->next;
      node->next->previous = node->previous;
      return node;
    }
    node = node->next;
  }
  while (node != *list);

  /* no match */
  return NULL;
}

static void waitonlist(lua_State *L, const char *channel, Proc **list)
{
  Proc *p = getself(L);

  /* link itself at the end of the list */
  if (*list == NULL) /* empty list ? */
  {
    *list = p;
    p->previous = p->next = p;
  }
  else
  {
    p->previous = (*list)->previous;
    p->next = *list;
    p->previous->next = p->next->previous = p;
  }

  p->channel = channel;
  
  do
  {
    /* wait on this condition variable */
    pthread_cond_wait(&p->cond, &kernel_access);
  }
  while (p->channel);
}

static int ll_send(lua_State *L)
{
  Proc *p;
  const char *channel = luaL_checkstring(L, 1);

  pthread_mutex_lock(&kernel_access);

  p = searchmatch(channel, &waitreceive);
  if (p) /* found a matching receiver */
  {
    movevalues(L, p->L); /* move values to receiver */
    p->channel = NULL;   /* mark receiver as not wiating */
    pthread_cond_signal(&p->cond); /* wake it up */
  }
  else
    waitonlist(L, channel, &waitsend);

  pthread_mutex_unlock(&kernel_access);
  return 0;
}

static int ll_send_noblock(lua_State *L)
{
  Proc *p;
  const char *channel = luaL_checkstring(L, 1);

  pthread_mutex_lock(&kernel_access);

  p = searchmatch(channel, &waitreceive);
  if (p) /* found a matching receiver */
  {
    movevalues(L, p->L); /* move values to receiver */
    p->channel = NULL;   /* mark receiver as not wiating */
    pthread_cond_signal(&p->cond); /* wake it up */
  }

  pthread_mutex_unlock(&kernel_access);
  return 0;
}

static int ll_receive(lua_State *L)
{
  Proc *p;
  const char *channel = luaL_checkstring(L, 1);
  lua_settop(L, 1);

  pthread_mutex_lock(&kernel_access);

  p = searchmatch(channel, &waitsend);
  if (p) /* found a matching sender */
  {
    movevalues(p->L, L); /* get values from sender */
    p->channel = NULL;   /* mark sender as not waiting */
    pthread_cond_signal(&p->cond); /* wake it up */
  }
  else
    waitonlist(L, channel, &waitreceive);

  pthread_mutex_unlock(&kernel_access);

  /* return all stack values but channel */
  return lua_gettop(L) - 1;
}

int LUA_EXPORT luaopen_lproc(lua_State *L);

static void *ll_thread(void *arg)
{
  lua_State *L = (lua_State*)arg;
  luaL_openlibs(L); /* open standard libraries */
  luaL_requiref(L, "lproc", luaopen_lproc, 1);
  lua_pop(L, 1);
  if (lua_pcall(L, 0, 0, 0) != 0) /* call main chunk */
    fprintf(stderr, "thread error: %s\n", lua_tostring(L, -1));
  pthread_cond_destroy(&getself(L)->cond);
  lua_close(L);
  return NULL;
}

static int ll_start(lua_State *L)
{
  pthread_t thread;
  const char *chunk = luaL_checkstring(L, 1);
  lua_State *L1 = luaL_newstate();

  if (L1 == NULL)
    luaL_error(L, "unable to create new state\n");

  if (luaL_loadstring(L1, chunk) != 0)
    luaL_error(L, "error starting thread: %s\n", lua_tostring(L1, -1));

  if (pthread_create(&thread, NULL, ll_thread, L1) != 0)
    luaL_error(L, "unable to create new thread\n");

  pthread_detach(thread);
  return 0;
}

static int ll_exit(lua_State *L)
{
  pthread_exit(NULL);
  return 0;
}

static const struct luaL_Reg ll_funcs[] =
{
  { "start", ll_start },
  { "send", ll_send },
  { "send_noblock", ll_send_noblock },
  { "receive", ll_receive },
  { "exit", ll_exit },
  { NULL, NULL },
};

int LUA_EXPORT luaopen_lproc(lua_State *L)
{
  /* create own control block */
  Proc *self = (Proc*)lua_newuserdata(L, sizeof(Proc));
  lua_setfield(L, LUA_REGISTRYINDEX, "_SELF");
  self->L = L;
  self->thread = pthread_self();
  self->channel = NULL;
  pthread_cond_init(&self->cond, NULL);
  luaL_newlib(L, ll_funcs);
  return 1;
}
