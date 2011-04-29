#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <stdlib.h>


void f2k3lua_stackDump(lua_State *L)
{
  int i;
  int top = lua_gettop(L);
  printf("stack: ");
  for (i=1; i <= top; i++)
  { /* repeat for each level */
    int t = lua_type(L, i);
    switch (t)
    {
      case LUA_TSTRING: /* strings */
        printf("\"%s\"", lua_tostring(L, i));
        break;

      case LUA_TBOOLEAN: /* booleans */
        printf(lua_toboolean(L, i) ? "true" : "false");
        break;

      case LUA_TNUMBER: /* numbers */
        printf("%g", lua_tonumber(L, i));
        break;

      default: /* other values */
        printf("%s", lua_typename(L, t));
        break;

    }
    printf("  "); /* put a separator */
  }
  printf("\n"); /* end the listing */
  fflush(stdout);
}

int f2k3lua_luaL_dofile(lua_State *L, char const *filename)
{
  return luaL_dofile(L, filename);
}

void dumpTable(lua_State* L, int idx)
{
  lua_pushnil(L);  /* first key */
  printf("start:\n");
  while (lua_next(L, idx) != 0) {
    /* `key' is at index -2 and `value' at index -1 */
    printf("%s - %s\n",
      lua_tostring(L, -2), lua_typename(L, lua_type(L, -1)));
    lua_pop(L, 1);  /* removes `value'; keeps `key' for next iteration */
  }
  printf("end\n");
}


void f2k3lua_lua_getglobal(lua_State *L, char const *k)
{
  lua_getglobal(L, k);
}

void f2k3lua_lua_setglobal(lua_State *L, char const *k)
{
  lua_setglobal(L, k);
}

char const* f2k3lua_luaL_checkstring(lua_State *L, int n)
{
  return luaL_checkstring(L, n);
}
     

int f2k3lua_lua_istable(lua_State *L, int n)
{
  return lua_istable(L, n);
}


void f2k3lua_lua_pop(lua_State *L, int n)
{
  lua_pop(L, n);
}

void f2k3lua_registersafeprint(lua_State *L, lua_CFunction safePrint)
{
  lua_pushcfunction(L, safePrint);
  lua_setglobal(L, "print");
}

void f2k3lua_opensandboxlibs(lua_State *L)
{
  luaopen_base(L);
  lua_pop(L, -1);
  luaopen_table(L);
  lua_pop(L, -1);
  luaopen_string(L);
  lua_pop(L, -1);
  luaopen_math(L);
  lua_pop(L, -1);
}

void f2k3lua_free(void* ptr)
{
  free(ptr);
}
