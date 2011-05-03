! Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
! Licensed under the MIT license, see LICENSE file.

MODULE lua_c_interfaces_simple
INTERFACE
  !> LUALIB_API lua_State *(luaL_newstate) (void);
  FUNCTION luaL_newstate_c() BIND(C, NAME="luaL_newstate")
    USE ISO_C_BINDING
    TYPE(C_PTR) :: luaL_newstate_c
  END FUNCTION luaL_newstate_c
  !
  !
  !> LUA_API void       (lua_close) (lua_State *L);
  SUBROUTINE lua_close_c(L) BIND(C, NAME="lua_close")
    USE ISO_C_BINDING
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE lua_close_c
  !
  !
  !> LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
  FUNCTION lua_tolstring_c(L, i, length) BIND(C, name="lua_tolstring")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: i
    INTEGER(KIND=C_SIZE_T), INTENT(OUT) :: length
    TYPE(C_PTR):: lua_tolstring_c
  END FUNCTION lua_tolstring_c
  !
  !
  !> void f2k3lua_lua_getglobal(lua_State *L, char const *k);
  SUBROUTINE lua_getglobal_c(L, k) BIND(C, name="f2k3lua_lua_getglobal")
    USE ISO_C_BINDING, ONLY:C_CHAR, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: k
  END SUBROUTINE lua_getglobal_c
  !
  !
  !> void f2k3lua_lua_setglobal(lua_State *L, char const *k);
  SUBROUTINE lua_setglobal_c(L, k) BIND(C, name="f2k3lua_lua_setglobal")
    USE ISO_C_BINDING, ONLY:C_CHAR, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: k
  END SUBROUTINE lua_setglobal_c


  !> int f2k3lua_lua_istable(lua_State *L, int n);
  FUNCTION lua_istable_c(L, n) BIND(C, name="f2k3lua_lua_istable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_istable_c
  END FUNCTION lua_istable_c

  !> int f2k3lua_lua_isnil(lua_State *L, int n);
  FUNCTION lua_isnil_c(L, n) BIND(C, name="f2k3lua_lua_isnil")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_isnil_c
  END FUNCTION lua_isnil_c

  ! LUA_API int (lua_isstring) (lua_State *L, int idx);
  FUNCTION lua_isstring_c(L, n) BIND(C, name="lua_isstring")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_isstring_c
  END FUNCTION lua_isstring_c
  !
  !
  ! > #define lua_pop(L,n) lua_settop(L, -(n)-1)
  SUBROUTINE lua_pop_c(L, n) BIND(C, name="f2k3lua_lua_pop")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
  END SUBROUTINE lua_pop_c
  !
  !
  !> LUA_API void  (lua_pushnil) (lua_State *L);
  SUBROUTINE lua_pushnil_c(L) BIND(C, name="lua_pushnil")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE lua_pushnil_c
  !
  !
  !> LUA_API void  (lua_pushstring) (lua_State *L, const char *s);
  SUBROUTINE lua_pushstring_c(L, str) BIND(C, name="lua_pushstring")
    USE ISO_C_BINDING, ONLY:C_CHAR, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: str
  END SUBROUTINE lua_pushstring_c
  !
  !
  !> LUA_API int             (lua_isnumber) (lua_State *L, int idx);
  FUNCTION lua_isnumber_c(L, n) BIND(C, name="lua_isnumber")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_isnumber_c
  END FUNCTION lua_isnumber_c
  !
  !
  !> LUA_API lua_Number      (lua_tonumber) (lua_State *L, int idx);
  FUNCTION lua_tonumber_c(L, n) BIND(C, name="lua_tonumber")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default
    REAL(KIND=C_DOUBLE) :: lua_tonumber_c 
  END FUNCTION lua_tonumber_c
  !
  !
  !> LUA_API int   (lua_next) (lua_State *L, int idx);
  FUNCTION lua_next_c(L, n) BIND(C, name="lua_next")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_next_c
  END FUNCTION lua_next_c
  !
  !
  !> LUA_API void  (lua_getfield) (lua_State *L, int idx, const char *k);
  SUBROUTINE lua_getfield_c(L, n, k) BIND(C, name="lua_getfield")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: k 
  END SUBROUTINE lua_getfield_c
  !
  !
  !> int f2k3lua_luaL_dofile(lua_State *L, char const *filename);
  FUNCTION luaL_dofile_c(L, fileName) BIND(C, name="f2k3lua_luaL_dofile")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: fileName
    INTEGER(KIND=C_INT) :: luaL_dofile_c
  END FUNCTION luaL_dofile_c
  !
  !
  !> char const* f2k3lua_luaL_checkstring(lua_State *L, int n);
  FUNCTION luaL_checkstring_c(L, n) BIND(C, name="f2k3lua_luaL_checkstring")
    USE ISO_C_BINDING, ONLY:C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    TYPE(C_PTR) :: luaL_checkstring_c
  END FUNCTION luaL_checkstring_c
  !
  !
  !> void f2k3lua_opensandboxlibs(lua_State *L);
  SUBROUTINE f2k3lua_opensandboxlibs_c(L) BIND(C, name="f2k3lua_opensandboxlibs")
    USE ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE f2k3lua_opensandboxlibs_c
  !
  !
  !> void f2k3lua_registersafeprint(lua_State *L, lua_CFunction cfunction)
  SUBROUTINE f2k3lua_registersafeprint_c(L, functioPtr) BIND(C, name="f2k3lua_registersafeprint")
    USE ISO_C_BINDING, ONLY:C_PTR, C_FUNPTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    TYPE(C_FUNPTR), VALUE :: functioPtr
  END SUBROUTINE f2k3lua_registersafeprint_c
  !
  !
  !> void stackDump(lua_State *L)
  SUBROUTINE dumpStack_c(L) BIND(C, name="f2k3lua_stackDump")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE dumpStack_c
  !
END INTERFACE
CONTAINS
END MODULE lua_c_interfaces_simple












