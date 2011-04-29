MODULE f2k3_lua_simple
  PRIVATE :: p_characterToCharArray

CONTAINS

SUBROUTINE p_characterToCharArray(fstr, charArray, error)
  USE ISO_C_BINDING, ONLY: C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: fstr
  CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(:), INTENT(OUT) :: charArray
  INTEGER, INTENT(OUT) :: error
  !
  INTEGER :: i, lenTrim
  ! ==================================================================== 
  lenTrim = LEN_TRIM(fstr)
  error = 0
  IF(SIZE(charArray) < (lenTrim + 1)) THEN
    error = 1
    RETURN
  END IF
  !
  DO i = 1, lenTrim
    charArray(i) = fstr(i:i)
  END DO
  charArray(lenTrim+1) = C_NULL_CHAR
END SUBROUTINE p_characterToCharArray

SUBROUTINE C_F_CSTR(cptr, ret, length)
  USE ISO_C_BINDING
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: cptr
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: ret
  INTEGER, INTENT(OUT) :: length
  !
  INTERFACE
    FUNCTION strlen_c(cstr) BIND(C,NAME="strlen")
      USE ISO_C_BINDING
      IMPLICIT NONE
      INTEGER(KIND=C_SIZE_T) :: strlen_c
      TYPE(C_PTR), VALUE :: cstr
    END FUNCTION strlen_c
  END INTERFACE
  ! ====================================================================
  length = strlen_c(CPTR)
  CALL C_F_POINTER(FPTR=ret, CPTR=cptr, SHAPE=(/ length /))
END SUBROUTINE

SUBROUTINE luaL_newstate(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: luaL_newstate_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  !
  L = luaL_newstate_c()  
END SUBROUTINE luaL_newstate


SUBROUTINE lua_close(L)
  USE ISO_C_BINDING
  USE lua_c_interfaces_simple, ONLY: lua_close_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  !
  CALL lua_close_c(L)
END SUBROUTINE lua_close

SUBROUTINE lua_tostring(L, n, str)
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T, C_CHAR
  USE lua_c_interfaces_simple, ONLY: lua_tolstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(OUT) :: str
  !
  TYPE(C_PTR) :: cstr
  INTEGER(KIND=C_SIZE_T) :: length
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: fstr
  !
  INTEGER :: i, cstrLength
  ! ==================================================================== 
  cstr = lua_tolstring_c(L, n, length)
  CALL C_F_CSTR(cstr, fstr, cstrLength)
  !
  DO i = 1, length
    str(i:i) = fstr(i)
  END DO
END SUBROUTINE lua_tostring

SUBROUTINE lua_getglobal(L, key)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE lua_c_interfaces_simple, ONLY: lua_getglobal_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(key)+1) :: c_key
  INTEGER :: i, error
  ! ====================================================================
  CALL p_characterToCharArray(key, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_getglobal_c(L,  c_key)
  !
END SUBROUTINE lua_getglobal

SUBROUTINE lua_setglobal(L, key)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE lua_c_interfaces_simple, ONLY: lua_setglobal_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(key)+1) :: c_key
  INTEGER :: i, error
  ! ====================================================================
  CALL p_characterToCharArray(key, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_setglobal_c(L,  c_key)
  !
END SUBROUTINE lua_setglobal

FUNCTION lua_istable(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_istable_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_istable
  !
  lua_istable = lua_istable_c(L,  n) /= 0
END FUNCTION lua_istable


FUNCTION lua_isstring(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_isstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isstring
  !
  lua_isstring = lua_isstring_c(L,  n) /= 0
END FUNCTION lua_isstring

FUNCTION lua_isnumber(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_isnumber_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnumber
  !
  lua_isnumber = lua_isnumber_c(L,  n) /= 0
END FUNCTION lua_isnumber

FUNCTION lua_tonumber(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_tonumber_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  DOUBLE PRECISION :: lua_tonumber
  !
  lua_tonumber = lua_tonumber_c(L, n)
END FUNCTION lua_tonumber

SUBROUTINE lua_pop(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_pop_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  !
  CALL lua_pop_c(L, n)
END SUBROUTINE lua_pop

SUBROUTINE lua_pushnil(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_pushnil_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  !
  CALL lua_pushnil_c(L)
END SUBROUTINE lua_pushnil

SUBROUTINE lua_pushstring(L, str)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR
  USE lua_c_interfaces_simple, ONLY: lua_pushstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  CHARACTER(LEN=*), INTENT(IN) :: str
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(str)+1) :: cstr
  INTEGER :: error
  ! ====================================================================
  CALL p_characterToCharArray(str, cstr, error)
  ! error cannot be /= 0
  !
  CALL lua_pushstring_c(L, cstr)
  !
END SUBROUTINE lua_pushstring


FUNCTION lua_next(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: lua_next_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_next
  !
  lua_next = lua_next_c(L, n) /= 0
END FUNCTION lua_next


SUBROUTINE lua_getfield(L, n, k)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE lua_c_interfaces_simple, ONLY: lua_getfield_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(IN) :: k
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(k)+1) :: c_key
  INTEGER :: error
  ! ====================================================================
  CALL p_characterToCharArray(k, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_getfield_c(L, n, c_key)
END SUBROUTINE lua_getfield

SUBROUTINE luaL_dofile(L, fileName, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE lua_c_interfaces_simple, ONLY: luaL_dofile_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: fileName
  INTEGER, INTENT(OUT) :: error
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(fileName)+1) :: cFileName
  INTEGER :: i, lenTrim
  ! ====================================================================
  CALL p_characterToCharArray(fileName, cFileName, error)
  ! error cannot be /= 0
  !
  error = luaL_dofile_c(L, cFileName)
END SUBROUTINE luaL_dofile

SUBROUTINE stackDump(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE lua_c_interfaces_simple, ONLY: dumpStack_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  !
  CALL dumpStack_c(L)
END SUBROUTINE stackDump

SUBROUTINE luaL_checkstring(L, n, fstr, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR
  USE lua_c_interfaces_simple, ONLY: luaL_checkstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(OUT) :: fstr
  INTEGER, INTENT(OUT) :: error
  !
  TYPE(C_PTR) :: cStr
  CHARACTER(LEN=1), DIMENSION(:), POINTER :: chars
  INTEGER :: i, length
  ! ====================================================================
  error = 0
  cStr = luaL_checkstring_c(L, n)
  CALL C_F_CSTR(cStr, chars, length)
  IF (LEN(fstr) < length) THEN
    error = 1
    RETURN
  END IF
  DO i = 1, length
    fstr(i:i) = chars(i)
  END DO
END SUBROUTINE luaL_checkstring


FUNCTION safePrint(L) BIND(C)
  USE ISO_C_BINDING, ONLY: C_PTR, C_INT
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L
  INTEGER(KIND=C_INT) :: safePrint
  !
  CHARACTER(LEN=4096) :: str
  INTEGER :: error
  ! ====================================================================
  str = " "
  CALL luaL_checkstring(L, -1, str, error)
  IF(error > 0 ) THEN
    print *, "TODO: push error values so assert(print(...)) works as expected"
    safePrint = 0
  ELSE
    print *, TRIM(str)
    safePrint = 0
  END IF
END FUNCTION safePrint

SUBROUTINE f2k3lua_registersafeprint(L)
  USE ISO_C_BINDING, ONLY: C_PTR, C_FUNLOC
  USE lua_c_interfaces_simple, ONLY: f2k3lua_registersafeprint_c
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L
  !
  CALL f2k3lua_registersafeprint_c(L, C_FUNLOC(safePrint))
END SUBROUTINE f2k3lua_registersafeprint

SUBROUTINE f2k3lua_opensandboxlibs(L)
  USE ISO_C_BINDING, ONLY: C_PTR, C_FUNLOC
  USE lua_c_interfaces_simple, ONLY: f2k3lua_opensandboxlibs_c
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L
  !
  CALL f2k3lua_opensandboxlibs_c(L)
END SUBROUTINE f2k3lua_opensandboxlibs

END MODULE f2k3_lua_simple
