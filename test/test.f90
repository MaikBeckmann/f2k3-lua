! Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
! Licensed under the MIT license, see LICENSE file.

PROGRAM test
  USE f2k3_lua_simple
  USE ISO_C_BINDING
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  !
  TYPE(C_PTR) :: L, CString
  INTEGER :: error, ioStat, i
  LOGICAL :: file_exists
  CHARACTER(LEN=1024) :: buf
  ! ====================================================================
  INQUIRE(FILE="conf.lua", EXIST=file_exists)
  IF( .NOT. file_exists ) THEN
    PRINT *, "[ERROR] file conf.lua doesn't exist."
    CALL p_reportExitCode(1)    
    GOTO 9999
  END IF
  !
  CALL luaL_newstate(L)
  CALL f2k3lua_registersafeprint(L)
  CALL f2k3lua_opensandboxlibs(L);
  IF( error > 0 ) GOTO 9999
  CALL luaL_dofile(L, "conf.lua", error)
  IF(error > 0) THEN
    buf = " "
    CALL lua_tostring(L, -1, buf)
    print * , TRIM(buf)
    GOTO 9999
  END IF
  CALL lua_getglobal(L, "buzz")
  IF(.NOT. lua_isstring(L, -1) ) THEN
    CALL stackDump(L)
    PRINT *, "expected string, got something else"
    GOTO 9999
  END IF
  buf = " "
  CALL lua_tostring(L, -1, buf)
  CALL lua_pop(L, 1) ! pop string from lua stack

  CALL lua_getglobal(L, "answer")
  IF(.NOT. lua_isnumber(L, -1) ) THEN
    CALL stackDump(L)
    PRINT *, "expected string, got something else"
    GOTO 9999
  END IF
  print *, lua_tonumber(L, -1)
  CALL lua_pop(L, 1) ! pop number from lua stack
  CALL stackDump(L)

  CALL lua_getglobal(L, "myArray")
  IF(.NOT. lua_istable(L, -1) ) THEN
    CALL stackDump(L)
    PRINT *, "expected table, got something else"
    GOTO 9999
  END IF
  CALL lua_pushnil(L)
  DO WHILE(lua_next(L, -2))
    CALL lua_pop(L, 1)
    CALL stackDump(L)
  END DO
  CALL lua_pop(L, 1) ! pop table
  !
  !
  ! <myTable>
  ! Iterate:
  CALL lua_getglobal(L, "myTable")
  IF(.NOT. lua_istable(L, -1) ) THEN
    CALL stackDump(L)
    PRINT *, "expected table, got something else"
    GOTO 9999
  END IF
  CALL lua_pushnil(L)
  DO WHILE(lua_next(L, -2))
    CALL stackDump(L)
    CALL lua_pop(L, 1)
  END DO
  !
  CALL lua_getfield(L, -1, "foo")
  CALL stackDump(L)  
  CALL lua_pop(L, 1) ! pop value of "foo"
  CALL lua_getfield(L, -1, "fuzz")
  CALL stackDump(L)
  CALL lua_pop(L, 1) ! pop value of "fuzz"
  CALL stackDump(L)
  !CALL lua_pop(L, 1) ! pop table
  ! </myTable>
  !
  !
  !
  !
9999 CONTINUE
  CALL lua_close(L)  
  print *, error
  !
CONTAINS
  SUBROUTINE p_reportExitCode(fehler)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: fehler
    !
    IF ( fehler > 0 ) THEN
      STOP 1
    ELSE
      PRINT *,0
    END IF
  END SUBROUTINE p_reportExitCode
END PROGRAM test
