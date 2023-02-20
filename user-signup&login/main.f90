! User Sign-Up and Login system ! 
  ! This is quite buggy, but I don't care.
  ! This was also amazingly fun to work on!
  ! I learnt so much about Fortran while working on this, like the "type" statement.
  ! Which act like classes in any OOP language, or structures in C.
  ! So I commented nothing like always, enjoy!

PROGRAM main

 use user_mod
 use util
 use execute_cmd

 IMPLICIT none
 ! ⤓  All the variable declarations
 
 ! Type statements
 TYPE (User), DIMENSION(524) :: users

 ! Booleans
 LOGICAL :: program_isRunning = .TRUE.
 LOGICAL :: is_loggedIn = .FALSE.

 ! Strings
 CHARACTER(:), ALLOCATABLE :: user_name
 CHARACTER(:), ALLOCATABLE :: cmd
 CHARACTER(:), ALLOCATABLE :: current_user

 ! String arrays
 CHARACTER(30), DIMENSION(6) :: options = ["Add User   ", "List Users ", "Remove User", "Open User  ", "Logged-Into", "Log Out    "]

 ! Characters
 CHARACTER :: program_char

 ! Integers
 INTEGER :: user_pin
 INTEGER :: arrow_index = 1
 INTEGER :: user_count = 0
 INTEGER :: users_index = 1

 DO WHILE (program_isRunning)
  CALL system("clear")
  CALL list_options

  CALL draw_line
  print *, "   Controls   "
  CALL draw_line

  print *, "[S] | [s] -> Go down"
  print *, "[W] | [w] -> Go up"
  print *, "[Q] | [q] -> Quit program"
  print *, "[E] | [e] -> Execute command / option"

  CALL nl
  read(*,*) program_char

  SELECT CASE (program_char)
   case ('e', 'E')

    IF (.not. ALLOCATED(cmd)) THEN
     ALLOCATE(CHARACTER(LEN(options(arrow_index))) :: cmd)
    ELSE
     DEALLOCATE(cmd)
     ALLOCATE(CHARACTER(LEN(options(arrow_index))) :: cmd)
    ENDIF

    cmd = options(arrow_index)
    CALL execute(cmd, LEN(cmd), users, user_count, users_index, current_user, is_loggedIn)

   case ('q', 'Q')
    print *, "goodbye"
    CALL quit(program_isRunning)

   case ('s', 'S')
    IF (arrow_index > SIZE(options) - 1) THEN
     arrow_index = SIZE(options)
    ELSE
     arrow_index = arrow_index + 1
    ENDIF

   case ('w', 'W')
    IF (arrow_index < 2) THEN
     arrow_index = 1
    ELSE
     arrow_index = arrow_index - 1
    ENDIF

  END SELECT
 END DO

 CONTAINS

 SUBROUTINE draw_line
  print *, "--------------"
 END SUBROUTINE draw_line


 SUBROUTINE nl ! New line
  print *, ""
 END SUBROUTINE nl


 SUBROUTINE list_options

  INTEGER :: index_
  DO index_ = 1, SIZE(options)
   IF (index_ == arrow_index ) THEN
    print *, ">   ", options(index_)
   ELSE
    print *, options(index_)
   ENDIF
  END DO

 CALL nl

 END SUBROUTINE list_options

END PROGRAM main
