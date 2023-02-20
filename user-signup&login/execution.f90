MODULE execute_cmd

 use user_mod
 use util

 CONTAINS

 SUBROUTINE quit(bool)
  LOGICAL :: bool
  bool = .FALSE.
 END SUBROUTINE quit


 SUBROUTINE execute(command, str_len, users, user_count, users_index, current_user, is_loggedIn)
  INTEGER :: str_len
  INTEGER :: user_pin
  INTEGER :: user_count
  INTEGER :: users_index

  INTEGER :: name_len
  INTEGER :: index_

  CHARACTER(str_len) :: command
  CHARACTER(30) :: user_name
  CHARACTER(:), ALLOCATABLE :: current_user

  CHARACTER :: p_char

  TYPE (User), DIMENSION(524) :: users

  LOGICAL :: is_loggedIn

  SELECT CASE (command)
   case ("Add User   ")
    print *, "What is the users name?"
    read(*,*) user_name

    name_len = LEN(user_name)

    print *, "The users pin?"
    read(*,*) user_pin

    CALL add_user(user_name, name_len, user_pin, users, user_count, users_index)

    print *, "Added user: ", user_name, "!"
    read(*,*) p_char

   case ("List Users ")
    CALL list_users(users, user_count)
    read(*,*) p_char

   case ("Remove User")
    print *, "Users to remove"
    CALL list_users(users, user_count)

    print *, "Enter in number:"
    read(*,*) index_

    CALL remove_user(users, index_, users_index, user_count)

    read(*,*) p_char

  case ("Open User  ")
   print *, "Users to login to:"
   CALL list_users(users, user_count)

   print *, "Enter in number:"
   read(*,*) index_

   user_name = users(index_)% u_name
   print *, "What is this users pin?"

   read(*,*) user_pin
   IF (user_name == users(index_)% u_name .and. user_pin == users(index_)% u_pin) THEN
    is_loggedIn = .TRUE.
    
    IF (.not. ALLOCATED(current_user)) THEN
     ALLOCATE(CHARACTER(LEN(user_name)) :: current_user)
    ELSE
     DEALLOCATE(current_user)
     ALLOCATE(CHARACTER(LEN(user_name)) :: current_user)
    ENDIF

    current_user = user_name
    print *, "You have logged into: ", current_user
   ENDIF

   read(*,*) p_char

   case ("Logged-Into")

    IF (ALLOCATED(current_user) .and. current_user /= "") THEN
     print *, "You are logged into: ", current_user
    ELSE
     print *, "You aren't logged in!"
    ENDIF

    read(*,*) p_char

   case ("Log Out    ")
    IF (ALLOCATED(current_user) .and. current_user /= "") THEN
     print *, "Logging out of: ", current_user
     current_user = ""
     DEALLOCATE(current_user)
    ELSE
     print *, "bro you aren't logged in"
    ENDIF

    read(*,*) p_char

  END SELECT
 END SUBROUTINE

END MODULE execute_cmd
