MODULE util

 use user_mod
 IMPLICIT none 

 CONTAINS

 SUBROUTINE remove_user(users, index_, users_index, user_count)
  TYPE (User), DIMENSION(524) :: users
  
  INTEGER :: index_
  INTEGER :: users_index
  INTEGER :: user_count

  INTEGER :: iterator
  INTEGER :: user_pin

  CHARACTER(:), ALLOCATABLE :: user_name
  CHARACTER(:), ALLOCATABLE :: index_name

  IF (.not. ALLOCATED(index_name)) THEN
   CALL get_uname(users(index_), index_name)
  ELSE
   DEALLOCATE(index_name)
   CALL get_uname(users(index_), index_name)
  ENDIF

  DO iterator = 1, user_count
   
   IF (.not. ALLOCATED(user_name)) THEN
    CALL get_uname(users(iterator), user_name)

    CALL get_u_pin(users(iterator), user_pin)
   ELSE
    DEALLOCATE(user_name)

    CALL get_uname(users(iterator), user_name)

    CALL get_u_pin(users(iterator), user_pin)
   ENDIF

   IF (user_name == index_name) THEN
    print *, "Removing user: ", index_name
    users(index_)% u_name = ""
    users(index_)% u_pin = 0

    user_count = user_count - 1
    users_index = users_index - 1

    EXIT
   ENDIF

  END DO
 END SUBROUTINE remove_user


 SUBROUTINE add_user(user_name, name_len, user_pin, users, user_count, users_index)

  INTEGER :: name_len
  INTEGER :: user_pin
  INTEGER :: user_count
  INTEGER :: users_index

  CHARACTER(name_len) :: user_name

  TYPE (User) :: users(524)
  IF (user_name /= "") THEN
   user_count = user_count + 1

   CALL set_uname(user_name, name_len, users(users_index))
   CALL set_u_pin(user_pin, users(users_index))
   users_index = users_index + 1
  ENDIF
  
 END SUBROUTINE add_user


 SUBROUTINE list_users(users, user_count)

  INTEGER :: index_
  INTEGER :: user_count

  INTEGER :: user_pin

  TYPE (User) :: users(524)

  CHARACTER(:), ALLOCATABLE :: name_user

  DO index_ = 1, user_count 
   CALL get_uname(users(index_), name_user)
   CALL get_u_pin(users(index_), user_pin)   

   IF (name_user /= "" .and. user_pin /= 0) THEN
    print *, index_, ": ", name_user, " | ", user_pin
   ENDIF
  END DO

 END SUBROUTINE list_users

END MODULE util
