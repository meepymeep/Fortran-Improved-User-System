MODULE user_mod
 ! ⤓ The type statement is like the struct keyword in C.
 TYPE User
  ! I actually made all the contents in here private
  ! as you can probably see from the getters, but I had to remove it
  ! and I can't really remember why.
  CHARACTER(50) :: u_name

  INTEGER :: u_pin
 END TYPE User

 CONTAINS
 ! Setters & Getters

 SUBROUTINE get_uname(this, str)
  TYPE (User) :: this
 
  CHARACTER(:), ALLOCATABLE :: str

  IF (.not. ALLOCATED(str)) THEN
   ALLOCATE(CHARACTER(LEN(this% u_name)) :: str)
  ELSE
   DEALLOCATE(str)
  ENDIF

  str = this% u_name
 END SUBROUTINE get_uname
 
 
 SUBROUTINE get_u_pin(this, num)
  TYPE (User) :: this
  INTEGER :: num

  num = this% u_pin
 END SUBROUTINE get_u_pin


 SUBROUTINE set_uname(u_name, name_len, a_user)
  TYPE (User) :: a_user
  CHARACTER(name_len) :: u_name
  INTEGER :: name_len

  a_user% u_name = u_name ! % is like the -> or . in C when editing something in a structure,
  ! for example: ⤓
  ! {struct_name}->age = 19 or {struct_name}.has_job = true
 END SUBROUTINE set_uname


 SUBROUTINE set_u_pin(u_pin, a_user)
  TYPE (User) :: a_user
  INTEGER :: u_pin

  a_user% u_pin = u_pin
 END SUBROUTINE set_u_pin

END MODULE user_mod
