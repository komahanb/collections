!=====================================================================!
! Program to test the hashcode and equals implementations
!=====================================================================!

program test_hashcode_equals
  
  use object_class, only: object
  use string_class, only: string

  type(string) :: first_name
  type(string) :: last_name

  class(object), allocatable :: obj

  ! Create the string objects
  first_name = string("Komahan")
  last_name  = string("Boopathy")

  allocate(obj, source = first_name)

  ! Print the hashcodes
  print * , "hashcode firstname :", first_name % hashcode()
  print * , "hashcode lastname  :", last_name % hashcode()
  print * , "hashcode object    :", obj % hashcode()

  ! Point the polymorphic variable to some other string
  deallocate(obj)

  allocate(obj, source = string("John"))
  print * , "hashcode object    :", obj % hashcode()
  
  ! Checks the equality
  print *, first_name % equals(last_name)
  print *, last_name  % equals(first_name)

  print *, first_name % equals(first_name)
  print *, last_name  % equals(last_name)

  ! Test other possibilities
  print *, last_name  % equals(1.213124234d0)
  print *, last_name  % equals(234231)
  print *, last_name  % equals(obj)
  print *, last_name  % equals("Boopathy")
  print *, last_name  % equals("boopathy")
  print *, last_name  % equals(last_name)
  
  ! Test the to_string implementation
  call obj % print()
  call first_name % print()
  
end program test_hashcode_equals
