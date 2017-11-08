!=====================================================================!
! Test program for list
!=====================================================================!

program test_list

  use string_class
  use linked_list_class

  class(doubly_linked_list), allocatable :: list
  
  allocate(list, source = doubly_linked_list())

  call list % append(1)
  call list % append(1.2)
  call list % append(string("komahan boopathy"))
  call list % append('komahan boopathy')
  call list % append(list)
  call list % append(1.0d0)
  call list % append(.true.)  

  print *, list % length

  deallocate(list)

end program test_list
