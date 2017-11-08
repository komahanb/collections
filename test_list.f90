!=====================================================================!
! Test program for list
!=====================================================================!

program test_list

  use string_class
  use list_class

  class(list), allocatable :: mylist
  
  allocate(mylist, source = list())

  call mylist % append(1)
  call mylist % append(1.2)
  call mylist % append(string("komahan boopathy"))
  call mylist % append('komahan boopathy')
  call mylist % append(mylist)
  call mylist % append(1.0d0)
  call mylist % append(.true.)
  
  print *, mylist % length
  
  deallocate(mylist)

end program test_list
