!=====================================================================!
! Test program for list
!=====================================================================!

program test_list

  use list_class

  class(list), allocatable :: mylist
  
  allocate(mylist, source = list())

  call mylist % append(1)
  call mylist % append(2)
  call mylist % append(1.2)
  !call mylist % append("komahan boopathy")
  !call mylist % append(mylist)
  !call mylist % append(1)
  
  !print *, mylist % length
  
  deallocate(mylist)

end program test_list
