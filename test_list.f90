!=====================================================================!
! Test program for list
!=====================================================================!

program test_list

  use string_class
  use linked_list_class
  use iterator_interface
  use object_class

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

  ! Sanity check on BDF coeffs
  test_iterator: block

    class(iterator), allocatable :: it
    type(object) :: tmp
    allocate(it, source = list % get_iterator())
    !allocate(it, source = list_iterator(list%head))
    
    do while(it % has_next())
       !tmp = it % next()
    end do

!    print *, it % has_next()
    !call it % next() % print()

    deallocate(it)

  end block test_iterator


  deallocate(list)
  
end program test_list
