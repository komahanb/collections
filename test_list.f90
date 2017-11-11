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
  
!!$  call list % append(1)
!!$  call list % append(1.2)
  call list % append(string("1hello"))
  call list % append(string("2boopathy"))
  call list % append(string("3komahan boopathy"))
  call list % append(string("4komahan"))
  call list % append(string("5komahan"))
!!$  call list % append('komahan boopathy')
!  call list % append(list)
!!$  call list % append(1.0d0)
!!$  call list % append(.true.)  

  print *, list % length

  call list % head % data % print()
  call list % head % next % data % print()
  call list % head % next % next % data % print()
  call list % head % next % next % next % data % print()

  ! Sanity check on BDF coeffs
  test_iterator: block

    class(iterator), allocatable :: it
    class(object), allocatable :: tmp

    allocate(it, source = list % get_iterator())
    !allocate(it, source = list_iterator(list%head))
    
    do while(it % has_next())
       call it % get_next(tmp)
       if (allocated(tmp)) call tmp % print()
       stop
    end do

!    print *, it % has_next()
    !call it % next() % print()

    deallocate(it)

  end block test_iterator


  deallocate(list)
  
end program test_list
