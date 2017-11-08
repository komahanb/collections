program test_hashtable
  
  use hashtable_class, only : hashtable
  use object_class, only : object
  use string_class, only : string
  
  implicit none

  class(hashtable), allocatable :: htable
  type(integer) :: i
  
  allocate(htable, source = hashtable(10, 1.5d0))
  
  do i = 1, 10
     allocate(htable % table(i), source = string('ko'))
  end do

  do i = 1, 10
     call htable % table(i) % print()
  end do

  call htable % print()
  print *, size(htable % table)

  deallocate(htable)
  
end program test_hashtable
