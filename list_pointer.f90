module list_class

  implicit none

  type :: node
     type(node), pointer :: next
     real, pointer :: data
  end type node

  type :: list
     type(node), pointer :: head     
   contains     
     procedure :: append     
  end type list

  interface list
     module procedure create_list
  end interface list

  interface node
     module procedure create_node
  end interface node

contains

  type(list) function create_list(head) result(self)
    type(node), target, intent(in) :: head
    self % head => head   
  end function create_list

  type(node) function create_node(item) result(self)
    real, intent(in) :: item
    allocate(self % data, source = item)
  end function create_node

  subroutine append(self, item)
    class(list) , intent(inout) :: self
    type(node)  , intent(inout) :: item
  end subroutine append

end module list_class

program test

  use list_class

  type(list) :: lst
  type(node), target :: a, b, c, d

  a = node(1.00)
  b = node(2.00)
  c = node(3.00)
  d = node(4.00)

  a % next => b
  b % next => c
  c % next => d

  !class(list) , allocatable :: lst
  !class(node)  , allocatable :: n

  !allocate(n, source = node(1))

  !allocate(lst, source = list())
  !deallocate(lst)

  lst = list(a)

  print *, lst % head % data
  print *, lst % head % next % data
  print *, lst % head % next % next % data
  print *, lst % head % next % next % next % data

end program test
