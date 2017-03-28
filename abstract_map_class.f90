module abstract_map_class

  use map_interface, only : map
  
  implicit none

  private
  public :: abstract_map

  type, abstract, extends(map) :: abstract_map
   contains
     procedure :: is_empty
  end type abstract_map
  
contains

  !===================================================================!
  ! Returns .true. if the map is empty
  !===================================================================!

  type(logical) function is_empty(this)

    class(abstract_map) :: this

    is_empty = this % size() .eq. 0

  end function is_empty

end module abstract_map_class
