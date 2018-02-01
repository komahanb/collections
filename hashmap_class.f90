module hashmap_class

  use abstract_map_class, only : abstract_map, map_entry
  use object_class, only : object
  use iso_fortran_env, only : dp => REAL64
  use iterator_interface, only : iterator

!!$  type, extends(abstract_map) :: hashmap
!!$
!!$     class(object), allocatable :: table(:)    ! number of values
!!$     type(integer)         :: count       ! number of keys
!!$     type(integer)         :: threshold   ! threshold for rehashing
!!$     type(integer)         :: load_factor ! factor to increase the size
!!$     type(integer)         :: mod_count   ! number of rehashings happened since creation
!!$
!!$   contains
!!$
!!$  end type hashmap
!!$
!!$  interface hashmap
!!$     module procedure create
!!$  end interface hashmap

contains

!!$  type(hashmap) function create(initial_capacity, load_factor) &
!!$       & result(this)
!!$    
!!$    type(integer) , intent(in) :: initial_capacity
!!$    type(real(dp)), intent(in) :: load_factor
!!$    
!!$    ! Sanity check in the initial capacity of hashtable
!!$    if (initial_capacity .lt. 0) then
!!$       print *, "Invalid initial capacity", initial_capacity
!!$       stop
!!$    end if
!!$
!!$    ! Sanity check on the load factor
!!$    if (load_factor .le. 0 .or. load_factor .ne. load_factor) then
!!$       print *, "Invalid load factor", load_factor
!!$       stop
!!$    end if
!!$    
!!$    ! Store the values into the object
!!$    this % load_factor = load_factor
!!$    this % threshold   = int(initial_capacity*load_factor)
!!$
!!$    ! Allocate space
!!$    ! this % table
!!$
!!$    ! Zero the number of size modifications so far (rehashing)
!!$    this % mod_count = 0
!!$
!!$  end function create

end module hashmap_class
