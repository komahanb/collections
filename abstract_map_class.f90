module abstract_map_class

  use map_interface, only : map
  use object_class, only : object

  implicit none

  private
  public :: abstract_map, map_entry

  type, abstract, extends(map) :: abstract_map
   contains
     procedure :: is_empty
  end type abstract_map
 
  type, extends(object) :: map_entry
     type(integer)                 :: hash
     class(*)        , allocatable :: key
     class(*)        , allocatable :: value
     type(map_entry) , pointer     :: next
   contains
     ! Override equals and hashcode of object type
!     procedure :: equals => equals
!     procedure :: hashcode => hashcode     
     !     procedure :: get_key
     !     procedure :: get_value, set_value
     !     procedure :: to_string
  end type map_entry

  ! Contructor interface
  interface map_entry
     module procedure create_map_entry
  end interface map_entry

contains
  
  type(map_entry) function create_map_entry(hash, key, value, next) & 
       & result(this)

    type(integer)   , intent(in) :: hash
    class(*)        , intent(in) :: key
    class(*)        , intent(in) :: value
    type(map_entry) , pointer   :: next

    this % hash  = hash

    allocate(this % key, source = key)
    allocate(this % value, source = value)
      
  end function create_map_entry









































  !===================================================================!
  ! Returns .true. if the map is empty
  !===================================================================!

  type(logical) function is_empty(this)

    class(abstract_map) :: this

    is_empty = this % size() .eq. 0

  end function is_empty

end module abstract_map_class
