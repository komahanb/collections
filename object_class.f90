module object_class

  implicit none

  private
  public :: object

  type :: object

     class(*), allocatable :: data
     type(integer)         :: hash = 0
     
   contains
     
     procedure :: equals
     procedure :: hashcode
     procedure :: print

!!$     procedure :: clone
!!$     procedure :: finalize
!!$     procedure :: get_class
     
!!$     generic :: assignment(=) => set_entry
     
  end type object
  
  interface object
     module procedure create_object
  end interface object

contains

  !===================================================================!
  ! Wrap the primitive/derived type and create an object
  !===================================================================!
  
  type(object) function create_object(data) result(this)
    
    class(*), intent(in), optional :: data
    
    if (present(data)) allocate(this % data, source = data)
    print *, "creating object", loc(this)
    this % hash = loc(this)
    
  end function create_object
  
  !===================================================================!
  ! Returns if the supplied instance is equal to the instance on which
  ! this method is invoked.
  !===================================================================!
  
  type(logical) function equals(this, element)

    class(object) , intent(in) :: this
    class(*)      , intent(in) :: element 

    ! compare the memory addresses
    equals = loc(element) .eq. loc(this)

  end function equals

  !===================================================================!
  ! Returns the hashcode of the object
  !===================================================================!

  type(integer) function hashcode(this)

    class(object), intent(inout) :: this

    ! Set hashcode if constructor was not invoked before
    if ( this % hash .eq. 0 ) then
       this % hash = loc(this)
    end if
    
    ! Return the memory address as hashcode
    hashcode = this % hash

  end function hashcode
  
  !===================================================================!
  ! Returns the string representation of the object
  !===================================================================!
  
  subroutine print(this)
    
    class(object), intent(inout) :: this
    
    print *, "object@", this % hashcode()
    
  end subroutine print

end module object_class
