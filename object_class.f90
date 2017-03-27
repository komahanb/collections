module object_class

  implicit none

  private
  public :: object

  type :: object

   contains
     
     procedure :: equals
     procedure :: hashcode
     procedure :: to_string

!!$     procedure :: clone
!!$     procedure :: finalize
!!$     procedure :: get_class

  end type object

contains
  
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

    ! Return the memory address as hashcode
    hashcode = loc(this)

  end function hashcode
  
  !===================================================================!
  ! Returns the string representation of the object
  !===================================================================!
  
  type(character(len=32)) function to_string(this)

    class(object), intent(inout) :: this

    to_string = "object@"//char(this % hashcode())

  end function to_string

end module object_class
