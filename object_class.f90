module object_class

  implicit none

  private
  public :: object

  type :: object

   contains

!!$     procedure :: clone
!!$     procedure :: equals
!!$     procedure :: finalize
!!$     procedure :: get_class
!!$     procedure :: hash_code
!!$     procedure :: to_string     

  end type object

contains

  pure type(object) function clone(this)
    class(object), intent(in) :: this
  end function clone
  
end module object_class
