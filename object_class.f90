!=====================================================================!
! Module containing Object type which is the topmost in hierarchy.
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module object_class

  implicit none

  private
  public :: object

  !-------------------------------------------------------------------!
  ! Type definition
  !-------------------------------------------------------------------!
  
  type :: object

   contains
     
     procedure :: equals
     procedure :: hashcode
     procedure :: print
     
  end type object

  !-------------------------------------------------------------------!
  ! Constructor function for type
  !-------------------------------------------------------------------!
  
!!$  interface object
!!$     module procedure create_object
!!$  end interface object

contains
  
  !===================================================================!
  ! Returns if the supplied instance is equal to the instance on which
  ! this method is invoked.
  !===================================================================!
  
  type(logical) function equals(this, element)
    
    class(object) , intent(in) :: this
    class(*) , intent(in) :: element 

    ! objects are equal only if they are coexistant in space-time
    equals = loc(element) .eq. loc(this)

  end function equals

  !===================================================================!
  ! Returns the unique hashcode of the object
  !===================================================================!

  type(integer) function hashcode(this)

    class(object), intent(in) :: this

    ! Return the memory address as hashcode
    hashcode = loc(this)

  end function hashcode
  
  !===================================================================!
  ! Returns the string representation of the object
  !===================================================================!
  
  subroutine print(this)
    
    class(object), intent(in) :: this
    
    print *, "object@", this % hashcode()
    
  end subroutine print

end module object_class
