!=====================================================================!
! Contains a derived type 'string' and implemented procedures
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module string_class
  
  use object_class, only : object

  implicit none
  
  private
  public :: string

  !-------------------------------------------------------------------!
  ! Derived type for string
  !-------------------------------------------------------------------!
  
  type, extends(object) :: string

     character(:), allocatable :: str ! character array
     type(integer) :: count ! length

   contains

     ! Override
     procedure :: print
     
     ! Destructor
     final :: destroy

  end type string

  !-------------------------------------------------------------------!
  ! Interface to construct a string
  !-------------------------------------------------------------------!
  
  interface string
     module procedure create
  end interface string
 
contains

  !===================================================================!
  ! Construct a string object from the supplied literal, find its
  ! length, initialize its hashcode as zero.
  !===================================================================!

  pure type(string) function create(str) result (this)

    type(character(*)), intent(in) :: str
    
    allocate(this % str, source=str) ! source copies, mold does not
    
    this % count = len(str)    

  end function create
  
  !===================================================================!
  ! Destructor for string object
  !===================================================================!
  
  pure subroutine destroy(this)

    type(string), intent(inout) :: this

    if(allocated(this % str)) deallocate(this % str)

  end subroutine destroy

  !===================================================================!
  ! Returns the string representation of the object
  !===================================================================!
  
  subroutine print(this)
    
    class(string), intent(in) :: this
    
    print *, "string : ", this % str
    
  end subroutine print
  
end module string_class
