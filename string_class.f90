!=====================================================================!
! Contains a derived type string and implemented procedures
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module string_class
  
  ! Dependencies
  use object_class, only : object

  implicit none
  
  private
  public :: string

  ! Derived type for string
  type, extends(object) :: string

     character(:), allocatable :: str   ! character array
     type(integer)             :: count ! length
     type(integer)             :: hash  ! hashcode value computed at creation

   contains

     ! Override the parent class methods
     procedure :: hashcode
     !procedure :: equals

     ! Destructor
     final :: destroy

  end type string
  
  ! Interface to construct a string
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
    this % hash = 0

  end function create
  
  !=================================================================!
  ! Destructor for the ABM integrator
  !=================================================================!
  
  pure subroutine destroy(this)

    type(string), intent(inout) :: this

    if(allocated(this % str)) deallocate(this % str)

  end subroutine destroy

  !===================================================================!
  ! Returns the hashcode of the string object
  !===================================================================!

  type(integer) function hashcode(this)

    class(string), intent(inout) :: this
    type(integer) :: h, i, length, s

    length = this % count
    h      = this % hash

    ! Find the hash only if it not found already
    if ( h .eq. 0 ) then

       ! Compute hash: s[1]*31^(n) + s[2]*31^(n-1) + ... + s[n]*31^0
       do i = 1, length
          s = ichar(this % str(i:i))
          h = h + s*(31**(length-1))
       end do
       
       ! Set into the instance
       this % hash = h

    end if

    ! Set the return value
    hashcode = h

  end function hashcode
  
end module string_class
