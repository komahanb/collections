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

   contains

     ! Override the parent class methods
     procedure :: equals

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
    type(integer) :: h, i, s
    
    allocate(this % str, source=str) ! source copies, mold does not
    this % count = len(str)
    
    ! Compute hash: s[1]*31^(n) + s[2]*31^(n-1) + ... + s[n]*31^0
    do i = 1, this % count
       s = ichar(this % str(i:i))
       h = h + s*(31**(this%count-1))
    end do

    ! Set into the instance
    this % hash = h

  end function create
  
  !=================================================================!
  ! Destructor for string object
  !=================================================================!
  
  pure subroutine destroy(this)

    type(string), intent(inout) :: this

    if(allocated(this % str)) deallocate(this % str)

  end subroutine destroy

  !===================================================================!
  ! Returns if the supplied string is equal to the string on which
  ! this method is invoked.
  !
  ! Note: This is an overridden procedure.
  !===================================================================!
  
  type(logical) function equals(this, element)

    class(string) , intent(in) :: this
    class(*)      , intent(in) :: element 

    ! Return .true. right away if they are the same memory locations
    if (loc(element) .eq. loc(this)) then
       equals = .true.
       return
    end if

    ! Determine the type at runtime
    select type(element)

    ! Raw character array
    type is (character(len=*))
       equals  = this % str .eq. element

    ! Wrapped string
    class is (string)
       equals = element % str .eq. this % str

    ! Not a string , so they can't be equal
    class default
       equals = .false.

    end select

  end function equals
  
end module string_class
