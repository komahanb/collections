!=====================================================================!
! 
!=====================================================================!

! http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/6-b14/java/util/Hashtable.java#Hashtable
module hashtable_class

  use iso_fortran_env, only : dp => REAL64
  use object_class, only : object

  implicit none

  private
  public :: hashtable

  type, extends(object) :: hashtable

     class(object), allocatable, dimension(:) :: table ! table of values
     type(integer)         :: count         ! number of keys
     type(integer)         :: threshold     ! threshold for rehashing
     type(integer)         :: load_factor   ! factor to increase the size
     type(integer)         :: mod_count     ! number of rehashings happened since creation

   contains

     ! overridden print method
     procedure :: print
     !procedure :: rehash
     
  end type hashtable

  ! Constructor
  interface hashtable
     module procedure create_hashtable
  end interface hashtable

contains
  
  type(hashtable) function create_hashtable(initial_capacity, load_factor) &
       & result(this)

    type(integer) , intent(in) :: initial_capacity
    type(real(dp)), intent(in) :: load_factor    

    ! Sanity check in the initial capacity of hashtable
    if (initial_capacity .le. 0) then
       print *, "Invalid initial capacity", initial_capacity
       stop
    end if

    ! Sanity check on the load factor
    if (load_factor .le. 0 .or. load_factor .ne. load_factor) then
       print *, "Invalid load factor", load_factor
       stop
    end if

    ! Store the values into the object
    this % load_factor = load_factor
    this % threshold   = int(initial_capacity*load_factor)

    ! Allocate space for entries
    !allocate( this % table (initial_capacity) )

    ! Set the number of entries
    this % count = size(this % table)

    ! Zero the number of size modifications so far (rehashing)
    this % mod_count = 0

  end function create_hashtable

  subroutine print(this)
    
    class(hashtable), intent(in) :: this
    
    print *, "hashtable@    ", this % hashcode()
    print *, " count:       ", this % count
    print *, " threshold:   ", this % threshold
    print *, " load_factor: ", this % load_factor
    print *, " mod_count:   ", this % mod_count

  end subroutine print
  
end module hashtable_class
