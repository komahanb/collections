!=====================================================================!
! The base abstract implementation for collection of objects as sets,
! lists. The deferred procedures are to be implemented by specialized
! collection classes.
! 
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module collection_interface

  use iterator_interface, only : iterator
  
  implicit none

  private
  public :: collection

  type, abstract :: collection
     
   contains

     ! provided methods
!!$     procedure :: add
!!$     procedure :: add_all
!!$     procedure :: clear
     procedure :: contains
!!$     procedure :: contains_all
     procedure :: is_empty
!!$     procedure :: remove
!!$     procedure :: remove_all
!!$     procedure :: retain_all
!!$     procedure :: to_array
!!$     procedure :: to_string

     ! deferred methods
     procedure(get_iterator), deferred :: get_iterator
     procedure(size)        , deferred :: size
     
  end type collection
  
  interface

     !-----------------------------------------!
     ! returns the size of collection
     !-----------------------------------------!

     pure type(integer) function size(this)
       import collection
       class(collection), intent(in) :: this       
     end function size

     !-----------------------------------------!
     ! returns an iterator to the collection
     !-----------------------------------------!

     pure function get_iterator(this)
       import collection
       import iterator
       class(collection), intent(in)  :: this
       class(iterator)  , allocatable :: get_iterator
     end function get_iterator

  end interface
  
contains

  !===================================================================!
  ! returns .true. if the collection is empty
  !===================================================================!

  pure type(logical) function is_empty(this)    
    class(collection), intent(in) :: this   
    is_empty = this % size() == 0
  end function is_empty

  !===================================================================!
  ! returns .true. if the collection contains the 'element'
  !===================================================================!

  pure type(logical) function contains(this, element)
    
    class(collection), intent(in) :: this
    class(*), intent(in)          :: element
    class(iterator), allocatable  :: it

    ! obtain the iterator
    allocate(it, source = this % get_iterator())

!!$    if (element .eq. null()) then
!!$    else
!!$    end if
    
    check: do while(it % has_next())
       
       if ( element .eq. it % next() ) then
          contains =  .true.
          return
       end if
       
    end do check
    
    contains = .false.
    
   end function contains
  

end module collection_interface
