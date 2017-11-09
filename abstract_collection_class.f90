!=====================================================================!
! The base abstract implementation for collection of objects as sets,
! lists. The deferred procedures are to be implemented by specialized
! collection classes.
! 
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module abstract_collection_class

  use collection_interface , only : collection
  use iterator_interface   , only : iterator
  use object_class         , only : object  
 
  implicit none

  private
  public :: abstract_collection

  type, abstract, extends(collection) :: abstract_collection

   contains

     procedure :: contains
     procedure :: is_empty    

  end type abstract_collection
  
contains

  !===================================================================!
  ! Returns .true. if the abstract_collection is empty
  !===================================================================!

  pure type(logical) function is_empty(this)
    
    class(abstract_collection), intent(in) :: this
    
    is_empty = this % size() .eq. 0
    
  end function is_empty

  !===================================================================!
  ! Returns .true. if the abstract_collection contains the 'element'
  !===================================================================!
  
  type(logical) function contains(this, element)

    class(abstract_collection) , intent(in)  :: this
    class(object)     , intent(in)  :: element
    class(iterator)   , allocatable :: it

    ! Obtain the iterator
    allocate(it, source = this % get_iterator())

    ! Assume that the element is not in the abstract_collection
    contains = .false.

    if (loc(element) .eq. 0) then

       check_null_obj: do while(it % has_next())

          ! If you get an not associated pointer, then yay!
          if ( .not. associated(it % next())) then
             contains =  .true.
             return
          end if

       end do check_null_obj

       print *, "NULL element encountered!!!"

    else

       check_not_null: do while(it % has_next())

          if (element % equals(it % next())) then
             contains =  .true.
             return
          end if

       end do check_not_null

    end if

  end function contains

end module abstract_collection_class
