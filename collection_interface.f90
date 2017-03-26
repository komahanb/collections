module collection_interface

  use iterator_interface, only : iterator
  
  implicit none
  
  type, abstract :: collection
     
   contains

     ! Provided methods
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

     ! Deferred methods
     procedure(get_iterator), deferred :: get_iterator
     procedure(size)        , deferred :: size
     
  end type collection
  
  interface

     !-----------------------------------------!
     ! Returns the size of collection
     !-----------------------------------------!

     pure type(integer) function size(this)
       import collection
       class(collection), intent(in) :: this       
     end function size

     !-----------------------------------------!
     ! Returns an iterator to the collection
     !-----------------------------------------!

     pure function get_iterator(this)
       import collection
       import iterator
       class(collection), intent(in)  :: this
       class(iterator)  , allocatable :: get_iterator
     end function get_iterator

  end interface
  
contains

  ! Tell of the collection is empty
  pure type(logical) function is_empty(this)    
    class(collection), intent(in) :: this   
    is_empty = (this % size() == 0)   
  end function is_empty

  pure type(logical) function contains(this, element)
    
    class(collection), intent(in) :: this
    class(*), intent(in)          :: element
    class(iterator), allocatable  :: it

    ! Obtain the iterator
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
