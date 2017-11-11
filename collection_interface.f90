!=====================================================================!
! Interface for all collection classes
! 
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module collection_interface

  use iterator_interface, only : iterator
  use object_class, only : object

  implicit none

  private
  public :: collection

  type, extends(object), abstract :: collection

   contains

     ! provided methods
!!$     procedure :: add
!!$     procedure :: add_all
!!$     procedure :: clear
     procedure(contains), deferred :: contains
!!$     procedure :: contains_all
     procedure(is_empty), deferred :: is_empty
!!$     procedure :: remove
!!$     procedure :: remove_all
!!$     procedure :: retain_all
!!$     procedure :: to_array
!!$     procedure :: to_string

     procedure(get_iterator), deferred :: get_iterator
     procedure(size), deferred :: size

  end type collection

  interface

     pure type(logical) function is_empty(this)
       import collection
       class(collection), intent(in) :: this
     end function is_empty
     
     type(logical) function contains(this, element)
       import collection, object, iterator
       class(collection) , intent(in)  :: this
       class(object)     , intent(in)  :: element
       class(iterator)   , allocatable :: it
     end function contains

     pure type(integer) function size(this)
       import collection
       class(collection), intent(in) :: this       
     end function size

      function get_iterator(this)
       import collection, iterator
       class(collection), intent(inout)  :: this
       class(iterator)  , allocatable :: get_iterator
     end function get_iterator

  end interface

contains

end module collection_interface
