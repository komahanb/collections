!=====================================================================!
! Iterator enables to cycle through a collection, obtaining or
! removing elements.
!
! Before you can access a collection through an iterator, you must
! obtain one. Each of the collection classes provides an iterator()
! method that returns an iterator to the start of the collection. By
! using this iterator object, you can access each element in the
! collection, one element at a time.
!
! In general, to use an iterator to cycle through the contents of a
! collection, follow these steps:
!
!  o Obtain an iterator to the start of the collection by calling the
!    collection's iterator( ) method.
!
!  o Set up a loop that makes a call to has_next(). Have the loop
!    iterate as long as has_next() returns true.
!
!  o Within the loop, obtain each element by calling next().
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

module iterator_interface

  use object_class, only : object

  implicit none

  private
  public :: iterator

  type, abstract :: iterator

   contains

     procedure(has_next), deferred :: has_next
     procedure(next)    , deferred :: next

     ! procedure :: remove ! optional

  end type iterator

  interface
     
     !----------------------------------------------------------------!
     ! tells if there is a next element in collection
     !----------------------------------------------------------------!
     pure type(logical) function has_next(this)
       import iterator
       class(iterator), intent(in) :: this
     end function has_next
     
     !----------------------------------------------------------------!
     ! returns the next element
     !----------------------------------------------------------------!     
     pure function next(this)
       import iterator
       import object
       class(iterator), intent(in) :: this     
       class(object), allocatable  :: next
       !       class(*), allocatable       :: next
     end function next

  end interface

contains

end module iterator_interface

!=====================================================================!
! listiterator extends iterator to allow bidirectional traversal of a
! list, and the modification of elements.
!
! author: komahan boopathy (komahan@gatech.edu)
!=====================================================================!

!!$module listiterator_interface
!!$  
!!$  implicit none
!!$  
!!$  use iterator_interface, only : iterator
!!$  use list_class, only: list
!!$
!!$  type, extends(iterator) :: listiterator
!!$
!!$     
!!$   contains
!!$
!!$     procedure :: add
!!$     procedure :: has_next
!!$     procedure :: has_previous
!!$     procedure :: next
!!$     procedure :: next_index
!!$     procedure :: previous_index
!!$     procedure :: remove
!!$     procedure :: set
!!$     
!!$  end type listiterator
!!$
!!$contains
!!$
!!$  subroutine add(this)
!!$    class(), intent(inout) :: this
!!$    class
!!$  end subroutine add
!!$
!!$module listiterator_interface
