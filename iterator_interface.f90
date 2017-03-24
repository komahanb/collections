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

  implicit none

  private

  public :: Iterator
  type, abstract :: Iterator

   contains

     procedure(has_next), deferred :: has_next
     procedure(next)    , deferred :: next

     ! procedure :: remove ! optional

  end type Iterator

  interface
     
     !----------------------------------------------------------------!
     ! Tells if there is a next element in collection
     !----------------------------------------------------------------!
     pure type(logical) function has_next(this)
       import iterator
       class(iterator), intent(in) :: this
     end function has_next
     
     !----------------------------------------------------------------!
     ! Returns the next element
     !----------------------------------------------------------------!     
     pure function next(this)
       import iterator
       class(iterator), intent(in) :: this
       class(*), allocatable       :: next
     end function next

  end interface

contains

end module iterator_interface

!=====================================================================!
! ListIterator extends Iterator to allow bidirectional traversal of a
! list, and the modification of elements.
!
! Author: Komahan Boopathy (komahan@gatech.edu)
!=====================================================================!

!!$module ListIterator_interface
!!$  
!!$  implicit none
!!$  
!!$  use iterator_interface, only : Iterator
!!$  use list_class, only: list
!!$
!!$  Type, Extends(Iterator) :: ListIterator
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
!!$  End Type ListIterator
!!$
!!$contains
!!$
!!$  subroutine add(this)
!!$    class(), intent(inout) :: this
!!$    class
!!$  end subroutine add
!!$
!!$module ListIterator_interface
