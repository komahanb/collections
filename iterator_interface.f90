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

  type, abstract :: Iterator

   contains

     procedure(has_next), deferred :: has_next ! tells if there is a next element
     procedure(next)    , deferred :: next     ! returns the next element

     ! procedure :: remove ! optional

  end type Iterator

  interface

     pure type(logical) function has_next(this)
       import iterator
       class(iterator), intent(in):: this
     end function has_next

     pure function next(this)
       import iterator
       class(iterator), intent(in) :: this
       class(*), allocatable :: next_interface
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
