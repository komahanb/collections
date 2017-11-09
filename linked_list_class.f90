!=====================================================================!
! Module containing linked list implementations
!=====================================================================!

module linked_list_class

  use object_class, only : object
  use abstract_collection_class, only : abstract_collection
  use iterator_interface, only : iterator

  implicit none

  private

  public :: node, doubly_linked_list, list_iterator

  !-------------------------------------------------------------------!
  ! Node is a special type of object that points to other nodes
  !-------------------------------------------------------------------!

  type, extends(object) :: node

     ! The data can be encapsulated as any object
     class(object), allocatable :: data

     ! Points to the next node
     type(node), allocatable :: next

     ! Points to the previous node
     type(node), allocatable :: prev

   contains

     procedure :: print

  end type node

  ! Publicly visible polymorphic function to create node
  interface node
     module procedure create_node
  end interface node

  !-------------------------------------------------------------------!
  ! Doubly linked list is a specialized object that points to next and
  ! previous nodes
  !-------------------------------------------------------------------!

  type, extends(abstract_collection) :: doubly_linked_list
     
     type(node), allocatable :: head
     type(node), allocatable :: tail
     type(integer) :: length
          
   contains

     ! Implement procedures deferred in interface
     procedure :: size
     procedure :: get_iterator

     ! Special methods
     procedure :: append

  end type doubly_linked_list

  ! Public function to create doubly linked list
  interface doubly_linked_list
     module procedure create_doubly_linked_list
  end interface doubly_linked_list

  !--------------------------------------------------------------------!
  ! Type that tells whether next node exists and return the node
  !--------------------------------------------------------------------!
  
  type, extends(iterator) :: list_iterator
     
     type(node), allocatable :: current_node

   contains
     
     procedure :: has_next
     procedure :: next
     !procedure :: has_prev
     !procedure :: prev

     final :: destroy
     
  end type list_iterator

  interface list_iterator
     module procedure create_list_iterator
  end interface list_iterator

contains

  !===================================================================!
  ! Create an iterator to traverse the list and perform operations
  !===================================================================!

  pure type(list_iterator) function create_list_iterator(current_node) &
       & result(this)

    !class(doubly_linked_list), intent(in) :: list    
    class(node), intent(in) :: current_node

    ! Point to the list
    !allocate(this % list, source = list)

    ! Point to the current node
    allocate(this % current_node, source = current_node)

  end function create_list_iterator
  
  !===================================================================!
  ! Constructor for node
  !===================================================================!
  
  pure type(node) function create_node(data) result(this)

    class(object), intent(in) :: data

    ! Use sourced allocation to determine datatype at runtime
    allocate(this % data, source=data)

    ! Set the next node to null
    if (allocated(this % next)) deallocate(this % next)

    ! Set the previous node to null
    if (allocated(this % prev)) deallocate(this % prev)

  end function create_node

    !===================================================================!
  ! Returns the string representation of the object
  !===================================================================!
  
  subroutine print(this)
    
    class(node), intent(in) :: this
    
    print *, "node : "
    call this % data % print()
    
    !xcall this % next % print()
    
  end subroutine print

  !===================================================================!
  ! Instantiate an empty list
  !===================================================================!

  type(doubly_linked_list) function create_doubly_linked_list() &
       & result(this)

    if (allocated(this % head)) deallocate(this % head)
    if (allocated(this % tail)) deallocate(this % tail)
    this % length = 0

  end function create_doubly_linked_list

  !===================================================================!
  ! Append the item to list of nodes
  !===================================================================!

  subroutine append(this, item)

    class(doubly_linked_list), intent(inout) :: this     
    class(object), intent(in) :: item
    
    type(node) :: newnode
    class(node), allocatable :: tmp
    
    call item % print()
    
    ! Create new node encapsulating the data
    newnode = node(item)
    
    if (this % length .eq. 0) then

       ! head, tail are the newly added node      
       allocate(this % head, source = newnode)
       allocate(this % tail, source = newnode)

       call this % head % print()
       call this % tail % print()
       
    else if (this % length .eq. 1) then

       allocate(this % head % next, source = newnode)
             
       ! Release the pointer to old tail node
       deallocate(this % tail)

       ! newnode is the new tail node
       allocate(this % tail, source = newnode)
       
       allocate(this % tail % prev, source = newnode)
          
    else

       allocate(tmp, source = this % head % next)
       
       ! Release the pointer to old tail node
       deallocate(this % tail)

       ! Now point to new tail node
       allocate(this % tail, source = newnode)

    end if

    ! The list has grown by 1
    this % length = this % length + 1

  end subroutine append

  !===================================================================!
  ! Size of the collection
  !===================================================================!

  pure type(integer) function size(this)

    class(doubly_linked_list), intent(in) :: this

    size = this % length

  end function size

  !===================================================================!
  ! Creates a new iterator instance, points it to the head and returns
  !===================================================================!

  pure function get_iterator(this)

    class(doubly_linked_list), intent(in) :: this
    class(iterator), allocatable :: get_iterator

    ! ? traverse from tail node if optional argument is given
    allocate(get_iterator, source = list_iterator(this % head))

  end function get_iterator

  !----------------------------------------------------------------!
  ! Tells if there is a next element in collection
  !----------------------------------------------------------------!

  pure type(logical) function has_next(this)

    class(list_iterator), intent(in) :: this

    has_next = allocated(this % current_node)

  end function has_next

  !----------------------------------------------------------------!
  ! Returns the next element in collection
  !----------------------------------------------------------------!

  function next(this) result(next_entry)

    class(list_iterator), intent(inout) :: this
    class(object), allocatable :: next_entry
    class(node), allocatable :: tmp
   
    allocate(next_entry, source = this % current_node)

    if (allocated(this % current_node % next)) then

       allocate(tmp, source = this % current_node % next)
       deallocate(this % current_node)
       allocate(this % current_node, source = tmp)
    else
       deallocate(this % current_node)
    end if

  end function next

  
  !===================================================================!
  ! Destroy the iterator when it goes out of scope
  !===================================================================!
  
  pure subroutine destroy(this)    
    
    type(list_iterator), intent(inout) :: this
    
    if(allocated(this % current_node)) deallocate(this % current_node)

  end subroutine destroy

end module linked_list_class
