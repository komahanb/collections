!=====================================================================!
! Module containing linked list implementations
!=====================================================================!

module linked_list_class

  use object_class, only : object

  implicit none

  private

  public :: node, doubly_linked_list

  !-------------------------------------------------------------------!
  ! Node is a special type of object that points to other nodes
  !-------------------------------------------------------------------!

  type, extends(object) :: node
         
     type(node), allocatable :: next

  end type node

  ! Publicly visible polymorphic function to create node
  interface node
     module procedure create_node
  end interface node

  !-------------------------------------------------------------------!
  ! Doubly linked list is a specialized object that points to next and
  ! previous nodes
  !-------------------------------------------------------------------!

  type, extends(object) :: doubly_linked_list
     
     class(node), allocatable :: head
     class(node), allocatable :: tail
     type(integer)            :: length
          
   contains

     procedure :: append

  end type doubly_linked_list

  ! public function to create doubly linked list
  interface doubly_linked_list
     module procedure create_doubly_linked_list
  end interface doubly_linked_list

contains

  !===================================================================!
  ! Constructor for node
  !===================================================================!
  
  pure type(node) function create_node(data) result(this)
    
    class(*), intent(in) :: data

    ! Use sourced allocation to determine datatype at runtime
    allocate(this % data, source=data)

    ! Set the next node to null
    if (allocated(this % next)) deallocate(this % next)
         
  end function create_node

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
     class(*), intent(in) :: item
     
     type(node) :: newnode
    
     ! Create new node encapsulating the data
     newnode = node(item)
     
     if (this % length .eq. 0) then

        ! head, tail are the newly added node
        allocate(this % tail, source = newnode)        
        allocate(this % head, source = newnode)
 
     else
              
        ! The next node of the previous tail node is the new node
        allocate(this % tail % next, source = newnode)
        
        ! Release the pointer to old tail node
        deallocate(this % tail)

        ! Now point to new tail node
        allocate(this % tail, source = newnode)

     end if

     ! The list has grown by 1
     this % length = this % length + 1
     
  end subroutine append
  
end module linked_list_class
