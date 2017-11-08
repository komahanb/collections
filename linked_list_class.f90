module linked_list_class

  implicit none

  type :: node
     
     class(*), allocatable :: data
     type(node), allocatable :: next ! will manually type guard to node

  end type node

  type :: list
     
     class(node), allocatable :: head
     class(node), allocatable :: tail
     type(integer)            :: length
          
   contains

     procedure :: append

  end type list

  interface node
     module procedure create_node
  end interface node

  interface list
     module procedure create_list
  end interface list

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
  
  type(list) function create_list() result(this)

    if (allocated(this % head)) deallocate(this % head)
    if (allocated(this % tail)) deallocate(this % tail)
    this % length = 0
    
  end function create_list

  !===================================================================!
  ! Destructor for list
  !===================================================================!
  
  subroutine destroy_list(this)
    
    type(list), intent(inout) :: this

    print *, "list dies"

!!$    if (allocated(this % head)) then
!!$       deallocate(this % head)
!!$    else
!!$       print *, "skip head"
!!$    end if
!!$
!!$    if (allocated(this % tail)) then
!!$       if (allocated(this % tail % next)) then
!!$          deallocate(this % tail % next)
!!$       end if
!!$       deallocate(this % tail)
!!$    else
!!$       print *, "skip tail"
!!$    end if
         
  end subroutine destroy_list

  !===================================================================!
  ! Append the item to list of nodes
  !===================================================================!
  
   subroutine append(this, item)

     class(list), intent(inout) :: this     
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
