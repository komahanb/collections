module map_interface

  use object_class, only: object
  
  implicit none

  type, abstract :: map
     
   contains
     procedure :: size
     procedure :: is_empty
  end type map

  interface
     
     type(integer) function size(this)
       import map
       class(map) :: this       
     end function size
     
     type(logical) function is_empty(this)
       import map
       class(map) :: this
     end function is_empty
     
  end interface
  
end module map_interface

