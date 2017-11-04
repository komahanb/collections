module map_interface

  use object_class, only: object
  
  implicit none

  private
  public :: map

  type, abstract :: map     
   contains
     procedure(size)           , deferred :: size
     procedure(is_empty)       , deferred :: is_empty
     procedure(contains_key)   , deferred :: contains_key
     procedure(contains_value) , deferred :: contains_value
     procedure(get)            , deferred :: get
     procedure(put)            , deferred :: put
     procedure(remove)         , deferred :: remove
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

     type(logical) function contains_key(this, key)
       import map
       class(map) :: this
       class(*), allocatable :: key
     end function contains_key

     type(logical) function contains_value(this, value)
       import map
       class(map) :: this
       class(*), allocatable :: value
     end function contains_value

     function get(this, key) result(val)
       import map
       class(map) :: this
       class(*), allocatable :: key
       class(*), allocatable :: val
     end function get

     subroutine put(this, key, val)
       import map
       class(map) :: this
       class(*), allocatable :: key
       class(*), allocatable :: val       
     end subroutine put

     function remove(this, key) result(val)
       import map
       class(map) :: this
       class(*), allocatable :: key
       class(*), allocatable :: val
     end function remove
     
  end interface

contains
  
end module map_interface
