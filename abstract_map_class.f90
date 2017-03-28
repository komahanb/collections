submodule (map_interface) abstract_map_class
  
contains

  type(integer) function size(this)
    class(map_interface) :: this       
  end function size

  type(logical) function is_empty(this)
    class(map_inteface) :: this
  end function is_empty

end submodule abstract_map_class
