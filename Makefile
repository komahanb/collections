default:
	gfortran-7 -c object_class.f90
	gfortran-7 -c string_class.f90
	gfortran-7 -c iterator_interface.f90
	gfortran-7 -c collection_interface.f90
	gfortran-7 -c abstract_collection_class.f90
	gfortran-7 -c map_interface.f90
	gfortran-7 -c abstract_map_class.f90
	gfortran-7 -c test_hashcode.f90	

test:
	gfortran-7 object_class.f90 string_class.f90 iterator_interface.f90 collection_interface.f90 abstract_list_class.f90 test_hashcode.f90

clean:
	rm *.o *.mod *~
