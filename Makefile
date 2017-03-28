default:
	gfortran-5 -c object_class.f90
	gfortran-5 -c string_class.f90
	gfortran-5 -c iterator_interface.f90
	gfortran-5 -c collection_interface.f90
	gfortran-5 -c map_interface.f90
	gfortran-5 -c abstract_map_class.f90
	gfortran-5 -c test_hashcode.f90

test:
	gfortran-5 object_class.f90 string_class.f90 iterator_interface.f90 collection_interface.f90 abstract_list_class.f90 test_hashcode.f90

clean:
	rm *.o *.mod *~
