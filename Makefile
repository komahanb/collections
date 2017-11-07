default:
	gfortran-6 -c object_class.f90
	gfortran-6 -c string_class.f90
	gfortran-6 -c iterator_interface.f90
	gfortran-6 -c collection_interface.f90
	gfortran-6 -c abstract_collection_class.f90
	gfortran-6 -c map_interface.f90
	gfortran-6 -c abstract_map_class.f90	
	gfortran-6 -c hashmap_class.f90	
	gfortran-6 -c abstract_map_class.f90
	ar rvs libcollections.a *.o *.mod

	gfortran-6 -c test_hashcode.f90
	gfortran-6 -o test_hashcode test_hashcode.o libcollections.a 
clean:
	rm *.o *.mod *~ *.a
