default:
	gfortran-7 -c object_class.f90
	gfortran-7 -c string_class.f90
	gfortran-7 -c hashtable_class.f90

	gfortran-7 -c iterator_interface.f90

	gfortran-7 -c collection_interface.f90
	gfortran-7 -c abstract_collection_class.f90

	gfortran-7 -c map_interface.f90
	gfortran-7 -c abstract_map_class.f90
	gfortran-7 -c hashmap_class.f90

	gfortran-7 -c list.f90

	ar rvs libcollections.a *.o *.mod

	gfortran-7 -c test_hashcode.f90
	gfortran-7 -o test_hashcode test_hashcode.o libcollections.a 

	gfortran-7 -c test_list.f90
	gfortran-7 -o test_list test_list.o libcollections.a 
clean:
	rm *.o *.mod *~ *.a
