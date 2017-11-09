default:
	gfortran-7 -g -c object_class.f90
	gfortran-7 -g -c string_class.f90
	gfortran-7 -g -c hashtable_class.f90

	gfortran-7 -g -c iterator_interface.f90

	gfortran-7 -g -c collection_interface.f90
	gfortran-7 -g -c abstract_collection_class.f90

	gfortran-7 -g -c map_interface.f90
	gfortran-7 -g -c abstract_map_class.f90
	gfortran-7 -g -c hashmap_class.f90

	gfortran-7 -g -c linked_list_class.f90

	ar rvs libcollections.a *.o *.mod


	gfortran-7 -g -c test_hashcode.f90
	gfortran-7 -g -o test_hashcode test_hashcode.o libcollections.a 

	gfortran-7 -g -c test_hashtable.f90
	gfortran-7 -g -o test_hashtable test_hashtable.o libcollections.a

	gfortran-7 -g -c test_list.f90
	gfortran-7 -g -o test_list test_list.o libcollections.a 
clean:
	rm *.o *.mod *~ *.a test_hashcode test_hashtable test_list
