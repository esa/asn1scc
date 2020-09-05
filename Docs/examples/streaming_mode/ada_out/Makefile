all:
	gprbuild -gnat2012 -P GPS_project.gpr mainprogram.adb --subdirs=debug -cargs -g -O0 -gnatwe -gnatwa
	touch builtWithoutCoverage
	
coverage:
	if [ -f builtWithoutCoverage ] ; then make clean ; fi
	gprbuild -gnat2012 -P GPS_project.gpr mainprogram.adb --subdirs=debug -cargs -g -O0 -gnatf -gnaty -gnatg -fstack-check -gnatwe -gnatwa -fprofile-arcs -ftest-coverage -largs -fprofile-arcs
	cd obj_x86/debug ; ./mainprogram
	cd obj_x86/debug ; gcov adaasn1rtl taste_dataview
	
clean:
	rm -f builtWithoutCoverage ;
	gnat clean -r -P GPS_project.gpr --subdirs=debug	