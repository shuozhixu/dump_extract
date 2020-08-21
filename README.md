From a large LAMMPS dump file, `dump.cac`, the program,  `dump_extract.f90`, extracts some atoms whose ids are stored in group_atomap.id, and then outputs these atoms to a smaller LAMMPS dump file, `dump.group`, with updated boundaries.

To compile the program:

	gfortran -o dump_extract dump_extract.f90

Then to run the program:

	./dump_extract
	
which then creats the output file.

If you use this program in your published work, please cite:

Shuozhi Xu, Liming Xiong, Qian Deng, David L. McDowell, [Mesh refinement schemes for the concurrent atomistic-continuum method](http://dx.doi.org/10.1016/j.ijsolstr.2016.03.030), Int. J. Solids Struct. 90 (2016) 144--152
