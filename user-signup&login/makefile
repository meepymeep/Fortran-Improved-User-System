compileAndrun:
	gfortran -c user.f90
	gfortran -c util.f90
	gfortran -c execution.f90
	gfortran -c main.f90
	gfortran -o main main.o user.o util.o execution.o
	./main
clean:
	$(RM) main
	$(RM) main.o
	$(RM) util.o
	$(RM) user.o
	$(RM) execution.o
	$(RM) util.mod
	$(RM) user_mod.mod
	$(RM) execute_cmd.mod
