# Makefile that builts a library lib$(LIB).a from all
# of the Fortran files found in the current directory.
# Usage: make LIB=<libname>

LIB=csom
FC=gfortran
SUFFIX=dylib
FFLAGS=-fPIC -O3
FSHARED=-shared

OBJ=$(patsubst %.f95,%.o,$(shell ls *.f95))
all: lib$(LIB).$(SUFFIX)
$(OBJ):
	$(FC) -c $(FFLAGS) $(FSHARED) $(patsubst %.o,%.f95,$(@F)) -o $@
lib$(LIB).$(SUFFIX): $(OBJ)
	$(FC) -o lib$(LIB).$(SUFFIX) -shared $?
clean:
	rm *.o *.$(SUFFIX)






