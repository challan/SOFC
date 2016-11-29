FC= ifort
PN=3D_SrO_Growth_Ternary
CFLAGS=-heap-arrays -O2
all:	$(PN).out
$(PN).out: $(PN).f90
	$(FC)   $(PN).f90 -o $(PN).out  $(CFLAGS)
