.PHONY: all help cfdtools seq mpi clean

help:
	@echo "-----------------------------------------------------------------"
	@echo "TYPHON & CFDtools project"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> CFDtools, TYPHON seq & mpi targets"
	@echo "  seq       -> sequential   Typhon executable 'Typhon-seq'"
	@echo "  mpi       -> MPI parallel Typhon executable 'Typhon-mpi'"
	@echo "  cfdtools  -> CFDtools libraries"
	@echo "  clean     -> remove all"
	@echo "Options:"
	@echo "  opt=optim -> Optimized options (default)"
	@echo "  opt=openmp-> Optimized options + OPENMP "
	@echo "  opt=debug -> Debugging options"
	@echo "  opt=profil-> Profiling options"
	@echo "-----------------------------------------------------------------"

all: cfdtools seq mpi

cfdtools:
	@(cd CFDTOOLS ; make all opt=$(opt))

seq: cfdtools
	@(cd SOURCE ; make seq opt=$(opt))

mpi: cfdtools
	@(cd SOURCE ; make mpi opt=$(opt))

clean:
	@(cd CFDTOOLS ; make clean)
	@(cd SOURCE   ; make clean)
