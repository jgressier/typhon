help:
	@echo "-----------------------------------------------------------------"
	@echo "TYPHON & CFDtools projetct"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> CFDtools, TYPHON seq & mpi targets"
	@echo "  seq       -> sequential   Typhon executable 'Typhon-seq'"
	@echo "  mpi       -> MPI parallel Typhon executable 'Typhon-mpi'"
	@echo "  cfdtools  -> CFDtools libraries
	@echo "Options:"
	@echo "  opt=optim -> Optimized options (default)"
	@echo "  opt=debug -> Debugging options"
	@echo "  opt=prof  -> Profiling options"
	@echo "-----------------------------------------------------------------"

all: cfdtools seq mpi

cfdtools:
	@(cd CFDTOOLS ; make all opt=$(opt))

seq:
	@(cd SOURCE ; make seq opt=$(opt))

mpi:
	@(cd SOURCE ; make mpi opt=$(opt))
