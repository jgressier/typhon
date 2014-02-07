TARGETS = seq omp mpi

CFDTOOLS = $(addprefix cfdtool.,$(TARGETS))

clean.TARGETS = $(addprefix clean.,all $(TARGETS))

.PHONY: help all $(TARGETS) cfdtools $(CFDTOOLS) clean $(clean.TARGETS)

# cleandeps cleanall

PRGBASE = typhon

help:
	@echo "-----------------------------------------------------------------"
	@echo "TYPHON & CFDtools project"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> CFDtools, TYPHON seq, omp & mpi targets"
	@echo "  seq       -> sequential      $(PRGBASE) executable '$(PRGBASE)-seq'"
	@echo "  omp       -> OpenMP parallel $(PRGBASE) executable '$(PRGBASE)-omp'"
	@echo "  mpi       -> MPI    parallel $(PRGBASE) executable '$(PRGBASE)-mpi'"
	@echo "  cfdtools  -> CFDtools libraries"
	@echo "  clean     -> remove all"
	@echo "Options:"
	@echo "  opt=optim -> Optimized options (default)"
	@echo "  opt=debug -> Debugging options"
	@echo "  opt=profil-> Profiling options"
	@echo "-----------------------------------------------------------------"

all: cfdtools $(TARGETS)

#cfdtools:
#	@cd CFDTOOLS ; $(MAKE) all opt=$(opt)

cfdtools: $(CFDTOOLS)

$(CFDTOOLS): cfdtool.%:
	@cd CFDTOOLS ; $(MAKE) all opt=$(opt) PRGEXT=$*

#$(TARGETS): cfdtools
#	@cd SOURCE ; $(MAKE) $@ opt=$(opt) PRGEXT=$@

$(TARGETS): %: cfdtool.%
	@cd SOURCE ; $(MAKE) $@ opt=$(opt) PRGEXT=$@

clean:
	@echo
	@echo "clean targets:"
	@echo "  clean.all       -> clean seq, omp & mpi targets"
	@echo "  clean.seq       -> clean seq    targets"
	@echo "  clean.omp       -> clean openmp targets"
	@echo "  clean.mpi       -> clean mpi    targets"

clean.all: $(clean.TARGETS)

$(clean.TARGETS): clean.%:
	@cd CFDTOOLS ; $(MAKE) $@ opt=$(opt) PRGEXT=$*
	@cd SOURCE   ; $(MAKE) $@ opt=$(opt) PRGEXT=$*

#cleandeps:
#	@cd CFDTOOLS ; $(MAKE) cleandeps
#	@cd SOURCE   ; $(MAKE) cleandeps

#cleanall: clean cleandeps

