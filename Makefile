TARGETS = seq omp mpi

CFDTOOLS = $(addprefix cfdtools.,$(TARGETS))

clean.TARGETS = $(addprefix clean.,$(TARGETS))

.PHONY: help all $(TARGETS) cfdtools $(CFDTOOLS) clean clean.all $(clean.TARGETS)

# cleandeps cleanall

PRGBASE = typhon

help:
	@echo "-----------------------------------------------------------------"
	@echo "TYPHON & CFDtools project"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all            -> seq, omp & mpi targets"
	@echo "                    CFDtools libraries, TYPHON executables"
	@echo "    seq          ->   sequential      TYPHON executable '$(PRGBASE)-seq'"
	@echo "    omp          ->   OpenMP parallel TYPHON executable '$(PRGBASE)-omp'"
	@echo "    mpi          ->   MPI    parallel TYPHON executable '$(PRGBASE)-mpi'"
	@echo "  cfdtools       -> CFDtools libraries"
	@echo "    cfdtools.seq ->   sequential      CFDtools libraries"
	@echo "    cfdtools.omp ->   OpenMP parallel CFDtools libraries"
	@echo "    cfdtools.mpi ->   MPI    parallel CFDtools libraries"
	@echo "  clean.all      -> remove all"
	@echo "    clean.seq    ->   remove all 'seq' targets"
	@echo "    clean.omp    ->   remove all 'omp' targets"
	@echo "    clean.mpi    ->   remove all 'mpi' targets"
	@echo "  dox            -> make Doxygen documentation"
	@echo "  dox-install    -> send documentation to website"
	@echo "Options:"
	@echo "  opt=optim  -> Optimized options (default)"
	@echo "  opt=debug  -> Debugging options"
	@echo "  opt=profil -> Profiling options"
	@echo "-----------------------------------------------------------------"

ifneq ($(filter-out $(TARGETS),$(PRGEXT)),)
$(error PRGEXT = "$(PRGEXT)" must be void or belong to ( $(TARGETS:%="%") ))
endif

all: cfdtools $(TARGETS)

#cfdtools:
#	@cd CFDTOOLS ; $(MAKE) all opt=$(opt)

cfdtools: $(CFDTOOLS)

$(CFDTOOLS): cfdtools.%:
	@cd CFDTOOLS ; $(MAKE) all opt=$(opt) PRGEXT=$*

$(TARGETS): %: cfdtools.%
	@cd SOURCE ; $(MAKE) $@ opt=$(opt) PRGEXT=$*

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

dox:
	doxygen doc/doxygen.conf 2>&1 | tee doxygen.log

dox-install: dox
	rsync -az -e ssh doc/html gressier,typhon@web.sf.net:htdocs/doc/
