# ----- Compiling rules -----
#
AR = ar ru
#v (option for ar)

LIBSTA = a
LIBDYN = so

.SUFFIXES: .f .f90 .$(MODEXT) .o .$(LIBSTA) .$(LIBDYN)

# default option
opt. = $(opt.opt)
optext = $(opt.$(opt))
ifeq ($(optext),)
  $(error wrong option: '$(opt)')
endif

F90CWOPT = $(F90C) $(F90OPT)

PRJDIREXT = $(PRGEXT).$(optext)

PRJOBJDIR = Obj.$(PRJDIREXT)
PRJLIBDIR = $(PRJDIR)/Lib.$(PRJDIREXT)

# Special f90 option setting for OpenMP
#
ifeq ($(PRGEXT),omp)
  F90OPT += $(OMPOPT)
endif

# Special f90 command setting and library definition for MPI
ifeq ($(PRGEXT),mpi)
  F90CMP = $(MPIF90CMP)
  EXTMPILIB = $(MPILIB)
endif

$(PRJOBJDIR) $(PRJLIBDIR):
	mkdir -p $@

# Macro to write command only on failure
#
echoonfail = cmd="$(1)" ; $$cmd || ( echo $$cmd ; exit 1 )

$(PRJLIBDIR)/%.$(LIBSTA):
	@echo "----------------------------------------------------------------"
	@echo "* Creating $*.$(LIBSTA) library"
	@rm -f $(PRJLIBDIR)/$*.$(LIBSTA)
	@$(AR) $(PRJLIBDIR)/$*.$(LIBSTA) $($*.objfiles)
	@echo "----------------------------------------------------------------"
	@echo "* LIBRARY $(PRJLIBDIR)/$*.$(LIBSTA) successfully created"
	@echo "----------------------------------------------------------------"

$(PRJINCDIR)/%.$(MODEXT):
	@echo "* MODULE: options [$(F90OPT)] : $*.$(MODEXT) ($($*.object)) from $($*.source)"
	@$(call echoonfail, $(F90CWOPT) -c ${$*.source} -o $(PRJOBJDIR)/$($*.object))
	@mv $*.$(MODEXT) $(PRJINCDIR)

$(PRJOBJDIR)/%.o:
	@echo "* OBJECT: options [$(F90OPT)] : $*.o from $<"
	@$(call echoonfail, $(F90CWOPT) -c $< -o $@)
	@test -n "$($*.module)" && rm -f $($*.module) || :

# rule for CFDTOOLS executables
#
$(PRJEXEDIR)/%: $(PRJOBJDIR)/%.o $(LIBDEPS:%=$(PRJLIBDIR)/lib%.$(LIBSTA))
	@echo "* EXE   : options [$(F90OPT)] : $* from $<"
	$(F90CWOPT) $< $(LOCALLINKOPT) -o $(PRJEXEDIR)/$*

