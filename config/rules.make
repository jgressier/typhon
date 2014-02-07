# ----- Compiling rules -----
#
AR = ar ru #v

LIBSTA = a
LIBDYN = so

.SUFFIXES: .f .f90 .$(MODEXT) .o .$(LIBSTA) .$(LIBDYN)

# options
#opt.opt    = optim
#opt.optim  = optim
#opt.dbg    = debug
#opt.debug  = debug
#opt.profil = profil
#opt.prof   = profil
#opt.gprof  = profil

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

ifeq ($(PRGEXT),omp)
F90OPT += $(OMPOPT)
endif
ifeq ($(PRGEXT),mpi)
F90CMP = $(MPIF90CMP)
EXTMPILIB = $(MPILIB)
endif

$(PRJOBJDIR) $(PRJLIBDIR):
	mkdir -p $@

$(PRJLIBDIR)/%.$(LIBSTA): $(PRJLIBDIR) $(PRJOBJDIR) %.target
	@echo "----------------------------------------------------------------"
	@echo "* Creating $*.$(LIBSTA) library"
	@rm -f $(PRJLIBDIR)/$*.$(LIBSTA)
	@$(AR) $(PRJLIBDIR)/$*.$(LIBSTA) $($*.objects)
	@echo "----------------------------------------------------------------"
	@echo "* LIBRARY $(PRJLIBDIR)/$*.$(LIBSTA) successfully created"
	@echo "----------------------------------------------------------------"

#$(PRJINCDIR)/%.$(MODEXT): $(PRJOBJDIR)

$(PRJINCDIR)/%.$(MODEXT):
	@echo "* MODULE: options [$(F90OPT)] : $*.$(MODEXT) from $($*.source)"
	@cmd="$(F90CWOPT) -c ${$*.source} -o $(PRJOBJDIR)/$($*.object)" ; $$cmd || ( echo $$cmd ; exit 1 )
	@mv $*.$(MODEXT) $(PRJINCDIR)

#$(PRJOBJDIR)/%.o: $(PRJOBJDIR)

$(PRJOBJDIR)/%.o:
	@echo "* OBJECT: options [$(F90OPT)] : $*.o from $<"
	@cmd="$(F90CWOPT) -c $< -o $@" ; $$cmd || ( echo $$cmd ; exit 1 )
	@test -n "$($*.module)" && rm -f $($*.module) || :

# rule for CFDTOOLS
$(PRJEXEDIR)/%: $(PRJOBJDIR)/%.o $(LIBDEPS:%=$(PRJLIBDIR)/lib%.$(LIBSTA))
	@echo "* EXE   : options [$(F90OPT)] : $* from $<"
	$(F90CWOPT) $< $(LOCALLINKOPT) -o $(PRJEXEDIR)/$*

# vpath %.$(MODEXT) $(PRJINCEXT):$(PRJINCDIR)

