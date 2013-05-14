# ----- Compiling rules -----
#
AR = ar ru #v

LIBSTA = a
LIBDYN = so

.SUFFIXES: .f .f90 .$(MODEXT) .o .$(LIBSTA) .$(LIBDYN)

dir.opt    = optim
dir.optim  = optim
dir.dbg    = debug
dir.debug  = debug
dir.profil = profil
dir.prof   = profil
dir.gprof  = profil
dir.omp    = openmp
dir.openmp = openmp

# default
dir. = $(dir.opt)

F90CWOPT = $(F90C) $(F90OPT) $(PRJINCOPT)

PRJOBJDIR := Obj.$(dir.$(opt))

$(PRJOBJDIR):
	mkdir -p $(PRJOBJDIR)

$(PRJLIBDIR)/%.$(LIBSTA): $(PRJOBJDIR) %.target
	@echo "----------------------------------------------------------------"
	@echo "* Creating $*.$(LIBSTA) library"
	@rm -f $(PRJLIBDIR)/$*.$(LIBSTA)
	@$(AR) $(PRJLIBDIR)/$*.$(LIBSTA) $($*.objects)
	@echo "----------------------------------------------------------------"
	@echo "* LIBRARY $(PRJLIBDIR)/$*.$(LIBSTA) successfully created"
	@echo "----------------------------------------------------------------"

$(PRJINCDIR)/%.$(MODEXT): $(PRJOBJDIR)

$(PRJINCDIR)/%.$(MODEXT):
	@echo "* MODULE: options [$(F90OPT)] : $*.$(MODEXT) from $($*.source)"
	@command="$(F90CWOPT) -c ${$*.source} -o $(PRJOBJDIR)/$($*.object)" ; $$command || ( echo $$command ; exit 1 )
	@mv $*.$(MODEXT) $(PRJINCDIR)

$(PRJOBJDIR)/%.o: $(PRJOBJDIR)

$(PRJOBJDIR)/%.o:
	@echo "* OBJECT: options [$(F90OPT)] : $*.o from $<"
	@command="$(F90CWOPT) -c $< -o $(PRJOBJDIR)/$*.o" ; $$command || ( echo $$command ; exit 1 )

$(PRJEXEDIR)/%: $(PRJOBJDIR)/%.o $(LIBDEPS:%=$(PRJLIBDIR)/lib%.$(LIBSTA))
	@echo "* EXE   : options [$(F90OPT)] : $* from $<"
	$(F90CWOPT) $< $(LOCALLINKOPT) -o $(PRJEXEDIR)/$*

# vpath %.$(MODEXT) $(PRJINCEXT):$(PRJINCDIR)

