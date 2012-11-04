# ----- Compiling rules ----- 
#
AR=ar ru
LIBSTA=a
LIBDYN=so

.SUFFIXES: .f .f90 .$(MODEXT) .o .$(LIBSTA) .$(LIBDYN)

   dir.opt=optim
 dir.optim=optim
   dir.dbg=debug
 dir.debug=debug
dir.profil=profil
  dir.prof=profil
 dir.gprof=profil
   dir.omp=openmp
dir.openmp=openmp

# default
dir.=$(dir.opt)

PRJOBJDIR:=Obj.$(dir.$(opt))

$(PRJOBJDIR):
	mkdir -p $(PRJOBJDIR)

$(PRJLIBDIR)/%.$(LIBSTA): $(PRJOBJDIR) %.target
	@echo ---------------------------------------------------------------
	@echo \* Creating $*.$(LIBSTA) library
	@touch $(PRJLIBDIR)/$*.$(LIBSTA) ; rm $(PRJLIBDIR)/$*.$(LIBSTA)
	@$(AR) $(PRJLIBDIR)/$*.$(LIBSTA) $($*.objects)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(PRJLIBDIR)/$*.$(LIBSTA) successfully created
	@echo ---------------------------------------------------------------

$(PRJINCDIR)/%.$(MODEXT): 
	@echo \* MODULE: options [$(F90OPT)] : $*.$(MODEXT) from $($*.source)
	@if [[ ! -d $(PRJOBJDIR) ]] ; then mkdir $(PRJOBJDIR) ; fi
	@$(F90C) $(F90OPT) $(PRJINCOPT) -c ${$*.source} -o $(PRJOBJDIR)/$($*.object)
	@mv $*.$(MODEXT) $(PRJINCDIR)

$(PRJOBJDIR)/%.o: 
	@echo \* OBJECT: options [$(F90OPT)] : $*.o from $<
	@if [[ ! -d $(PRJOBJDIR) ]] ; then mkdir $(PRJOBJDIR) ; fi
	@command="$(F90C) $(F90OPT) $(PRJINCOPT) -c $< -o $(PRJOBJDIR)/$*.o" ; $$command || ( echo $$command ; exit 1 )

$(PRJEXEDIR)/%: $(PRJOBJDIR)/%.o $(LIBDEPS:%=$(PRJLIBDIR)/lib%.$(LIBSTA))
	@echo \* "EXE   ": options [$(F90OPT)] : $* from $<
	@$(F90C) $(F90OPT) $(PRJINCOPT)  $< $(LOCALLINKOPT) -o $(PRJEXEDIR)/$*

# vpath %.$(MODEXT) $(PRJINCEXT):$(PRJINCDIR)
