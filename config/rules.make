# ----- Compiling rules ----- 
#
AR=ar ruv
LIBSTA=a
LIBDYN=so

.SUFFIXES: .f .f90 .$(MODEXT) .o .$(LIBSTA) .$(LIBDYN)

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
	@$(F90C) $(F90OPT) -I$(PRJINCEXT) -c ${$*.source} -o $(PRJOBJDIR)/$($*.object)
	@mv $*.$(MODEXT) $(PRJINCDIR)

$(PRJOBJDIR)/%.o: 
	@echo \* OBJECT: options [$(F90OPT)] : $*.o from $<
	@if [[ ! -d $(PRJOBJDIR) ]] ; then mkdir $(PRJOBJDIR) ; fi
	@$(F90C) $(F90OPT) -I$(PRJINCEXT) -c $< -o $(PRJOBJDIR)/$*.o

$(PRJEXEDIR)/%: $(PRJOBJDIR)/%.o $(LIBDEPS:%=$(PRJLIBDIR)/lib%.$(LIBSTA))
	@echo \* "EXE   ": options [$(F90OPT)] : $* from $<
	$(F90C) $(F90OPT) -I$(PRJINCEXT)  $< $(LOCALLINKOPT) -o $(PRJEXEDIR)/$*

vpath %.$(MODEXT) $(PRJINCEXT):$(PRJINCDIR)
