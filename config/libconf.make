# IMPORTANT:
# This file must be included AFTER variable LDIR was set
# (since targets and dependencies undergo immediate evaluation)

.PHONY: \
  this.library \
  this.modules \
  this.clean \

$(LDIR).MODTRGTS := $(notdir $($(LDIR).f90MODFILES:%.f90=%.target))

$(LDIR).f90names := $(notdir $($(LDIR).f90files))

$(LDIR).objnames := $($(LDIR).f90names:%.f90=%.o)

$(LDIR).objfiles := $($(LDIR).objnames:%=$(PRJOBJDIR)/%)

$(LIBNAME).objfiles := $($(LDIR).objfiles)

this.modules: $($(LDIR).MODTRGTS)

depends.make: Makefile $($(LDIR).f90files) $($(LDIR).f90exefiles)
	$(MAKEDEPENDS)

this.clean:
	-rm $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

$(PRJLIBDIR)/$(LIBNAME).$(LIBSTA): $($(LDIR).objfiles)

