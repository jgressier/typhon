.PHONY: \
  default \
  this.library \
  this.modules \
  this.clean \

##GG:>>>
##GG: removed from .PHONY list
##  $(LIBNAME).target \
##GG:<<<

$(LDIR).MODTRGTS := $(notdir $($(LDIR).f90MODFILES:%.f90=%.target))

$(LDIR).f90names := $(notdir $($(LDIR).f90files))

$(LDIR).objnames := $($(LDIR).f90names:%.f90=%.o)

$(LDIR).objfiles := $($(LDIR).objnames:%=$(PRJOBJDIR)/%)

$(LIBNAME).objfiles := $($(LDIR).objfiles)

##GG:>>>
##GG: dependency removed
##$(LIBNAME).target: $($(LDIR).objfiles)
##GG:<<<

this.modules: $($(LDIR).MODTRGTS)

depends.make: Makefile $($(LDIR).f90files) $($(LDIR).f90exefiles)
	$(MAKEDEPENDS)

this.clean:
	-rm $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

##this.library: $($(LDIR).objfiles)

##GG:>>>
##GG: dependency added
$(PRJLIBDIR)/$(LIBNAME).$(LIBSTA): $($(LDIR).objfiles)
##GG:<<<

