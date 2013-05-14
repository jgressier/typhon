.PHONY: \
  default \
  this.library \
  this.modules \
  this.f90 \
  this.basef90 \
  this.objects \
  this.clean \
  $(LIBNAME).target \

this.modules = $(notdir $(this.f90modules:%.f90=%.target))
this.modules: $(this.modules)

this.basef90 = $(notdir $(this.f90))
this.objects = $(this.basef90:%.f90=%.o)

$(LIBNAME).objects = $(this.objects:%=$(PRJOBJDIR)/%)
$(LIBNAME).target: $($(LIBNAME).objects)

depends.make: Makefile $(this.f90) $(this.f90exe)
	$(MAKEDEPENDS)

this.clean:
	-rm $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

