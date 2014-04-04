############################################################
##   Models library compilation
#
.SUFFIXES:

.PHONY: this.tools this.library

#
# architecture dependent and project variables

PRJDIR := ..

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

# Directory
LDIR := Models

# Library name
LIBNAME := libcfdmodels

####### Files

# List of f90 modules
$(LDIR).f90MODFILES := \
    QUANTITY.f90     \

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \

# List of needed libraries
LIBDEPS :=
LOCALLINKOPT :=

# List of f90 program files
$(LDIR).f90exefiles := \

# Dependencies for tools
this.tools: $($(LDIR).f90exefiles:%.f90=$(PRJEXEDIR)/%)

include $(CONFIGDIR)/libconf.make

this.library: $(PRJOBJDIR) $(PRJLIBDIR) $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

include depends.make

clean:
	rm -f $(PRJOBJDIR)/*.o $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

