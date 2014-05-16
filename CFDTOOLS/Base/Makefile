############################################################
##   Base library compilation
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
LDIR := Base

# Library name
LIBNAME := libcfdbase

####### Files

# List of f90 modules
$(LDIR).f90MODFILES := \
    COLOR.f90        \
    CONNECTIVITY.f90 \
    CONNECT_CSR.f90  \
    ENDIAN.f90       \
    FTNARGS.f90      \
    IO_UNIT.f90      \
    IOCFD.f90        \
    LIBRLE.f90       \
    LIBSORT.f90      \
    MPICOMM.f90      \
    PACKET.f90       \
    STRING.f90       \
    TIMER.f90        \
    TYPHMAKE.f90     \

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

