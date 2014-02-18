############################################################
##   Base library compilation
#
.SUFFIXES:

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
    LIBSORT.f90      \
    MESHPREC.f90     \
    PACKET.f90       \
    STRING.f90       \
    TIMER.f90        \

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

##GG:>>>
##GG: dependency modified from:
##this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)
##GG: to:
this.library: $(PRJOBJDIR) $(PRJLIBDIR) $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)
##GG: or:
##default: $(PRJOBJDIR) $(PRJLIBDIR)
##GG:<<<

include depends.make

