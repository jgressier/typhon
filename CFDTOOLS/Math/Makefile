############################################################
##   Math library compilation
#
.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR := ..

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

# Directory
LDIR := Math

# Library name
LIBNAME := libcfdmath

####### Files

# List of f90 modules
$(LDIR).f90MODFILES := \
    MATH.f90          \
    VEC2D.f90         \
    VEC3D.f90         \
    FCT_CONTAINER.f90 \
    FCT_DEF.f90       \
    FCT_ENV.f90       \
    FCT_EVAL.f90      \
    FCT_FUNC.f90      \
    FCT_MATH.f90      \
    FCT_NODE.f90      \
    FCT_PARSER.f90    \

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \

# List of needed libraries
LIBDEPS := cfdmath cfdbase
LOCALLINKOPT := -L$(PRJLIBDIR) $(LIBDEPS:%=-l%)

# List of f90 program files
$(LDIR).f90exefiles := \
    check_fct_cpu.f90       \
    check_fct_env.f90       \
    check_fct_eval.f90      \
    check_fct_parser.f90    \
    check_fct_memoryleak.f90 \

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

