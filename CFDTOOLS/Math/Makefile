.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR = ..

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME := libcfdmath

# List of f90 modules
this.f90modules = \
  MATH.f90 \
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
this.f90 = $(this.f90modules) \

LIBDEPS = cfdmath cfdbase
LOCALLINKOPT = -L$(PRJLIBDIR) $(LIBDEPS:%=-l%)

# List of f90 program files
this.f90exe = \
  check_fct_cpu.f90       \
  check_fct_env.f90       \
  check_fct_eval.f90      \
  check_fct_parser.f90    \
  check_fct_memoryleak.f90 \

# Dependencies for tools
this.tools: $(this.f90exe:%.f90=$(PRJEXEDIR)/%)

include $(CONFIGDIR)/libconf.make

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

include depends.make

